# 1. Giới thiệu Dataset
# Tên dataset: Cars93
# Nguồn: Package 'MASS' (mặc định của R)
# Mô tả: Dữ liệu về 93 mẫu xe ô tô được bán tại Mỹ năm 1993.
# 
# Các biến quan trọng (27 biến):
#   Manufacturer: Hãng sản xuất
#   Type: Loại xe (Small, Sporty, Compact, Midsize, Large, Van)
#   Price: Giá xe (Midrange)
#   MPG.city / MPG.highway: Mức tiêu thụ nhiên liệu (Thành phố / Cao tốc)
#   AirBags: Tình trạng túi khí
#   Cylinders: Số xi-lanh (Lưu ý: Có cả chữ "rotary" lọt vào)
#   Luggage.room: Sức chứa hành lý (Có chứa Missing Values - NA)
#   Origin: Nguồn gốc xe (USA hoặc non-USA)
#   ... và nhiều biến khác về thông số kỹ thuật (Weight, Length, v.v.)
# ==========================================

# 2.1. BƯỚC 1: Load và Khám phá Dữ liệu

# Load thư viện MASS và gọi dataset

library(MASS)       # Chứa dataset Cars93
# library(stringr)    # Xử lý chuỗi/text chuyên nghiệp

# Load dữ liệu
data(Cars93)
df <- Cars93 # Copy ra biến mới để bảo toàn data gốc

# In ra kích thước ban đầu
cat("Kích thước ban đầu:", nrow(df), "dòng,", ncol(df), "cột\n")

# ==============================================================================
# PHẦN 2: ĐỊNH NGHĨA CÁC HÀM TỰ TẠO (CUSTOM UTILITY FUNCTIONS)
# Viết hàm giúp code gọn gàng, có thể tái sử dụng cho các project sau này.
# ==============================================================================

# 2.1. Hàm tóm tắt Missing Data
# Trả về một data frame hiển thị tỷ lệ % dữ liệu thiếu của từng cột
summarize_missing <- function(data) {
  missing_count <- sapply(data, function(x) sum(is.na(x)))
  missing_percent <- round((missing_count / nrow(data)) * 100, 2)
  result <- data.frame(
    Variable = names(data),
    Missing_Count = missing_count,
    Missing_Percent = missing_percent
  )
  # Chỉ hiện những cột có dữ liệu thiếu
  result <- result[result$Missing_Count > 0, ]
  return(result[order(-result$Missing_Count), ])
}

# 2.2. Hàm xử lý Outliers (Winsorization)
# Thay thế các giá trị quá lớn/quá nhỏ bằng giá trị ở ngưỡng phần trăm (percentile) nhất định
cap_outliers <- function(x, lower_percentile = 0.05, upper_percentile = 0.95) {
  if (!is.numeric(x)) return(x) # Bỏ qua nếu không phải biến định lượng
  
  qnt <- quantile(x, probs = c(lower_percentile, upper_percentile), na.rm = TRUE)
  capped_x <- x
  capped_x[x < qnt[1]] <- qnt[1]
  capped_x[x > qnt[2]] <- qnt[2]
  
  return(capped_x)
}

# 2.3. Hàm tính Mode (Giá trị xuất hiện nhiều nhất) dùng cho biến phân loại
get_mode <- function(v) {
  uniqv <- unique(v[!is.na(v)])
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


# ==============================================================================
# PHẦN 3: CHUẨN HÓA CẤU TRÚC VÀ TÊN CỘT (STRUCTURAL CLEANING)
# ==============================================================================

# 3.1. Chuẩn hóa tên cột (đưa về chuẩn snake_case)
# Ví dụ: "MPG.city" thành "mpg_city", "Rear.seat.room" thành "rear_seat_room"
old_names <- names(df)
new_names <- tolower(old_names)               # Chuyển hết thành chữ thường
new_names <- gsub("\\.", "_", new_names)      # Thay dấu chấm bằng dấu gạch dưới
names(df) <- new_names

# Kiểm tra lại tên cột
print("Tên cột sau khi chuẩn hóa:")
print(names(df))

# 3.2. Loại bỏ các cột không mang lại giá trị phân tích tĩnh
# Ví dụ cột 'make' (kết hợp của manufacturer và model) có thể gây trùng lặp thông tin
df$make <- NULL 


# ==============================================================================
# PHẦN 4: CHUẨN HÓA CHUỖI VÀ LỖI NHẬP LIỆU (DATA ENTRY ERRORS)
# ==============================================================================

# 4.1. Xử lý cột cylinders (Số xi lanh)
# Cột này bị dính text "rotary" khiến R nhận dạng nó là Factor thay vì Numeric
table(df$cylinders) # Xem chi tiết

# Biến "rotary" (động cơ xoay, không có xi lanh truyền thống) thành NA để xử lý sau
df$cylinders <- as.character(df$cylinders)
df$cylinders[df$cylinders == "rotary"] <- NA

# Chuyển cột về đúng định dạng số
df$cylinders <- as.numeric(df$cylinders)

# 4.2. Chuẩn hóa các cột chuỗi (Trim khoảng trắng dư thừa nếu có)
# Giả sử người nhập liệu vô tình gõ khoảng trắng ở đầu/cuối: " Toyota ", "USA "
char_cols <- sapply(df, is.character)
df[char_cols] <- lapply(df[char_cols], function(x) str_trim(x, side = "both"))


# ==============================================================================
# PHẦN 5: XỬ LÝ MISSING DATA (ADVANCED IMPUTATION)
# Thay vì điền Mean/Median cho toàn bộ, ta điền theo nhóm (Grouped Imputation)
# ==============================================================================

# Kiểm tra tình trạng Missing hiện tại
print("Báo cáo Missing Data trước khi xử lý:")
print(summarize_missing(df))

# 5.1. Xử lý missing cột 'luggage_room' (Sức chứa hành lý)
# Lý do thiếu: Xe Van hoặc xe tải nhỏ không có cốp kín.
# Giải pháp: Gán giá trị 0 cho những xe dạng "Van", "Sporty" không có dữ liệu
missing_luggage <- is.na(df$luggage_room)
df$luggage_room[missing_luggage & df$type %in% c("Van", "Sporty")] <- 0

# Nếu vẫn còn thiếu ở các dòng xe khác, điền bằng Median của NHÓM XE (Type) đó
types <- unique(df$type)
for (t in types) {
  group_median <- median(df$luggage_room[df$type == t], na.rm = TRUE)
  condition <- is.na(df$luggage_room) & df$type == t
  df$luggage_room[condition] <- group_median
}

# 5.2. Xử lý missing cột 'rear_seat_room' (Không gian ghế sau)
# Điền bằng giá trị Mean của cùng loại xe (Type)
for (t in types) {
  group_mean <- mean(df$rear_seat_room[df$type == t], na.rm = TRUE)
  condition <- is.na(df$rear_seat_room) & df$type == t
  df$rear_seat_room[condition] <- round(group_mean, 1) # Làm tròn 1 chữ số
}

# 5.3. Xử lý missing cột 'cylinders' (Vừa tạo ra ở Phần 4.1)
# Động cơ rotary thường tương đương sức mạnh máy 4 hoặc 6 xi lanh. Ta điền bằng Mode.
df$cylinders[is.na(df$cylinders)] <- get_mode(df$cylinders)

# Kiểm tra lại xem đã sạch NA chưa
cat("\nSố lượng NA còn lại trong toàn bộ dataset:", sum(is.na(df)), "\n")


# ==============================================================================
# PHẦN 6: CHUYỂN ĐỔI BIẾN PHÂN LOẠI (CATEGORICAL ENCODING)
# ==============================================================================

# 6.1. Biến 'origin' (Nguồn gốc xe) -> Factor có thứ tự hoặc đổi nhãn
df$origin <- factor(df$origin, 
                    levels = c("USA", "non-USA"),
                    labels = c("Domestic", "Imported"))

# 6.2. Biến 'type' (Loại xe) -> Ordered Factor (Biến có thứ tự từ nhỏ đến lớn)
# Sắp xếp logic theo kích thước xe trung bình
df$type <- factor(df$type, 
                  levels = c("Small", "Sporty", "Compact", "Midsize", "Large", "Van"),
                  ordered = TRUE)

# 6.3. Biến 'airbags' (Túi khí) -> Rút gọn levels
df$airbags <- factor(df$airbags,
                     levels = c("Driver & Passenger", "Driver only", "None"),
                     labels = c("Both", "Driver_Only", "None"))

# 6.4. Biến 'drive_train' (Hệ dẫn động) -> Rút gọn tên cho chuẩn code
df$drive_train <- factor(df$drive_train,
                         levels = c("4WD", "Front", "Rear"))

# 6.5. Tự động hóa chuyển Factor cho các biến Character còn lại (như Manufacturer, Model)
# Tìm tất cả các cột dạng character và chuyển sang factor
char_cols <- sapply(df, is.character)
df[char_cols] <- lapply(df[char_cols], factor)


# ==============================================================================
# PHẦN 7: KIỂM SOÁT NGOẠI LỆ (OUTLIERS TREATMENT)
# Sử dụng hàm cap_outliers đã viết ở phần 2
# ==============================================================================

# Chọn các biến định lượng (Numeric) liên tục có khả năng chứa ngoại lệ lớn
numeric_vars_to_cap <- c("price", "min_price", "max_price", "horsepower", "rev_per_mile")

# Áp dụng hàm cap_outliers qua lapply
df[numeric_vars_to_cap] <- lapply(df[numeric_vars_to_cap], cap_outliers, 
                                  lower_percentile = 0.05, 
                                  upper_percentile = 0.95)

# Lưu ý: Việc Cap này giúp các mô hình Machine Learning sau này ổn định hơn, 
# không bị nhiễu bởi những chiếc xe có giá hoặc sức ngựa quá cao/thấp một cách vô lý.


# ==============================================================================
# PHẦN 8: KỸ THUẬT ĐẶC TRƯNG (FEATURE ENGINEERING)
# Tạo ra các biến mới có giá trị phân tích cao hơn
# ==============================================================================

# 8.1. Tạo biến 'avg_mpg' (Tiêu thụ nhiên liệu trung bình)
# Kết hợp mức tiêu thụ nội thành và cao tốc (giả định chạy 55% nội thành, 45% cao tốc)
df$avg_mpg <- round((df$mpg_city * 0.55) + (df$mpg_highway * 0.45), 2)

# 8.2. Phân loại mức giá xe (Price_Category) thông qua kỹ thuật Binning
# Chia đều mức giá thành 3 nhóm dựa trên hàm quantile
price_breaks <- quantile(df$price, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE)
df$price_category <- cut(df$price, 
                         breaks = price_breaks, 
                         labels = c("Budget", "Standard", "Premium"),
                         include.lowest = TRUE)

# 8.3. Tạo biến tỷ lệ Sức mạnh trên Trọng lượng (Power_to_Weight Ratio)
# Đặc trưng cực kỳ quan trọng để dự đoán độ bốc của xe (Horsepower / Weight)
df$power_to_weight <- round(df$horsepower / df$weight, 4)

# 8.4. Nhóm hóa Hộp số (Manual Transmission Availability)
# Cột man_trans_avail đang là Yes/No, chuyển thành biến nhị phân 1/0
df$is_manual_avail <- ifelse(df$man_trans_avail == "Yes", 1, 0)


# ==============================================================================
# PHẦN 9: SANITY CHECKS VÀ XUẤT DỮ LIỆU
# Bước cuối cùng để đảm bảo pipeline chạy đúng và không sinh ra lỗi ẩn
# ==============================================================================

# 9.1. Kiểm tra cấu trúc cuối cùng
print("Cấu trúc Data Frame sau khi Clean:")
str(df)

# 9.2. Assertions (Kiểm tra bắt buộc)
# Dừng script và báo lỗi nếu dữ liệu vẫn còn NA
if(sum(is.na(df)) > 0) {
  warning("CẢNH BÁO: Dữ liệu vẫn còn chứa NA. Hãy kiểm tra lại Pipeline!")
} else {
  print("THÀNH CÔNG: Dữ liệu đã sạch bóng Missing Values.")
}

# 9.3. Xóa các biến tạm trong môi trường làm việc để giải phóng RAM (Best practice)
rm(char_cols, missing_luggage, new_names, old_names, t, types, 
   group_median, group_mean, condition, numeric_vars_to_cap, price_breaks)

# 9.4. Xuất dữ liệu đã làm sạch
# Lưu dưới dạng file CSV sẵn sàng cho phân tích/Machine Learning
# write.csv(df, "cars93_fully_cleaned_engineered.csv", row.names = FALSE)

print("PIPELINE HOÀN TẤT!")
# ===================== KẾT THÚC SCRIPT ========================================