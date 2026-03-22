#Ví dụ build-in function
numbers <- c(1,2,3,4,5)
mean(numbers)
sum(numbers)
length(numbers)


#ví dụ user-defind function (sẽ học sau)
calculate_average


# funtion không có tham số



# ==============================================================================
# BÀI TẬP THỰC HÀNH
# ==============================================================================

# ------------------------------------------------------------------------------
# Bài tập 1: Function cơ bản
# ------------------------------------------------------------------------------

# 1. Viết function tính diện tích hình chữ nhật
# Input: chiều dài, chiều rộng
# Output: diện tích
s_hcn<- function(a, b) {
  return(a*b)
}
s_hcn(10,2)
# 2. Viết function tính chu vi hình tròn
# Input: bán kính
# Output: chu vi
circle_cirumference<- function(r) {
  return(2*r* pi)
}
circle_cirumference(5)
# 3. Viết function chuyển đổi nhiệt độ từ Celsius sang Fahrenheit
# Công thức: F = C * 9/5 + 32
C_to_F<- function(c){
  return(c*9/5 +32)
}
C_to_F(1)
# ------------------------------------------------------------------------------
# Bài tập 2: Function với validation
# ------------------------------------------------------------------------------

# 1. Viết function kiểm tra số chẵn/lẻ
# Input: một số nguyên
# Output: "Chẵn" hoặc "Lẻ"
# Validate: input phải là số nguyên
check_even_old<- function(x){
  #kt kiểu dữ liệu
  if (!is.numeric(x) || length(x) != 1 || is.na(x)){
    stop("Nhập lại")
  }
  #Kt có phải số nguyên không
  if (x %% 1 != 0) {
    stop(" không phải số nguyên")
  }
  # xác định chẵn/lẻ
  if (x %% 2 == 0){
    return(paste(x, "là số chẵn"))
  } else {
    return(paste(x, "là số lẻ"))
  }
}
check_even_old(3)
# 2. Viết function tính điểm trung bình
# Input: vector điểm số
# Output: điểm trung bình
# Validate: 
#   - Điểm phải từ 0 đến 10
#   - Loại bỏ giá trị NA
calculate_average_score <- function(scores) {
  # Remove NA values
  scores <- scores[!is.na(scores)]
  
  # Validate: Filter valid scores (0 to 10)
  valid_scores <- scores[scores >= 0 & scores <= 10]
  
  # Return result
  if(length(valid_scores) == 0) {
    return(NA) # Return NA if no valid scores exist
  }
  
  return(mean(valid_scores))
}
score<- c(8, 7.5, 9, NA)
calculate_average_score(score)
  
}
# ------------------------------------------------------------------------------
# Bài tập 3: Function thống kê
# ------------------------------------------------------------------------------

# 1. Viết function tính toán tổng quan
# Input: vector số
# Output: list(mean, median, sd, min, max, range)
calculate_summary_stats <- function(x) {
  # 1. Validation: Kiểm tra đầu vào có phải là số không
  if (!is.numeric(x)) {
    stop("Lỗi: Dữ liệu đầu vào phải là số (numeric)")
  }
  
  # 2. Xử lý NA: Lọc bỏ các giá trị thiếu để hàm không bị lỗi
  x_clean <- x[!is.na(x)]
  if (length(x_clean) == 0) {
    return("Không có dữ liệu hợp lệ để tính toán")
  }
  
  # 3. Tính toán và gộp vào một list để trả về nhiều giá trị cùng lúc
  ket_qua <- list(
    mean = mean(x_clean),
    median = median(x_clean),
    sd = sd(x_clean),
    min = min(x_clean),
    max = max(x_clean),
    range = max(x_clean) - min(x_clean) # Khoảng biến thiên
  )
  
  return(ket_qua)
}
calculate_summary_stats(c(15, 20, 25, 30, NA))
# 2. Viết function tính hoán vị P(n, r)
# Công thức: P(n,r) = n! / (n-r)!
calculate_permutation <- function(n, r){
  if (n < 0 || r < 0 || r > n) {
    stop(" Lỗi: số lượng k hợp lệ")
  }
  # hàm factorial() trong R dùng để tính giai thừa (!)
  p_result <- factorial(n) / factorial(n - r)
  return(p_result)
}
calculate_permutation(5, 3)
# 3. Viết function tính tổ hợp C(n, r)
# Công thức: C(n,r) = n! / (r! * (n-r)!)
calculate_combination <- function(n, r) {
  if (n < 0 || r < 0 || r > n ) {
    stop("Lỗi")
  }
  c_result <- factorial(n) / (factorial(r) * factorial(n - r))
  return(c_result)
}
calculate_combination(5, 3)
# ------------------------------------------------------------------------------
# Bài tập 4: Function nâng cao
# ------------------------------------------------------------------------------

# 1. Viết function tìm các số nguyên tố từ 1 đến n
# Input: n
# Output: vector các số nguyên tố
find_primes_upto_n <- function(n) {
  if (n < 2) return(NULL)
  # KT 1 số 
  is_prime <- function(x) {
    if (x == 2) return( TRUE)
    if (x %% 2 == 0) return(FALSE)
    for (i in 3:sqrt(x)) {
      if (x %% i == 0) return(FALSE)
    }
    return(TRUE)
  }
  
  #TẠo vector rỗng chứa kq
  primes <- c()
  for (i in 2:n) {
    if (is_prime(i)) {
      primes <- c(primes, i)
    }
  }
  return(primes)
}
find_primes_upto_n(20)
# 2. Viết function tạo tam giác Pascal với n hàng
# Gợi ý: Sử dụng tổ hợp C(n, k)
pascal_triangle <- function(n) {
  if (n <= 0) stop("Số hàng phải lớn hơn 0")
  
  for (i in 0:(n-1)) {
    row_values <- c()
    for (k in 0:i) {
      row_values <- c(row_values, choose(i, k))
    }
    # In từng dòng ra console
    print(row_values)
  }
}
pascal_triangle(5)
# 3. Viết function phân loại sinh viên dựa vào điểm
# Input: điểm số
# Output: xếp loại (Xuất sắc, Giỏi, Khá, TB, Yếu)
# Kèm theo GPA scale 4.0
classify_student <- function(score) {
  if (score < 0 || score > 10 || is.na(score)) {
    return("Điểm không hợp lệ")
  }
  
  if (score >= 9.0) {
    return(list(XepLoai = "Xuất sắc", GPA = 4.0))
  } else if (score >= 8.0) {
    return(list(XepLoai = "Giỏi", GPA = 3.5))
  } else if (score >= 7.0) {
    return(list(XepLoai = "Khá", GPA = 3.0))
  } else if (score >= 5.0) {
    return(list(XepLoai = "Trung bình", GPA = 2.0))
  } else {
    return(list(XepLoai = "Yếu", GPA = 0.0))
  }
}
classify_student(8.5)
# ------------------------------------------------------------------------------
# Bài tập 5: Ứng dụng thực tế
# ------------------------------------------------------------------------------

# 1. Viết function tính lương ròng
# Input: lương cơ bản, phụ cấp, số ngày làm việc, số giờ tăng ca
# Output: lương ròng sau thuế
calculate_net_salary <- function(base_salary, allowance, worked_days, overtime_hours) {
  daily_rate <- base_salary / 22
  hourly_rate <- daily_rate / 8
  
  # Tính tổng thu nhập (Gross)
  gross_income <- (daily_rate * worked_days) + allowance + (overtime_hours * hourly_rate * 1.5)
  
  # Tính thuế thu nhập (đơn giản hóa)
  tax <- ifelse(gross_income > 11000000, (gross_income - 11000000) * 0.1, 0)
  
  net_income <- gross_income - tax
  return(net_income)
}
calculate_net_salary(
  base_salary = 22000000, 
  allowance = 3000000,
  worked_days = 22,
  overtime_hours = 8)
# 2. Viết function chuẩn hóa điểm thi
# Input: vector điểm thô
# Output: vector điểm chuẩn hóa (0-100)
# Công thức: (điểm - min) / (max - min) * 100
normalize_scores <- function(scores) {
  scores <- scores[!is.na(scores)] # Bỏ NA
  min_val <- min(scores)
  max_val <- max(scores)
  
  # Xử lý trường hợp tất cả các điểm đều bằng nhau để tránh lỗi chia cho 0
  if (min_val == max_val) {
    return(rep(100, length(scores))) 
  }
  
  normalized <- (scores - min_val) / (max_val - min_val) * 100
  return(normalized)
}
normalize_scores(c(4, 6, 8, 10))
# 3. Viết function phân tích dữ liệu sinh viên
# Input: data frame (tên, tuổi, điểm)
# Output: thống kê mô tả đầy đủ
analyze_student_data <- function(df) {
  # Validate: Đảm bảo Data Frame có đúng các cột cần thiết
  required_cols <- c("Ten", "Tuoi", "Diem")
  if (!all(required_cols %in% colnames(df))) {
    stop("Data frame phải có các cột: Ten, Tuoi, Diem")
  }
  
  stats <- list(
    Tong_SV = nrow(df),
    Tuoi_Trung_Binh = mean(df$Tuoi, na.rm = TRUE),
    Diem_Trung_Binh = mean(df$Diem, na.rm = TRUE),
    Diem_Cao_Nhat = max(df$Diem, na.rm = TRUE),
    Sinh_Vien_Xuat_Sac = df$Ten[df$Diem >= 9] # Lọc tên SV điểm >= 9
  )
  
  return(stats)
}
df_sv <- data.frame(Ten = c("An", "Bình", "Châu"), Tuoi = c(20, 21, 20), Diem = c(7.5, 9.2, 8.8))
analyze_student_data(df_sv)
# ==============================================================================
# TÀI LIỆU THAM KHẢO
# ==============================================================================

# 1. R Documentation - Functions: ?function
# 2. Advanced R - Functions: https://adv-r.hadley.nz/functions.html
# 3. R for Data Science - Functions: https://r4ds.had.co.nz/functions.html
# 4. Quick-R - Creating Functions: https://www.statmethods.net/management/userfunctions.html

# ==============================================================================
# TỔNG KẾT
# ==============================================================================

# Những điểm cần nhớ:
# 1. ✅ Function giúp code dễ đọc, dễ bảo trì và tái sử dụng
# 2. ✅ Sử dụng tên function có ý nghĩa (động từ + danh từ)
# 3. ✅ Tham số mặc định giúp function linh hoạt hơn
# 4. ✅ Luôn validate input trước khi xử lý
# 5. ✅ Function nên ngắn gọn, tập trung vào một nhiệm vụ
# 6. ✅ Viết documentation cho function phức tạp
# 7. ✅ Sử dụng return() rõ ràng hoặc để R tự return biểu thức cuối

# Quy trình viết function tốt:
# 1. Xác định mục đích: Function này làm gì?
# 2. Thiết kế input/output: Cần tham số gì? Trả về gì?
# 3. Viết code đơn giản: Viết logic cơ bản trước
# 4. Thêm validation: Kiểm tra input hợp lệ
# 5. Test kỹ lưỡng: Thử với nhiều trường hợp khác nhau
# 6. Refactor: Cải thiện code, thêm tính năng
# 7. Document: Viết hướng dẫn sử dụng

# Lưu ý quan trọng:
# - Function là nền tảng của lập trình R
# - Thực hành viết function thường xuyên để thành thạo
# - Đọc code của người khác để học cách viết function tốt
# - Các function trong bài học tiếp theo (Thống kê) đều sử dụng các nguyên tắc này
