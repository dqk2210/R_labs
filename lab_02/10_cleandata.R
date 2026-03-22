# ==============================================================================
# ĐỒ ÁN: QUY TRÌNH LÀM SẠCH VÀ CHUẨN HÓA DỮ LIỆU BẤT ĐỘNG SẢN
# Tên dataset: Ames Housing Dataset
# Đặc trưng: Hơn 80 biến số và phân loại
# ==============================================
# ==============================================================================
# ĐỒ ÁN: QUY TRÌNH LÀM SẠCH VÀ CHUẨN HÓA DỮ LIỆU BẤT ĐỘNG SẢN
# Người thực hiện: Đỗ Quốc Khánh (Tùng)
# ==============================================================================

# ==============================================================================
# MÔ TẢ BỘ DỮ LIỆU (DATASET DESCRIPTION)
# ==============================================================================
# Tên dataset: Ames Housing Dataset
# Nguồn: Được thu thập bởi Giáo sư Dean De Cock (Thay thế cho Boston Housing dataset)
# Mục tiêu phân tích: Làm sạch dữ liệu để chuẩn bị cho các mô hình Machine Learning 
#                     dự đoán giá bán bất động sản (SalePrice).
# Quy mô dữ liệu: 2930 quan sát (dòng) x 82 đặc trưng (cột)
#
# CÁC NHÓM BIẾN QUAN TRỌNG TỚI QUÁ TRÌNH PHÂN TÍCH:
#
# 1. Biến mục tiêu (Target Variable):
#    - SalePrice: Giá bán thực tế của căn nhà (USD).
#
# 2. Biến Không gian & Diện tích (Numerical):
#    - Lot.Frontage: Chiều dài mặt tiền tiếp giáp với đường (Linear feet).
#    - Lot.Area: Tổng diện tích lô đất (Square feet).
#    - Gr.Liv.Area: Diện tích không gian sống trên mặt đất.
#    - Total.Bsmt.SF: Tổng diện tích tầng hầm.
#
# 3. Biến Thời gian (Temporal):
#    - Year.Built: Năm xây dựng ban đầu.
#    - Year.Remod.Add: Năm trùng tu/sửa chữa gần nhất.
#    - Yr.Sold: Năm giao dịch bán nhà.
#
# 4. Biến Phân loại Định danh (Nominal Categorical):
#    - Neighborhood: Tên khu dân cư nằm trong thành phố Ames.
#    - Bldg.Type: Loại hình nhà ở (1Fam = Nhà đơn lẻ, Twnhs = Nhà phố...).
#    - House.Style: Kiểu kiến trúc/Số tầng (1Story, 2Story, Split-level...).
#
# 5. Biến Phân loại Thứ bậc (Ordinal Categorical):
#    - Đại diện cho các đánh giá chất lượng và tình trạng (Qual/Cond).
#    - Ví dụ: Exter.Qual (Ngoại thất), Kitchen.Qual (Bếp), Bsmt.Qual (Hầm).
#    - Thang đo chuẩn: Ex (Excellent) > Gd (Good) > TA (Typical/Average) 
#                      > Fa (Fair) > Po (Poor) > None (Không có tiện ích).
# ==============================================================================
# ------------------------------------------------------------------------------
# BƯỚC 1: LOAD VÀ KHÁM PHÁ DỮ LIỆU BAN ĐẦU
# ------------------------------------------------------------------------------

# 1.1. Đọc dữ liệu từ file csv (Đảm bảo file AmesHousing.csv cùng thư mục)
housing <- read.csv("lab_02/housing.csv")

# 1.2. Xem bảng dữ liệu tổng quát
 View(housing) 

# 1.3. Xem 6 dòng đầu tiên để biết định dạng
head(housing)

# 1.4. Kiểm tra cấu trúc các biến (Numeric, Character,...)
str(housing)

# 1.5. Tóm tắt thống kê các biến số
summary(housing)

# 1.6. Kiểm tra tên của tất cả các cột
colnames(housing)

# 1.7. Kiểm tra số lượng dòng và cột ban đầu
dim(housing)

# 1.8. Loại bỏ các cột định danh không mang giá trị dự báo (ID columns)
# Cột 1 là Order (Số thứ tự), Cột 2 là PID (Mã định danh)
housing <- housing[, -1] # Xóa cột Order
housing <- housing[, -1] # Xóa cột PID (sau khi xóa Order thì PID thành cột 1)

# Kiểm tra lại kích thước sau khi xóa
dim(housing)

# ------------------------------------------------------------------------------
# BƯỚC 2: XỬ LÝ MISSING DATA (DỮ LIỆU THIẾU)
# ------------------------------------------------------------------------------

# 2.1. Phát hiện tổng số giá trị thiếu trong toàn bộ dataset
sum(is.na(housing))

# 2.2. Đếm số dòng có ít nhất một giá trị thiếu
sum(!complete.cases(housing))

# 2.3. Hiển thị danh sách các cột bị thiếu và số lượng thiếu
colSums(is.na(housing))[colSums(is.na(housing)) > 0]

# --- XỬ LÝ BIẾN SỐ (NUMERICAL VARIABLES) ---

# 1. Biến Lot.Frontage (Chiều dài mặt tiền)
summary(housing$Lot.Frontage)
# Điền NA bằng trung vị (Median) để tránh nhiễu từ outliers
housing$Lot.Frontage[is.na(housing$Lot.Frontage)] <- median(housing$Lot.Frontage, na.rm = TRUE)

# 2. Biến Mas.Vnr.Area (Diện tích lớp ốp gạch)
# Nếu thiếu thì coi như căn nhà không có diện tích ốp (bằng 0)
housing$Mas.Vnr.Area[is.na(housing$Mas.Vnr.Area)] <- 0

# 3. Biến Garage.Yr.Blt (Năm xây gara)
# Nếu không có gara, ta lấy năm xây nhà để điền vào cho đồng bộ
housing$Garage.Yr.Blt[is.na(housing$Garage.Yr.Blt)] <- housing$Year.Built[is.na(housing$Garage.Yr.Blt)]

# --- XỬ LÝ BIẾN PHÂN LOẠI (CATEGORICAL VARIABLES) ---
# Trong dataset này, NA ở nhiều cột tiện ích có nghĩa là "Không có" (None)

# Điền giá trị "None" cho các biến tiện ích chung
housing$Pool.QC[is.na(housing$Pool.QC)] <- "None"
housing$Alley[is.na(housing$Alley)] <- "None"
housing$Fence[is.na(housing$Fence)] <- "None"
housing$Fireplace.Qu[is.na(housing$Fireplace.Qu)] <- "None"
housing$Misc.Feature[is.na(housing$Misc.Feature)] <- "None"
housing$Mas.Vnr.Type[is.na(housing$Mas.Vnr.Type)] <- "None"

# Xử lý nhóm biến Tầng hầm (Basement)
# NA nghĩa là nhà không có tầng hầm
housing$Bsmt.Qual[is.na(housing$Bsmt.Qual)] <- "None"
housing$Bsmt.Cond[is.na(housing$Bsmt.Cond)] <- "None"
housing$Bsmt.Exposure[is.na(housing$Bsmt.Exposure)] <- "None"
housing$BsmtFin.Type.1[is.na(housing$BsmtFin.Type.1)] <- "None"
housing$BsmtFin.Type.2[is.na(housing$BsmtFin.Type.2)] <- "None"

# Xử lý nhóm biến Gara (Garage)
# NA nghĩa là nhà không có Gara
housing$Garage.Type[is.na(housing$Garage.Type)] <- "None"
housing$Garage.Finish[is.na(housing$Garage.Finish)] <- "None"
housing$Garage.Qual[is.na(housing$Garage.Qual)] <- "None"
housing$Garage.Cond[is.na(housing$Garage.Cond)] <- "None"

# Xử lý biến Electrical (Hệ thống điện)
# Chỉ thiếu 1 dòng, điền bằng giá trị xuất hiện nhiều nhất (Mode) là "SBrkr"
housing$Electrical[is.na(housing$Electrical)] <- "SBrkr"

# Kiểm tra lại xem còn NA không
sum(is.na(housing))

# ------------------------------------------------------------------------------
# BƯỚC 3: CHUYỂN ĐỔI BIẾN ĐỊNH DANH (NOMINAL FACTORS)
# ------------------------------------------------------------------------------
# Chuyển đổi thủ công từng biến để kiểm soát dữ liệu chặt chẽ

housing$MS.Zoning <- factor(housing$MS.Zoning)
housing$Street <- factor(housing$Street)
housing$Alley <- factor(housing$Alley)
housing$Land.Contour <- factor(housing$Land.Contour)
housing$Lot.Config <- factor(housing$Lot.Config)
housing$Neighborhood <- factor(housing$Neighborhood)
housing$Condition.1 <- factor(housing$Condition.1)
housing$Condition.2 <- factor(housing$Condition.2)
housing$Bldg.Type <- factor(housing$Bldg.Type)
housing$House.Style <- factor(housing$House.Style)
housing$Roof.Style <- factor(housing$Roof.Style)
housing$Roof.Matl <- factor(housing$Roof.Matl)
housing$Exterior.1st <- factor(housing$Exterior.1st)
housing$Exterior.2nd <- factor(housing$Exterior.2nd)
housing$Mas.Vnr.Type <- factor(housing$Mas.Vnr.Type)
housing$Foundation <- factor(housing$Foundation)
housing$Heating <- factor(housing$Heating)
housing$Central.Air <- factor(housing$Central.Air)
housing$Electrical <- factor(housing$Electrical)
housing$Garage.Type <- factor(housing$Garage.Type)
housing$Misc.Feature <- factor(housing$Misc.Feature)
housing$Sale.Type <- factor(housing$Sale.Type)
housing$Sale.Condition <- factor(housing$Sale.Condition)

# Biến MSSubClass (Mã số loại nhà - thực chất là phân loại chứ không phải số học)
housing$MSSubClass <- factor(housing$MSSubClass)

# ------------------------------------------------------------------------------
# BƯỚC 4: CHUYỂN ĐỔI BIẾN CÓ THỨ TỰ (ORDINAL FACTORS)
# ------------------------------------------------------------------------------

# --- 4.1. Thang đo chất lượng chung ---
# Thứ tự logic: Poor < Fair < Typical < Good < Excellent
levels_quality <- c("None", "Po", "Fa", "TA", "Gd", "Ex")

housing$Exter.Qual <- factor(housing$Exter.Qual, 
                             levels = levels_quality, 
                             ordered = TRUE)

housing$Exter.Cond <- factor(housing$Exter.Cond, 
                             levels = levels_quality, 
                             ordered = TRUE)

housing$Bsmt.Qual <- factor(housing$Bsmt.Qual, 
                            levels = levels_quality, 
                            ordered = TRUE)

housing$Bsmt.Cond <- factor(housing$Bsmt.Cond, 
                            levels = levels_quality, 
                            ordered = TRUE)

housing$Heating.QC <- factor(housing$Heating.QC, 
                             levels = levels_quality, 
                             ordered = TRUE)

housing$Kitchen.Qual <- factor(housing$Kitchen.Qual, 
                               levels = levels_quality, 
                               ordered = TRUE)

housing$Fireplace.Qu <- factor(housing$Fireplace.Qu, 
                               levels = levels_quality, 
                               ordered = TRUE)

housing$Garage.Qual <- factor(housing$Garage.Qual, 
                              levels = levels_quality, 
                              ordered = TRUE)

housing$Garage.Cond <- factor(housing$Garage.Cond, 
                              levels = levels_quality, 
                              ordered = TRUE)

housing$Pool.QC <- factor(housing$Pool.QC, 
                          levels = levels_quality, 
                          ordered = TRUE)

# --- 4.2. Thang đo hình dáng thửa đất (Lot.Shape) ---
# Thứ tự: Reg (Đều) > IR1 (Hơi lệch) > IR2 (Lệch) > IR3 (Rất lệch)
housing$Lot.Shape <- factor(housing$Lot.Shape, 
                            levels = c("IR3", "IR2", "IR1", "Reg"), 
                            ordered = TRUE)

# --- 4.3. Thang đo độ dốc địa hình (Land.Slope) ---
# Thứ tự: Sev (Dốc đứng) < Mod (Hơi dốc) < Gtl (Thoải)
housing$Land.Slope <- factor(housing$Land.Slope, 
                             levels = c("Sev", "Mod", "Gtl"), 
                             ordered = TRUE)

# --- 4.4. Thang đo độ thông thoáng hầm (Bsmt.Exposure) ---
housing$Bsmt.Exposure <- factor(housing$Bsmt.Exposure, 
                                levels = c("None", "No", "Mn", "Av", "Gd"), 
                                ordered = TRUE)

# --- 4.5. Thang đo độ hoàn thiện tầng hầm (BsmtFin.Type) ---
levels_fin <- c("None", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ")

housing$BsmtFin.Type.1 <- factor(housing$BsmtFin.Type.1, 
                                 levels = levels_fin, 
                                 ordered = TRUE)

housing$BsmtFin.Type.2 <- factor(housing$BsmtFin.Type.2, 
                                 levels = levels_fin, 
                                 ordered = TRUE)

# --- 4.6. Thang đo độ hoàn thiện Gara (Garage.Finish) ---
housing$Garage.Finish <- factor(housing$Garage.Finish, 
                                levels = c("None", "Unf", "RFn", "Fin"), 
                                ordered = TRUE)

# --- 4.7. Thang đo tiện ích đường lát mặt (Paved.Drive) ---
housing$Paved.Drive <- factor(housing$Paved.Drive, 
                              levels = c("N", "P", "Y"), 
                              ordered = TRUE)

# ------------------------------------------------------------------------------
# BƯỚC 5: XỬ LÝ LỖI LOGIC VÀ TẠO ĐẶC TRƯNG MỚI (FEATURE ENGINEERING)
# ------------------------------------------------------------------------------

# 5.1. Kiểm tra và đồng nhất dữ liệu văn bản (Consistency)
# Sửa các lỗi nhập liệu sai chính tả ở cột Street (nếu có)
housing$Street[housing$Street == "Paved"] <- "Pave"
housing$Street[housing$Street == "PAVE"] <- "Pave"
housing$Street[housing$Street == "Gravel"] <- "Grvl"

# 5.2. Kiểm tra lỗi năm xây dựng (Year.Built)
# Đảm bảo không có năm xây dựng nào vô lý (lớn hơn năm hiện tại)
housing$Year.Built[housing$Year.Built > 2026] <- 2026

# 5.3. Tạo biến Age_at_Sale (Tuổi của ngôi nhà tại thời điểm bán)
housing$Age_at_Sale <- housing$Yr.Sold - housing$Year.Built

# Nếu có lỗi nhập liệu dẫn đến tuổi âm, set về 0
housing$Age_at_Sale[housing$Age_at_Sale < 0] <- 0

# 5.4. Tạo biến Total_Living_Area (Tổng diện tích sống: Hầm + Các tầng)
housing$Total_Living_Area <- housing$Gr.Liv.Area + housing$Total.Bsmt.SF

# ------------------------------------------------------------------------------
# BƯỚC 6: KIỂM TRA CUỐI CÙNG VÀ LƯU KẾT QUẢ
# ------------------------------------------------------------------------------

# 6.1. Kiểm tra xem còn dòng nào chứa NA không
sum(!complete.cases(housing))

# 6.2. Xem cấu trúc dữ liệu sau khi đã làm sạch toàn bộ
str(housing)

# 6.3. Tóm tắt thống kê lần cuối
summary(housing)

# 6.4. Lưu dữ liệu đã làm sạch ra file CSV mới
write.csv(housing, "AmesHousing_Cleaned.csv", row.names = FALSE)

# 6.5. Lưu định dạng RData để giữ nguyên các cấu trúc Factor và Ordered Factor
save(housing, file = "AmesHousing_InR.RData")



