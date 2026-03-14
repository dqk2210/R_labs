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
  if (x %% 2 == 0) {
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
average_score<- function(scores){
  valid_scores<- scores[!is.na(scores)]
  
  
}
# ------------------------------------------------------------------------------
# Bài tập 3: Function thống kê
# ------------------------------------------------------------------------------

# 1. Viết function tính toán tổng quan
# Input: vector số
# Output: list(mean, median, sd, min, max, range)

# 2. Viết function tính hoán vị P(n, r)
# Công thức: P(n,r) = n! / (n-r)!

# 3. Viết function tính tổ hợp C(n, r)
# Công thức: C(n,r) = n! / (r! * (n-r)!)

# ------------------------------------------------------------------------------
# Bài tập 4: Function nâng cao
# ------------------------------------------------------------------------------

# 1. Viết function tìm các số nguyên tố từ 1 đến n
# Input: n
# Output: vector các số nguyên tố

# 2. Viết function tạo tam giác Pascal với n hàng
# Gợi ý: Sử dụng tổ hợp C(n, k)
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
# 3. Viết function phân loại sinh viên dựa vào điểm
# Input: điểm số
# Output: xếp loại (Xuất sắc, Giỏi, Khá, TB, Yếu)
# Kèm theo GPA scale 4.0

# ------------------------------------------------------------------------------
# Bài tập 5: Ứng dụng thực tế
# ------------------------------------------------------------------------------

# 1. Viết function tính lương ròng
# Input: lương cơ bản, phụ cấp, số ngày làm việc, số giờ tăng ca
# Output: lương ròng sau thuế
calculate_summary_stats <- function(v) {
  v <- v[!is.na(v)] 
  if(length(v) == 0) return(NULL)
  
  return(list(
    mean = mean(v),
    median = median(v),
    sd = sd(v),
    min = min(v),
    max = max(v),
    range = max(v) - min(v)
  ))
}
# 2. Viết function chuẩn hóa điểm thi
# Input: vector điểm thô
# Output: vector điểm chuẩn hóa (0-100)
# Công thức: (điểm - min) / (max - min) * 100
calculate_permutation <- function(n, r) {
  if (r > n | n < 0 | r < 0) return(NA)
  return(factorial(n) / factorial(n - r))
}
# 3. Viết function phân tích dữ liệu sinh viên
# Input: data frame (tên, tuổi, điểm)
# Output: thống kê mô tả đầy đủ
calculate_combination <- function(n, r) {
  if (r > n | n < 0 | r < 0) return(NA)
  return(factorial(n) / (factorial(r) * factorial(n - r)))
}
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
