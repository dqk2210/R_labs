# ==============================================================================
# BÀI TẬP THỰC HÀNH
# ==============================================================================

# ------------------------------------------------------------------------------
# Bài tập 1: Bar Chart
# ------------------------------------------------------------------------------

# Sử dụng dữ liệu sau:
subjects <- c("Toán", "Lý", "Hóa", "Văn", "Anh")
scores <- c(8, 7.5, 9, 8.5, 7)

# Yêu cầu:
# 1. Vẽ bar chart cơ bản
# 2. Thêm tiêu đề "Điểm thi của bạn"
# 3. Tô màu khác nhau cho mỗi môn
# 4. Thêm giá trị điểm lên đầu mỗi cột
# 5. Vẽ bar chart ngang
barplot(scores,
        names.arg = subjects, 
        main = "Điểm thi của bạn",
        ylab = "Môn",
        xlab = "Điểm",
        xlim = c(0,10),
        col = c("blue","red","green","orange","black"),
        horiz = TRUE
)
text(y = 1:5*1.2-0.5,
     x = scores +0.3,
     labels = scores
  
)
# ------------------------------------------------------------------------------
# Bài tập 2: Histogram
# ------------------------------------------------------------------------------

# Tạo dữ liệu: Điểm thi của 100 sinh viên
set.seed(2024)
exam_scores <- rnorm(100, mean = 70, sd = 10)

# Yêu cầu:
# 1. Vẽ histogram với 10 bins
# 2. Thêm tiêu đề và nhãn trục phù hợp
# 3. Tô màu xanh lam
# 4. Thêm đường thẳng đứng màu đỏ tại vị trí điểm trung bình
# 5. Vẽ histogram khác với 20 bins, so sánh sự khác biệt
par(mfrow = c(1,2))
hist(exam_scores,
     breaks = 10,
     main = "Điểm thi (10 bin)",
     xlab = "Điểm",
     ylab = "Số lượng",
     col = "skyblue"
     )
abline(v=mean(exam_scores),
       col = "red",
       lwd =2,
       lty = 2)

hist(exam_scores,
     main = "Điểm thi của 100 sinh viên",
     ylab = "Số lượng",
     xlab = "Điểm ",
     col = "skyblue",
     breaks = 20)
abline(v = mean(exam_scores), 
       col = "red", 
       lwd = 2, 
       lty = 2)
par(mfrow = c(1,1))
# ------------------------------------------------------------------------------
# Bài tập 3: Box Plot
# ------------------------------------------------------------------------------

# Sử dụng dữ liệu iris
data("iris")
# Yêu cầu:
# 1. Vẽ box plot so sánh Petal.Length giữa 3 loài
# 2. Tô màu khác nhau cho mỗi loài
# 3. Thêm tiêu đề phù hợp
# 4. Nhìn vào biểu đồ và trả lời:
#    - Loài nào có petal dài nhất?
#    - Loài nào có độ biến thiên lớn nhất?
#    - Có outliers không? Ở loài nào?
boxplot(Penta)
# ------------------------------------------------------------------------------
# Bài tập 4: Scatter Plot
# ------------------------------------------------------------------------------

# Sử dụng dữ liệu mtcars

# Yêu cầu:
# 1. Vẽ scatter plot giữa hp (horsepower) và mpg
# 2. Tô màu các điểm theo số cy-lanh (cyl)
# 3. Thêm đường hồi quy tuyến tính
# 4. Thêm legend giải thích màu
# 5. Nhận xét về mối quan hệ giữa hp và mpg

# ------------------------------------------------------------------------------
# Bài tập 5: Nhiều biểu đồ
# ------------------------------------------------------------------------------

# Sử dụng dữ liệu mtcars

# Yêu cầu:
# Tạo một figure với 4 biểu đồ (2x2) để phân tích biến hp:
# 1. Histogram của hp
# 2. Box plot của hp
# 3. Box plot so sánh hp theo cyl
# 4. Scatter plot hp vs mpg

# ------------------------------------------------------------------------------
# Bài tập 6: Tổng hợp
# ------------------------------------------------------------------------------

# Tạo dữ liệu bán hàng của 4 quý
Q1 <- c(100, 120, 110, 130)
Q2 <- c(150, 140, 160, 155)
Q3 <- c(180, 170, 190, 185)
Q4 <- c(200, 210, 195, 220)
products <- c("Sản phẩm A", "Sản phẩm B", "Sản phẩm C", "Sản phẩm D")

# Yêu cầu:
# 1. Vẽ grouped bar chart so sánh doanh thu 4 quý
# 2. Vẽ line plot cho từng sản phẩm qua 4 quý
# 3. Tính tổng doanh thu mỗi quý, vẽ bar chart
# 4. Tạo figure 2x2 hiển thị:
#    - Grouped bar chart
#    - Line plot tất cả sản phẩm
#    - Pie chart tổng doanh thu mỗi quý
#    - Bar chart tổng doanh thu mỗi sản phẩm

# ==============================================================================
# TÀI LIỆU THAM KHẢO
# ==============================================================================

# 1. R Graphics Cookbook: https://r-graphics.org/
# 2. Quick-R Graphics: https://www.statmethods.net/graphs/
# 3. R Documentation: ?plot, ?hist, ?boxplot, ?barplot
# 4. R Color Chart: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf

# ==============================================================================
# TỔNG KẾT
# ==============================================================================

# Những điểm cần nhớ:

# 1. ✅ Chọn biểu đồ phù hợp với loại dữ liệu:
#    - Bar chart: Dữ liệu phân loại
#    - Histogram: Phân phối dữ liệu liên tục
#    - Box plot: So sánh nhóm, tìm outliers
#    - Scatter plot: Mối quan hệ giữa 2 biến
#    - Line plot: Xu hướng theo thời gian
#    - Pie chart: Tỷ lệ phần trăm (ít nhóm)

# 2. ✅ Luôn thêm tiêu đề và nhãn trục rõ ràng

# 3. ✅ Sử dụng màu sắc hợp lý:
#    - Không quá nhiều màu
#    - Màu có ý nghĩa (đỏ = cảnh báo, xanh lá = tốt)
#    - Đảm bảo đọc được khi in đen trắng

# 4. ✅ Box plot giúp:
#    - Thấy trung vị, Q1, Q3
#    - Phát hiện outliers
#    - So sánh nhiều nhóm

# 5. ✅ Histogram vs Bar chart:
#    - Histogram: Dữ liệu liên tục, không có khoảng cách giữa cột
#    - Bar chart: Dữ liệu phân loại, có khoảng cách

# 6. ✅ Sử dụng par(mfrow) để vẽ nhiều biểu đồ cùng lúc

# 7. ✅ Lưu biểu đồ: png(), pdf(), jpeg() + dev.off()

# Quy trình vẽ biểu đồ tốt:
# 1. Xác định mục đích: Muốn truyền đạt thông tin gì?
# 2. Chọn loại biểu đồ phù hợp
# 3. Vẽ biểu đồ cơ bản
# 4. Thêm tiêu đề, nhãn, màu sắc
# 5. Kiểm tra xem biểu đồ có dễ hiểu không
# 6. Lưu lại nếu cần

# Lưu ý quan trọng:
# - Biểu đồ phải đơn giản, dễ hiểu
# - Không thêm quá nhiều thông tin vào một biểu đồ
# - Luôn nghĩ về người xem
# - "A picture is worth a thousand words" - Một hình ảnh đáng giá ngàn lời

# Cập nhật: Tháng 3/2026