# 2.1. Giới thiệu Dataset
# Tên dataset: Student Alcohol Consumption
# Nguồn: UCI Machine Learning Repository
# Mô tả: Dữ liệu về tình trạng tiêu thụ rượu của học sinh trung học ở Bồ Đào Nha
# 
# Các biến quan trọng:
#   
# school: Trường học (GP hoặc MS)
# sex: Giới tính
# age: Tuổi
# Medu, Fedu: Trình độ học vấn của cha mẹ (0-4)
# studytime: Thời gian học mỗi tuần
# Dalc: Mức độ uống rượu trong tuần (1-5)
# Walc: Mức độ uống rượu cuối tuần (1-5)
# Ctrl + Shift + C



#Load dữ liệu 
alcohol <- read.csv("R_Intro/labs/data/dataset - student alcohol consumption/student-alcohol.csv")

View(alcohol)


#xem 6 dòng đầu
head(alcohol)

#kt cấu trúc
str(alcohol)

# Tóm tắt thống kê
summary(alcohol)

# Loại bỏ cột đầu tiên (có thể là cột ID không k cần thiết)
head(alcohol)[,-1]
alcohol <- alcohol[,-1] 

#Lưu ý
# 
# [,-1] nghĩa là " lấy tất cả các dòng, loại bỏ cột "
# Luôn kiểm tra trước khi xóa để

# Bước 2
alcohol[!complete.cases(alcohol), ]
