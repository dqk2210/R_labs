# 1. Tạo dataframe 
# Dataframe được tạo từ các vector có chung độ dài

column1 <- c(1:3)
column2 <- c("Tung", "Tom", "Anna")
column3 <- c(T, T, F)

dataset1 <- data.frame(column1, column2, column3)
# Hiện thị ra console
dataset1
print(dataset1)
# View dữ liệu
View(dataset1)

# Tên của các cột
colnames(dataset1)

# Đổi tên cột 2
colnames(dataset1)[2] <- "Name"
dataset1

# Đổi tên cột 1 loạt
colnames(dataset1)<- c("#", "Name", "Check")
dataset1


# Thêm dòng mới cho dataframe 
newRow <- c(4, "Nhat Tung", T)
dataset2 <- rbind(dataset1, newRow)
dataset2


newRowDF <- data.frame(5, "Lisa", F)
names(newRowDF) <- c("#", "Name", "Check")
dataset3<- rbind(dataset2,newRowDF)
dataset3


#3. Thêm cột mới 
newColumn <- c("a", "b", "c", "d", "f")
dataset4<-cbind(dataset3, newColumn)
dataset4

#Cách 2: 
dataset4$newColumn2<- c(1,2,3,4,5)
dataset4




#4. Truy xuất dữ liệu 
# truy xuất bảng chỉ số
dataset4[3,2] # dòng 3 cột 2

# Truy xuất dữ liệu bằng chỉ số và tên cột
dataset4[3, "Check"]



# Truy xuát bằng tên cột 
dataset4["Name"]
dataset4[,"Name"] # Cách 2
dataset4$Name # Cách 3

  
#5. Các hàm thông dụng
head(dataset4) # Hiện thị 5 dòng đầu
tail(dataset4) # Hiện thị vài dòng cuối
str(dataset4) # Hiện thị cấu trúc dữ liệu
summary(dataset4)


#6. Thay đổi dữ liệu cột
dataset4$Check<-as.logical(dataset4$Check)
summary(dataset4)

####### Bài tập 
######## Bộ dữ liệu Iris
data() # Các bộ dữ liệu được build trong R
iris
View(iris)
str(iris)
summary(iris)
head(iris)
tail(iris)

CO2
View(CO2)
str(CO2)
summary(CO2)
head(CO2)
tail(CO2)

