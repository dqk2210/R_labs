
Sys.setlocale("LC_ALL", "English")
Sys.setenv(LANGUAGE = "en")




View(longley)
summary(longley)

str(longley)


#Ma trận tương quan 
cor(longley)

#Tính riêng hệ số tương quan giữa GNP và Employed
# Dùng để kt trước khi đưa vào mô hình hổi quy 
cor(longley$GNP, longley$Employed)

#Cài đặt package (chỉ chạy lần đầu)
install.packages("corrplot")
install.packages("ggcorplot")
 