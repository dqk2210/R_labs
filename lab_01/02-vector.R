#Vector 

# Sử dụng c()
v1 <- c(1,2,3,4,5)
v2 <- c("A", "B", "C")

# Lưu ý:


v_mix <- c(TRUE, 1, "A", 2, 1.5)

#Sử  dụng hàm khác
v3 <-1:10
class(v3)

v4 <-c(1.5:3.5)

v7<-rep(1, times=10)
v8<-rep(c(1,2), times=3)
v9<-rep(c(1,2,3), each = 3)
v10<-rep(c(1,2), times=3, each=3)
v10

s1<-seq(1,5)
s2<-seq(from=10, to=20)
s3<-seq(from=1, to=10, by=2)

today<-as.Date(Sys.Date())
dates<-seq(today, as.Date("2030-12-31"), by="day")
dates

#Các phép toán
x<-c(1:4)
y<-seq(2,8, by=2)

x+y
x*y
x-y
x/y

#INDEX
x[1]
x[1:3]
y[c(1,3)]

c(c(2:5), x[1:4])

y[-2]


vetorNamed <- c("Khanh", "Do", "18 years old")
names(vectorNamed)<- c("name", "surname", " ")