set.seed(123)
# Tạo 3 nhóm dữ liệu rõ ràng
group1 <- data.frame(x = rnorm(50, 2, 0.5), y = rnorm(50, 2, 0.5))
group2 <- data.frame(x = rnorm(50, 8, 0.6), y = rnorm(50, 3, 0.6))
group3 <- data.frame(x = rnorm(50, 5, 0.5), y = rnorm(50, 7, 0.5))

all_data <- rbind(group1, group2, group3)
true_labels <- c(rep(1,50), rep(2, 50), rep(3, 50))

par(mfrow = c(1,2))

#Trước khi clustering
plot(all_data$x, all_data$y, pch = 19, col = "gray", cex= 1.2,
     xlab = "Feature 1", ylab = "Feature 2",
     main = "TRƯỚC clustering\n(không có nhãn)")


#Sau khi clustering
plot(all_data$x, all_data$y, pch = 19, cex= 1.2,
     col =c("red", "blue", "green")[true_labels],
     xlab = "Feature 1", ylab = "Feature 2",
     main = "Sau clustering\n(Máy từ tìm 3 nhóm)")

legend("topright", legend = c("Cụm 1", "Cụm 2", "Cụm 3"),
       col = c("red", "blue", "green"), pch = 19, cex = 0.9)
par(mfrow = c(1,1))




#Phân cụm khách hàng
set.seed(42)

#Tạo dữ liệu khách hàng
customers <- data.frame(
  Age = c(rnorm(70, 25, 4), rnorm(60, 40, 5), rnorm(70,65, 6)),
  Income = c(rnorm(70,30,8), rnorm(60, 70, 10), rnorm(70,45,8)),
  Spending = c(rnorm(70,20,5), rnorm(60,80,12), rnorm(70,40,8))
)
#K-Means
km<- kmeans(customers, centers = 3, nstart = 25)
customers$Cluster<- km$cluster

#Visualization
par(mfrow = c(1,2))

plot(customers$Age, customers$Income,
     col = c("red", "blue", "green")[customers$Cluster],
     pch = 19, cex = 1.3,
     xlab = "Tuổi", ylab = "Thu nhập (triệu/tháng)",
     main = "Age vs Income")
points(km$centers[, 1:2], pch = 4, cex = 3, lwd = 3)

plot(customers$Income, customers$Spending,
     col = c("red", "blue", "green")[customers$Cluster],
     pch = 19, cex = 1.3,
     xlab = "Thu nhập", ylab = "Chi tiêu",
     main = "Income vs Spending")
points(km$centers[, 2:3], pch = 4, cex = 3, lwd = 3)

# Thống kê từng cụm
cluster_summary <- data.frame(
  Cum = 1:3,
  So_luong = as.numeric(table(customers$Cluster)),
  Tuoi_TB = tapply(customers$Age, customers$Cluster, mean),
  Thu_nhap_TB = tapply(customers$Income, customers$Cluster, mean),
  Chi_tieu_TB = tapply(customers$Spending, customers$Cluster, mean)
)

# Làm tròn
cluster_summary[, 3:5] <- round(cluster_summary[, 3:5], 1)

cluster_summary

# K- MEANs

#Tạo dữ liệu mẫu 
set.seed(42)

#Tạo dữ liệu mẫu
data_points <- data.frame(
  x = c(rnorm(30, 2, 0.5), rnorm(30, 8, 0.6), rnorm(30, 5, 0.5)),
  y = c(rnorm(30, 2, 0.5), rnorm(30, 3, 0.6), rnorm(30, 7, 0.5))
)
par(mfrow = c(1,3))
