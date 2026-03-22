# BÀI THỰC HÀNH: XÂY DỰNG MÔ HÌNH HỒI QUY LOGISTIC CHO WATER POTABILITY
# =================================================

# TÓM TẮT BÀI TOÁN:
# Dự đoán khả năng uống được của nước (Potability) dựa trên các chỉ số hóa lý.
# Biến mục tiêu: Potability (0 = Không uống được / Not Potable, 1 = Uống được / Potable)

# Bước 1: Cài đặt (nếu chưa có) và gọi các gói cần thiết
if(!require(caret)) install.packages("caret")
if(!require(pROC)) install.packages("pROC")
if(!require(readr)) install.packages("readr")
if(!require(ggplot2)) install.packages("ggplot2")

# Tải các thư viện vào phiên làm việc
library(ggplot2)
library(caret) 
library(pROC)  
library(readr)

# Bước 2: Tải và khám phá dữ liệu 
water_potability <- read.csv("lab_03/water_potability.csv")
dataset_name <- "Water potability dataset"


# 1. Tìm trung vị của các cột bị thiếu dữ liệu (bỏ qua NA khi tính toán)
median_ph <- median(water_potability$ph, na.rm = TRUE)
median_Sulfate <- median(water_potability$Sulfate, na.rm = TRUE)
median_Trihalomethanes <- median(water_potability$Trihalomethanes, na.rm = TRUE)

# 2. Điền các giá trị trung vị này vào những ô bị trống (NA)
water_potability$ph[is.na(water_potability$ph)] <- median_ph
water_potability$Sulfate[is.na(water_potability$Sulfate)] <- median_Sulfate
water_potability$Trihalomethanes[is.na(water_potability$Trihalomethanes)] <- median_Trihalomethanes

# Kiểm tra lại xem còn ô trống nào không
cat("Số lượng ô trống sau khi xử lý:", sum(is.na(water_potability)), "\n")

# Thông tin cơ bản
cat("\nThông tin về tập dữ liệu", dataset_name, "sau khi loại bỏ NA:\n")
cat("Số mẫu:", nrow(water_potability), "\n")
cat("Số đặc trưng:", ncol(water_potability)-1, "\n\n")

cat("Cấu trúc dữ liệu:\n")
str(water_potability)

# Thống kê dữ liệu
print(summary(water_potability))

# Kiểm tra phân phối biến mục tiêu
cat("\nPhân phối biến mục tiêu (Potability):\n")
print(table(water_potability$Potability))
print(prop.table(table(water_potability$Potability)) * 100)


# Bước 3: Phân chia dữ liệu (Train / Test)
set.seed(42)
train_indices <- createDataPartition(water_potability$Potability, p = 0.7, list = FALSE)
train_data <- water_potability[train_indices, ]
test_data <- water_potability[-train_indices, ]

cat("\nKích thước tập huấn luyện:", dim(train_data), "\n")
cat("Kích thước tập kiểm tra:", dim(test_data), "\n")


# Bước 4: Xây dựng và huấn luyện mô hình
# Ở đây ta sử dụng tất cả các biến (dấu chấm ".") để dự đoán
model <- glm(Potability ~ ., 
             data = train_data, 
             family = binomial(link="logit"))


# Bước 5: Kiểm tra mô hình
cat("\nTóm tắt mô hình:\n")
summary_model <- summary(model)
print(summary_model)

cat("\nHệ số hồi quy và tỷ số Odds (Odds Ratio):\n")
coef_df <- data.frame(
  feature = names(coef(model)),
  coefficients = coef(model),
  odds_ratio = exp(coef(model)),
  p_value = summary_model$coefficients[, 4]
)
print(coef_df)

# Bước 6: Dự đoán trên tập test
probabilities <- predict(model, newdata = test_data, type = "response")

# Đặt ngưỡng 0.5 để phân loại
predicted_classes <- ifelse(probabilities > 0.5, "Potable", "Not_Potable")
predicted_classes <- factor(predicted_classes, levels = c("Not_Potable", "Potable"))

# Bước 7: Đánh giá mô hình
cat("\nMa trận nhầm lẫn (Confusion Matrix):\n")
conf_matrix <- confusionMatrix(predicted_classes, test_data$Potability, positive = "Potable")
print(conf_matrix$table)

cat("\nCác chỉ số đánh giá mô hình:\n")
cat("Độ chính xác (Accuracy):", round(conf_matrix$overall["Accuracy"], 4), "\n")
cat("Độ nhạy (Sensitivity/Recall):", round(conf_matrix$byClass["Sensitivity"], 4), "\n")
cat("Độ đặc hiệu (Specificity):", round(conf_matrix$byClass["Specificity"], 4), "\n")
cat("Độ chính xác dương tính (Precision):", round(conf_matrix$byClass["Pos Pred Value"], 4), "\n")
cat("Chỉ số F1:", round(conf_matrix$byClass["F1"], 4), "\n")

# Vẽ đường cong ROC
roc_obj <- roc(test_data$Potability, probabilities)
auc_value <- auc(roc_obj)
cat("\nDiện tích dưới đường cong ROC (AUC):", round(auc_value, 4), "\n")

plot(roc_obj, main="Đường cong ROC - Water Potability", col="blue", lwd=2)

# Chuyển đổi bảng Confusion Matrix thành dạng Data Frame để vẽ
conf_df <- as.data.frame(conf_matrix$table)

# Vẽ biểu đồ nhiệt (Heatmap)
ggplot(data = conf_df, aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), color = "white") +
  geom_text(aes(label = sprintf("%d", Freq)), vjust = 0.5, size = 6, color = "black") +
  scale_fill_gradient(low = "lightblue", high = "royalblue") +
  theme_minimal() +
  labs(title = "Biểu đồ Ma trận nhầm lẫn (Confusion Matrix)",
       x = "Giá trị thực tế (Reference)",
       y = "Giá trị dự đoán (Prediction)",
       fill = "Số lượng")
