train <- read.csv("C:\\Users\\hainuo\\Desktop\\训练集.csv")
test  <- read.csv("C:\\Users\\hainuo\\Desktop\\验证集.csv")

# 创建 t 变量
train$t <- train$Year - 1948
test$t  <- test$Year  - 1948

# 线性回归 N(t) = a + b * t
fit <- lm(Population ~ t, data = train)

# 输出参数
cat("===== 线性拟合参数 =====")
print(coef(fit))

# 预测验证集并计算 RMSE
pred <- predict(fit, newdata = test)
rmse <- sqrt(mean((test$Population - pred)^2))
cat("\n===== 验证集误差 =====")
cat("验证集 RMSE:", round(rmse, 1), "万人\n")

# 对比实际与预测
cat("\n===== 验证集实际值 vs 预测值 =====")
result <- cbind(test[, c("Year", "Population")], Predicted = round(pred, 0), Residual = round(test$Population - pred, 0))
print(result)