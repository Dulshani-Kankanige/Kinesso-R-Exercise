## Kinesso R Exercise ##

# 1. read and examine data
toys_data <- read_excel("toy_sales_data.xlsx", sheet = "data")
attach(toys_data)
names(toys_data)
head(toys_data)


# 2. plot the data using ggplot
ggplot(data <- toys_data, aes(x = month, y = sales)) + geom_point() +
  labs(x = "Month", y = "Sales")

ggplot(data <- toys_data, aes(x = month, y = tv_spend)) + geom_point() +
  labs(x = "Month", y = "TV Investment")

ggplot(data <- toys_data, aes(x = month, y = digital_spend)) + geom_point() +
  labs(x = "Month", y = "Digital Investment")


# 3. correlation
cor(toys_data[, 2:4], method = "pearson")


# 4. fit regression model
model <- lm(sales ~ tv_spend + digital_spend)
summary(model)

# a. Adjusted R-squared: 0.5586
# b. tv_spend --> p-value: 0.009747, significance: 0.01
#    digital_spend --> p-value: 0.000162, significance: 0.001


# 5. contribution
contrib_pct <- (sum(toys_data[, 3]) / sum(toys_data[, 2])) * 100
contrib_abs <- sum(toys_data[, 3]) / sum(toys_data[, 2])


# 6. ROI
roi <- (sum(toys_data[, 2]) - sum(toys_data[, 3])) / sum(toys_data[, 3])


# 7. predictions
new_toys_data <- read_excel("toy_sales_data.xlsx", sheet = "planned_spend")
attach(new_toys_data)
names(new_toys_data)
head(new_toys_data)

predict(model, newdata = new_toys_data)