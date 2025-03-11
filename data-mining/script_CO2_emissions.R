install.packages("forecast")
install.packages("neuralnet")
install.packages("MLmetrics")

library(ggplot2)
library(forecast)
library(caret)
library(neuralnet)
library(MLmetrics)

############################# przygotowanie danych, wyklucznie NAs

data_raw <- data_merged[-c(1, 2, 3), ]
colnames(data_raw) <- data_raw[1, ]
data_raw <- data_raw[-1, ]
data_raw <- data_raw[, -(3:4)]
data_raw <- data_raw[, -(60:63)]
data_raw[data_raw == ""] <- NA

data <- na.omit(data_raw)

############################# str i summary

str(data)
summary(data)

############################# plot

par(mar = c(5, 4, 2, 2), mfrow = c(1, 1))

data_matrix <- as.matrix(as.data.frame(lapply(data[, 5:length(data)], as.numeric)))
colnames(data_matrix) <- colnames(data[, 5:length(data)])
years <- colnames(data[, 5:length(data)])

matplot(t(data_matrix), type = "l", col = rainbow(nrow(data_matrix)),
        xlab = "Year", ylab = "Value", main = "CO2 Emission", xaxt = 'n')
axis(1, at = 1:length(years), labels = years, las = 2, cex.axis = 0.8)

############################# boxplot

data_countries_columns <- t(data_matrix)
colnames(data_countries_columns) <- data$"Country Name"

par(mar = c(7, 4, 2, 2), mfrow = c(1, 1), cex.axis = 1.3)

scope = seq(1, 141, 20)

for(i in scope){
  
  if(i == 141)
    j <- i + 7
  else
    j <- i + 19
  
  labels <- substr(colnames(data_countries_columns[, i:j]), 1, 10)
  boxplot(data_countries_columns[, i:j], las = 2, 
        main = paste("Boxplots for countries from colum", i, "to", j), 
        ylab = "CO2 Emission", col = "mistyrose", names = labels)
}


############################# histogram lata

par(mfrow = c(4, 4), mar = c(2, 2, 1, 1))

k <- 5
n <- 15
for (i in 1:4){

  if(i == 4)
    n <- 6
  
  for (j in k:(k + n)) {
    hist(data[, j], main = paste("Year", colnames(data)[j]), col = "lavender", 
         xlab = "value", ylab = "count")
  }
  
  k <- k + 16
}

############################# histogram państwa

par(mfrow = c(4, 4), mar = c(2, 2, 1, 1))

k <- 1
n <- 15
for (i in 1:10){
  
  if(i == 10)
    n <- 3
  
  for (j in k:(k + n)) {
    
    hist(data_countries_columns[, j], main = paste(colnames(data_countries_columns)[j]), 
         col = "lavender", xlab = "value", ylab = "count")
    
  }
  
  k <- k + 16
}


############################# trend

selected_years_indices <- round(seq(1, length(years), length.out = 5))
selected_years <- years[selected_years_indices]

par(mfrow = c(4, 4), mar = c(2, 2, 1, 1))

k <- 1
n <- 15
for (i in 1:10){
  
  if(i == 10)
    n <- 3
  
  for (j in k:(k + n)) {
    
    country <- data_countries_columns[, j]
    label <- substr(colnames(data_countries_columns)[j], 1, 18)
    
    time <- seq_along(country)
    plot(time, country, type = "l", main = label, xlab = "Time", ylab = "Value", xaxt = 'n')
    axis(1, at = selected_years_indices, labels = selected_years, las = 2, cex.axis = 0.55)
    
    trend <- lm(country ~ time)
    abline(trend, col = "red")  
  }
  
  k <- k + 16
}


############################# przygotowanie do modeli - zmiana na numeryczne

data_numeric <- as.data.frame(data)

NumRegion <- as.numeric(factor(data$Region, levels = unique(data$Region)))
NumIncomeGroup <- as.numeric(factor(data$IncomeGroup, levels = unique(data$IncomeGroup)))

data_numeric <- cbind(data_numeric[, 1:3], NumRegion, data_numeric[, 4], NumIncomeGroup,
                      data_numeric[, 5:ncol(data_numeric)])
colnames(data_numeric)[5] = "IncomeGroup"

############################# podział na zbiory knn

set.seed(14159265) #Pi :)

data_numeric_cut <- data_numeric[, c(4, 6:61)]
n <- nrow(data_numeric_cut)

maxs <- apply(data_numeric_cut, 2, max)
mins <- apply(data_numeric_cut, 2, min)

data_std_knn <- as.data.frame(scale(data_numeric_cut, center = mins, scale = maxs - mins))

train_indices <- sample(1:n, 0.8 * n)

## zbiór TRENINGOWY
data_train_knn <- data_std_knn[train_indices, 1:56]

remaining_indices <- setdiff(seq_len(n), train_indices)
validation_indices <- sample(remaining_indices, size = 4)

## zbiór testowy do PREDYKCJI
data_validation_knn <- data_std_knn[validation_indices, 1:56]

test_indices <- setdiff(remaining_indices, validation_indices)

## zbiór testowy do WALIDACJI
data_test_knn <- data_std_knn[test_indices, 1:56]

train_y_knn <- data_std_knn[train_indices, 57]
test_y_knn <- data_std_knn[test_indices, 57]
validation_y_knn <- data_std_knn[validation_indices, 57]

############################# regresyjny model knn 

### zmiana na zbiór z 4 obs.
test_y_knn <- validation_y_knn
data_test_knn <- data_validation_knn
###

model_knn<- knnreg(data_train_knn, train_y_knn, k = 4)
str(model_knn)

pred_y_knn = predict(model_knn, data.frame(data_test_knn))

############################# ocena modelu knn

r2_knn =(cor(test_y_knn, pred_y_knn))^2

par(mfrow = c(1, 1), mar = c(4, 4, 2, 2))
plot(test_y_knn, pred_y_knn, main = "Real vs predicted, knn", xlab = "actual", ylab = "predicted")
abline(0, 1, col = "red")

############################# ocena reszt knn

residuals_knn <- test_y_knn - pred_y_knn
hist(residuals_knn, breaks = 20, main = "Histogram of residuals, knn", xlab = "Residuals")

qqnorm(residuals_knn)
qqline(residuals_knn)

acf(residuals_knn)
pacf(residuals_knn)

plot(pred_y_knn, residuals_knn, main = "Residuals vs fitted values, knn", xlab = "fitted values", ylab = "residuals")

############################# prognozy knn

mse_knn = mean((test_y_knn - pred_y_knn)^2)
rmse_knn = caret::RMSE(test_y_knn, pred_y_knn)

mape_knn <- mean(abs((test_y_knn - pred_y_knn) / test_y_knn)) * 100

mean_pred <- mean(train_y_knn)
dispersion <- sqrt(mean((pred_y_knn - test_y_knn)^2)) / sqrt(mean((test_y_knn - mean_pred)^2))
bias <- mean((pred_y_knn - test_y_knn) / test_y_knn)
theil_knn <- dispersion * bias

directional_accuracy <- mean(sign(test_y_knn - mean(test_y_knn)) == sign(pred_y_knn - mean(test_y_knn)))
janus_knn <- 2 * directional_accuracy - 1

############################# statystyki knn

print(paste("R2: ", r2_knn))
print(paste("MSE: ", mse_knn))
print(paste("RMSE: ", rmse_knn))
print(paste("MAPE: ", mape_knn))

############################# ploty porównawcze knn

par(mfrow = c(2, 2), mar = c(2, 2, 1, 1))

time <- seq_along(data_std_knn[1, 3:57])
selected_years_indices <- round(seq(1, length(years), length.out = 9))
selected_years <- years[selected_years_indices]

### zmiana na zbiór z 4 obs.
test_indices <- validation_indices
###

plot_test_knn <- data_std_knn[test_indices, 3:57]
label <- data[test_indices, 1]

for(i in 1:4){
  
    # if(i == 2){
    #   y_range <- range(plot_test_knn[i, ], na.rm = TRUE)
    #   ylim <- c(y_range[1], y_range[2] + 0.01)
    #   plot(time, plot_test_knn[i, ], type = "b", pch = 19, col = "blue",
    #        main = label[i], xlab = "Time", ylab = "Value", xaxt = 'n', ylim = ylim)
    #   axis(1, at = selected_years_indices, labels = selected_years, las = 2, cex.axis = 0.55)
    #   points(time[length(time)], pred_y_knn[i], col = "red", pch = 19)
    # }else{
  
    plot(time, plot_test_knn[i, ], type = "b", pch = 19, col = "blue", main = label[i],
         xlab = "Time", ylab = "Value", xaxt = 'n')
    axis(1, at = selected_years_indices, labels = selected_years, las = 2, cex.axis = 0.55)
    
    points(time[length(time)], pred_y_knn[i], col = "red", pch = 19)
    #}
}

print(paste("real: ", test_y_knn))
print(paste("predicted: ", pred_y_knn))

############################# podział na zbiory mlp

set.seed(14159265) #Pi :)

data_numeric_cut <- data_numeric[, c(4, 6:61)]
n <- nrow(data_numeric_cut)

maxs <- apply(data_numeric_cut, 2, max)
mins <- apply(data_numeric_cut, 2, min)

data_std_mlp <- as.data.frame(scale(data_numeric_cut, center = mins, scale = maxs - mins))

train_indices <- sample(1:n, 0.8 * n)

data_train_mlp <- data_std_mlp[train_indices, ]

remaining_indices <- setdiff(seq_len(n), train_indices)
validation_indices <- sample(remaining_indices, size = 4)

## do predykcji
data_validation_mlp <- data_std_mlp[validation_indices, ]

test_indices <- setdiff(remaining_indices, validation_indices)

## do walidacji
data_test_mlp <- data_std_mlp[test_indices, ]

############################# sieci neuronowe mlp

colnames(data_train_mlp)[3:57] <- paste0("year", colnames(data_train_mlp)[3:57])
colnames(data_test_mlp)[3:57] <- paste0("year", colnames(data_test_mlp)[3:57])
colnames(data_validation_mlp)[3:57] <- paste0("year", colnames(data_validation_mlp)[3:57])


model_mlp <- neuralnet(year2014 ~ . ,
                       data = data_train_mlp, hidden = c(4, 3),
                       linear.output = TRUE)

plot(model_mlp)

## zmiana na zbiór z 4 obs.
data_test_mlp <- data_validation_mlp
##

pred_compute_mlp <- compute(model_mlp, data_test_mlp[, 1:56])

pred_y_mlp <- unlist(pred_compute_mlp$net.result)
test_y_mlp <- data_test_mlp$year2014

## zmiana na zbiór z 4 obs.
test_y_mlp <- data_validation_mlp$year2014
##

############################# ocena modelu mlp

r2_mlp =(cor(test_y_mlp, pred_y_mlp))^2

par(mfrow = c(1, 1), mar = c(4, 4, 2, 2))
plot(test_y_mlp, pred_y_mlp, main = "Real vs predicted, knn", xlab = "actual", ylab = "predicted")
abline(0, 1, col = "red")


############################# ocena reszt mlp

residuals_mlp <- test_y_mlp - pred_y_mlp
hist(residuals_mlp, breaks = 20, main = "Histogram of residuals, knn", xlab = "residuals")

qqnorm(residuals_mlp)
qqline(residuals_mlp)

acf(residuals_mlp)
pacf(residuals_mlp)

plot(pred_y_mlp, residuals_mlp, main = "Residuals vs fitted values, knn", xlab = "fitted values", ylab = "residuals")

############################# prognozy mlp

mse_mlp = mean((test_y_mlp - pred_y_mlp)^2)
rmse_mlp = sqrt(mse_mlp)

mape_mlp <- mean(abs((test_y_mlp - pred_y_mlp) / test_y_mlp)) * 100

mean_pred <- mean(data_train_mlp$year2014)
dispersion <- sqrt(mean((pred_y_mlp - test_y_mlp)^2)) / sqrt(mean((test_y_mlp - mean_pred)^2))
bias <- mean((pred_y_mlp - test_y_mlp) / test_y_mlp)
theil_mlp <- dispersion * bias

directional_accuracy <- mean(sign(test_y_mlp - mean(test_y_mlp)) == sign(pred_y_mlp - mean(test_y_mlp)))
janus_mlp <- 2 * directional_accuracy - 1

############################# statystyki mlp

print(paste("R2: ", r2_mlp))
print(paste("MSE: ", mse_mlp))
print(paste("RMSE: ", rmse_mlp))
print(paste("MAPE: ", mape_mlp))

############################# ploty porównawcze mlp

par(mfrow = c(2, 2), mar = c(2, 2, 1, 1))

time <- seq_along(data_test_mlp[i, 3:57])
selected_years_indices <- round(seq(1, length(years), length.out = 9))
selected_years <- years[selected_years_indices]

## zmiana na zbiór z 4 obs.
test_indices <- validation_indices
##

plot_test_mlp <- data_std_mlp[test_indices, 3:57]
label <- data[test_indices, 1]


for(i in 1:4){
  
  # if(i == 2){
  #   y_range <- range(plot_test_mlp[i, ], na.rm = TRUE)
  #   ylim <- c(y_range[1], y_range[2] + 0.003)
  #   plot(time, plot_test_mlp[i, ], type = "b", pch = 19, col = "blue",
  #        main = label[i], xlab = "Time", ylab = "Value", xaxt = 'n', ylim = ylim)
  #   axis(1, at = selected_years_indices, labels = selected_years, las = 2, cex.axis = 0.55)
  #   points(time[length(time)], pred_y_mlp[i], col = "red", pch = 19)
  # }else if(i == 3){
  #   y_range <- range(plot_test_mlp[i, ], na.rm = TRUE)
  #   ylim <- c(y_range[1], y_range[2] + 0.001)
  #   plot(time, plot_test_mlp[i, ], type = "b", pch = 19, col = "blue",
  #        main = label[i], xlab = "Time", ylab = "Value", xaxt = 'n', ylim = ylim)
  #   axis(1, at = selected_years_indices, labels = selected_years, las = 2, cex.axis = 0.55)
  #   points(time[length(time)], pred_y_mlp[i], col = "red", pch = 19)
  # }else{
  
  plot(time, plot_test_mlp[i, ], type = "b", pch = 19, col = "blue", main = label[i],
       xlab = "Time", ylab = "Value", xaxt = 'n')
  axis(1, at = selected_years_indices, labels = selected_years, las = 2, cex.axis = 0.55)
  
  points(time[length(time)], pred_y_mlp[i], col = "red", pch = 19)
  #}
}

print(paste("real: ", test_y_mlp))
print(paste("predicted: ", pred_y_mlp))

############################################################################
