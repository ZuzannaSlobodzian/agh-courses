install.packages("plotly")
library(plotly)

#przygotowanie danych (uzupełnienie danych)
data <- read.csv("daily_ice_edge.csv",header=TRUE,encoding="UTF-8")
data_new <- data.frame(matrix(NA, nrow = 0, ncol = 361))
for( i in 1:1589){
  data_new[nrow(data_new) + 1, ] = data[i, 2:362] 
  data_new[nrow(data_new) + 1, ] = (data[i, 2:362] + data[i+1, 2:362])/2
}
data_new2 <- data_new
for( i in 1590:9530){
  data_new2[nrow(data_new2) + 1, ] = data[i, 2:362] 
}
colnames(data_new) <- colnames(data[ ,2:362])


#kształt Antarktydy  
longDegrees <- seq(1:361)
rangeMin <- numeric()

for( i in 2:361){
  rangeMin <- append(rangeMin, min(data_new2[ ,i]))
}
rangeMin <- (-1)*rangeMin

r <- 90 - rangeMin
x <- r * cos(longDegrees*pi/180)
y <- r * sin(longDegrees*pi/180)

plot(y, x, type = "l", lty = 1)

#tworzenie danych na podstawie modelu matematycznego
days <- seq(1:11119)

model_data <- data.frame(matrix(NA, nrow = 11119, ncol = 361))
for( i in 1:361 ){
  range_v <- 90 + data_new2[ ,i]
  ssp <- spectrum(range_v, plot = FALSE)  
  per <- 1/ssp$freq[ssp$spec==max(ssp$spec)]
  reslm <- lm(range_v ~ sin(2*pi/per*days)+cos(2*pi/per*days))
  model_data[ ,i] = fitted(reslm)
}
colnames(model_data) <- colnames(data[ ,2:362])



longDegrees <- seq(1:361)

r_real <- as.numeric(90 + data_new2[1, ])
x_real <- r_real * cos(longDegrees*pi/180)
y_real <- r_real * sin(longDegrees*pi/180)

r_model <- as.numeric(model_data[1, ])
x_model <- r_model * cos(longDegrees*pi/180)
y_model <- r_model * sin(longDegrees*pi/180)

plot(y, x, type = "l", lty = 1, col='red', xlab='x', ylab='y', xlim=c(-40,40), ylim=c(-40,40))
lines(y_real, x_real, col='blue')
lines(y_model, x_model, col='green')
title("1")
legend("topleft", legend=c("min", "real", "model"), col=c("red", "blue", "green"),
       title="Range", lty=1, cex=0.8, text.font=4, bg='lightyellow')



#animacja
for( i in days ){
  longDegrees <- seq(1:361)
  
  r_real <- as.numeric(90 + data_new2[i, ])
  x_real <- r_real * cos(longDegrees*pi/180)
  y_real <- r_real * sin(longDegrees*pi/180)
  
  r_model <- as.numeric(model_data[i, ])
  x_model <- r_model * cos(longDegrees*pi/180)
  y_model <- r_model * sin(longDegrees*pi/180)
  
  filename=paste(sprintf('%05d', i), ".png", sep="")  
  png(filename=filename)
  plot(y, x, type = "l", lty = 1, col='red', xlab='x', ylab='y', xlim=c(-40,40), ylim=c(-40,40))
  lines(y_real, x_real, col='blue')
  lines(y_model, x_model, col='green')
  title(i)
  legend("topleft", legend=c("min", "real", "model"), col=c("red", "blue", "green"),
         title="Range", lty=1, cex=0.8, text.font=4, bg='lightyellow')
  dev.off()
}


year_model <- data.frame(matrix(NA, nrow = 0, ncol = 361))
day <- 1
for (i in 1:365){ 
  vector <- seq(day,11119,365)
  temp_frame <- model_data[vector, ]
  year_model[i, ] <- colMeans(temp_frame[, ])
  day <- day + 1
}
  
for( i in 1:365 ){
  longDegrees <- seq(1:361)

  r_year <- as.numeric(year_model[i, ])
  x_year <- r_year * cos(longDegrees*pi/180)
  y_year <- r_year * sin(longDegrees*pi/180) #90?
  
  filename=paste(sprintf('%05d', i), ".png", sep="")  
  png(filename=filename)
  plot(y, x, type = "l", lty = 1, col='red', xlab='x', ylab='y', xlim=c(-40,40), ylim=c(-40,40))
  lines(y_year, x_year, col='lightskyblue')
  title(i)
  legend("topleft", legend=c("min range", "year model"), col=c("red", "lightskyblue"),
         title="Legend", lty=1, cex=0.8, text.font=4, bg='lightyellow')
  dev.off()
}

longDegrees <- seq(1:361)

r_year <- as.numeric(year_model[1, ])
x_year <- r_year * cos(longDegrees*pi/180)
y_year <- r_year * sin(longDegrees*pi/180)

filename=paste(sprintf('%05d', 1), ".png", sep="")  
png(filename=filename)
plot(y, x, type = "l", lty = 1, col='red', xlab='x', ylab='y', xlim=c(-40,40), ylim=c(-40,40))
lines(y_year, x_year, col='green')
title(1)
legend("topleft", legend=c("min", "year model"), col=c("red", "green"),
       title="Range", lty=1, cex=0.8, text.font=4, bg='lightyellow')
dev.off()




#ffmpeg -r 60 -i %05d.png anim.gif