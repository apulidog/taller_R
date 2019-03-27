head(cars)
summary(cars)
str(cars)
scatter.smooth(x=cars$speed, y=cars$dist, main="Dist ~ Speed")
par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(cars$speed, main="Speed", sub=paste("Outlier rows: ", 
                                            boxplot.stats(cars$speed)$out))  # box plot for 'speed'
boxplot(cars$dist, main="Distance", sub=paste("Outlier rows: ", 
                                              boxplot.stats(cars$dist)$out)) 
library(e1071)
par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(cars$speed), main="Density Plot: Speed", ylab="Frequency", 
     sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))  # density plot for 'speed'
polygon(density(cars$speed), col="green")
plot(density(cars$dist), main="Density Plot: Distance", ylab="Frequency", 
     sub=paste("Skewness:", round(e1071::skewness(cars$dist), 2)))  # density plot for 'dist'
polygon(density(cars$dist), col="red")
cor(cars$speed, cars$dist) 
scatter.smooth(x=marg_hom$marg, y=marg_hom$hom, main="Marg/Hom")
par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(marg_hom$marg, main="Marg", sub=paste("Outlier rows: ", 
                                            boxplot.stats(marg_hom$marg)$out))  # box plot for 'speed'
boxplot(marg_hom$hom, main="Hom", sub=paste("Outlier rows: ", 
                                              boxplot.stats(marg_hom$hom)$out)) 
par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(marg_hom$marg), main="Density Plot: Marg", ylab="Frequency", 
     sub=paste("Skewness:", round(e1071::skewness(marg_hom$marg), 2)))  # density plot for 'speed'
polygon(density(marg_hom$marg), col="green")
plot(density(marg_hom$hom), main="Density Plot: Hom", ylab="Frequency", 
     sub=paste("Skewness:", round(e1071::skewness(marg_hom$hom), 2)))  # density plot for 'dist'
polygon(density(marg_hom$hom), col="red")
cor(marg_hom$marg, marg_hom$hom) 
