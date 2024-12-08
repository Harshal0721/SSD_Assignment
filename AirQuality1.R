install.packages("missForest")

install.packages("MASS") 




library(MASS)
library(missForest)
# Load the 'airquality' dataset
data(airquality)
str(airquality)
# Summary Statistics for a Numerical Variable (e.g., 'Temp')
summary(airquality$Temp)  # Summary of the Temperature variable
sd(airquality$Temp, na.rm = TRUE)  # Standard deviation of the Temperature variable, ignoring NA

#found some NA values in dataset,hence imputing with missForest
set.seed(42)
airquality1 <-missForest(airquality)
airquality_imputed <- airquality1$ximp

# Histogram for Temp
hist(airquality_imputed$Temp, main="Histogram of Temperature", xlab="Temperature", col="lightgreen", breaks=10)

# Boxplot for Temp
boxplot(airquality_imputed$Temp, main="Boxplot of Temperature", ylab="Temperature")

#  Correlation between Numerical Variables (Temp and Ozone)
cor(airquality_imputed$Temp, airquality_imputed$Ozone, use="complete.obs")  # Use complete cases for correlation

#  Scatter Plot with Trend Line between 'Temp' and 'Ozone'
plot(airquality_imputed$Temp, airquality_imputed$Ozone, main="Scatter Plot of Temp vs Ozone", 
     xlab="Temperature", ylab="Ozone", pch=19, col="blue")
abline(lm(airquality_imputed$Ozone ~ airquality_imputed$Temp, na.action = na.omit), col="red")


# Linear Regression Model for 'Ozone' prediction using 'Temp' and 'Wind'
model <- lm(Ozone ~ Temp + Wind  , data=airquality_imputed)
summary(model)

#  Diagnostic Plots for the Regression Model
par(mfrow=c(2,2))
plot(model, which=1:4, labels.id=NULL)
mtext("First  Model", side = 3, outer = TRUE, line = -2, cex = 1.5)

# Using Boxcox transformation to improve model
par(mfrow=c(1,1))
boxcox_result <- boxcox(model, lambda = seq(-2, 2, by = 0.1))
mtext("Optimal Lambda", side = 3, outer = TRUE, line = -2, cex = 1.5)

optimal_lambda <- boxcox_result$x[which.max(boxcox_result$y)] ##found optimal lambda=0.303

airquality_imputed$Ozone_transformed <- (airquality_imputed$Ozone^optimal_lambda - 1) / optimal_lambda

new_model <- lm(Ozone_transformed ~  Wind + Temp, data = airquality_imputed)
summary(new_model)

par(mfrow=c(2,2))
plot(new_model, which=1:4, labels.id=NULL)
mtext("Refined  Model", side = 3, outer = TRUE, line = -2, cex = 1.5)

#  Principal Component Analysis (PCA)
# Perform PCA on numerical variables in the dataset (Temp, Ozone, Solar.R, Wind)
pca <- prcomp(airquality_imputed[, c("Temp", "Ozone", "Solar.R", "Wind")], scale.=TRUE)

# Scree Plot
par(mfrow=c(1, 1))  # Reset plot layout
plot(pca, type="l", main="Scree Plot")

# Summary of PCA
summary(pca)

# Biplot for PCA
biplot(pca, scale=0, main="PCA Biplot", cex=0.7, col=c("black", "blue"), xlabs=rep("", nrow(airquality)))




