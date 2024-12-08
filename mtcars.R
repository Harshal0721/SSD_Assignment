# Load the dataset
data(mtcars)

# Display the structure of the dataset
str(mtcars)

# Summary statistics for mpg
mpg_stats <- summary(mtcars$mpg)
mpg_sd <- sd(mtcars$mpg)

print(mpg_stats)
print(paste("Standard Deviation:", mpg_sd))

# Histogram
hist(mtcars$mpg, main="Histogram of MPG", xlab="Miles per Gallon", col="lightblue", border="black",breaks='Sturges')

# Boxplot
boxplot(mtcars$mpg, main="Boxplot of MPG", ylab="Miles per Gallon")

# Bar plot for cylinders
barplot(table(mtcars$cyl), main="Bar Plot of Cylinders", xlab="Number of Cylinders", ylab="Frequency", col="lightyellow")

# Pearson correlation between mpg and hp
cor_mpg_hp <- cor(mtcars$mpg, mtcars$hp)
print(paste("Pearson Correlation between mpg and hp:", cor_mpg_hp))

# Scatter plot with trend line
plot(mtcars$mpg, mtcars$hp, main="Scatter Plot of MPG vs HP", xlab="Miles per Gallon", ylab="Horsepower")
abline(lm(mtcars$hp ~ mtcars$mpg), col="red")  # Add trend line

# Fit a linear regression model
model <- lm(mpg ~ hp + wt, data=mtcars)

# Display the summary of the model
summary(model)
par(mfrow=c(2,2))
plot(model, which=1:4, labels.id=NULL) 
mtext("First Model", side = 3, outer = TRUE, line = -2, cex = 1.5)

#---------
influential_points <- c(17,20,31)
# Remove Influential Points
mtcars_clean <- mtcars[-influential_points, ]

# Re-run the Regression Model
model_updated <- lm(mpg ~ hp + wt, data=mtcars_clean)
summary(model_updated)

# Re-check Diagnostics
par(mfrow=c(2, 2))  # Reset 2x2 plot layout
plot(model_updated, which=1:4, labels.id=NULL)
mtext("Refined Model", side = 3, outer = TRUE, line = -2, cex = 1.5)
#---------


# Perform PCA on the numerical columns
pca <- prcomp(mtcars[, sapply(mtcars, is.numeric)], scale=TRUE)

# Plot the explained variance
par(mfrow=c(1,1))


plot(pca, type="l", main="Scree Plot")
summary(pca)
# Biplot of the first two principal components
biplot(pca, scale=0, main="PCA Biplot", cex=0.7, col=c("black", "blue"), xlabs=rep("", nrow(mtcars)))






