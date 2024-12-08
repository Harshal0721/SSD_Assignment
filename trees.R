# Load and inspect the dataset
data(trees)
str(trees)

#  Summary Statistics for a Numerical Variable (e.g., 'Height')
summary(trees$Height)  # Summary of the Height variable
sd(trees$Height)  # Standard deviation of the Height variable

#  Visualization: Histogram and Boxplot for 'Height'
# Histogram for Height
hist(trees$Height, main="Histogram of Height", xlab="Height", col="lightblue")

# Boxplot for Height
boxplot(trees$Height, main="Boxplot of Height", ylab="Height")

#  Correlation between Numerical Variables (e.g., Height and Volume)
cor(trees$Height, trees$Volume)

#  Scatter Plot with Trend Line between 'Height' and 'Volume'
plot(trees$Height, trees$Volume, main="Scatter Plot of Height vs Volume", 
     xlab="Height", ylab="Volume", pch=19, col="blue")
abline(lm(trees$Volume ~ trees$Height), col="red")

#  Linear Regression Model for 'Volume' prediction using 'Height' and 'Girth'
model <- lm(Volume ~  Girth + Height , data=trees)
summary(model)
mtext("First Model", side = 3, outer = TRUE, line = -2, cex = 1.5)


#  Diagnostic Plots for the Regression Model
par(mfrow=c(2,2))
plot(model, which=1:4, labels.id=NULL)

#Refined Model(after Log transformation)
model_log <- lm(log(Volume) ~ Girth + Height, data = trees)
summary(model_log)
par(mfrow=c(2,2))
plot(model_log, which=1:4, labels.id=NULL)
mtext("Refined Model", side = 3, outer = TRUE, line = -2, cex = 1.5)


#  Principal Component Analysis (PCA)
# Perform PCA on numerical variables in the dataset (Height and Volume)
pca <- prcomp(trees, scale.=TRUE)

# Scree Plot
par(mfrow=c(1, 1))  # Reset plot layout
plot(pca, type="l", main="Scree Plot")

# Summary of PCA
summary(pca)

# Biplot for PCA
biplot(pca, scale=0, main="PCA Biplot", cex=0.7, col=c("black", "blue"), xlabs=rep("", nrow(trees)))





