# Load and inspect the dataset
data(USJudgeRatings)
str(USJudgeRatings)


# Summary statistics for a numerical variable (e.g., INTG)
summary(USJudgeRatings$INTG)
sd(USJudgeRatings$INTG)

# Visualization: Histogram and Boxplot
hist(USJudgeRatings$INTG, main="Histogram of INTG", xlab="Integrity Rating", col="lightblue")
boxplot(USJudgeRatings$INTG, main="Boxplot of INTG", ylab="Integrity Rating")

###################################################################################

# Correlation between INTG and DMNR
cor(USJudgeRatings$INTG, USJudgeRatings$DMNR)

# Scatter plot with trend line
plot(USJudgeRatings$INTG, USJudgeRatings$DMNR, main="Scatter Plot of INTG vs DMNR", 
     xlab="Integrity", ylab="Demeanor", pch=19, col="blue")
abline(lm(USJudgeRatings$DMNR ~ USJudgeRatings$INTG), col="red")

# Linear regression: Predict RTEN using INTG, DMNR, and CFMG
model <- lm(RTEN ~ DMNR + CFMG, data=USJudgeRatings)
summary(model)

# Diagnostics: Residual plots
par(mfrow=c(2,2))
plot(model, which=1:4, labels.id=NULL) 
mtext("First Model", side = 3, outer = TRUE, line = -2, cex = 1.5)


# Refined Model

influential_points <- c(10,23,40)
 #Remove Influential Points
USJudgeRatings_clean <- USJudgeRatings[-influential_points, ]

# Re-run the Regression Model
model_updated <- lm(RTEN ~ DMNR + CFMG , data=USJudgeRatings_clean)
summary(model_updated)

#Re-check Diagnostics
par(mfrow=c(2, 2))  # Reset 2x2 plot layout
plot(model_updated, which=1:4, labels.id=NULL)
mtext("Refined  Model", side = 3, outer = TRUE, line = -2, cex = 1.5)

###################################################################################

# Principal Component Analysis (PCA)
pca <- prcomp(USJudgeRatings, scale.=TRUE)
par(mfrow=c(1,1))
plot(pca, type="l", main="Scree Plot")
summary(pca)
biplot(pca, scale=0, main="PCA Biplot", cex=0.7, col=c("black", "blue"), xlabs=rep("", nrow(USJudgeRatings)))



