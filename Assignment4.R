gauge <- read.table("gauge.txt", header = T)
library("ggplot2")

newdata<- aggregate(gauge[,"gain"], list(gauge$density), mean)
colnames(newdata) <- c("density", "avg_gain")
newdata$trans_avg_gain <- log(newdata$avg_gain)
newdata

# Non-Transformed Plot
ggplot(newdata, aes(x=avg_gain, y=density)) +
  geom_point() + geom_smooth(method = "lm", se = TRUE)

linear_model <- lm(density ~ avg_gain, data = newdata) 

# Transformed Plot
ggplot(newdata, aes(x=trans_avg_gain, y=density)) +
  geom_point() + geom_smooth(method = "lm", se = TRUE)

trans_linear_model <- lm(density ~ trans_avg_gain, data = newdata) 

# Prediction Intervals
pred_dataframe <- cbind(newdata, predict(trans_linear_model, interval = "prediction")) 

# Prediction Interval Graph
ggplot(pred_dataframe, aes(x = trans_avg_gain)) + 
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = "PI", color = "PI", alpha = "PI"),
              linetype = 2, size = 1) + 
  geom_point(aes(y = density)) +
  geom_smooth(aes(y = density,  fill = "CI", color = "CI", alpha = "CI"), 
              method = "lm", se = TRUE) + 
  labs(x = "ln(Gain)", y = "Density (g/cm^3)", title = "Scatterplot of Density vs. Transformed") +
  scale_fill_manual('',values = c("CI"="tomato", "PI"="salmon1")) + 
  scale_color_manual('', values = c("CI" = "grey37", "PI" = "maroon2")) + 
  scale_alpha_manual('', values = c("CI" = 0.3, "PI" = 0.2))


#####################################Old loop##########################################
coefficients_DF <- data.frame(matrix(nrow = 9, ncol = 2))
colnames(coefficients_DF) <- names(trans_linear_model$coefficients)

prediction_DF <- data.frame(matrix(ncol = 3, nrow = 9))
names(prediction_DF) <- c("prediction", "lwr", "upr")

for (i in 1:9) {
  subset_data <- newdata[-i,]
  
  trans_linear_model_subset <- lm(trans_avg_gain ~ density, data = subset_data) 
  
  coefficients_DF[i,] <- trans_linear_model_subset$coefficients
  
  prediction_DF[i, ] <- predict(trans_linear_model_subset, newdata = data.frame("density" = newdata[i, "density"]), 
                                interval = "prediction", level = 0.95)
  
}

prediction_DF$actual = newdata$trans_avg_gain


######################################new loop#############################################
coefficients_DF <- data.frame(matrix(nrow = 9, ncol = 2))
colnames(coefficients_DF) <- names(trans_linear_model$coefficients)

prediction_DF <- data.frame(matrix(ncol = 3, nrow = 9))
names(prediction_DF) <- c("prediction", "lwr", "upr")

for (i in 1:9) {
  subset_data <- newdata[-i,]
  
  trans_linear_model_subset <- lm(density ~ trans_avg_gain, data = subset_data) 
  
  coefficients_DF[i,] <- trans_linear_model_subset$coefficients
  
  prediction_DF[i, ] <- predict(trans_linear_model_subset, newdata = data.frame("trans_avg_gain" = newdata[i, "trans_avg_gain"]), 
                                interval = "prediction", level = 0.95)
  
}

prediction_DF$actual = newdata$density
#####################################################################################
gauge <- read.table("gauge.txt", header = T)
library("ggplot2")

newdata<- aggregate(gauge[,"gain"], list(gauge$density), mean)
colnames(newdata) <- c("density", "avg_gain")
newdata$trans_avg_gain <- log(newdata$avg_gain)
newdata

# Non-Transformed Plot
ggplot(newdata, aes(x=avg_gain, y=density)) +
  geom_point() + geom_smooth(method = "lm", se = TRUE, aes(fill = "CI"), color = "grey37", alpha = 0.3) + 
  scale_fill_manual('', values = c("CI" = "tomato")) +
  labs(x = "Gain", y = "Density (g/cm^3)", title = "Scatterplot of Density vs. Gain")

linear_model <- lm(density ~ avg_gain, data = newdata) 

# Transformed Model
trans_linear_model <- lm(density ~ trans_avg_gain, data = newdata) 

# Prediction Intervals
pred_dataframe <- cbind(newdata, predict(trans_linear_model, interval = "prediction")) 

# Prediction Interval Graph
ggplot(pred_dataframe, aes(x = trans_avg_gain)) + 
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = "PI", color = "PI", alpha = "PI"),
              linetype = 2, size = 1) + 
  geom_point(aes(y = density)) +
  geom_smooth(aes(y = density,  fill = "CI", color = "CI", alpha = "CI"), 
              method = "lm", se = TRUE) + 
  labs(x = "ln(Gain)", y = "Density (g/cm^3)", title = "Scatterplot of Density vs. Transformed") +
  scale_fill_manual('',values = c("CI"="tomato", "PI"="salmon1")) + 
  scale_color_manual('', values = c("CI" = "grey37", "PI" = "maroon2")) + 
  scale_alpha_manual('', values = c("CI" = 0.3, "PI" = 0.2))



# Function for ggplot Q-Q Plot
qqplot.data <- function (vec, title = NULL, col = "black", linecol = "black", width = 1) # argument: vector of numbers
{
  # following four lines from base R's qqline()
  y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  
  d <- data.frame(resids = vec)
  
  ggplot(d, aes(sample = resids)) + stat_qq(colour = col) + 
    geom_abline(slope = slope, intercept = int, col = linecol, size = width) + 
    ggtitle(title) + xlab("Theoretical Quantiles") + ylab("Sample Quantiles")
  
}

# Residual Analysis
install.packages("MASS")
library(MASS)

summary(trans_linear_model)

# Dataframe Set-Up
residual_DF <- data.frame(
  "density" = newdata$density,
  "trans_avg_gain" = newdata$trans_avg_gain,
  "predictions" = trans_linear_model$fitted.values,
  "residuals" = trans_linear_model$residuals,
  "std_residuals" = trans_linear_model$residuals / sd(trans_linear_model$residuals),
  "jackknifed_residuals" = studres(trans_linear_model)
)

# Plots
ggplot(residual_DF, aes(x = trans_avg_gain, y = jackknifed_residuals)) + 
  geom_point(color = "grey37", size = 2) + 
  geom_path(color = "tomato", size = 1, alpha = 0.75) +
  labs(x = "Transformed Gain", y = "Jack Knifed Residuals", title = "Jack Knifed Residuals by Transformed Gain") 

ggplot(residual_DF, aes(x = density, y = jackknifed_residuals)) + 
  geom_point(color = "grey37", size = 2) + 
  geom_path(color = "tomato", size = 1, alpha = 0.75) +
  labs(x = "Density (gm/cm^3)", y = "Jack Knifed Residuals", title = "Jack Knifed Residuals by Density") 


qqplot.data(residual_DF$jackknifed_residuals, title = "Q-Q Plot of Jack Knifed Residuals", col = "black", linecol = "tomato")




# Non Transformed Residual Analysis
summary(trans_linear_model)

summary(trans_linear_model)

# RESIDUALS FOR NON TRANSFORMED MODEL 
# Dataframe Set-Up
residual_nontrans_DF <- data.frame(
  "density" = newdata$density,
  "avg_gain" = newdata$avg_gain,
  "predictions" = linear_model$fitted.values,
  "residuals" = linear_model$residuals,
  "std_residuals" = linear_model$residuals / sd(linear_model$residuals),
  "jackknifed_residuals" = studres(linear_model)
)

# Plots
ggplot(residual_nontrans_DF, aes(x = avg_gain, y = jackknifed_residuals)) + 
  geom_point(color = "grey37", size = 2) + 
  geom_path(color = "tomato", size = 1, alpha = 0.75) +
  labs(x = "Gain", y = "Jack Knifed Residuals", title = "Jack Knifed Residuals by Gain") 

ggplot(residual_nontrans_DF, aes(x = density, y = jackknifed_residuals)) + 
  geom_point(color = "grey37", size = 2) + 
  geom_path(color = "tomato", size = 1, alpha = 0.75) +
  labs(x = "Density (gm/cm^3)", y = "Jack Knifed Residuals", title = "Jack Knifed Residuals by Density") 


qqplot.data(residual_nontrans_DF$jackknifed_residuals, title = "Q-Q Plot of Jack Knifed Residuals", col = "black", linecol = "tomato")

#Confidence Interval
sigmahat = ((1/7)*sum((trans_linear_model$residuals)^2))^(1/2)
xbar = sum(newdata$trans_avg_gain)/9
conf_int = sum((newdata$trans_avg_gain-xbar)^2)