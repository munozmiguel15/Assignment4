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
