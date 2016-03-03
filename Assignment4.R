####Code so far####
gauge <- read.table("gauge.txt", header = T)
library("ggplot2")

newdata<- aggregate(gauge[,"gain"], list(gauge$density), mean)
colnames(newdata) <- c("density", "avg_gain")
newdata$trans_avg_gain <- log(newdata$avg_gain)
newdata

# Non-Transformed Plot
ggplot(newdata, aes(x=density, y=avg_gain)) +
  geom_point() + geom_smooth(method = "lm", se = TRUE)

linear_model <- lm(trans_avg_gain ~ density, data = newdata) 

# Transformed Plot
ggplot(newdata, aes(x=density, y=trans_avg_gain)) +
  geom_point() + geom_smooth(method = "lm", se = TRUE)

trans_linear_model <- lm(trans_avg_gain ~ density, data = newdata) 

# Prediction Intervals
pred_dataframe <- cbind(newdata, predict(trans_linear_model, interval = "prediction")) 

# Prediction Interval Graph
ggplot(pred_dataframe, aes(x = density)) + 
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = "PI", color = "PI", alpha = "PI"),
              linetype = 2, size = 1) + 
  geom_point(aes(y = trans_avg_gain)) +
  geom_smooth(aes(y = trans_avg_gain,  fill = "CI", color = "CI", alpha = "CI"), 
              method = "lm", se = TRUE) + 
  labs(x = "Density (g/cm^3)", y = "ln(Gain)", title = "Scatterplot of Transformed Gain vs. Density") +
  scale_fill_manual('',values = c("CI"="tomato", "PI"="salmon1")) + 
  scale_color_manual('', values = c("CI" = "grey37", "PI" = "maroon2")) + 
  scale_alpha_manual('', values = c("CI" = 0.3, "PI" = 0.2))
