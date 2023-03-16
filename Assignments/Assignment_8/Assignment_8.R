library(modelr)
library(easystats)
library(broom)
library(tidyverse)
library(fitdistrplus)
library(patchwork)

# 1. loads the “/Data/mushroom_growth.csv” data set
df <- read_csv("../../Data/mushroom_growth.csv")
# creates several plots exploring relationships between the response and predictors
df %>% 
  ggplot(aes(x = Light, y = GrowthRate, color = Species)) +
  geom_boxplot(alpha = .5) +
  facet_wrap(~Humidity)

df %>% 
  ggplot(aes(x = Nitrogen, y = GrowthRate, color = Species)) +
  geom_boxplot(alpha = .5) +
  facet_wrap(~Humidity)

df %>% 
  ggplot(aes(x = Temperature, y = GrowthRate, color = Species)) +
  geom_boxplot(alpha = .5) +
  facet_wrap(~Humidity)

df %>% 
  ggplot(aes(x = Species, y = GrowthRate)) +
  geom_boxplot(alpha = .5) +
  facet_wrap(~Humidity)

df %>% 
  ggplot(aes(x = Humidity, y = GrowthRate, color = Temperature)) +
  geom_boxplot(alpha = .5) +
  facet_wrap(~Species)

# defines at least 4 models that explain the dependent variable “GrowthRate”
mod1 <- glm(data= df,
            formula = GrowthRate ~ Humidity + Temperature + Light + Species)
summary(mod1)
check_model(mod1) # Somewhat matches, but not extremely close

mod2 <- glm(data= df,
    formula = GrowthRate ~ Species * Humidity * Light)
summary(mod2)
check_model(mod2) # Looks more accurate than mod1 based on this

mod3 <- glm(data= df,
    formula = GrowthRate ~ Light + Humidity)
summary(mod3)
check_model(mod3) # This one appears to fit pretty well (not a perfect fit)

mod4 <- glm(data= df,
    formula = GrowthRate ~ Temperature * Species)
summary(mod4)
check_model(mod4) # This one is alright but also not super close and has high colliniarity values

mod5 <- glm(data= df,
            formula = GrowthRate ~ Humidity*Temperature*Nitrogen*Light*Species)
check_model(mod5) # As expected, very high collinearity 

compare_performance(mod1, mod2, mod3, mod4) %>% plot # mod2 appears to explain what is seen the best
compare_performance(mod1, mod2, mod3, mod4, mod5) %>% plot # In this mod5 appears best, but it has high collinearity, which suggests it isn't the best model
   # I am going to omit mod5

# calculates the mean sq. error of each model
compare_performance(mod1, mod2, mod3, mod4)
# selects the best model you tried
 # Out of these models, we can see that mod2 has the highest AICc weight, highest R2 value, and the lowest RMSE, which point to this being the most accurate model
df %>% 
  gather_predictions(mod1,mod2,mod3,mod4, type = "response") %>% 
  ggplot(aes(x=Light,y= GrowthRate)) +
  geom_point(aes(y=pred,color=model), size=3, alpha =.5) +
  geom_smooth(alpha = .5, aes(y=pred,color=model)) +
  theme_minimal()

# adds predictions based on new hypothetical values for the independent variables used in your model
pred <- add_predictions(df, mod2, type = "response")
pred %>%
  ggplot(aes(x=Light, y=GrowthRate, color =Species)) + 
  geom_smooth() +
  geom_point(size = 3, alpha =.5) +
  facet_wrap(~Humidity) +
  theme_minimal()
head(pred)

# plots these predictions alongside the real data
   # Original data
df %>%
  ggplot(aes(x = Light, y = GrowthRate, color = Species)) +
  geom_point(size = 3, alpha = .5) +
  facet_wrap(~Humidity) +
  geom_smooth() +
  theme_minimal()

   # Prediction
pred %>%
  ggplot(aes(x = Light, y = pred, color = Species)) +
  geom_point(size = 3, alpha = .5, shape = 21, fill = "black") +
  geom_point(data = df, aes(x = Light, y = GrowthRate, color = Species), size = 3, alpha = .5) +
  geom_smooth(aes(y = pred), alpha = .025) +
  facet_wrap(~Humidity) +
  theme_minimal()

pred %>% # This graph is essentially the same thing as the one above, just without black points
  ggplot(aes(x = Light, y = pred, color = Species)) +
  geom_point(data = df, aes(x = Light, y = GrowthRate, color = Species), size = 3, alpha = .5) +
  geom_smooth(aes(y = pred), alpha = .5) +
  facet_wrap(~Humidity) +
  theme_minimal()

performance(mod2)

# Write the code you would use to model the data found in “/Data/non_linear_relationship.csv” with a linear model (there are a few ways of doing this)
dmv <- read_csv("../../Data/non_linear_relationship.csv")
dmv %>% ggplot(aes(x=predictor, y=response)) +
  geom_point()
# linear model
newmodel <- lm(response ~ predictor, data = dmv)
summary(newmodel)
