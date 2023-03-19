library(janitor)
library(tidyverse)
library(easystats)
library(readr)
library(patchwork)
library(modelr)
library(broom)
library(tidyr)

# 1. Read in the unicef data (10 pts) 
# 2. Get it into tidy format (10 pts)
df <- read.csv("./unicef-u5mr.csv") %>% 
  janitor::clean_names() %>%
  pivot_longer(cols = starts_with("u5mr_"),
               names_to = "year",
               values_to = "u5mr") %>%
  mutate(year = str_remove(year, "u5mr_"))

# 3. Plot each country’s U5MR over time (20 points)
df %>%
  ggplot(aes(x = as.numeric(year), y = u5mr, group = country_name)) +
  geom_line() +
  facet_grid(rows = vars(year), cols = vars(continent)) +
  facet_wrap(~continent) +
  scale_x_continuous(breaks = c(1960, 1980, 2000), 
                     labels = c("1960", "1980", "2000"), 
                     expand = c(0,0)) +
  labs(x = "Year", 
       y = "U5MR") +
  theme_minimal()

# 4. Save this plot as LASTNAME_Plot_1.png (5 pts) 
ggsave("./LEWIS_Plot_1.png", dpi = 300)

# 5. Create another plot that shows the mean U5MR for all the countries within a given continent at each year (20 pts)
df %>%
  group_by(continent, year) %>%
  summarise(mean_u5mr = mean(u5mr, na.rm = TRUE)) %>%
  ggplot(aes(x = as.numeric(year), y = mean_u5mr, group = continent, color = continent)) +
  geom_line() +
  scale_x_continuous(breaks = c(1960, 1980, 2000), 
                     labels = c("1960", "1980", "2000"), 
                     expand = c(0,0)) +
  labs(x = "Year",
       y = "Mean U5MR",
       color = "Continent") +
  theme_minimal()

# 6. Save that plot as LASTNAME_Plot_2.png (5 pts) 
ggsave("./LEWIS_Plot_2.png", dpi = 300)

# 7. Create three models of U5MR (20 pts)
mod1 <- glm(data= df,
            formula = u5mr ~ year)
mod2 <- glm(data= df,
            formula = u5mr ~ year + continent)
mod3 <- glm(data= df,
            formula = u5mr ~ year * continent)

# 8. Compare the three models with respect to their performance
summary(mod1) # There appears to be a general decrease (u5mr) throughout the years (specifically 1971 through 2015)
summary(mod2) # Similar to model 1 (but from 1966 through 2015), including continents with significant decreases in Europe, Oceania, the Americas, and Asia respectively
summary(mod3) # 1954 through 2015, there are a lot of years that stand out in the Americas, specifically

check_model(mod1) # this model appears to fit decently well
check_model(mod2) # this model also appears to fit decently well and accounts for contentent as well as year
check_model(mod3) # this model appears to fit well, but it has a high collinearity between variables, which isn't desireable

testing <- sample(1:nrow(df), size = round(nrow(df)*.2))
test <- df[testing,]
train <- df[-testing,]

   # mod2: the full model has a higher R2 value than the test (as expected)
rsquare(mod2,test) # test model
rsquare(mod2, df) # full model 

   # mod3: the full model has a higher R2 value than the test (as expected)
rsquare(mod3,test) # test model
rsquare(mod3, df) # full model

performance(mod1)
performance(mod2)
performance(mod3) # mod3 appears to work well, but the issue of collinearity negatively impacts my view of the model

compare_performance(mod1, mod2, mod3) %>% plot # Here, it appears that mod 3 accounts for more of what is seen, this will need to be analyzed further

# Comment: Out of the three models, mod2 appears to be the most accurate as it accounts for the variables year and continent, while not having a high amount of collinearity (unlike mod3) 

# 9. Plot the 3 models’ predictions like so: (10 pts)
mod1_pred <- add_predictions(df, model = mod1) %>% select("pred")
mod2_pred <- add_predictions(df, model = mod2) %>% select("pred")
mod3_pred <- add_predictions(df, model = mod3) %>% select("pred")
pred_df <- bind_cols(mod1_pred, mod2_pred, mod3_pred)

df <- bind_cols(df, pred_df) %>% 
  rename(mod1_pred = pred...6) %>% 
  rename(mod2_pred = pred...7) %>% 
  rename(mod3_pred = pred...8) %>% 
  select(-ends_with("...9"), -ends_with("...10"), -ends_with("...11")) %>% 
  pivot_longer(cols = c(mod1_pred, mod2_pred, mod3_pred), 
               names_to = "model", 
               values_to = "pred") %>% 
  mutate(model = sub("_pred", "", model))

df %>%
  ggplot(aes(x = as.numeric(year), y = pred, color = continent)) +
  facet_grid(rows = vars(year), cols = vars(continent)) +
  facet_wrap(~model) +
  scale_x_continuous(breaks = c(1960, 1980, 2000), 
                     labels = c("1960", "1980", "2000"), 
                     expand = c(0,0)) +
  labs(x = "Year", 
       y = "Predicted U5MR", 
       title = "Model Predictions",
       color = "Continent") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  stat_smooth(method = "lm", se = FALSE)