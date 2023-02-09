# Task 1:
read.csv("cleaned_covid_data.csv")

# Task 2:
library(tidyverse)
covid <- read.csv("cleaned_covid_data.csv")
A_states <- covid %>%
  filter(grepl("^A", Province_State))

# Task 3:
A_states %>% 
  ggplot(aes(x = Last_Update, y = Deaths)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~Province_State, scales = "free") +
  labs(x = "Time")

# Task 4:
library(dplyr)
state_max_fatality_rate <- covid %>%
  group_by(Province_State) %>%
  summarize(Maximum_Fatality_Ratio = max(Case_Fatality_Ratio, na.rm = TRUE)) %>%
  ungroup()

state_max_fatality_rate <- state_max_fatality_rate %>% 
  arrange(desc(Maximum_Fatality_Ratio))

head(state_max_fatality_rate)

# Task 5:
state_max_fatality_rate %>%
  ggplot(aes(x = reorder(Province_State, Maximum_Fatality_Ratio, decreasing = TRUE), y = Maximum_Fatality_Ratio)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Province_State", y = "Maximum_Fatality_Ratio")

# Task 6:
cumulative_deaths <- covid %>%
  group_by(Last_Update) %>%
  summarize(total_deaths = sum(Deaths))

cumulative_deaths %>% 
  ggplot(aes(x = Last_Update, y = total_deaths, group = 1)) +
  geom_line() +
  xlab("Date") +
  ylab("Cumulative Deaths")