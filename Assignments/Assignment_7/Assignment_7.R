library(tidyverse)
library(janitor)
library(GGally)

# Import the Assignment_7/Utah_Religions_by_County.csv
# Clean it up into “tidy” shape
df <- read.csv("Utah_Religions_by_County.csv") %>% 
  clean_names() %>% 
  pivot_longer(cols = -c(county, pop_2010, religious, non_religious),
               names_to = "religion",
               values_to = "proportion")

skimr :: skim(df)
summary(df)

# Address the questions:
# “Does population of a county correlate with the proportion of any specific religious group in that county?”
df %>% # Yes, it correlates with both catholic and lds religious groups, some others may have correlations, but there might not be enough data on other religious groups to clearly see it.
  ggplot(aes(x = proportion, y = pop_2010)) +
  geom_point() +
  geom_smooth(se = TRUE) +
  facet_wrap(~religion) +
  labs(title = "Population and Proportion of Religious Groups by County in Utah",
       x = "Proportion of Religious Group",
       y = "Population",
       color = "County") +
  theme_minimal()

# “Does proportion of any specific religion in a given county correlate with the proportion of non-religious people?”
df %>% # Yes, there is a negative correlation in the proportion of nonreligious people to religious people specifically in the lds religion
  ggplot(aes(x = non_religious, y = proportion)) + # The rest of the religions have no apparent correlation (maybe if broken down further)
  geom_point() +
  geom_smooth(se = TRUE) +
  facet_wrap(~religion) +
  labs(title = "Proportion of Religious Groups and Non-Religious People by County in Utah",
       x = "Proportion of Non-Religious People",
       y = "Proportion of Religious Group",
       color = "County") +
  theme_minimal()

# Explore the cleaned data set with a series of figures (I want to see you exploring the data set)
df %>% # Looking into just southern baptist convention
  filter(religion == "southern_baptist_convention") %>%
  ggplot(aes(x = proportion, y = pop_2010)) +
  geom_point() +
  geom_smooth(se = TRUE) +
  facet_wrap(~religion) +
  labs(title = "Population and Proportion of Religious Groups by County in Utah",
       x = "Proportion of Religious Group",
       y = "Population",
       color = "County") +
  theme_minimal()

df %>% # Looking into just evangelical
  filter(religion == "evangelical") %>%
  ggplot(aes(x = proportion, y = pop_2010)) +
  geom_point() +
  geom_smooth(se = TRUE) +
  facet_wrap(~religion) +
  labs(title = "Population and Proportion of Religious Groups by County in Utah",
       x = "Proportion of Religious Group",
       y = "Population",
       color = "County") +
  theme_minimal()

df %>% # Looking into just catholic
  filter(religion == "catholic") %>% 
  ggplot(aes(x = non_religious, y = proportion)) +
  geom_point() +
  geom_smooth(se = TRUE) +
  facet_wrap(~religion) +
  labs(title = "Proportion of Religious Groups and Non-Religious People by County in Utah",
       x = "Proportion of Non-Religious People",
       y = "Proportion of Religious Group",
       color = "County") +
  theme_minimal()

df %>% # I wanted to compare lds and catholic religions, as they are most prominent
  filter(religion == c("lds", "catholic")) %>%
  ggplot(aes(x = non_religious, y = proportion)) +
  geom_point() +
  geom_smooth(se = TRUE) +
  facet_wrap(~religion) +
  labs(title = "Proportion of Religious Groups and Non-Religious People by County in Utah",
       x = "Proportion of Non-Religious People",
       y = "Proportion of Religious Group",
       color = "County") +
  theme_minimal()


df %>% # Just taking a look into the same thing, but just lds
  filter(religion == "lds") %>% 
  ggplot(aes(x = non_religious, y = proportion)) +
  geom_point() +
  geom_smooth(se = TRUE) +
  facet_wrap(~religion) +
  labs(title = "Proportion of Religious Groups and Non-Religious People by County in Utah",
       x = "Proportion of Non-Religious People",
       y = "Proportion of Religious Group",
       color = "County") +
  theme_minimal()

# Just stick to figures and maybe correlation indices…no need for statistical tests yet
ggpairs(df, cardinality_threshold = 30)
#  I plotted the meaningful correlations from ggpairs below 
   # There are some repeats from above here too

df %>%
  ggplot(aes(x = pop_2010, y = religious)) +
  geom_point() +
  geom_smooth(se = TRUE) +
  facet_wrap(~religion) +
  theme_minimal()

df %>%
  ggplot(aes(x = pop_2010, y = non_religious)) +
  geom_point() +
  geom_smooth(se = TRUE) +
  facet_wrap(~religion) +
  theme_minimal()


df %>%
  ggplot(aes(x = religious, y = non_religious)) +
  geom_point() +
  geom_smooth(se = TRUE) +
  facet_wrap(~religion) +
  theme_minimal()

df %>%
  ggplot(aes(x = pop_2010, y = proportion)) +
  geom_point() +
  geom_smooth(se = TRUE) +
  facet_wrap(~religion) +
  theme_minimal()

df %>%
  ggplot(aes(x = proportion, y = religious)) +
  geom_point() +
  geom_smooth(se = TRUE) +
  facet_wrap(~religion) +
  theme_minimal()

# Here I just tried seeing what else I could look into (though, they don't really go anywhere)
df %>%
  ggplot(aes(x = religious, y = pop_2010, color = county)) +
  geom_point() +
  geom_smooth(se = TRUE) +
  theme_minimal() +
  facet_wrap(~religion)

df %>%
  ggplot(aes(x = county, y = religious)) +
  geom_point() +
  geom_smooth() +
  theme_minimal() +
  facet_wrap(~religion) +
  theme(axis.text.x = element_text(angle=90, hjust=1))

df %>%
  ggplot(aes(x = proportion, y = pop_2010, color = county)) +
  geom_point() +
  geom_smooth() +
  theme_minimal() +
  facet_wrap(~religion)