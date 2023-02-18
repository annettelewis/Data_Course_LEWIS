library(tidyverse)
library(gganimate)
# Your task is to Write an R script that:
  # Cleans this data into tidy (long) form
dat <- read_csv("../../Data/BioLog_Plate_Data.csv") %>% 
  pivot_longer(cols = c(Hr_24, Hr_48, Hr_144),
  names_to = 'Hours',
  values_to = 'Absorbance') %>% 
  separate(col = Hours,
           into = c('trash','Time'),
           convert = TRUE, 
           sep = "_") %>% 
  select(-trash)

# Creates a new column specifying whether a sample is from soil or water
dat$Type <- ifelse (dat$`Sample ID` == "Soil_1", "Soil", 
                    ifelse(dat$`Sample ID` == "Soil_2", "Soil", "Water"))

# Generates a plot that matches this one (note just plotting dilution == 0.1):
dat %>%
    filter(Dilution == 0.1) %>% 
    group_by(Time, Type, Substrate) %>%
    summarize(mean_Absorbance = mean(Absorbance)) %>% 
    ggplot(aes(x = Time, y = mean_Absorbance, color = Type)) +
    geom_line() +
    geom_smooth(se = FALSE) +
    facet_wrap(~Substrate)

# Generates an animated plot that matches this one (absorbance values are mean of all 3 replicates for each group):
  # This plot is just showing values for the substrate “Itaconic Acid”
dat %>%
  filter(Substrate == "Itaconic Acid") %>% 
  group_by(Time, `Sample ID`, Dilution) %>% 
  summarize(mean_Absorbance = mean(Absorbance)) %>%
  ggplot(aes(x = Time, y = mean_Absorbance, color = `Sample ID`)) +
  geom_line() +
  facet_wrap(~Dilution) + 
  transition_reveal(Time)