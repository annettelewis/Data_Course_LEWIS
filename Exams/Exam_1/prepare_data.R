library(tidyverse)

path <- "./data/csse_covid_19_daily_reports_us/"
outpath <- "./data/cleaned_covid_data.csv"
csvs <- list.files(path, full.names = TRUE,pattern=".csv$") %>% as.list()
read_covid <- function(x){
  read_csv(x,
           col_types = cols(
             Province_State = col_character(),
             Country_Region = col_character(),
             Last_Update = col_datetime(format = ""),
             Lat = col_double(),
             Long_ = col_double(),
             Confirmed = col_double(),
             Deaths = col_double(),
             Recovered = col_double(),
             Active = col_double(),
             FIPS = col_double(),
             Incident_Rate = col_double(),
             Total_Test_Results = col_double(),
             People_Hospitalized = col_double(),
             Case_Fatality_Ratio = col_double(),
             UID = col_double(),
             ISO3 = col_character(),
             Testing_Rate = col_double(),
             Hospitalization_Rate = col_double()
           ))
}

dat <- map(csvs, read_covid)

df <- reduce(dat,full_join)

df %>% 
  mutate(Last_Update = as.Date(Last_Update)) %>% 
  filter(ISO3 == "USA") %>% 
  filter(Province_State != "Diamond Princess") %>% 
  filter(Province_State != "Recovered") %>% 
  filter(Province_State != "Grand Princess") %>% 
  select(Province_State,Last_Update,Confirmed,Deaths,Recovered,Active,Case_Fatality_Ratio) %>% 
  write_csv(file=outpath)


df <- read_csv(outpath)


df %>% ggplot(aes(x=Last_Update,y=Recovered)) +
  geom_point() +
  facet_wrap(~Province_State)

df %>% ggplot(aes(x=Last_Update,y=Active)) +
  geom_point() +
  facet_wrap(~Province_State,scales = "free")

df %>% glimpse
