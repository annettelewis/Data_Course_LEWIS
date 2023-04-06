library(tidyverse)
library(janitor)
library(leaflet) #interactive street maps, this might even be more helpful (it works like google maps and you can add leaflets)
library(easystats)
library(modelr)
library(ggmap)
library(sf)
library(GGally)
library(plotly)
library(kableExtra)
library(patchwork)
theme_set(theme_minimal())
# sf package and even rgdal might be helpful for this, renderings and tiff files can be done with this

# data from tornadoes: New Orleans December 2022
# clean data
df <- read.csv("clean_data.csv") %>% 
  janitor::clean_names() %>% 
  mutate(roofsolar = case_when(roofsolar == "SOLAR PANEL" ~ TRUE)) %>% # This column was just "yes" or "no"
  mutate(roofshape = ifelse(roofshascr < 0.80, NA, roofshape)) %>% # I only want to include roofshapes that the computer was highly confident about
  select(-c(roofshascr, roofcondit_discolordetect, roofcondit_discolorscore, roofcondit_discolorpercen, 
            trampscr, roofcondit_tarppercen))

df$rooftopgeo <- gsub("POINT \\(|\\)", "", df$rooftopgeo) # clear unnecessary words to obtain long and lat

df <- df %>% # Lat -> y-axis; Long -> x-axis
  separate(rooftopgeo, into = c("long", "lat"), sep = " ", convert = TRUE) # separate long and lat

df$damage_level <- ifelse(df$damage_level == "", NA, df$damage_level) # Turn blank observations into NA


#----------------------------------------------------------------

mostdamage <- df %>% filter(catastrophescore >= 50)
nodamage <- df %>% filter(catastrophescore == 0)
decimated <-df %>% filter(catastrophescore == 100)
middamage <- df %>% filter(catastrophescore < 50 & catastrophescore >= 15)
leastdamage <- df %>% filter(catastrophescore < 15 & catastrophescore >= 2)
minimaldamage <- df %>% filter(catastrophescore == 1)

# Maps
alldamage <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(lng=mostdamage$long, lat=mostdamage$lat, color = "red", radius = 2) %>%
  addCircleMarkers(lng=middamage$long, lat=middamage$lat, color = "orange", radius = 2) %>%
  addCircleMarkers(lng=leastdamage$long, lat=leastdamage$lat, color = "blue", radius = 2) %>%
  addCircleMarkers(lng=nodamage$long, lat=nodamage$lat, color = "gray", radius = .5)
alldamage

damage <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(lng=mostdamage$long, lat=mostdamage$lat, color = "red", radius = 2) %>% 
  addCircleMarkers(lng=middamage$long, lat=middamage$lat, color = "orange", radius = 2) %>% 
  addCircleMarkers(lng=leastdamage$long, lat=leastdamage$lat, color = "blue", radius = 2)
damage

high <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(lng=mostdamage$long, lat=mostdamage$lat, color = "red", radius = 2)
high

mid <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(lng=middamage$long, lat=middamage$lat, color = "orange", radius = 2)
mid

least <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(lng=leastdamage$long, lat=leastdamage$lat, color = "blue", radius = 2)
least

none <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(lng=nodamage$long, lat=nodamage$lat, color = "gray", radius = .5)
none

destroyed <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(lng=decimated$long, lat=decimated$lat, color = "maroon", radius = 2)
destroyed

# Models:
mod1 <- df %>% glm(formula = catastrophescore ~ long + lat + roofshape + rooftree + roofmateri, family = "gaussian")
mod2 <- df %>% glm(formula = catastrophescore ~ long + lat + trampoline + deck + pool + enclosure + divingboar + waterslide + playground + sportcourt + primarystr + roofsolar + rooftree + roofmateri * roofshape, family = "gaussian")
mod3 <- df %>% glm(formula = catastrophescore ~ long + lat + enclosure + roofmateri + roofsolar + rooftree + roofshape, family = "gaussian")

formula(mod1)
formula(mod2)
formula(mod3)


leastdamage <- df %>% 
  filter(catastrophescore < 80)

leastdamage <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=leastdamage$long, lat=leastdamage$lat)
leastdamage


leaflet() %>% 
  addProviderTiles("Stamen.Toner") %>% 
  addPolygons(data = shape, fill = NA, color = "grey") %>% 
  addCircleMarkers(data = points, color = "red", radius = 3)


df$catastrophescore %>% unique()

# This is a reduced version of the dataset for faster loading times
practice <- df[1:20,]
m <- leaflet(data = practice) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=practice$long, lat=practice$lat, popup="Example")
m  # Print the map

# This will take the longest to load (FULL DATASET)
n <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=df$long, lat=df$lat)
n  # Print the map


mod1 <- df %>% glm(formula = catastrophescore ~ long*lat + roofshape, family = "binomial")
# I think my next goal is to separate the damage levels and characterization of the state

mutate

df %>% 
  ggplot(aes(x=b_imgdate, y=roofcondit_structuraldamagepercen, color = damage_level)) +
  geom_point() +
  facet_wrap(~roofmateri)


df$damage_level %>% unique()

roofcondit <- df %>% 
  select(starts_with("roofcondit_"), buildings_ids, damage_level)
names(df$roofcondit) <- c(paste0("visit",1:(ncol(bp)-1)), "pat_id")
#-------example code for what I want to modify (separate roofconditions to )
bp <- df %>% 
  select(starts_with("bp_"), pat_id)
names(bp) <- c(paste0("visit",1:(ncol(bp)-1)), "pat_id")

bp <- bp %>% 
  pivot_longer(starts_with("visit"),
               names_to = "visit",
               names_prefix = "visit",
               names_transform = as.numeric) %>% 
  separate(value, into = c('systolic', 'diastolic'), convert = TRUE)
#---------------------------

roofcondit <- df %>% 
  select(starts_with("roofcondit_"), buildings_ids)
names(bp) <- c(paste0("visit",1:(ncol(bp)-1)), "pat_id")

# This shows correlations of the most damage caused (trying to see if there's any common threads)
ggpairs(mostdamage, columns = , cardinality_threshold = 500)

count <- mostdamage[,16:30]
ggpairs(count, cardinality_threshold = 500)
ggpairs(roofcondit, cardinality_threshold = 40000)




df %>% 
  select(long, lat, roofmateri, roofshape, catastrophescore) %>% 
  ggpairs(cardinality_threshold = 40000)

df %>% 
  ggplot(aes(x=c(lat,long), y=catastrophescore, color = damage_level)) +
  geom_line() +
  facet_wrap(~roofshape)

df_grouped <- df %>%
  group_by(lat, long)

ggplot(df_grouped, aes(x = roofcondit, y = catastrophescore, color = roofshape)) +
  geom_point() +
  facet_wrap(~ roofmateri)





df$b_imgdate %>%  unique()


#Replace na values with blank using is.na()
df[is.na()] <- ""

#Display the dataframe
print(my_dataframe)


df$damage_level %>% class()

# first standardize the spatial objects
points <- sf::st_as_sf(points, coords = c("x", "y"), crs = 4326)
shape <- sf::st_transform(shape, crs = 4326)

# and then plot them
leaflet() %>% 
  addProviderTiles("Stamen.Toner") %>% 
  addPolygons(data = shape, fill = NA, color = "grey") %>% 
  addCircleMarkers(data = points, color = "red", radius = 3)

df$roofsolar %>%unique()

df %>% ggmap(rooftopgeo)


# library(GGally)
# ggpairs(df, columns = !c(building_ids, b_imgdate, ), cardinality_threshold = 45000)

df %>%
  leaflet(data = rooftopgeo)

m <- leaflet() %>% setView(lng = -71.0589, lat = 42.3601, zoom = 12)
m %>% addTiles()

ggmap()




pls <- df[,2:30]
ggpairs(pls, cardinality_threshold = 4500)

pls <- pls %>% select(trampscr, long, lat, roofshascr, roofcondit, 
                      rooftree, catastrophescore, roofcondit_structuraldamagepercen, 
                      roofcondit_missingmaterialpercen, roofcondit_debrispercent, 
                      roofcondit_discolorpercen, roofcondit_discolorscore, damage_level)
names(df)
df %>% select(lat, long, poolarea, trampoline, deck, pool, enclosure, divingboar, waterslide, playground, sportcourt, primarystr, catastrophescore) %>% 
  ggpairs()

song %>% select(rooftopgeo, poolarea, trampoline, deck, pool, enclosure, divingboar, waterslide, playground, sportcourt, primarystr, catastrophescore) %>% 
  ggpairs(cardinality_threshold = 45000)

# group long and lat together
grouped_pls <- pls %>% 
  group_by(long, lat) %>% 
  summarise_all(mean) %>% 
  ungroup()

# plot pairwise scatter plots
df %>% 
  select(-buildings_ids) %>% 
  ggpairs(cardinality_threshold = 4500)


ggpairs(pls, cardinality_threshold = 4500)
