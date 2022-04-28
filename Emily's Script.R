library(tidyverse)
library(readxl)
install.packages("devtools")
devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)
library(sf)
library(units)

pop_data <-
  read_excel("data/mn_county_estimates_sdc_2019_tcm36-442553.xlsx") %>% 
  transmute(
    county = `County Name`,
    population = `Total Population, 2019`
  ) %>% 
  print()

pop_cattle <-
  read_excel("data/cattle data 2020.xlsx") %>% 
  print()

pop_wolves <-
  read_excel("data/Wolf pop per county all MN Counties.xlsx") %>% 
  print()

all_data <-
  pop_data %>% 
  left_join(pop_cattle) %>% 
  left_join(pop_wolves) %>% 
  print()

counties_sf <- get_urbn_map(map = "counties", sf = TRUE)


counties_sf %>% 
  filter(state_name == "Minnesota") %>%
  mutate(
    county = str_remove(county_name, " County"),
    area = as.numeric(st_area(geometry) / 1000000)
  ) %>% 
  left_join(all_data, by = "county") %>% 
  mutate(
    population_density = population / area,
    wolf_density = n_wolves / area,
    human_wolf_ratio = wolf_density/population_density
  ) %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = human_wolf_ratio),
          color = "#D1E5F0", size = 0.05) +
  coord_sf(datum = NA) +
  labs(fill = "Ratio",
       subtitle = "Ratio of Wolf Density to Human Density",
       title = "Prediction of Human-Wolf Conflict")


counties_sf %>%
  filter(state_name == "Minnesota") %>%
  mutate(
    county = str_remove(county_name, " County"),
    area = as.numeric(st_area(geometry) / 1000000)
  ) %>%
  left_join(all_data, by = "county") %>%
  mutate(
    cattle_density = n_cattle_calves / area,
    wolf_density = n_wolves / area,
    cattle_wolf_ratio = wolf_density/cattle_density
  ) %>%
  ggplot() +
  geom_sf(mapping = aes(fill = cattle_wolf_ratio),
          color = "#D1E5F0", size = 0.05) +
  coord_sf(datum = NA) +
  labs(fill = "Ratio",
       subtitle = "Ratio of Wolf Density to Cattle Density",
       title = "Prediction of Wolf Predation on Cattle")

       
density_data <-
  counties_sf %>% 
  filter(state_name == "Minnesota") %>%
  select(county_name) %>% 
  rename(county = county_name) %>% 
  mutate(
    county = str_remove(county, " County"),
    area = as.numeric(st_area(geometry) / 1000000)
  ) %>% 
  left_join(all_data, by = "county") %>% 
  mutate(
    cattle_density = n_cattle_calves / area,
    wolf_density = n_wolves / area,
    population_density = population /area
  ) %>%
  st_drop_geometry() %>%
  as_tibble() %>% 
  print()
 density_data


ggplot(data = density_data) +
  geom_histogram(mapping = aes(x = population_density), binwidth = 50)

ggplot(data = density_data) +
  geom_histogram(mapping = aes(x = wolf_density), binwidth = 50)




       
       