## Combine CSVs of GPS points ## 


# setup -----------------------------------------------------------------

library(tidyverse)


# True Grass --------------------------------------------------------------

# find the appropriate files
all.files <- list.files('data/raw')
site.files <- all.files[grepl('TrueGrass', all.files)]

# read in the data 
# first data points
points_12.21.23 <- 
  read_csv(paste0('data/raw/', site.files[3])) %>%
  mutate(
    Site = 'True Grass',
    Species = 'UmCa',
    School = 'Laurence Jones Middle School',
    Grade = 6,
    Date = '12/15/23')
  
points_1.5.24 <- 
  read_csv(r"(data\raw\TrueGrass_1-5-24.csv)") %>% 
  mutate(
    Site = 'True Grass',
    Name = str_replace(string = Name, pattern = '_', replacement = "-"),
    Name = str_replace(string = Name, pattern = 'Dog', replacement = "CoSe"),
    Name = str_replace(string = Name, pattern = 'Wil', replacement = "SaSp"),
    Species = substring(Name,4,7))

points_2.21.24 <-
  read_csv(paste0('data/raw/', site.files[1])) %>% 
  mutate(
    Site = 'True Grass',
    Species = substring(Name,4,7),
    School = NA,
    Grade = NA,
    Date = NA)
  
# combine new and old files 

TrueGrass_plnt_points <- 
  rbind(points_12.21.23,
        points_1.5.24,
        points_2.21.24)

write_csv(TrueGrass_plnt_points, 'data/clean/TrueGrass_PlantCoordinates.csv')


# Cohen -------------------------------------------------------------------

# find the appropriate files
all.files <- list.files('data/raw')
site.files <- all.files[grepl('SoMar', all.files)]

# create the empty repository
site_points <- tibble()

# read in the data 
for(x in site.files) {
  # read in raw data and add species label
  raw_points <- 
    read.csv(paste0('data/raw/', x)) %>%
    mutate(Species = substring(Name,4,7))
  
  # add raw data to repository
  site_points <- rbind(site_points, raw_points)
  
  # proof the data
  print(
    site_points %>% 
          # find the number of points for each species
          group_by(Species) %>% 
          summarize(
            count = n()) %>% 
          # find the maximum number label for each species
          left_join(
            site_points %>% 
              group_by(Species) %>% 
              filter(Number == max(Number)) %>% 
              dplyr::select(Number, Name, Species)),
    n = nrow(site_points))}

site_points <-
  site_points %>% 
  mutate(
    Site = 'SoMar')

# pull planting date
site_points <- 
  site_points %>% 
  mutate(dup = Name) %>% 
  separate(dup, into = c('trash01', 'trash02', 'Date'), sep= '-') %>% 
  filter(!grepl('B', Date)) %>% 
  mutate(Date = ifelse(
    is.na(Date),
    NA,
    paste0(Date, '/24')),
    `Planting Date` = lubridate::mdy(Date),
    School = case_when(
      `Planting Date` == lubridate::mdy('1/17/2024') ~ 'Petaluma Jr. High',
      `Planting Date` == lubridate::mdy('1/18/2024') ~ 'Petaluma Jr. High',
      `Planting Date` == lubridate::mdy('1/24/2024') ~ 'Staff'),
    Grade = if_else(
      School == "Petaluma Jr. High",
      '7',
      'Adult')) %>% 
  dplyr::select(-c(Date, trash01, trash02))

write_csv(site_points,
          paste0('data/clean/',
                 unique(site_points$Site),
                 "_PlantCoordinates_",
                 Sys.Date(), ".csv"))

# proof plant numbers
proof_checklist <- 
  site_points %>% 
  # find the number of points for each species
  group_by(Species) %>% 
  summarize(
    count = n()) %>% 
  # find the maximum number label for each species
  left_join(
    site_points %>% 
      group_by(Species) %>% 
      filter(Number == max(Number)) %>% 
      dplyr::select(Number, Name, Species))

# need to proof

# CoSe
# CrDo
# AeCa
