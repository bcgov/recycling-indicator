# Copyright 2019 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

## Load in libraries

library(readr)
library(dplyr)
library(envreportutils)
library(bcmaps)
library(sf)
library(rmapshaper)
library(lwgeom)

## Data Download -------------------------------------------------------

## Get British Columbia EPR Data sets from B.C. Data Catalogue
## data not currently stored on catalogue

data.dir <- soe_path("Operations ORCS/Data- Working/sustainability/EPR/")# to run on O:/


if (!file.exists(file.path("data/all.regions.csv"))) {
  source(file.path(scratch, 'clean_readxl.R'))
} else { "data is ready to load"

}

data.dir = "data"

all.regions <- read_csv(file.path(data.dir,'all.regions.csv'))

all.finance <- read_csv(file.path(data.dir,'all.finance.csv'))



# Read in population data -------------------------------
# BC Pop Stats (ignore and get directly from Stats Can)
# https://www.bcstats.gov.bc.ca/apps/PopulationEstimates.aspx
# manual export of population per regional district (2000 - 2018) and store in data folder

pop.0 <- read_csv(file.path(data.dir, 'Population_Estimates.csv'))

pop <- pop.0 %>%
  mutate(regional_district = gsub("-", " ", `Regional District`),
         pop = Total,
         year = as.character(Year)) %>%
  select(-c('X1','Gender','Regional District','Total',"Year")) %>%
  mutate(regional_district = ifelse(regional_district == "Powell River",
                                    "Qathet",
                                    ifelse(regional_district == "Stikine",
                                           "Kitimat Stikine",
                                           ifelse(regional_district == "Comox Valley",
                                                  "Comox Strathcona",
                                                  ifelse(regional_district == "Strathcona",
                                                         "Comox Strathcona",
                                                         regional_district)))))


pop <- pop %>%
  group_by(regional_district, year) %>%
  summarize(pop = sum(pop))


# read in spatial data ------------------------------------------------------------

reg_dist <- combine_nr_rd() %>%
  rmapshaper::ms_simplify(0.01) %>%
  st_intersection(bc_bound()) %>%
  st_transform(4326) %>%
  group_by(ADMIN_AREA_NAME,ADMIN_AREA_ABBREVIATION) %>%
  summarize()

