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

x <- c("dplyr","ggplot2","tidyr","stringr","reshape", "bcmaps", "sf", "envreportutils") #raster","sp","sf","rgdal","xlsx","rJava","tibble","mapview","gtools")
lapply(x, library, character.only = TRUE) ; rm(x)  # load the required packages

source('00_Functions.R')

## Load  data files
#data.dir <- "data/" # to run on C:
data.dir <- soe_path("Operations ORCS/Data - Working/sustainability/EPR/")# to run on O:/

## DATA INCLUDES
# - beverage         (drafted)
# - oil filters      (drafted)
# - tires            (drafted - regional data doesn't match other regions)
# - Paints-Flam-Pest (drafted)
# - Elect
# - Lead-Acid Batteries         (not much data - drop?)
# - Pharmacy          (straight-forward)
# - PPP               (straight-forward)
#
# - Program Financials (2014 - 2017) Lots of holes with who reported and who didnt

# Read in population data
# BC Pop Stats (ignore and get directly from Stats Can)
# https://www.bcstats.gov.bc.ca/apps/PopulationEstimates.aspx
# manual export of population per regional district (2000 - 2018) and store in data folder

pop.0 <- read.csv(paste('data','Population_Estimates.csv',sep = "/"),
                header = TRUE)
pop <- pop.0 %>%
       mutate(regional_district = gsub("-", " ", Regional.District),
              n = Total, year = as.character(Year)) %>%
       select(-c('ï..','Gender','Regional.District','Total',"Year"))

# extract the raw unit data and add with population and maps ---------
priority <- priority_raw %>%
      dplyr::filter(measure %in%
                  c("Absolute Collection-Units Collected-",
                    "Absolute Collection-Weight Collected (Tonnes)-")) %>%

      dplyr::select(-c(organization)) %>%
      mutate(regional_district = gsub("-", " ", regional_district)) %>%
      gather("year", "n",3:length(.)) %>%
      mutate(n = as.numeric(n))

# group together the multiple companies
sum.pdata <- priority %>%
      group_by(measure, regional_district, year) %>%
      summarise(total = sum(n, na.rm = TRUE))


# merge in the pop.long form data
ppdata <- left_join(sum.pdata, pop,
                    by = c("regional_district","year")) %>%
      mutate(unit.per.cap = total / n)

# break up into weight and units
units.per.cap <- ppdata %>%
      filter(measure == 'Absolute Collection-Units Collected-' )
weight.per.cap <-  ppdata %>%
      filter(measure == 'Absolute Collection-Weight Collected (Tonnes)-' )

## Units per capita
ggplot(units.per.cap,aes(year,unit.per.cap)) +
      facet_wrap(~ regional_district) +
      geom_bar(stat = "identity",position = "dodge") +
      labs(title = "Regional Units Recycled per capita",
           x = "Year", y = "units per capita")

## weight per capita
ggplot(weight.per.cap,aes(year,unit.per.cap)) +
      facet_wrap(~ regional_district) +
      geom_bar(stat = "identity",position="dodge") +
      labs(title = "Regional weight of recycling (tonnes) per capita",
           x = "Year", y = "weight per cap (tonnes")

# calculate the provincial average
bc.units.per.cap <- units.per.cap %>%
      na.omit() %>%
      group_by(year) %>%
      summarise(bc_ave = mean(unit.per.cap))

regional.units.per.cap <- units.per.cap %>%
      na.omit() %>%
      group_by(year,regional_district) %>%
      summarise(ave = mean(unit.per.cap))

# join the regional and prov. ave data and calculate the difference
diff.df <- regional.units.per.cap %>%
      left_join(.,bc.units.per.cap, by = 'year') %>%
      mutate(delta = ave-bc_ave) %>%
      mutate(response = ifelse(delta < 0,"below", "above")) %>%
      mutate(response = ifelse(delta == 0,"No data",response))

# Diverging barcharts
ggplot(diff.df, aes(x = regional_district,
                    y = delta,
                    label= delta)) +
      facet_wrap(~year) +
      geom_bar(stat='identity', aes(fill=response), width=.5)  +
      scale_fill_manual(name="Mileage",
                  labels = c("Above Average", "Below Average", "No Data"),
                  values = c("above"="#00ba38", "below"="#f8766d", "no data" = 'grey')) +
      labs(title= "Regional difference from the average BC units per capita recycling") +
      coord_flip()

# Diverging Barcharts (all years)
ggplot(diff.df, aes(x = regional_district,
                    y = delta,
                    label = delta)) +
      geom_bar(stat='identity', aes(fill = response), width =.5)  +
      scale_fill_manual(name="Mileage",
                    labels = c("Above Average", "Below Average","No data"),
                    values = c("above"="#00ba38", "below"="#f8766d", "no data" = 'grey')) +
       labs(title= "Regional difference from the average BC units per capita recycling") +
      coord_flip()
#ggsave(paste('out/',"06_regional_bc_ave_unitsPerCap_allyrs.png"))


# Work in progress
#-------------------------
# MAke a map to show spatial context

library(leaflet)
library(mapview)

# addd the BC boundary data
bc <- bc_bound() # plot(st_geometry(bc))
rd <- regional_districts()
rd.wgs<-st_transform(rd,4326) # convert to lat longs

# join the abbreviations to the data
upc <- left_join(units.per.cap, rdKey, by = "Region")

upc.sp <- merge(rd.wgs,upc, by.x = "ADMIN_AREA_ABBREVIATION", by.y="Abrev")

m <- leaflet(data = upc.sp )%>%
  addTiles() %>%
  addPolygons(fillColor = "green")

m

rd.names <- rd[,c("ADMIN_AREA_ABBREVIATION","ADMIN_AREA_NAME")]


## make some graphical outputs : STILL TO DO


            st_geometry(rd)

            unique(rd$ADMIN_AREA_ABBREVIATION)

            leaflet(data = rd.wgs)%>%
              addTiles() %>%
              addPolygons(fillColor = "green")



                water_locations_map <- leaflet(rf) %>%
                  addTiles() %>%
                  addCircleMarkers(lng = ~location.longitude,
                                   lat = ~location.latitude)


            mapview(rd)


                mapview()
                leaflet(rd)%>%
                addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                           opacity = 1.0, fillOpacity = 0.5,
                           fillColor = ~colorQuantile("YlOrRd", ADMIN_AREA_NAME)(ADMIN_AREA_NAME))


            bc <- bc_bound()
            plot(st_geometry(bc))

            rd <- regional_districts()


            kootenays <- rd[rd$ADMIN_AREA_NAME == "Regional District of Central Kootenay", ]
            plot(st_geometry(kootenays), col = "lightseagreen", add = TRUE)


            sum.pdata1 <- sum.pdata %>% spread(Measure,total)


            sum.pdata1 <sum.pdata1 %>%
              mutate(units.per.capita = "Absolute Collection-Units Collected-" / "Population-")





# Grab the financial data -------------------------------

fdata <-financial %>%
      gather("year", "n", 3:length(.)) %>%
      mutate(n.m = n/1000000)

sum.fdata <- fdata %>%
      group_by(measure, year) %>%
      summarise(total = sum(n.m,na.rm = TRUE))

to.remove <-c("Total reported revenues - Encorp","Total reported expenditure* - Encorp","Total reported expenditure - excluding deposits refunded* - Encorp","Total deposits collected Encorp","Total deposits refunded Encorp")
to.keep <- c('Deposits Charged','Deposits Refunded','Expenditure-Consumer Awareness')

sum.fdata <- sum.fdata %>%
  filter(measure %in% to.keep )

#Deposits charged and refunded over time

# Does spending more on consumer awareness decrease unclaimed deposits
ggplot(sum.fdata, aes(year, total, fill = measure)) +
      geom_bar(stat="identity",position="dodge") +
      labs(title="Unclaimed deposits and consumer-expenditure", x = "Year", y = " Amount ($1,000,000)")
    #ggsave(paste('out/',"01_Beverage_UnitsMoved.png"))

#-------------------------------------------------------
# Grab the units moved

udata <- units %>%
  dplyr::select(-c(organization)) %>%
  gather("year", "n",2:length(.)) %>%
  mutate(n.m = n/1000000)

# summarise per year
sum.udata <- udata %>%
  group_by(measure,year) %>%
  summarise(total = sum(n.m,na.rm = TRUE))

  # make a pretty graph
  ggplot(sum.udata, aes(year, total, fill = measure)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Recycled Units Returned and Sold",
        x = "Year", y = "Total no. (millions)")
    ggsave(paste('out/',"01_Beverage_UnitsMoved.png"))

#calculate the recovery rate and make a line plot
sum.udata1 <-  udata %>%
  group_by(measure,year) %>%
  summarise(total = sum(n.m, na.rm = TRUE)) %>%
  spread(., measure, total)

sum.udata1$RecoveryRate = sum.udata1$'Units Recovered' / sum.udata1$'Units Sold' *100
sum.udata1 <- sum.udata1[-1,]

ggplot(sum.udata1, aes(x = year, y = RecoveryRate))+
      geom_point() +
      ylim(60,100) +
      geom_hline(yintercept = 75, color = "red", lty = 2) +
      labs(title="Recycled Units Recovery Rate (%)", x = "Year", y = "Recovery Rate %")
  ggsave(paste('out/',"02_Beverage_UnitsMoved.png"))

# Other catergories : Still to be updated # NOT WORKING ---------------------------------------------------------------
# Grab data from "other catergory"

odata <- rdata %>% dplyr::filter(Catergory == 'Other') %>%
    dplyr::select(-c(Company, Catergory,Region)) %>%
    gather("year", "n",2:19)
odata$n <- replaceCommas(odata$n) # fix the fromatting in numeric
odata$n.t <-odata$n/1000
odata$year = sub("X","",odata$year)  # get rid of x on year column
odata$Measure = sub("tonnes","Tonnes",odata$Measure)  # get rid of x on year column

odata <- odata %>% group_by(Measure,year) %>%
  summarise(total = sum(n.t,na.rm = TRUE))

# split in +ve and -ve amounts
# make a nice plot
ggplot(odata, aes(year,total,fill=Measure))+
  geom_bar(stat="identity",position="dodge") +
  labs(title="Tonnes of material and reduction of pollutants", x = "Year", y = "Tonnes (thousands)") +
  scale_y_continuous(limits = c(0,250))
#------------------------------------------------------------

###################################################################################################
## OIL Lubraicant and Filters ------------------------------

#data = oil_units, oil_financial, recovery_pp

# extract the raw unit data and add with population and maps....
pdata <- recovery_pp %>%
  dplyr::filter(!regional_district == '') %>%
  gather("year", "n",3:length(.))

## do a basic graph to check it out
ggplot(pdata, aes(year, n, fill = measure)) +
  geom_bar(stat = "identity",position = "dodge") +
  labs(title = "Absolute collection (kg/litres) per person", x = "Year", y = "Collection (kg) per person")

# used oil is order of magnitude more!
pdata.1 <- pdata %>% dplyr::filter(!measure == 'oil_lt_pp')
pdata.1<- pdata.1[complete.cases(pdata.1), ]

## do a basic graph to check it out
ggplot(pdata.1,aes(year, n, fill = measure)) +
  geom_bar(stat="identity",position="dodge") +
  labs(title="Absolute collection (kg) per person", x = "Year", y = "Collection (kg) per person")

# calculate the provincial average
bc_units_per_cap_yr <- pdata %>%  # per yr
  na.omit() %>%
  group_by(year, measure) %>%
  summarise(BCave = mean(n))

bc_units_per_cap <- pdata %>%     # all years
  na.omit() %>%
  group_by(measure) %>%
  summarise(BCave = mean(n))

regional_units_per_cap_yr <- pdata %>% na.omit() %>%
  group_by(year, measure, regional_district) %>%
  summarise(ave = mean(n))

regional_units_per_cap <- pdata %>% na.omit() %>%
  group_by(measure, regional_district) %>%
  summarise(ave = mean(n))

# join the regional and prov. ave data and calculate the difference
diff.df <-left_join(regional_units_per_cap,
                    bc_units_per_cap,
                    by = c('measure')) %>%
          mutate(delta = ave - BCave,
                 response = ifelse(delta < 0, "below ave", "above ave"))

# join the regional and prov. ave data and calculate the difference per year
diff.df.yr <-left_join(regional_units_per_cap_yr,
                    bc_units_per_cap_yr,
                    by = c('measure','year')) %>%
                mutate(delta = ave - BCave,
                    response = ifelse(delta < 0,
                          "below ave", "above ave"))

# Diverging Barcharts ( all years combined )
p_dif <- ggplot(diff.df, aes(x = regional_district,
                          y = delta,label = delta)) +
        facet_wrap(~measure)+
        geom_point()+
        geom_bar(stat = 'identity',
                      aes(fill = response), width =.5)  +
        scale_fill_manual(name = "Mileage",
                      labels = c("Above Average", "Below Average"),
                      values = c("above ave" = "#00ba38",
                                    "below ave" = "#f8766d")) +
                labs(title = "Regional difference from the average BC
                              units per capita recycling", y = " Difference from BC Ave") +
         coord_flip() #+
p_dif

# Diverging Barcharts ( per year )

#diff.df <- diff.df %>% filter(measure =="oil_lt_pp" )
# Diverging Barcharts (all years)
p_dif_yr <- ggplot(diff.df.yr, aes(x = regional_district,
                             y = delta,label = delta)) +
  facet_wrap(~year)+
  geom_point()+
  geom_bar(stat = 'identity',
           aes(fill = response), width =.5)  +
  scale_fill_manual(name = "Mileage",
                    labels = c("Above Average", "Below Average"),
                    values = c("above ave" = "#00ba38",
                               "below ave" = "#f8766d")) +
  labs(title = "Regional difference from the average BC
                              units per capita recycling", y = " Difference from BC Ave") +
  coord_flip() #+
p_dif_yr

# note this combines all the measures combined( not split)

# get oil financial data -------------------------------------

oil.fdata <-oil_financial %>%
  gather("year", "n", 3:length(.)) %>%
  mutate(n.m = n/1000000)

to.keep <- c("Expenditure-Total","Revenue-Total")

sum.fdata <- oil.fdata %>%
  group_by(measure, year) %>%
  summarise(total = sum(n.m,na.rm = TRUE)) %>%
  filter(measure %in% to.keep)

# Does spending more on consumer awareness decrease unclaimed deposits
ggplot(sum.fdata, aes(year, total, fill = measure)) +
  geom_bar(stat="identity",position="dodge") +
  labs(title=" Oil Recycling Expenditure and Revenue", x = "Year", y = " Amount ($1,000,000)")


# oil units moved -----------------------------------
udata <- oil_units %>%
      gather("year", "n",2:length(.)) %>%
      mutate(n.m = n/1000000)

# summarise per year
sum.udata <- udata %>%
      group_by(measure,year) %>%
      summarise(total = sum(n.m,na.rm = TRUE))

# make a pretty graph
ggplot(sum.udata, aes(year, total, fill = measure)) +
      facet_wrap(~measure) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Recycled Units Returned and Sold",
          x = "Year", y = "Total no. (millions)")

## this needs some more work to split out the data types

###############################################################
# TIRE ---------------------------------------------------

# financial data -------------------------------------

tire.fdata <-tire_financial %>%
  gather("year", "n", 2:length(.)) %>%
  mutate(n.m = n/1000000)

to.keep <- c("Expenditure-Total","Revenue-Total",
             "Expenditure-Communications & Education",
             "Expenditure-Program Incentives",
             "Balance")

sum.fdata <- tire.fdata %>%
      group_by(measure,year) %>%
      summarise(total = sum(n.m,na.rm = TRUE)) %>%
      filter(measure %in% to.keep)

# Does spending more on consumer awareness decrease unclaimed deposits
ggplot(sum.fdata, aes(year, total, fill = measure)) +
      geom_bar(stat="identity",position="dodge") +
      labs(title="Tire Recycling Expenditure and Revenue",
            x = "Year",
            y = " Amount ($1,000,000)") +
      theme(axis.text.x = element_text(angle = 90))

### possible to dig a bit deeper into this


# Tire units moved -----------------------------------

udata <- tire_units %>%
      gather("year", "n",2:length(.)) %>%
      mutate(n.m = n/1000000)

to.keep <- c("Total Sold", "Total Collected")

# summarise per year
sum.udata <- udata %>%
      group_by(measure,year) %>%
      summarise(total = sum(n.m,na.rm = TRUE)) %>%
      filter(measure %in% to.keep)

# make a pretty graph
ggplot(sum.udata, aes(year, total, fill = measure)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Recycled Units Returned and Sold",
           x = "Year",
           y = "Total no. (millions)") +
      theme(axis.text.x = element_text(angle = 90))

sum.udata.l <- sum.udata %>%
      spread(measure, total) %>%
      mutate(prop.recovered = `Total Collected`/`Total Sold`)

ggplot(sum.udata.l, aes(x = year, y = prop.recovered))+
     geom_point() +
     ylim(0,1) +
     labs(title="Recycled Units Recovery Rate (%)",
          x = "Year", y = "Recovery Rate %")

#########################################################
# pfp indicator ---------------------------------------

pfp_recovery
pfp_financial
pfp_units

# extract the raw unit data and add with population and maps....
pfp_data <- pfp_recovery %>%
  dplyr::filter(!regional_district == '') %>%
  gather("year", "total",3:length(.))

## do a basic graph to check it out
ggplot(pfp_data, aes(year, total , fill = measure)) +
      geom_bar(stat = "identity",position = "dodge") +
      labs(title = "Absolute collection per person",
         x = "Year",
         y = "Total Tubskids Collected") +
      theme(axis.text.x = element_text(angle = 90))

# add population data
pfpdata <- left_join(pfp_data, pop,
                by = c("regional_district","year")) %>%
      mutate(unit.per.cap = total / n)

# calculate the provincial average
bc_units_per_cap_yr <- pfpdata %>%  # per yr
      na.omit() %>%
      group_by(year, measure) %>%
      summarise(BCave = mean(total))

bc_units_per_cap <- pfpdata %>%     # all years
      na.omit() %>%
      group_by(measure) %>%
      summarise(BCave = mean(total))

regional_units_per_cap_yr <- pfpdata %>%
      na.omit() %>%
      group_by(year, measure, regional_district) %>%
      summarise(ave = mean(total))

regional_units_per_cap <- pfpdata %>%
      na.omit() %>%
      group_by(measure, regional_district) %>%
      summarise(ave = mean(total))

# join the regional and prov. ave data and calculate the difference
diff.df <-left_join(regional_units_per_cap,
                    bc_units_per_cap,
                    by = c('measure')) %>%
      mutate(delta = ave - BCave,
         response = ifelse(delta < 0, "below ave", "above ave"))

# join the regional and prov. ave data and calculate the difference per year
diff.df.yr <-left_join(regional_units_per_cap_yr,
                       bc_units_per_cap_yr,
                       by = c('measure','year')) %>%
      mutate(delta = ave - BCave,
         response = ifelse(delta < 0,
                           "below ave", "above ave"))

# Diverging Barcharts ( all years combined )
p_dif <- ggplot(diff.df, aes(x = regional_district,
                             y = delta,label = delta)) +
      facet_wrap(~measure)+
      geom_point()+
      geom_bar(stat = 'identity',
           aes(fill = response), width =.5)  +
      scale_fill_manual(name = "Mileage",
                    labels = c("Above Average", "Below Average"),
                    values = c("above ave" = "#00ba38",
                               "below ave" = "#f8766d")) +
      labs(title = "Regional difference from the average BC
                              units per capita recycling", y = " Difference from BC Ave") +
      coord_flip() #+
p_dif















