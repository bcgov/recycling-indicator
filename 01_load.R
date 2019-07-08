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

x <- c("dplyr","ggplot2","tidyr","stringr","reshape",
       "bcmaps", "sf", "envreportutils") #raster","sp","sf","rgdal","xlsx","rJava","tibble","mapview","gtools")
lapply(x, library, character.only = TRUE) ; rm(x)  # load the required packages

## Load  data files

data.dir <- soe_path("Operations ORCS/Data - Working/sustainability/EPR/")# to run on O:/

# Read in population data -------------------------------
# BC Pop Stats (ignore and get directly from Stats Can)
# https://www.bcstats.gov.bc.ca/apps/PopulationEstimates.aspx
# manual export of population per regional district (2000 - 2018) and store in data folder

pop.0 <- read.csv(paste('data','Population_Estimates.csv',sep = "/"),
                  header = TRUE)

pop <- pop.0 %>%
  mutate(regional_district = gsub("-", " ", Regional.District),
         n = Total, year = as.character(Year)) %>%
  select(-c('ï..','Gender','Regional.District','Total',"Year")) %>%
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
   summarize(n = sum(n))

#######################################################################

source(paste('scratch','clean_readxl.R',sep = '/'))

##or

#all.finance <- read.csv(paste('data','all.finance.csv',sep = "/"), header = TRUE)
#all.regions <- read.csv(paste('data','all.regions.csv',sep = "/"), header = TRUE)
#all.units <- read.csv(paste('data','all.units.csv',sep = "/"), header = TRUE)


# Beverage ------------------------------------------------------

regions <- all.regions %>% filter(type == 'bev')

priority <- regions %>%
  dplyr::filter(measure %in%
                  c("Absolute Collection-Units Collected-",
                    "Absolute Collection-Weight Collected (Tonnes)-")) %>%
  select(-c(organization,type)) %>%
  group_by(measure, regional_district) %>%
  summarise_all(., sum, na.rm = TRUE) %>%
  gather("year", "n",3:length(.)) %>%
  left_join(.,pop, by = c("regional_district","year")) %>%
  dplyr::rename(.,'pop' = 'n.y') %>%
  dplyr::rename('n' = 'n.x')

priority$n.pop = priority$n / priority$pop # this is not working in dplyr version

# break up into weight and units
units.per.cap <- priority %>%
  filter(measure == 'Absolute Collection-Units Collected-' )
weight.per.cap <-  priority %>%
  filter(measure == 'Absolute Collection-Weight Collected (Tonnes)-' )

## Units per capita
ggplot(units.per.cap, aes(year,n.pop)) +
  facet_wrap(~ regional_district) +
  geom_bar(stat = "identity",position = "dodge") +
  labs(title = "Regional Units Recycled per capita",
       x = "Year", y = "units per capita") +
  theme(axis.text.x = element_text(angle = 90,vjust=0.5))

## weight per capita
ggplot(weight.per.cap,aes(year,n.pop)) +
  facet_wrap(~ regional_district) +
  geom_bar(stat = "identity",position="dodge") +
  labs(title = "Regional weight of recycling (tonnes) per capita",
       x = "Year", y = "weight per cap (tonnes") +
  theme(axis.text.x = element_text(angle = 90))

# calculate the provincial average
bc.units.per.cap <- units.per.cap %>%
  na.omit() %>%
  group_by(year) %>%
  summarise(bc_ave = mean(n.pop))

regional.units.per.cap <- units.per.cap %>%
  na.omit() %>%
  group_by(year,regional_district) %>%
  summarise(ave = mean(n.pop))

# join the regional and prov. ave data and calculate the difference
diff.df <- regional.units.per.cap %>%
  left_join(., bc.units.per.cap, by = 'year') %>%
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
                    values = c("above" = "#008000", "below"="#FF0000", "no data" = 'grey')) +
  labs(title= "Regional difference from BC Ave",
       subtitle = " Bev units per capita") +
  coord_flip()


# Diverging Barcharts (all years)
ggplot(diff.df, aes(x = regional_district,
                    y = delta,
                    label = delta)) +
  geom_bar(stat='identity', aes(fill = response), width =.5)  +
  scale_fill_manual(name="Mileage",
                    labels = c("Above Average", "Below Average","No data"),
                    values = c("above" = "#008000", "below"="#FF0000", "no data" = 'grey')) +
  labs(title= "Regional difference from BC Ave",
       subtitle = " Bev units per capita") +
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

?combine_nr_rd

nr <- combine_nr_rd(class = 'sf')

unique(nr$ADMIN_AREA_NAME)
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

finance <- all.finance %>% filter(type == 'bev')

to.keep <- c('Deposits Charged','Deposits Refunded',
             'Expenditure-Consumer Awareness')

fdata <-finance %>%
      gather("year", "n", 4:length(.)) %>%
      mutate(n.m = n/1000000) %>%
      group_by(measure, year) %>%
      summarise(total = sum(n.m,na.rm = TRUE)) %>%
      filter(measure %in% to.keep)

ggplot(fdata, aes(year, total, fill = measure)) +
      geom_bar(stat="identity",position="dodge") +
      labs(title="Unclaimed deposits and consumer-expenditure",
           x = "Year", y = " Amount ($1,000,000)") +
      theme(axis.text.x = element_text(angle = 90))

# look at revenue vs education $$
to.keep = c("Balance (Including Deposits Charged/Returned)",
            "Expenditure-Consumer Awareness")

fdata <-finance %>%
      select(-c("organization","type"))%>%
      group_by(measure) %>%
      summarise_all(., sum, na.rm = TRUE) %>%
      gather("year", "n", 2:length(.)) %>%
      mutate(n.m = n/1000000) %>%
      group_by(measure, year) %>%
      summarise(total = sum(n.m,na.rm = TRUE)) %>%
      filter(measure %in% to.keep)

ggplot(fdata, aes(year, total, fill = measure)) +
      geom_bar(stat="identity",position="dodge") +
      labs(title="Unclaimed deposits and consumer-expenditure",
           x = "Year", y = " Amount ($1,000,000)") +
      theme(axis.text.x = element_text(angle = 90))

# Grab the unit data -------------------------------
units <- all.units %>% filter(type == 'bev')

udata <- units %>%
      dplyr::select(-c(organization, type)) %>%
      group_by(measure) %>%
      summarise_all(., sum, na.rm = TRUE) %>%
      gather("year", "n", 2:length(.)) %>%
      mutate(n.m = n/1000000)

# make a pretty graph
ggplot(udata, aes(year, n.m, fill = measure)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Recycled Units Returned and Sold",
               x = "Year",
               y = "Total no. (millions)") +
      theme(axis.text.x = element_text(angle = 90))

#calculate the recovery rate and make a line plot
sum.udata1 <- units  %>%
      select(-c(organization, type)) %>%
      gather("year", "n", 2:length(.)) %>%
      mutate(n.m = n/1000000) %>%
      group_by(measure,year) %>%
      summarise(total = sum(n.m, na.rm = TRUE)) %>%
      spread(., measure, total)

sum.udata1$RecoveryRate = sum.udata1$'Units Recovered' / sum.udata1$'Units Sold' *100
sum.udata1 <- sum.udata1[-1,]

ggplot(sum.udata1, aes(x = year, y = RecoveryRate))+
      geom_point() +
      ylim(60,100) +
      geom_hline(yintercept = 75, color = "red", lty = 2) +
      labs(title="Recycled Units Recovery Rate (%)",
              x = "Year",
              y = "Recovery Rate %")+
      theme(axis.text.x = element_text(angle = 90))


###################################################################################################
## OIL Lubraicant and Filters ------------------------------

regions <- all.regions %>% filter(type == 'oil')

pdata <- regions %>%
      select(-c(organization, type)) %>%
      filter(!regional_district == '') %>%
      group_by(measure, regional_district) %>%
      summarise_all(., sum, na.rm = TRUE) %>%
      gather("year", "n",3:length(.)) %>%
      filter(year > 2009)

## do a basic graph to check it out
ggplot(pdata, aes(year, n, fill = measure)) +
      geom_bar(stat = "identity",position = "dodge") +
      labs(title = "Absolute collection (kg/litres) per person",
           x = "Year",
           y = "Collection (kg) per person")+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5))

# used oil is order of magnitude more!
pdata.1 <- pdata %>% dplyr::filter(!measure == 'oil_lt_pp')
pdata.1<- pdata.1[complete.cases(pdata.1), ]

## do a basic graph to check it out
ggplot(pdata.1,aes(year, n, fill = measure)) +
      geom_bar(stat="identity",position="dodge") +
      labs(title="Absolute collection (kg) per person",
           x = "Year",
           y = "Collection (kg) per person")

####################################################

# calculate the provincial average
bc_units_per_cap_yr <- pdata %>%  # per yr
  na.omit() %>%
  group_by(year, measure) %>%
  summarise(BCave = mean(n))

bc_units_per_cap <- pdata %>%     # all years
  na.omit() %>%
  group_by(measure) %>%
  summarise(BCave = mean(n))

regional_units_per_cap_yr <- pdata %>%
  na.omit() %>%
  group_by(year, measure, regional_district) %>%
  summarise(ave = mean(n))

regional_units_per_cap <- pdata %>%
  na.omit() %>%
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

# split into oil and non - oil data sets

diff.df.oil <- diff.df %>%
  filter(measure == "oil_lt_pp")
diff.df.yr.oil <- diff.df.yr %>%
  filter(measure == "oil_lt_pp")

diff.df <- diff.df %>%
  filter(!measure == "oil_lt_pp")
diff.df.yr <- diff.df.yr %>%
  filter(!measure == "oil_lt_pp")

# OIL : Diverging Barcharts ( all years combined : oil only )
p_dif.oil <- ggplot(diff.df.oil, aes(x = regional_district,
                          y = delta,label = delta)) +
        geom_point()+
        geom_bar(stat = 'identity',
                      aes(fill = response), width =.5)  +
        scale_fill_manual(name = "Mileage",
                      labels = c("Above Average", "Below Average"),
                      values = c("above ave" = "#00ba38",
                                    "below ave" = "#f8766d")) +
                labs(title = "Regional difference from the average BC
                              in oil lts per capita recycling", y = " Difference from BC Ave") +
         coord_flip()
p_dif.oil


# OTHER mEASURES : Diverging Barcharts ( all years combined )
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

finance <- all.finance %>% filter(type == 'oil')

odata <- finance %>%
  dplyr::select(-c(organization, type)) %>%
  group_by(measure) %>%
  summarise_all(., sum, na.rm = TRUE) %>%
  gather("year", "n", 2:length(.)) %>%
  mutate(n.m = n/1000000)

to.keep <- c("Expenditure-Total","Revenue-Total", "Balance")

sum.fdata <- odata %>%
  group_by(measure, year) %>%
  summarise(total = sum(n.m,na.rm = TRUE)) %>%
  filter(measure %in% to.keep)

# Does spending more on consumer awareness decrease unclaimed deposits
ggplot(sum.fdata, aes(year, total, fill = measure)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = " Oil Recycling Expenditure and Revenue",
       x = "Year", y = " Amount ($1,000,000)")+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5))


# oil units moved -----------------------------------

units <- all.units %>% filter(type == 'oil')

udata <- units %>%
  dplyr::select(-c(organization, type)) %>%
  group_by(measure) %>%
  gather("year", "n", 2:length(.)) %>%
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
       x = "Year", y = "Total no. (millions)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "none")

## this needs some more work to split out the data types
#unique(sum.udata$measure)
#to.keep <- c("Expenditure-Total","Revenue-Total", "Balance")


###############################################################

# 3) Tire data
# TIRE ---------------------------------------------------

# financial data -------------------------------------
finance <- all.finance %>% filter(type == 'tire')

tire.fdata  <- finance %>%
  dplyr::select(-c(organization, type)) %>%
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
#ggplot(sum.fdata, aes(year, total, fill = measure) +
#         geom_bar() + #stat = "identity", position="dodge") +
#         labs(title = "Tire Recycling Expenditure and Revenue",
#              x = "Year",
#              y = " Amount ($1,000,000)") +
#         theme(axis.text.x = element_text(angle = 90)))

### possible to dig a bit deeper into this


# Tire units moved -----------------------------------
units <- all.units %>% filter(type == 'tire')

udata <- tire_units %>%
      select(-c(organization, type)) %>%
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


## Elect -

# regional data set only

elect_compile


#########################################################
# pfp indicator ---------------------------------------

pfp_recovery <- all.regions %>% filter(type == 'pfp')

pfp_data <- pfp_recovery %>%
  select(-c(organization,type)) %>%
  dplyr::filter(!regional_district == '') %>%
  group_by(measure, regional_district) %>%
  summarise_all(., sum, na.rm = TRUE) %>%
  gather("year", "n",3:length(.)) %>%
  left_join(.,pop, by = c("regional_district","year")) %>%
  dplyr::rename(.,'pop' = 'n.y') %>%
  dplyr::rename('n' = 'n.x') %>%
  filter(year > 2006) %>%
  mutate(unit.per.cap = n / pop)

## do a basic graph to check it out
ggplot(pfp_data, aes(year, n , fill = measure)) +
      geom_bar(stat = "identity",position = "dodge") +
      labs(title = "Absolute collection per person",
         x = "Year",
         y = "Total Tubskids Collected") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
              legend.position = "none")

# calculate the provincial average
bc_units_per_cap_yr <- pfp_data %>%  # per yr
      na.omit() %>%
      group_by(year, measure) %>%
      summarise(BCave = mean(unit.per.cap))

bc_units_per_cap <- pfp_data %>%     # all years
      na.omit() %>%
      group_by(measure) %>%
      summarise(BCave = mean(unit.per.cap))

regional_units_per_cap_yr <- pfp_data %>%
      na.omit() %>%
      group_by(year, measure, regional_district) %>%
      summarise(ave = mean(unit.per.cap))

regional_units_per_cap <- pfp_data %>%
      na.omit() %>%
      group_by(measure, regional_district) %>%
      summarise(ave = mean(unit.per.cap))

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
  coord_flip() + theme(legend.position = "none") #+

p_dif_yr



# pfp financial -------------------------------

finance <- all.finance %>% filter(type == 'pfp')

pfp.fdata <-finance %>%
  dplyr::select(-c(organization, type)) %>%
  gather("year", "n", 2:length(.)) %>%
  mutate(n.m = n/1000000)

to.keep <- c("Expenditure-Total","Revenue-Total",
             "Expenditure-Education and Printed Materials",
             "Balance")

sum.fdata <- pfp.fdata %>%
      group_by(measure, year) %>%
      summarise(total = sum(n.m, na.rm = TRUE)) %>%
      filter(measure %in% to.keep)

# Does spending more on consumer awareness decrease unclaimed deposits
ggplot(sum.fdata, aes(year, total, fill = measure)) +
      geom_bar(stat="identity",position="dodge") +
      labs(title="PFP Recycling Expenditure and Revenue",
          x = "Year",
          y = " Amount ($1,000,000)") +
      theme(axis.text.x = element_text(angle = 90))

### possible to dig a bit deeper into this
ggplot(sum.fdata, aes(year, total)) +
      facet_wrap(~measure)+
      geom_bar(stat="identity",position="dodge") +
      labs(title="PFP Recycling Expenditure and Revenue",
       x = "Year",
       y = " Amount ($1,000,000)") +
      theme(axis.text.x = element_text(angle = 90))

# expenditure is very very low and not worth reporting.
#head(sum.fdata)

sum.fdata %>% filter(measure == "Expenditure-Education and Printed Materials")


# pfp units ---------------------------

pfp_units <- all.units %>% filter(type == 'pfp')

udata <- pfp_units %>%
  select(-c(organization, type)) %>%
  gather("year", "n",2:length(.)) %>%
  mutate(n.m = n/1000000)

unique(udata$measure)

# summarise per year
sum.udata <- udata %>%
      group_by(measure,year) %>%
      summarise(total = sum(n.m,na.rm = TRUE)) %>%
      filter(!(measure == "All HHW Collected (Litres)"))


# make a pretty graph
ggplot(sum.udata, aes(year, total, fill = measure)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Recycled Units Returned and Sold",
          x = "Year",
          y = "Total no. (millions)") +
      theme(axis.text.x = element_text(angle = 90),
            legend.position = 'none')

# split into different metrics (paint / aersol/ non aersol)

## More digging required.

# Pharm ---------------------------------------------------
# extract the raw unit data and add with population and maps....
pharm_recovery <- all.regions %>% filter(type == "pharm")

ph_data <- pharm_recovery %>%
  select (-c(organization,type)) %>%
  dplyr::filter(!regional_district == '') %>%
  group_by(measure, regional_district) %>%
  summarise_all(., sum, na.rm = TRUE) %>%
  gather("year", "total",3:length(.)) %>%
  filter(year > 2007)

# note units only have data from 2008 - 2012
units.per.cap <- ph_data %>%
      filter(measure == 'Absolute Collection-Number of Containers-' )
weight.per.cap <-  ph_data %>%
      filter(measure == 'Absolute Collection-Weight Collected (kg)-' )

## Units per capita
#ggplot(units.per.cap,aes(year,total)) +
#      #facet_wrap(~ regional_district) +
#      geom_bar(stat = "identity",position = "dodge") +
#      labs(title = "Regional Units Recycled per capita",
#            x = "Year", y = "units per capita")

## weight per capita # raw data are not very informative as metro Van. is much larger
#ggplot(weight.per.cap,aes(year,total)) +
#      facet_wrap(~ regional_district) +
#      geom_bar(stat = "identity",position="dodge") +
#      labs(title = "Regional weight of recycling (tonnes) per capita",
#            x = "Year", y = "weight per cap (tonnes")

# Add population data set and

ph_data <- left_join(weight.per.cap, pop,
                     by = c("regional_district","year")) %>%
            mutate(weight.per.cap = total / n)

# calculate the provincial average
bc.weight.per.cap <- ph_data %>%
        na.omit() %>%
        group_by(year) %>%
        summarise(bc_ave = mean(weight.per.cap))

regional.weight.per.cap <- ph_data %>%
        na.omit() %>%
        group_by(year,regional_district) %>%
        summarise(ave = mean(weight.per.cap))

# join the regional and prov. ave data and calculate the difference
diff_df <- regional.weight.per.cap %>%
      left_join(.,bc.weight.per.cap, by = 'year') %>%
      mutate(delta = ave-bc_ave) %>%
      mutate(response = ifelse(delta < 0,"below", "above")) %>%
      mutate(response = ifelse(delta == 0,"No data", response))

# Diverging barcharts
ggplot(diff_df, aes(x = regional_district,
                    y = delta,
                    label= delta)) +
      facet_wrap(~year) +
      geom_bar(stat='identity', aes(fill=response), width=.5)  +
      scale_fill_manual(name="Mileage",
                    labels = c("Above Average", "Below Average", "No Data"),
                    values = c("above"="#00ba38", "below"="#f8766d", "no data" = 'grey')) +
      labs(title= "Difference from BC average BC") +
      coord_flip()

# Diverging Barcharts (all years)
ggplot(diff_df, aes(x = regional_district,
                    y = delta,
                    label = delta)) +
      geom_bar(stat = 'identity', aes(fill = response), width = .5)  +
      scale_fill_manual(name = "Mileage",
                    labels = c("Above Average",
                               "Below Average",
                               "No data"),
                    values = c("above" = "#00ba38",
                               "below" = "#f8766d",
                               "no data" = 'grey')) +
  labs(title = "Regional difference from the average BC units per capita recycling") +
  coord_flip()

# pharm units moved -----------------------------------
pharm_units <- all.units %>% filter(type == "pharm")

udata <- pharm_units %>%
  select(-c(organization, type)) %>%
  gather("year", "n",2:length(.)) %>%
  mutate(n.t = n/1000)

to.keep = "Absolute Collection-Weight of Unused/Expired Medications Returned (kg)"

# summarise per year
sum.udata <- udata %>%
  group_by(measure,year) %>%
  summarise(total = sum(n.t, na.rm = TRUE)) %>%
  filter(measure %in% to.keep)

# make a pretty graph
ggplot(sum.udata, aes(year, total, fill = measure)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Weight of unused/expired medication returned",
       x = "Year", y = "Total weight (1000's kgs)") +
  theme(legend.position = "none",
            axis.text.x = element_text(angle = 90))



# ppp regional data ---------------------------------


ppp_data <- ppp_recovery %>%
      dplyr::filter(!regional_district == '') %>%
      gather("year", "total", 3:length(.)) %>%
      left_join(., pop, c("regional_district", "year")) %>%
      mutate(weight.per.cap = total / n)

# calculate the provincial average
bc.weight.per.cap <- ppp_data %>%
      na.omit() %>%
      group_by(year) %>%
      summarise(bc_ave = mean(weight.per.cap))

regional.weight.per.cap <- ppp_data %>%
      na.omit() %>%
      group_by(year,regional_district) %>%
      summarise(ave = mean(weight.per.cap))

# join the regional and prov. ave data and calculate the difference
diff_df <- regional.weight.per.cap %>%
      left_join(.,bc.weight.per.cap, by = 'year') %>%
      mutate(delta = ave-bc_ave,
           response = ifelse(delta < 0,"below", "above")) %>%
      mutate(response = ifelse(delta == 0,"No data",response))

# Diverging barcharts
ggplot(diff_df, aes(x = regional_district,
                    y = delta,
                    label= delta)) +
      facet_wrap(~year) +
      geom_bar(stat='identity', aes(fill=response), width=.5)  +
      scale_fill_manual(name="Mileage",
                    labels = c("Above Average", "Below Average", "No Data"),
                    values = c("above"="#00ba38", "below"="#f8766d", "no data" = 'grey')) +
      labs(title= "Difference from BC average BC") +
      coord_flip()

# Diverging Barcharts (all years)
ggplot(diff_df, aes(x = regional_district,
                    y = delta,
                    label = delta)) +
      geom_bar(stat = 'identity', aes(fill = response), width = .5)  +
      scale_fill_manual(name = "Mileage",
                    labels = c("Above Average",
                               "Below Average",
                               "No data"),
                    values = c("above" = "#00ba38",
                               "below" = "#f8766d",
                               "no data" = 'grey')) +
      labs(title = "Regional difference from the average BC ") +
      coord_flip()

# ppp_finance -----------------------------
ppp.fdata <- ppp_finance %>%
      gather("year", "n", 2:length(.)) %>%
      mutate(n.m = n/1000000)

sum.fdata <- ppp.fdata %>%
      group_by(measure, year) %>%
      summarise(total = sum(n.m, na.rm = TRUE))

# Does spending more on consumer awareness decrease unclaimed deposits
ggplot(sum.fdata, aes(year, total, fill = measure)) +
      geom_bar(stat="identity",position="dodge") +
      labs(title="PFP Recycling Expenditure and Revenue",
          x = "Year",
          y = " Amount ($1,000,000)") +
      theme(axis.text.x = element_text(angle = 90))
