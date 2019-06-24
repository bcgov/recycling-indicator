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
data.dir <- "data/" # to run on C:
#data.dir <- soe_path("Operations ORCS/Data - Working/sustainability/EPR/")# to run on O:/

financial
units
priority_raw

rdKey <- read.csv(paste(data.dir,"/RD_key.csv",sep = ""))

#------------------------------------------------------
# extract the population data and export as csv
pop <- priority_raw %>%
          dplyr::filter(measure == 'Population-') %>%
          dplyr::select(-c(organization,measure)) %>%
          gather("year", "n", 2:19) %>%
          mutate(pop = as.numeric(n)) %>%
          dplyr::select(-c(n))

#-------------------------------------------------------

# extract the raw unit data and add with population and maps....
priority <- priority_raw %>%
  dplyr::filter(measure %in%
                  c("Absolute Collection-Units Collected-",
                    "Absolute Collection-Weight Collected (Tonnes)-")) %>%
  dplyr::select(-c(organization)) %>%
  gather("year", "n",3:length(.)) %>%
  mutate(n = as.numeric(n))

# group together the multiple companies
sum.pdata <- priority %>%
  group_by(measure, regional_district, year) %>%
  summarise(total = sum(n, na.rm = TRUE))

      ## do a basic graph to check it out
      #ggplot(sum.pdata,aes(year,total,fill=measure)) +
      #  geom_bar(stat="identity",position="dodge") +
      #  labs(title="Unclaimed deposits and consumer-expenditure", x = "Year", y = " Amount ($1,000,000)")

# merge in the pop.long form data
ppdata <- left_join(sum.pdata, pop,
                    by = c("regional_district","year")) %>%
      mutate(unit.per.cap = total / pop)

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
ggsave(paste('out/',"04_Beverage_UnitsPerCap.png"))

## weight per capita
ggplot(weight.per.cap,aes(year,unit.per.cap)) +
  facet_wrap(~ regional_district) +
  geom_bar(stat = "identity",position="dodge") +
  labs(title = "Regional weight of recycling (tonnes) per capita",
           x = "Year", y = "weight per cap (tonnes")
ggsave(paste('out/',"04_Beverage_weightPerCap.png"))

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
#ggsave(paste('out/',"05_regional_bc_ave_unitsPerCap_per_yr.png"))

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





#----------------------------
# Grab the financial data
fdata <- rdata %>% dplyr::filter(Catergory == 'Financial') %>%
          dplyr::select(-c(Catergory,Region)) %>%
          dplyr::filter(!Company == 'EncorpPacific_BRCCC') %>%
          gather("year", "n",3:20)
#fdata$n = sub("$","",fdata$n)  # get rid of x on year column
fdata$n <- replaceDollars(fdata$n) # fix the fromatting in numeric
fdata$n <- replaceCommas(fdata$n) # fix the fromatting in numeric
fdata$n.m <-fdata$n/1000000
fdata$year = sub("X","",fdata$year)  # get rid of x on year column

# unique(fdata$Measure)
sum.fdata <- fdata %>% group_by(Measure,year) %>%
  summarise(total = sum(n.m,na.rm = TRUE))

  to.remove <-c("Total reported revenues - Encorp","Total reported expenditure* - Encorp","Total reported expenditure - excluding deposits refunded* - Encorp","Total deposits collected Encorp","Total deposits refunded Encorp")
  #to.keep <- c('Deposits Charged','Deposits Refunded','Expenditure-Consumer Awareness')
  to.keep <- c('Unclaimed Deposits','Expenditure-Consumer Awareness')

   sum.fdata <- sum.fdata %>%
  filter(Measure %in% to.keep )

#Deposits charged and refunded over time?
#unique(fdata$Measure)

# Does spending more on consumer awareness decrease unclaimed deposits
    ggplot(sum.fdata,aes(year,total,fill=Measure)) +
      geom_bar(stat="identity",position="dodge") +
      labs(title="Unclaimed deposits and consumer-expenditure", x = "Year", y = " Amount ($1,000,000)")
    #ggsave(paste('out/',"01_Beverage_UnitsMoved.png"))


-------------------------------------------------------
# Grab the units moved
udata <- rdata %>% dplyr::filter(Catergory == 'Units Moved') %>%
  dplyr::select(-c(Catergory,Region))%>%
  dplyr::filter(!Company == 'EncorpPacific_BRCCC') %>%
  dplyr::filter(!Measure == 'Recovery Rate (%)  Regulation Target 75%') %>%
  gather("year", "n",3:20)

udata$n <- replaceCommas(udata$n) # fix the fromatting in numeric
udata$n.m <-udata$n/1000000
udata$year = sub("X","",udata$year)  # get rid of x on year column

# summarise per year
sum.udata <- udata %>% group_by(Measure,year) %>%
  summarise(total = sum(n.m,na.rm = TRUE))

  # make a pretty graph
  ggplot(sum.udata,aes(year,total,fill=Measure)) +
    geom_bar(stat="identity",position="dodge") +
    labs(title="Recycled Units Returned and Sold", x = "Year", y = "Total no. (millions)")
    ggsave(paste('out/',"01_Beverage_UnitsMoved.png"))

    #calculate the recovery rate and make a line plot
sum.udata1 <-  udata %>% group_by(Measure,year) %>%
  summarise(total = sum(n.m,na.rm = TRUE)) %>%
  spread(., Measure,total)

sum.udata1$RecoveryRate = sum.udata1$'Units Recovered' / sum.udata1$'Units Sold' *100
sum.udata1 <- sum.udata1[-1,]

    ggplot(sum.udata1, aes(x = year, y = RecoveryRate))+
      geom_point() +
      ylim(60,100) +
      geom_hline(yintercept = 75, color = "red", lty = 2) +
      labs(title="Recycled Units Recovery Rate (%)", x = "Year", y = "Recovery Rate %")
      ggsave(paste('out/',"02_Beverage_UnitsMoved.png"))

----------------------------------------------------------------

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
#ggsave(paste('out/',"03_Beverage_Other.png"))


###################################################################################################
## OIL Lubraicant and Filters

odata<- read.csv(paste(data.dir,"/LubOilFilt.csv",sep = ""))
#data alread populated by capita?

# extract the raw unit data and add with population and maps....
pdata <- odata %>% dplyr::filter(Catergory == 'Priority Measures') %>%
  dplyr::filter(!Region == '') %>%
  gather("year", "n",4:18)

pdata$year = sub("X","",pdata$year)  # get rid of x on year column
pdata$n <- replaceCommas(pdata$n) # fix the fromatting in numeric
pdata$n <- as.numeric(as.character(pdata$n)) # convert to number
pdata<- pdata[complete.cases(pdata),]

## do a basic graph to check it out
ggplot(pdata,aes(year,n,fill=Measure)) +
  geom_bar(stat="identity",position="dodge") +
  labs(title="Absolute collection (kg/litres) per person", x = "Year", y = "Collection (kg) per person")

# used oil is order of magnitude more!
pdata.1 <- pdata %>% dplyr::filter(!Measure == 'Used Oil - Absolute Collection (litres)-Per Person')
pdata.1<- pdata.1[complete.cases(pdata.1),]


## do a basic graph to check it out
ggplot(pdata.1,aes(year,n,fill=Measure)) +
  geom_bar(stat="identity",position="dodge") +
  labs(title="Absolute collection (kg) per person", x = "Year", y = "Collection (kg) per person")




# merge in the pop.long form data
ppdata <- left_join(sum.pdata,pop.long, by = c("Region","year")) %>%
  dplyr::select(-c('n')) %>%
  mutate(unit.per.cap = total / pop)





# break up into weight and units
units.per.cap <- ppdata %>% filter(Measure == 'Absolute Collection-Units Collected-' )
weight.per.cap <-  ppdata %>% filter(Measure == 'Absolute Collection-Weight Collected (Tonnes)-' )

## Units per capita
ggplot(units.per.cap,aes(year,unit.per.cap)) + facet_wrap(~Region)+
  geom_bar(stat="identity",position="dodge") +
  labs(title="Regional Units Recycled per capita", x = "Year", y = " units per capita")
ggsave(paste('out/',"04_Beverage_UnitsPerCap.png"))

## weight per capita
ggplot(weight.per.cap,aes(year,unit.per.cap)) + facet_wrap(~Region)+
  geom_bar(stat="identity",position="dodge") +
  labs(title="Regional weight of recycling (tonnes) per capita", x = "Year", y = "weight per cap (tonnes")
ggsave(paste('out/',"04_Beverage_weightPerCap.png"))


# calculate the provincial average
bc.units.per.cap <- units.per.cap %>% na.omit() %>%
  group_by(year) %>%
  summarise(BCave = mean(unit.per.cap))

regional.units.per.cap <- units.per.cap %>% na.omit() %>%
  group_by(year,Region) %>%
  summarise(ave = mean(unit.per.cap))

# join the regional and prov. ave data and calculate the difference

diff.df <-left_join(regional.units.per.cap,bc.units.per.cap, by = 'year')
diff.df <- diff.df %>% mutate(delta = ave-BCave)
diff.df$response <- ifelse( diff.df$delta < 0, "below", "above")
# calculate the deviation from average

# Diverging Barcharts
p4 <- ggplot(diff.df, aes(x=Region, y=delta,label=delta)) + facet_wrap(~year) +
  geom_bar(stat='identity', aes(fill=response), width=.5)  +
  scale_fill_manual(name="Mileage",
                    labels = c("Above Average", "Below Average"),
                    values = c("above"="#00ba38", "below"="#f8766d")) +
  labs(title= "Regional difference from the average BC units per capita recycling") +
  coord_flip()
ggsave(paste('out/',"05_regional_bc_ave_unitsPerCap_per_yr.png"))

# Diverging Barcharts (all years)
p5 <-  ggplot(diff.df, aes(x=Region, y=delta, label = delta)) +
  geom_bar(stat='identity', aes(fill=response), width=.5)  +
  scale_fill_manual(name="Mileage",
                    labels = c("Above Average", "Below Average"),
                    values = c("above"="#00ba38", "below"="#f8766d")) +
  labs(title= "Regional difference from the average BC units per capita recycling") +
  coord_flip()
ggsave(paste('out/',"06_regional_bc_ave_unitsPerCap_allyrs.png"))



