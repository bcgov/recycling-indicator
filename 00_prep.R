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

x <- c("dplyr","ggplot2","tidyr","stringr","here","reshape") #raster","sp","sf","rgdal","xlsx","rJava","tibble","mapview","gtools")
lapply(x, library, character.only = TRUE) ; rm(x)  # load the required packages


source('00_Functions.R')

## Load  data files
data.dir <- "data/"

rdata<- read.csv(paste(data.dir,"bev.csv",sep = ""))

head(rdata)
unique(rdata$Company)
unique(rdata$Catergory)
unique(rdata$Measure)

------------------------------------------------------
# extract the population data and export as csv
pop <- rdata %>% dplyr::filter(Measure == 'Population-') %>%
          dplyr::select(-c(Company,Catergory,Measure))
write.csv(pop, paste(data.dir,"popbc.csv",sep = ""), row.names = FALSE)


## STILL TO COMPLETE

-------------------------------------------------------
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
unique(fdata$Measure)

# Does spending more on consumer awareness decrease unclaimed deposits
    ggplot(sum.fdata,aes(year,total,fill=Measure)) +
      geom_bar(stat="identity",position="dodge") +
      labs(title="Unclaimed deposits and consumer-expenditure", x = "Year", y = " Amount ($1,000,000)")
    #ggsave(paste('out/',"01_Beverage_UnitsMoved.png"))


## may need to correct per unit




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
ggsave(paste('out/',"03_Beverage_Other.png"))




