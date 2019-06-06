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

-------------------------------------------------------
# Grab the financial data
fdata <- rdata %>% dplyr::filter(Catergory == 'Financial') %>%
          dplyr::select(-c(Catergory,Region))

fdata <- fdata %>% group_by(Measure)



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
  labs(title="Recycled Units Recovery Rate", x = "Year", y = "Recovery Rate %")




# create a nice plot :

ggplot(sum.udata,aes(year,total,fill=Measure)) +
  geom_bar(stat="identity",position="dodge")
  labs(title="Recycled units returned and sold", y = "Year", x = "Total no.")



ggplot(sum.udata,aes(x = year)) +
  geom_area(aes(y=`Units Recovered`, fill="units_recovered")) +
  geom_area(aes(y=`Units Sold`, fill="units_sold"))
labs(title="Area Chart of Returns Percentage",
     subtitle="From Wide Data format",
     caption="Source: Economics",
     y="Returns %") +  # title and caption
  #scale_x_date(labels = lbls, breaks = brks) +  # change to monthly ticks and labels
  scale_fill_manual(name="",
                    values = c("psavert"="#00ba38", "uempmed"="#f8766d")) +  # line color
  theme(panel.grid.minor = element_blank())  # turn off minor grid)







  library(ggplot2)
  library(lubridate)
  theme_set(theme_bw())

  df <- economics[, c("date", "psavert", "uempmed")]
  df <- df[lubridate::year(df$date) %in% c(1967:1981), ]

  # labels and breaks for X axis text
  brks <- df$date[seq(1, length(df$date), 12)]
  lbls <- lubridate::year(brks)

  # plot
  ggplot(df, aes(x=date)) +
    geom_area(aes(y=psavert+uempmed, fill="psavert")) +
    geom_area(aes(y=uempmed, fill="uempmed")) +
    labs(title="Area Chart of Returns Percentage",
         subtitle="From Wide Data format",
         caption="Source: Economics",
         y="Returns %") +  # title and caption
    scale_x_date(labels = lbls, breaks = brks) +  # change to monthly ticks and labels
    scale_fill_manual(name="",
                      values = c("psavert"="#00ba38", "uempmed"="#f8766d")) +  # line color
    theme(panel.grid.minor = element_blank())  # turn off minor grid)



  group_by(Measure) %>%
  summarise(sum = sum())


paste(data.dir)
