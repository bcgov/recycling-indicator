library(readxl)
library(cellranger) # letter_to_num
library(dplyr)
library(tidyr)
library(envreportutils)
library(stringr)

#excel_file <- file.path(
#  soe_path("Operations ORCS/Data - Working/sustainability/EPR"),
#  "EPR annual report info roll up 2017.xlsb.xlsx"
#)

# Or use local copy to test
excel_file <- file.path("C:/Temp/Github/recycling-indicator/data",
          "EPR annual report info roll up 2017.xlsb.xlsx"
)

#list.files("C:/Temp/Github/recycling-indicator/data")

rename_by_pos <- function(df, index, new_name){
  colnames(df)[index] = new_name
  df
}

ncols_from_range <- function(range) {
  diff(letter_to_num(strsplit(gsub("[0-9]", "", range), ":")[[1]])) + 1
}


# Beverage functions and data extract -------------------------

#read_bevs_recovery <- function(file, range, org) {
#  read_excel(excel_file, sheet = "Bevs(2000-2017)",
#             range = range, col_names = TRUE) %>%
#    #rename_at(1:2, ~ c("measure", "regional_district")) %>%
#    rename_by_pos(1,"measure") %>%
#    rename_by_pos(2,"regional_district") %>%
#    filter(measure != "Priority Measures") %>%
#    mutate(organization = org,
#           type = "bev") %>%
#    select(organization, type, everything())
#}

read_bevs_financial <- function(file, range, org) {
  cols <- ncols_from_range(range)
  read_excel(excel_file, sheet = "Bevs(2000-2017)",
             range = range,
             col_types = c("text", "skip", rep("numeric", cols - 2)),
             col_names = c("measure", "foo",
                           seq(2000, length.out = cols - 2))) %>%
    mutate(organization = org,
           type = "bev") %>%
    select(organization, type, everything())

}

read_bevs_units <- function(file, range, org) {
  cols <- ncols_from_range(range)
  read_excel(excel_file, sheet = "Bevs(2000-2017)",
             range = range,
             col_types = c("text", "skip", rep("numeric", cols - 2)),
             col_names = c("measure", "foo",
                           seq(2000, length.out = cols - 2))) %>%
    mutate(organization = org,
          type = "bev") %>%
    select(organization, type, everything())
}

read_bevs_recovery_raw <- function(file, range,org) {
  cols <- ncols_from_range(range)
  read_excel(excel_file, sheet = "Bevs(2000-2017)",
             range = range,
             col_types = c("text", "text", rep("numeric", cols - 2)),
             col_names = c("measure", "regional_district",
                           seq(2000, length.out = cols - 2))) %>%
    filter(measure != "Priority Measures") %>%
    mutate(regional_district = gsub("-", " ", regional_district),
           organization = org,
           type = "bev") %>%
    select(organization, type, measure, everything())
}

#--------------------------------------------------------------
# extract Bev data

#encorp_priority <- read_bevs_recovery(excel_file, "B5:U38", "Encorp Pacific")
#bc_brewers_priority <- read_bevs_recovery(excel_file, range = "B138:U170", "BC Brewers")

encorp_financial <- read_bevs_financial(excel_file, "B45:U53", "Encorp Pacific")
bc_brewers_financial <- read_bevs_financial(excel_file, "B185:U193", "BC Brewers")

encorp_units <- read_bevs_units(excel_file, "B55:U56", "Encorp Pacific")
bc_brewers_units <- read_bevs_units(excel_file, "B195:U196", "BC Brewers")

encorp_priority_raw <- read_bevs_recovery_raw(excel_file, "B65:U122", "Encorp Pacific")
bc_brewers_priority_raw <- read_bevs_recovery_raw(excel_file, "B209:U293", "BC Brewers")


# combine data sets into single file per metric (financial/units/priority)
financial <- rbind(encorp_financial, bc_brewers_financial)

units <- rbind(encorp_units, bc_brewers_units)

priority_raw <- rbind(encorp_priority_raw, bc_brewers_priority_raw )


# Functions to get Oil data from xlxs.--------------------------------

read_oil_recovery <- function(file, range,type) {
  cols <- ncols_from_range(range)
read_excel(excel_file, sheet = "Oil(2003-2017) ",
               range = range,
               col_types = c("text", rep("numeric", cols - 1)),
               col_names = c("regional_district",
                   seq(2003, length.out = cols - 1))) %>%
          mutate(measure = type,
                 regional_district = gsub("-", " ", regional_district),
                 organization = NA,
                 type = "oil") %>%
          select(organization,type,measure, everything()
  )
}

read_oil_financial <- function(file, range) {
  cols <- ncols_from_range(range)
  read_excel(excel_file, sheet = "Oil(2003-2017) ",
             range = range,
             col_types = c("text", rep("numeric", cols - 1)),
             col_names = c("measure",
                           seq(2003, length.out = cols - 1))) %>%
    mutate(type = "oil",
           organization = NA) %>%
    select(organization,type,measure, everything())
}

read_oil_units <- function(file, range, org) {
  cols <- ncols_from_range(range)
read_excel(excel_file, sheet = "Oil(2003-2017) ",
             range = range,
             col_types = c("text", rep("numeric", cols - 1)),
             col_names = c("measure",
                seq(2003, length.out = cols - 1))) %>%
        mutate(type = "oil",
               organization = NA) %>%
        select(organization,type,measure, everything())
}

oil_recovery <- read_oil_recovery(excel_file, "B12:Q40","oil_lt_pp")
filter_recovery <- read_oil_recovery(excel_file, "B42:Q70","filters_kg_pp")
contain_recovery <- read_oil_recovery(excel_file, "B72:Q100", "containers_kg_pp")
anti_recovery <- read_oil_recovery(excel_file, "B102:Q130", "antifreeze_kg_pp")

recovery_pp <- rbind(oil_recovery,
                     filter_recovery,
                     contain_recovery,
                     anti_recovery)

oil_financial <- read_oil_financial(excel_file, "B147:Q152")

oil_units <- read_oil_units(excel_file, "B154:Q163")


# TIRE INDICATOR ------------------------------------------------------

read_tire_units <- function(file, range) {
    cols <- ncols_from_range(range)
    read_excel(excel_file, sheet = "Tires(2007-2017)",
               range = range,
               col_types = c("text", rep("numeric", cols - 1)),
               col_names = c("measure",
                             seq(2007, length.out = cols - 1))) %>%
      mutate(type = "tire",
             organization = NA) %>%
      select(organization,type,measure, everything())
}

read_tire_financial <- function(file, range) {
  cols <- ncols_from_range(range)
  read_excel(excel_file, sheet = "Tires(2007-2017)",
             range = range,
             col_types = c("text", rep("numeric", cols - 1)),
             col_names = c("measure",
                           seq(2007, length.out = cols - 1))) %>%
    mutate(type = "tire",
           organization = NA) %>%
    select(organization,type,measure, everything())
}


tire_financial <- read_tire_financial(excel_file,"B31:M37")

tire_units <- read_tire_units(excel_file,"B39:M49")


# PAINTS _FLAM_PEST ----------------------------------------

read_pfp_recovery <- function(file, range) {
  cols <- ncols_from_range(range)
  read_excel(excel_file, sheet = "Paints-Flam-Pest(2000-2017)",
             range = range,
             col_types = c("text", "text", rep("numeric", cols - 2)),
             col_names = c("measure_long","foo",
                           seq(2000, length.out = cols - 2))) %>%
        mutate(test = str_detect(measure_long,"Population|Per Person")) %>%
        filter(test == "FALSE") %>%
        mutate(regional_district =  gsub(".*-(.*)", "\\1", measure_long),
              measure = 'Absolute Collection- Total Tubskids',
              type = 'pfp',
              organization = NA) %>%
        select(-c('foo',"measure_long","test")) %>%
        select(organization, type, measure, regional_district, everything())

}

read_pfp_financial <- function(file, range) {
  cols <- ncols_from_range(range)
  read_excel(excel_file, sheet = "Paints-Flam-Pest(2000-2017)",
             range = range,
             col_types = c("text", "text", rep("numeric", cols - 2)),
             col_names = c("measure","foo",
                           seq(2000, length.out = cols - 2))) %>%
    mutate(type = "pfp",
           organization = NA) %>%
    select(organization, type, measure, everything()) %>%
    select(-c('foo'))
}

read_pfp_units <- function(file, range) {
   cols <- ncols_from_range(range)
  read_excel(excel_file, sheet = "Paints-Flam-Pest(2000-2017)",
             range = range,
             col_types = c("text","text", rep("numeric", cols - 2)),
             col_names = c("measure", "foo",
                           seq(2000, length.out = cols - 2))) %>%
          mutate(type = "pfp",
                 organization = NA) %>%
          select(organization, type, measure, everything()) %>%
          select(-c('foo'))
}

pfp_recovery <- read_pfp_recovery(excel_file,"B98:U187")

pfp_financial <- read_pfp_financial(excel_file,"B66:U72")

pfp_units <- read_pfp_units(excel_file,"B74:U87")


# Elect ------------------------------------------------

# this is compiled data for different types of collections
# (potentially not informative)

#read_elect_recovery <- function(file, range) {
#  cols <- ncols_from_range(range)
#  read_excel(excel_file, sheet = "Elect(2007-2017)",
#                range = range, col_names = TRUE) %>%
#          select(c('Regional District',
#                grep("Recovery Rate or Capture  Rate", names(.)))) %>%
#          rename_at(2:5, ~  seq(2014, length.out = 4)) %>%
#          mutate(regional_district = gsub("-", " ", `Regional District`),
#                 measure = 'Recovery Rate or Capture  Rate',
#                 organization = 'EPRA') %>%
#          select(organization, type, measure, everything()) %>%
#          select(-("Regional District"))
#}

#elect_recovery_combo <- read_elect_recovery(excel_file,"B57:M87")

read_elect_region <- function(file, range, type) {
  cols <- ncols_from_range(range)
  read_excel(excel_file, sheet = "Elect(2007-2017)",
                 range = range,
                 col_types = c("text", "text", rep("numeric", cols - 2)),
                 col_names = c("measure", "regional_district",
                               seq(2007, length.out = cols - 2))) %>%
    mutate(regional_district = gsub("-", " ", regional_district),
           organization = type,
           type = "elect") %>%
    select(organization, type, measure, everything())
}

elect_CESA <- read_elect_region (excel_file,"B142:N169","CESA")
elect_smoke <- read_elect_region (excel_file,"B223:N250","Smoke_CO")
elect_lamp <- read_elect_region (excel_file,"B304:N332","Lamp_Lighting")
elect_thermo <- read_elect_region (excel_file,"B347:N430","Thermo")
elect_call2cell <- read_elect_region (excel_file,"B483:N510","Call2Cell")
elect_mmr <- read_elect_region (excel_file,"B664:N691","MMR")

elect_compile <- rbind(elect_CESA, elect_smoke, elect_thermo,
                       elect_lamp, elect_call2cell, elect_mmr)

# Pharm ------------------------------------------------

read_pharm_recovery <- function(file, range) {
  cols <- ncols_from_range(range)
  read_excel(excel_file, sheet = "Pharm(2000-2017)",
             range = range,
             col_types = c("text", "text", rep("numeric", cols - 2)),
             col_names = c("measure", "regional_district",
                           seq(2000, length.out = cols - 2))) %>%
        mutate(test = str_detect(measure, "Population")) %>%
        filter(test == "FALSE") %>%
        mutate(regional_district = gsub("-", " ", regional_district),
               type = "pharm",
               organization = NA) %>%
        select(organization, type, measure, regional_district, everything())  %>%
        select(-c ("test"))

}

# pharm units

read_pharm_units <- function(file, range) {
  cols <- ncols_from_range(range)
  read_excel(excel_file, sheet = "Pharm(2000-2017)",
             range = range,
             col_types = c("text", "text", rep("numeric", cols - 2)),
             col_names = c("measure", "foo",
                    seq(2000, length.out = cols - 2))) %>%
    mutate(type = "pharm",
           organization = NA )%>%
    select(organization, type, measure,everything()) %>%
    select(-c('foo'))
}

pharm_recovery <- read_pharm_recovery(excel_file,"B81:U164")

pharm_units <- read_pharm_units(excel_file,"B50:U51")

# PPP ----------------------------------------------

read_ppp_recovery <- function(file, range) {
  cols <- ncols_from_range(range)
  read_excel(excel_file, sheet = "PPP(2014-2017)",
             range = range,
             col_types = c("text", rep("numeric", cols - 1)),
             col_names = c("regional_district",
                           seq(2014, length.out = cols - 1))) %>%
    mutate(regional_district = gsub("-", " ", regional_district),
           measure = "tonnes of ppp",
           type = "ppp") %>%
    select(type, measure, regional_district, everything())
}

read_ppp_financial <- function(file, range) {
  cols <- ncols_from_range(range)
  read_excel(excel_file, sheet = "PPP(2014-2017)",
             range = range,
             col_types = c("text", rep("numeric", cols - 1)),
             col_names = c("measure",
                           seq(2014, length.out = cols - 1))) %>%
    mutate(type = "ppp") %>%
    select(measure, type, everything())
}

ppp_recovery <- read_ppp_recovery(excel_file,"A20:E48")

ppp_financial <- read_ppp_financial(excel_file,"A15:E16")


# combined datasets into three datasets --------------

all.finance <- bind_rows(financial, oil_financial, tire_financial,
                         ppp_financial, pfp_financial)

all.regions <- bind_rows(priority_raw, recovery_pp, pfp_recovery,
                         elect_compile, ppp_recovery, pharm_recovery)

## still to fix this bit
r.to.fix <- c("Comox", "Strathcona","Skeena-Queen Charlotte","Powell River")

SC.data <- all.regions %>%
      filter(regional_district %in% r.to.fix)



all.units <- bind_rows(units, oil_units, tire_units, pfp_units,
                       pharm_units)

write.csv(all.finance, paste('data', "all.finance.csv", sep = "/"),
          row.names = FALSE )

write.csv(all.regions, paste('data', "all.regions.csv", sep = "/"),
          row.names = FALSE )

write.csv(all.units, paste('data', "all.units.csv", sep = "/"),
          row.names = FALSE )
