
####################################################
### Looking at Youth getting back into school
####################################################

### Set wd
setwd("Path/to/files")

### Load Libraries
library(tidyverse)
library(data.table)
library(dplyr)

library(stringr)
library(ggplot2)
library(reshape2)
library(tidycensus)

library(sp)
library(rgdal)
library(sf)
library(rgeos)
library(tmap)
library(leaflet)

library(survey)
library(srvyr)
library(ipumsr)

library(plotly)

########################################################
### Load CPS Data
## Ref: https://cran.r-project.org/web/packages/ipumsr/vignettes/ipums-cps.html
########################################################

### Load Data
cps_ddi_file2 <- "cps_00008.xml"
cps_data_file2 <- "cps_00008.dat"

cps_ddi2 <- read_ipums_ddi(cps_ddi_file2) # Contains metadata, nice to have as separate object
cps_data2 <- read_ipums_micro(cps_ddi_file2, data_file = cps_data_file2)

### View Variable Value Labels
ipums_val_labels(cps_ddi2, EMPSTAT)
ipums_val_labels(cps_ddi2, SCHLCOLL)
ipums_val_labels(cps_ddi2, EDUC)


#########################################################
### Create Dataset
#########################################################

##########################
### Youth that were part-time in school in January and Feb 2020
##########################

relevant_group <- cps_data2 %>%
  filter(AGE %in% c(18:24),
         SCHLCOLL == 4,
         MONTH %in% c(1,2),
         YEAR == 2020
         ) %>%
  dplyr::select(CPSID,CPSIDP)

##########################
### Get Youth that were part-time in school in January and Feb 2020
##########################

dat <- cps_data2 %>%
  inner_join(relevant_group)

##########################
### Look at Data
##########################

### Number of times surveys
dat %>%
  group_by(CPSID,CPSIDP) %>%
  summarise(count=n()) %>%
  summary()

dat %>%
  group_by(CPSID,CPSIDP) %>%
  summarise(count=n()) %>%
  group_by(count) %>%
  summarise(n())

### First Last month and year
dat %>%
  group_by(CPSID,CPSIDP) %>%
  summarise(
    count=n(),
    first_month = first(MONTH),
    last_month = last(MONTH),
    first_year=first(YEAR),
    last_year=last(YEAR)
    )

dat %>%
  group_by(CPSID,CPSIDP) %>%
  summarise(
    count=n(),
    first_month = first(MONTH),
    last_month = last(MONTH),
    first_year=first(YEAR),
    last_year=last(YEAR)
  ) %>%
  group_by(first_month,first_year,last_month,last_year) %>%
  summarise(count=n())

##########################
### Get Youth that were interviewed after onset of COVID
##########################

relevant_group2 <- dat %>%
  filter(MONTH %in% c(4:5),
         YEAR == 2020,
         SCHLCOLL == 5) %>%
  dplyr::select(CPSID,CPSIDP)

##########################
### Get Youth that were no longer part-time in school and are not attending school
##########################

dat2 <- dat %>%
  inner_join(relevant_group2)


##########################
### Get Youth that were interviewed again in Sept onward
##########################

relevant_group3 <- dat2 %>%
  mutate(survey_month=as.Date(paste(YEAR,"-",MONTH,"-01",sep = ""))) %>%
  filter(survey_month > as.Date("2020-09-01")) %>%
  group_by(CPSID,CPSIDP) %>% ###
  summarise(
    first_edu=first(EDUC),
    last_educ=last(EDUC)
  ) %>%
  filter(first_edu == last_educ) %>%
  dplyr::select(CPSID,CPSIDP)

##########################
### Get Youth that were no longer part-time in school and are not attending school and were interviewed again later
##########################

dat3 <- dat2 %>%
  inner_join(relevant_group3)


#########################################################
### Analysis
#########################################################

dat4<-dat3 %>%
  mutate(survey_month=as.Date(paste(YEAR,"-",MONTH,"-01",sep = ""))) %>%
  filter(survey_month > as.Date("2020-09-01"))  %>%
  filter(!duplicated(.))

### Plot
dat4 %>%
  group_by(SCHLCOLL) %>%
  summarise(
    unweighted_count=n(),
    weighted_count=round(sum(WTFINL,na.rm = T),0)
  ) %>%
  mutate(
    unweighted_percent=prop.table(unweighted_count)*100,
    weighted_percent=round(prop.table(weighted_count)*100,1),
    weighted_percent_label=paste(weighted_percent,"%","")
  ) %>%
  filter(SCHLCOLL>2) %>%
  mutate(SCHLCOLL = case_when(
    SCHLCOLL == 3 ~ "Full-time College Enrolled",
    SCHLCOLL == 4 ~ "Part-time College Enrolled",
    SCHLCOLL == 5 ~ "Not Enolled in College"
  )) %>%
  ggplot(aes(SCHLCOLL,weighted_percent)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=weighted_percent_label),vjust=-.5,color="black",size=5) +
  geom_text(aes(label=weighted_count),vjust=3.5,color="white",size=4) +
  xlab("") + ylab("Percent") +
  ggtitle("Almost 70% of Part-time College Dropouts from COVID have not re-enrolled") +
  labs(caption = "Data from Current Population Survey.") +
  scale_x_discrete(limits = c("Full-time College Enrolled",
                              "Part-time College Enrolled",
                              "Not Enolled in College"))
ggsave("part_time_dropouts.png")


##################################################################################################################
##################################################################################################################
### Analysis for Full time
##################################################################################################################
##################################################################################################################


#########################################################
### Create Dataset
#########################################################

##########################
### Youth that were part-time in school in January and Feb 2020
##########################

relevant_group <- cps_data2 %>%
  filter(AGE %in% c(18:24),
         SCHLCOLL == 3,
         MONTH %in% c(1,2),
         YEAR == 2020
  ) %>%
  dplyr::select(CPSID,CPSIDP)

##########################
### Get Youth that were part-time in school in January and Feb 2020
##########################

dat <- cps_data2 %>%
  inner_join(relevant_group)

##########################
### Look at Data
##########################

### Number of times surveys
dat %>%
  group_by(CPSID,CPSIDP) %>%
  summarise(count=n()) %>%
  summary()

dat %>%
  group_by(CPSID,CPSIDP) %>%
  summarise(count=n()) %>%
  group_by(count) %>%
  summarise(n())

### First Last month and year
dat %>%
  group_by(CPSID,CPSIDP) %>%
  summarise(
    count=n(),
    first_month = first(MONTH),
    last_month = last(MONTH),
    first_year=first(YEAR),
    last_year=last(YEAR)
  )

dat %>%
  group_by(CPSID,CPSIDP) %>%
  summarise(
    count=n(),
    first_month = first(MONTH),
    last_month = last(MONTH),
    first_year=first(YEAR),
    last_year=last(YEAR)
  ) %>%
  group_by(first_month,first_year,last_month,last_year) %>%
  summarise(count=n())

##########################
### Get Youth that were interviewed after onset of COVID
##########################

relevant_group2 <- dat %>%
  filter(MONTH %in% c(4:5),
         YEAR == 2020,
         SCHLCOLL == 5) %>%
  dplyr::select(CPSID,CPSIDP)

##########################
### Get Youth that were no longer part-time in school and are not attending school
##########################

dat2 <- dat %>%
  inner_join(relevant_group2)


##########################
### Get Youth that were interviewed again in Sept onward
##########################

relevant_group3 <- dat2 %>%
  mutate(survey_month=as.Date(paste(YEAR,"-",MONTH,"-01",sep = ""))) %>%
  filter(survey_month > as.Date("2020-09-01")) %>%
  group_by(CPSID,CPSIDP) %>% ###
  summarise(
    first_edu=first(EDUC),
    last_educ=last(EDUC)
  ) %>%
  filter(first_edu == last_educ) %>%
  dplyr::select(CPSID,CPSIDP)

##########################
### Get Youth that were no longer part-time in school and are not attending school and were interviewed again later
##########################

dat3 <- dat2 %>%
  inner_join(relevant_group3)


#########################################################
### Analysis
#########################################################

dat4<-dat3 %>%
  mutate(survey_month=as.Date(paste(YEAR,"-",MONTH,"-01",sep = ""))) %>%
  filter(survey_month > as.Date("2020-09-01")) %>%
  filter(!duplicated(.))

### Plot
dat4 %>%
  group_by(SCHLCOLL) %>%
  summarise(
    unweighted_count=n(),
    weighted_count=round(sum(WTFINL,na.rm = T),0)
  ) %>%
  mutate(
    unweighted_percent=prop.table(unweighted_count)*100,
    weighted_percent=round(prop.table(weighted_count)*100,1),
    weighted_percent_label=paste(weighted_percent,"%","")
  ) %>%
  filter(SCHLCOLL>2) %>%
  mutate(SCHLCOLL = case_when(
    SCHLCOLL == 3 ~ "Full-time College Enrolled",
    SCHLCOLL == 4 ~ "Part-time College Enrolled",
    SCHLCOLL == 5 ~ "Not Enolled in College"
  )) %>%
  ggplot(aes(SCHLCOLL,weighted_percent)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=weighted_percent_label),vjust=-.5,color="black",size=5) +
  geom_text(aes(label=weighted_count),vjust=3.5,color="white",size=4) +
  xlab("") + ylab("Percent") +
  ggtitle("Almost 50% of Full-time College Dropouts from COVID have not re-enrolled") +
  labs(caption = "Data from Current Population Survey.") +
  scale_x_discrete(limits = c("Full-time College Enrolled",
                              "Part-time College Enrolled",
                              "Not Enolled in College"))
ggsave("full_time_dropouts.png")
