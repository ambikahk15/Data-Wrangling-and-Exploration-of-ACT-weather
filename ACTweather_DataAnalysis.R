#****************************************************************************
# Intro to Data Science:11516
# U3227622: Ambika Kapanaiah  
# Description:Assignment1-Data wrangling
#****************************************************************************

#preparing the environment for loading data
#removing all env variables.
rm(list = ls())
#setting the working directory for the current script to execute
#getting the directory of the current script
#this following has to be executed twice if the script has not invoked R studio.
#hence executing twice to set the working directory to the script path
for(i in 1:2){
  Wrkng_Dir <- setwd(dirname(rstudioapi::getActiveDocumentContext()$path))}
Wrkng_Dir
#Appending data files folder to the path
dataFile_Path <- file.path(Wrkng_Dir, "data")
dataFile_Path
#getting the file names under InputData folder of captured data
filenames <- list.files(dataFile_Path)
filenames
#calculating the number of files in InputData folder
numfiles <- length(filenames) 
numfiles

#PART A----reading all the csv files into single data frame#####################

#1.load the files into working directory one by one#############################
#2. Concatenate all data into one data frame####################################

if(!("tidyverse" %in% rownames(installed.packages()))){
  install.packages("tidyverse")
}
library(tidyverse)
library(dplyr)

#appended all the data after reading csv into single tibble tblACT_wthr
tblACT_wthr <-
  list.files(path = dataFile_Path,
             pattern = "*.csv", 
             full.names = T) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"), skip = 7)) 
tblACT_wthr

#3. check for problems while loading############################################
#"calm" value in 9am and 3pm wind speed replaced with 0 km/hr
tblACT_wthr$`9am wind speed (km/h)` <- lapply(tblACT_wthr$`9am wind speed (km/h)`, 
                                              gsub, pattern = "Calm", replacement = 0, 
                                              fixed = TRUE)
tblACT_wthr$`3pm wind speed (km/h)` <- lapply(tblACT_wthr$`3pm wind speed (km/h)`, 
                                              gsub, pattern = "Calm", replacement = 0, 
                                              fixed = TRUE)

#View(tblACT_wthr)

#converting few columns to double and few to numeric
cols_num <- c(colnames(tblACT_wthr[2]),colnames(tblACT_wthr[3]),
              colnames(tblACT_wthr[10]),colnames(tblACT_wthr[15]),
              colnames(tblACT_wthr[16]),colnames(tblACT_wthr[21]))
tblACT_wthr[cols_num] <- sapply(tblACT_wthr[cols_num],as.double)

cols_num <- c(colnames(tblACT_wthr[4]),colnames(tblACT_wthr[8]),
              colnames(tblACT_wthr[11]),colnames(tblACT_wthr[12]),
              colnames(tblACT_wthr[14]),colnames(tblACT_wthr[17]),
              colnames(tblACT_wthr[18]),colnames(tblACT_wthr[20]))
tblACT_wthr[cols_num] <- sapply(tblACT_wthr[cols_num],as.numeric)

sapply(tblACT_wthr, class)


####################################----PartB-----#####################################


#1. removing NAs (columns with all NA)##########################################

all_na <- function(x) any(!is.na(x))
tblACT_wthr <- tblACT_wthr %>% select_if(all_na)
tblACT_wthr

#2. removing cols with more than 90% of NA######################################
#calculating threshold as 90%
threshold = round(nrow(tblACT_wthr) * 0.9)
#checking after summing up NA's are greater than or equal to 90% threshold
tblActwthr_tidied <- tblACT_wthr[,colSums(is.na(
  tblACT_wthr[ , 1:ncol(tblACT_wthr)])) < threshold]
tblActwthr_tidied

#3. replacing column names with space to underscore#############################
names(tblActwthr_tidied) <- gsub(" ", "_", names(tblActwthr_tidied))
colnames(tblActwthr_tidied)

#4.Changing date from char type to date data type###############################
tblActwthr_tidied$Date <- as.Date(tblActwthr_tidied$Date,format="%d/%m/%Y")
class(tblActwthr_tidied$Date)

#5. extracting date column and adding month and year column.####################
tblActwthr_tidied$Month <- as.numeric(format(as.Date(tblActwthr_tidied$Date),
                                             "%m"))
tblActwthr_tidied$Year <- as.numeric(format(as.Date(tblActwthr_tidied$Date),
                                            "%Y"))
#relocating Month and year column after Date column
tblActwthr_tidied <- tblActwthr_tidied[c(1,20,21,2,3,4,5,6,7,8,9,10,11,12,
                                         13,14,15,16,17,18,19)]
head(tblActwthr_tidied)

#6. Month and Year column changed to ordinals.##################################
tblActwthr_tidied$Month <- factor(tblActwthr_tidied$Month, levels = c(1:12))
tblActwthr_tidied$Year <- factor(tblActwthr_tidied$Year, 
                                 levels = c(2018, 2019 , 2020), ordered = FALSE)
class(tblActwthr_tidied$Month)
class(tblActwthr_tidied$Year)
head(tblActwthr_tidied$Month)
head(tblActwthr_tidied$Year)

#7. Imputation for column with NA replacing with median values.#################

#for each cols which are numeric, extracting row number with na and replacing with
#median of that column
for (cols in which(sapply(tblActwthr_tidied, is.numeric))) {
  for (row in which(is.na(tblActwthr_tidied[, cols]))) {
    tblActwthr_tidied[row, cols] <- median(tblActwthr_tidied[[cols]],
                                           na.rm = TRUE)}}

head(tblActwthr_tidied)

########################------Part C----------##################################

#1.Min median mean max for specified columns using summarise function.
stats_tblActWhthr <- tblActwthr_tidied %>% 
  summarise(min_mntemp=min(Minimum_temperature),
            med_mntemp=median(Minimum_temperature),
            mean_mntemp=mean(Minimum_temperature),
            max_mntemp=max(Minimum_temperature),
            min_mxtemp=min(Maximum_temperature),
            med_mxtemp=median(Maximum_temperature),
            mean_mxtemp=mean(Maximum_temperature),
            max_mxtemp=max(Maximum_temperature),
            min_9amtemp=min(`9am_Temperature`),
            med_9amtemp=median(`9am_Temperature`),
            mean_9amtemp=mean(`9am_Temperature`),
            max_9amtemp=max(`9am_Temperature`),
            min_3pmtemp=min(`3pm_Temperature`),
            med_3pmtemp=median(`3pm_Temperature`),
            mean_3pmtemp=mean(`3pm_Temperature`),
            max_3pmtemp=max(`3pm_Temperature`),
            min_mxgstspd=min(`Speed_of_maximum_wind_gust_(km/h)`),
            med_mxgstspd=median(`Speed_of_maximum_wind_gust_(km/h)`),
            mean_mxgstspd=mean(`Speed_of_maximum_wind_gust_(km/h)`),
            max_spdofmxwndgust=max(`Speed_of_maximum_wind_gust_(km/h)`))


stats_tblActWhthr

#2.and 3. average min and max temp per##########################################
#month and year(Minimium_Teperature and Maximum_Temperature)
minmax_temp_month <- tblActwthr_tidied %>% group_by(Year, Month) %>% 
  summarise(mean_mintemp_prmn = mean(Minimum_temperature),
            mean_maxtemp_prmn = mean(Maximum_temperature))
minmax_temp_year <- tblActwthr_tidied %>% group_by(Year) %>% 
  summarise(mean_mintemp_pryr = mean(Minimum_temperature),
            mean_maxtemp_pryr = mean(Maximum_temperature))

minmax_temp_month
minmax_temp_year

#4. wind gust speed average#####################################################
#calculation after grouped by direction of wind gust
windgst_avg <- tblActwthr_tidied %>% 
  group_by(Direction_of_maximum_wind_gust) %>% 
  summarise(mean_gst = mean(`Speed_of_maximum_wind_gust_(km/h)`))
windgst_avg

#5. highest rainfall year and month.###########################################
#grouped by monthly each year getting the maximum rainfall in mm 
#Extracting max value corresponding month and year and printing the same.

tbl_maxrain <- tblActwthr_tidied %>%
  group_by(Year,Month) %>%
  summarise(rain = max(`Rainfall_(mm)`))

Month <- tbl_maxrain[which.max(tbl_maxrain$rain),"Month"]
Year <- tbl_maxrain[which.max(tbl_maxrain$rain),"Year"]
rain <- round(tbl_maxrain[which.max(tbl_maxrain$rain),"rain"],2)
print(paste0("Highest rainfall happens in month-",Month,
             "  and in year-",Year[[1]],
             " and is=",rain,"mm"))

#6. no rainfall month and year.################################################
#grouping by month and year summing up all rainfall in mm
#checking which month has 0mm out of all.
tbl_norain <- tblActwthr_tidied %>%
      group_by(Year,Month) %>%
      summarise(rain = sum(`Rainfall_(mm)`))
yrnorain <- tbl_norain[which(tbl_norain$rain == 0),"Year"]
mntnorain <- tbl_norain[which(tbl_norain$rain == 0),"Month"]
if(any(tbl_norain$rain==0)){
  print(paste("There is no rainfall in the month",mntnorain,"and the year-",
              yrnorain,))
}else{print("There is rainfall in one or the other months in every year,so there was no dry month")}
  


#7. highest humidity level-2019#################################################
#filter only 2019 data gather humid data from 9am and 3pm column
#extract the average for each month and then get the max value 
#and print the corresponding month

tbl_meanhumid <- tblActwthr_tidied %>%
  filter(Year==2019) %>%
  gather(key= "humid_key",
         value= "humid_val",
         c(`9am_relative_humidity_(%)`,`3pm_relative_humidity_(%)`)) %>%
  group_by(Month) %>%
  summarise(humid = mean(humid_val))

Month <- tbl_meanhumid[which.max(tbl_meanhumid$humid),"Month"]
humid <- round(tbl_meanhumid[which.max(tbl_meanhumid$humid),"humid"],2)
print(paste0("Highest humidity is in month-",Month,
             " and is=",humid,"%"," in the year-2019"))



#8. min max and avg for temp,wind_speed and humidity per month#################

#filtering 2019 data and gathering 9am and 3pm values with temp,
#windspeed and humid as keys
#grouping data by month and summarising min max and average 
#values for temp, windspeed and humidity.
Mnthlydata_2019 <- tblActwthr_tidied %>% 
  filter(Year==2019) %>%
  gather(key= "temp_key",
         value= "temp_val",
         c(`9am_Temperature`, `3pm_Temperature`)) %>%
  gather(key= "wndspd_key",
         value= "wndspd_val",
         c(`9am_wind_speed_(km/h)`, `3pm_wind_speed_(km/h)`)) %>%
  gather(key= "humid_key",
         value= "humid_val",
         c(`9am_relative_humidity_(%)`, `3pm_relative_humidity_(%)`)) %>%
  group_by(Month) %>%
  summarize(min_temp=min(temp_val),
            max_temp=max(temp_val),
            avg_temp=mean(temp_val),
            wndspd_min=min(wndspd_val),
            wndspd_max=max(wndspd_val),
            wndspd_avg=mean(wndspd_val),
            humid_min=min(humid_val),
            humid_max=max(humid_val),
            humid_avg=mean(humid_val))

Mnthlydata_2019

#min max and avg temp , wind_speed and humidity per quarter####################
if(!("lubridate" %in% rownames(installed.packages()))){
  install.packages("lubridate")
}
library(lubridate)

#filtering 2019 data and gathering 9am and 3pm values with temp,
#windspeed and humid as keys
#grouping data by quarter and summarising min max and average 
#values for temp, windspeed and humidity.

qrterlydata_2019 <- tblActwthr_tidied %>% 
  filter(Year==2019) %>%
  gather(key= "temp_key",
         value= "temp_val",
         c(`9am_Temperature`, `3pm_Temperature`)) %>%
  gather(key= "wndspd_key",
         value= "wndspd_val",
         c(`9am_wind_speed_(km/h)`, `3pm_wind_speed_(km/h)`)) %>%
  gather(key= "humid_key",
         value= "humid_val",
         c(`9am_relative_humidity_(%)`, `3pm_relative_humidity_(%)`)) %>%
  group_by(Date=quarter(Date, with_year = T)) %>%
  summarize(min_temp=min(temp_val),
            max_temp=max(temp_val),
            avg_temp=mean(temp_val),
            wndspd_min=min(wndspd_val),
            wndspd_max=max(wndspd_val),
            wndspd_avg=mean(wndspd_val),
            humid_min=min(humid_val),
            humid_max=max(humid_val),
            humid_avg=mean(humid_val))

qrterlydata_2019

#9. barplots for all the specified variables.##################################
if(!("ggplot2" %in% rownames(installed.packages()))){
  install.packages("ggplot2")
}
library(ggplot2)

if(!("reshape2" %in% rownames(installed.packages()))){
  install.packages("reshape2")
}
library(reshape2)

#Selecting data to gather all the values to single column across(min,max and avg)
#renaming value column to "Temp", "WindSpd" and "Humidity" accordingly
Mnthlydata_temp <- melt(select(Mnthlydata_2019,c(1:4)))
colnames(Mnthlydata_temp)[2] <- "Temp"
Mnthlydata_wndspd <- melt(select(Mnthlydata_2019,c(1),c(5:7)))
colnames(Mnthlydata_wndspd)[2] <- "Windspd"
Mnthlydata_humid <- melt(select(Mnthlydata_2019,c(1),c(8:10)))
colnames(Mnthlydata_humid)[2] <- "Humidity"

#bar plot for min max and avg of temperature-2019 each Month
Mnthlydata_temp

Mnthlydata_temp %>%
  ggplot(aes(Month, value, fill = Temp)) +
  geom_bar(position="dodge", stat = "identity",color="black") + 
  ggtitle("Min,Max,Average Temperature-2019") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Months(1 to 12)-2019")+
  ylab("min,max,avg temp(DegC)")

#bar plot for min max and avg of windspeed-2019 each Month
Mnthlydata_wndspd

Mnthlydata_wndspd %>%
  ggplot(aes(Month, value, fill = Windspd)) +
  geom_bar(position="dodge", stat = "identity",color="black") + 
  ggtitle("Min,Max,Average Windspeed-2019") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Months(1 to 12)-2019")+
  ylab("min,max,avg windspeed(km/h)")

#bar plot for min max and avg of humidity-2019 each Month
Mnthlydata_humid

Mnthlydata_humid %>%
  ggplot(aes(Month, value, fill = Humidity)) +
  geom_bar(position="dodge", stat = "identity",color="black") + 
  ggtitle("Min,Max,Average Humidity-2019") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Months(1 to 12)-2019")+
  ylab("min,max,avg humidity(%))")


###quarterly -2019 plots for each variable
#creating column quatrter to represent each quarter data
qrterlydata_2019$quarter <- factor(c(1,2,3,4))
#relocating column quarter as first column
qrterlydata_2019 <- qrterlydata_2019 %>% relocate(quarter, .before = min_temp)
#Removing Date column as it is unnecessary.
qrterlydata_2019 <- select(qrterlydata_2019, -c(Date))

#Selting data to gather all the values to single column across(min,max and avg)
#renaming value column to "Temp", "WindSpd" and "Humidity" accordingly
qrterly_temp <- melt(select(qrterlydata_2019,c(1:4)))
colnames(qrterly_temp)[2] <- "Temp"
qrterly_wndspd <- melt(select(qrterlydata_2019,c(1),c(5:7)))
colnames(qrterly_wndspd)[2] <- "WindSpd"
qrterly_humid <- melt(select(qrterlydata_2019,c(1),c(8:10)))
colnames(qrterly_humid)[2] <- "Humidity"

#bar plot for min max and avg of temperature-2019 each quarter.
qrterly_temp

qrterly_temp %>%
  ggplot(aes(quarter, value, fill = Temp)) +
  geom_bar(position="dodge", stat = "identity",color="black") + 
  ggtitle("Min,Max,Average Temperature-2019") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Quarter(1 to 4)-2019")+
  ylab("min,max,avg temp(DegC)")

#bar plot for min max and average of windspeed-2019 each quarter.
qrterly_wndspd

qrterly_wndspd %>%
  ggplot(aes(quarter, value, fill = WindSpd)) +
  geom_bar(position="dodge", stat = "identity",color="black") + 
  ggtitle("Min,Max,Average Windspeed-2019") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Quarter(1 to 4)-2019")+
  ylab("min,max,avg windspeed(km/h)")

#bar plot for min max and average of humidity-2019 each quarter
qrterly_humid

qrterly_humid %>%
  ggplot(aes(quarter, value, fill = Humidity)) +
  geom_bar(position="dodge", stat = "identity",color="black") + 
  ggtitle("Min,Max,Average Humidity-2019") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Quarter(1 to 4)-2019")+
  ylab("min,max,avg humidity(%))")


################End of the code################################################
