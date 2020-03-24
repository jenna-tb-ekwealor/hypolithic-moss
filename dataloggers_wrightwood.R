library(reshape)
library(lattice)
library(ggplot2)
library(tidyverse)
library(lubridate)

setwd("/Users/jennaekwealor/Documents/3dmoss/hypoliths/wrightwood/")

source('../ibutton.functions.R')

# load ibutton data
ibutton.data.temp <- read.ibutton.folder("temp")
ibutton.data.RH <- read.ibutton.folder("RH")

## which ones failed? you could just check the number of rows in each, and pick 
## the ones which are suspiciously short, indicating that the ibutton stopped 
## recording temperature.  This function automates this process to make it a bit
## more objective: it points out ones which recorded less than the median number
## of datapoints for the experiment.  It assumes that all the ibuttons were
## supposed to run for equal amounts of time
id.broken(ibutton.data.temp)
id.broken(ibutton.data.RH)

## get the lengths for your own use:
sapply(ibutton.data.temp,nrow)
sapply(ibutton.data.RH,nrow)

## sometimes, fieldworkers record the data incorrectly -- for example, one 
## common mistake is to save data from the same ibutton twice with different 
## filenames.  However, each ibutton has a unique registration number.  check 
## for any number >1 in this table to identify this error. Additionally, if you
## recorded the registration numbers (written on the back of the ibuttons) you
## could use this to get them from the datafiles themselves

table(get.registration.numbers("temp"))
table(get.registration.numbers("RH"))

## correct the dates and make df 
## Now that the data is checked, this function takes the list of ibuttons and
## combines them together.  It also reformats the "Date.Time" variable, into a
## format that R recognizes as a date and time.
#ibutton.df <- as.data.frame(ibutton.data)
ibutton.df.temp <- na.omit(ibuttons.to.data.frame(ibutton.data.temp))
head(ibutton.df.temp)
summary(ibutton.df.temp)
# remove row names
rownames(ibutton.df.temp) <- c()
# make new 'habitat' column
ibutton.df.temp$habitat <- ifelse(grepl("surface", ibutton.df.temp$ibutton), "Surface", ifelse(grepl("hypo", ibutton.df.temp$ibutton), "Hypolith", ""))

ibutton.df.RH <- na.omit(ibuttons.to.data.frame(ibutton.data.RH))
head(ibutton.df.RH)
summary(ibutton.df.RH)
# remove row names
rownames(ibutton.df.RH) <- c()
# make new 'treatment' column
ibutton.df.RH$habitat <- ifelse(grepl("surface", ibutton.df.RH$ibutton), "Surface", ifelse(grepl("hypo", ibutton.df.RH$ibutton), "Hypolith", ""))





####### want daily average high and low... need to be able to group by day


# fix format
ibutton.df.temp$Date.Time <- as.POSIXct(ibutton.df.temp$Date.Time)
ibutton.df.RH$Date.Time <- as.POSIXct(ibutton.df.RH$Date.Time)


# temp

# pull out the high TEMP from each day, for each habitat 
ibutton.df.temp %>% 
  mutate(date = date(Date.Time)) %>% 
  group_by(date, habitat) %>% 
  summarize(high = max(Value)) -> ibutton.df.temp.dailyhigh

head(ibutton.df.temp.dailyhigh)

ibutton.df.temp.dailyhigh %>% 
  group_by(habitat) %>% 
  summarize(meandailyhigh = mean(high), n = n(), sd.dailyhigh = sd(high)) -> ibutton.df.temp.dailyhigh.mean
head(ibutton.df.temp.dailyhigh.mean)

ggplot(ibutton.df.temp.dailyhigh, aes(habitat, high)) +
  geom_boxplot()

t.test(high ~ habitat, data = ibutton.df.temp.dailyhigh, paired = TRUE)

# Paired t-test
# 
# data:  high by habitat
# t = -16.64, df = 163, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -2.885311 -2.273164
# sample estimates:
#   mean of the differences 
# -2.579238 





# pull out the low TEMP from each day, for each habitat 
ibutton.df.temp %>% 
  mutate(date = date(Date.Time)) %>% 
  group_by(date, habitat) %>% 
  summarize(low = min(Value), n = n()) -> ibutton.df.temp.dailylow
head(ibutton.df.temp.dailylow)

ibutton.df.temp.dailylow %>% 
  group_by(habitat) %>% 
  summarize(meandailylow = mean(low), n = n(), sd.dailylow = sd(low)) -> ibutton.df.temp.dailylow.mean
head(ibutton.df.temp.dailylow.mean)

ggplot(ibutton.df.temp.dailylow, aes(habitat, low)) +
  geom_boxplot()

t.test(low ~ habitat, data = ibutton.df.temp.dailylow, paired = TRUE)

# Paired t-test
# 
# data:  low by habitat
# t = 23.619, df = 163, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   1.729706 2.045307
# sample estimates:
#   mean of the differences 
# 1.887506 




# RH

# pull out the high RH from each day, for each habitat 
ibutton.df.RH %>% 
  mutate(date = date(Date.Time)) %>% 
  group_by(date, habitat) %>% 
  summarize(high = max(Value), n = n(), sd.Value = sd(Value)) -> ibutton.df.RH.dailyhigh

head(ibutton.df.RH.dailyhigh)

ggplot(ibutton.df.RH.dailyhigh, aes(habitat, high)) +
  geom_boxplot()

ibutton.df.RH.dailyhigh %>% 
  group_by(habitat) %>% 
  summarize(meandailyhigh = mean(high), n = n(), sd.dailyhigh = sd(high)) -> ibutton.df.RH.dailyhigh.mean
head(ibutton.df.RH.dailyhigh.mean)

t.test(high ~ habitat, data = ibutton.df.RH.dailyhigh, paired = TRUE)

# Paired t-test
# 
# data:  high by habitat
# t = -0.3373, df = 163, p-value = 0.7363
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -2.604137  1.844271
# sample estimates:
#   mean of the differences 
# -0.3799329 



# pull out the low RH from each day, for each habitat 
ibutton.df.RH %>% 
  mutate(date = date(Date.Time)) %>% 
  group_by(date, habitat) %>% 
  summarize(low = min(Value), n = n(), sd.Value = sd(Value)) -> ibutton.df.RH.dailylow
head(ibutton.df.RH.dailylow)

ibutton.df.RH.dailylow %>% 
  group_by(habitat) %>% 
  summarize(meandailylow = mean(low), n = n(), sd.dailylow = sd(low)) -> ibutton.df.RH.dailylow.mean
head(ibutton.df.RH.dailylow.mean)

t.test(low ~ habitat, data = ibutton.df.RH.dailylow, paired = TRUE)

# Paired t-test
# 
# data:  low by habitat
# t = 21.331, df = 30, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   8.027844 9.727769
# sample estimates:
#   mean of the differences 
# 8.877806 


ggplot(ibutton.df.RH.dailylow, aes(habitat, low)) +
  geom_boxplot()

