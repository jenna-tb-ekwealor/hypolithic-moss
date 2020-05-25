library(reshape)
library(lattice)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(rstatix)
library(ggpubr)
library(patchwork)
library(dplyr)


setwd("/Users/jennaekwealor/Documents/3dmoss/hypolithic-moss/wrightwood/")
cols <- c("Hypolithic" = "#7FC97F", "Surface" = "#BEAED4")
temp ="#FDC086" 
RH = "#FFFF99"

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
# id.broken(ibutton.data.temp)
# id.broken(ibutton.data.RH)

## get the lengths for your own use:
# sapply(ibutton.data.temp,nrow)
# sapply(ibutton.data.RH,nrow)

## sometimes, fieldworkers record the data incorrectly -- for example, one 
## common mistake is to save data from the same ibutton twice with different 
## filenames.  However, each ibutton has a unique registration number.  check 
## for any number >1 in this table to identify this error. Additionally, if you
## recorded the registration numbers (written on the back of the ibuttons) you
## could use this to get them from the datafiles themselves

# table(get.registration.numbers("temp"))
# table(get.registration.numbers("RH"))

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
ibutton.df.temp$habitat <- ifelse(grepl("surface", ibutton.df.temp$ibutton), "Surface", ifelse(grepl("hypo", ibutton.df.temp$ibutton), "Hypolithic", ""))

ibutton.df.RH <- na.omit(ibuttons.to.data.frame(ibutton.data.RH))
head(ibutton.df.RH)
summary(ibutton.df.RH)
# remove row names
rownames(ibutton.df.RH) <- c()
# make new 'treatment' column
ibutton.df.RH$habitat <- ifelse(grepl("surface", ibutton.df.RH$ibutton), "Surface", ifelse(grepl("hypo", ibutton.df.RH$ibutton), "Hypolithic", ""))





# fix format
ibutton.df.temp$Date.Time <- as.POSIXct(ibutton.df.temp$Date.Time)
ibutton.df.RH$Date.Time <- as.POSIXct(ibutton.df.RH$Date.Time)

ibutton.df.temp$habitat <- as.factor(ibutton.df.temp$habitat)
ibutton.df.RH$habitat <- as.factor(ibutton.df.RH$habitat)

##### temp #####

##### probability density function for bins of temperature ####
ibutton.df.temp %>% 
  mutate(date = date(Date.Time)) %>% 
  group_by(habitat) %>% 
  ggplot() + 
  # geom_density(aes(x=Value, fill = habitat)) +
  geom_histogram(aes(x=Value, fill = habitat), binwidth = 2, color = "black") +
  facet_wrap(~habitat) +
  scale_fill_manual(name = "Microhabitat", values = cols) +
  theme_minimal() + 
  xlab("Temperature (°C)") +
  ylab("Density") 

# calculate differences 
ibutton.df.temp %>% 
  mutate(date = date(Date.Time)) %>% 
  dplyr::filter(habitat == "Surface") -> surface.temp

ibutton.df.temp %>% 
  mutate(date = date(Date.Time)) %>% 
  dplyr::filter(habitat == "Hypolithic") -> hypolithic.temp

# combine differences into a df with Date.Time and diff 
diff <- surface.temp$Value - hypolithic.temp$Value
Date.Time <- ibutton.df.temp$Date.Time[1:length(diff)]

tempdiff <- data.frame("diff" = diff,
                       "Date.Time" = Date.Time
)


# density function of temp differences

tempdiff %>% 
  mutate(date = date(Date.Time)) %>% 
  ggplot() + 
  # geom_density(aes(x=Value, fill = habitat)) +
  geom_histogram(aes(y=..density..,x=diff), binwidth = 2, color = "black", fill = temp) +
  # scale_fill_manual(name = "Microhabitat", values = cols) +
  theme_minimal() + 
  xlab("Difference in temperature (surface - hypolithic,°C)") +
  ylab("Density") -> p.tempdiff

p.tempdiff

# pull out pdf data
ggplot_build(p.tempdiff) 
pdf.tempdiff <- (ggplot_build(p.tempdiff))$data[[1]]

# add columns that estimate the number of minutes in each bin
# these data were collected every hour so each point in the pdf reperesents an hour
# however, if there are 2 points in a bin, it may be only 1 hour 
# so all counts/bin have to have 1 subtracted from it
# (all bins have at least 2 points)
pdf.tempdiff %>% mutate(hours = count - 1) -> pdf.tempdiff


#### plot tempdiff ####

# color lines based on day and night for temperature buffering hypothesis
tempdiff$Date <- date(tempdiff$Date.Time)
# pull out hour and add one because data were logged at the :59 minute of each our
tempdiff$Hour <- (hour(tempdiff$Date.Time))+1


tempdiff %>% mutate(daynight = ifelse(Hour >= 6 & Hour < 18, "day","night")) -> tempdiff
tempdiff %>% mutate(daynight_color = ifelse(daynight == "day", "\'blue\'", "\'black\'")) -> tempdiff

ggplot(data = tempdiff) +
  geom_line(size = 0.2, aes(x = Date.Time, y = diff, color = daynight, group = 1)) +
  scale_color_manual(values = c("skyblue", "midnightblue"),labels = c("Day", "Night"), name = "") +
  theme_light() + 
  xlab("") +
  ylab("Difference in temperature (surface - hypolithic,°C)") +
  theme_light()


############ pull out the high TEMP from each day, for each habitat ############
ibutton.df.temp %>% 
  mutate(date = date(Date.Time)) %>% 
  group_by(date, habitat) %>% 
  summarize(high = max(Value)) -> ibutton.df.temp.dailyhigh

head(ibutton.df.temp.dailyhigh)

ibutton.df.temp.dailyhigh %>% 
  group_by(habitat) %>% 
  summarize(meandailyhigh = mean(high), n = n(), sd.dailyhigh = sd(high)) -> ibutton.df.temp.dailyhigh.mean
head(ibutton.df.temp.dailyhigh.mean)

# # A tibble: 2 x 4
# habitat  meandailyhigh     n sd.dailyhigh
# <chr>            <dbl> <int>        <dbl>
#   1 Hypolith          19.1   164         11.4
# 2 Surface           21.7   164         12.5

ggplot(ibutton.df.temp.dailyhigh, aes(habitat, high)) +
  geom_boxplot()

# high temp stats 
pwc.ibutton.df.temp.dailyhigh <- ibutton.df.temp.dailyhigh %>% ungroup() %>% rstatix::t_test(high ~ habitat, paired = TRUE)

pwc.ibutton.df.temp.dailyhigh

# # A tibble: 1 x 8
# .y.   group1   group2     n1    n2 statistic    df        p
# * <chr> <chr>    <chr>   <int> <int>     <dbl> <dbl>    <dbl>
#   1 high  Hypolith Surface   164   164     -16.6   163 5.69e-37

anova.ibutton.df.temp.dailyhigh <-  ibutton.df.temp.dailyhigh %>% ungroup() %>% 
  anova_test(dv=high, wid=date, within=habitat)

get_anova_table(anova.ibutton.df.temp.dailyhigh)
# ANOVA Table (type III tests)
# 
# Effect DFn DFd       F        p p<.05   ges
# 1 habitat   1 163 276.885 5.69e-37     * 0.012

# Visualization: box plots with p-values
pwc.ibutton.df.temp.dailyhigh <- pwc.ibutton.df.temp.dailyhigh %>% add_xy_position(x = "habitat")




# bxp.ibutton.df.temp.dailyhigh <- ggboxplot(
#   ibutton.df.temp.dailyhigh, x = "habitat", y = "high",
#   color = "habitat", palette = "Accent",
#   add = "point"
# )
# bxp.ibutton.df.temp.dailyhigh
# 
# bxp.ibutton.df.temp.dailyhigh + 
#   stat_pvalue_manual(pwc.ibutton.df.temp.dailyhigh, tip.length = 0, hide.ns = TRUE) +
#   labs(
#     subtitle = get_test_label(anova.ibutton.df.temp.dailyhigh, detailed = FALSE),
#     caption = get_pwc_label(pwc.ibutton.df.temp.dailyhigh)
#   )




########### pull out the low TEMP from each day, for each habitat #############
ibutton.df.temp %>% 
  mutate(date = date(Date.Time)) %>% 
  group_by(date, habitat) %>% 
  summarize(low = min(Value), n = n()) -> ibutton.df.temp.dailylow
head(ibutton.df.temp.dailylow)

ibutton.df.temp.dailylow %>% 
  group_by(habitat) %>% 
  summarize(meandailylow = mean(low), n = n(), sd.dailylow = sd(low)) -> ibutton.df.temp.dailylow.mean
head(ibutton.df.temp.dailylow.mean)

# # A tibble: 2 x 4
# habitat  meandailylow     n sd.dailylow
# <chr>           <dbl> <int>       <dbl>
#   1 Hypolith         3.79   164        5.28
# 2 Surface          1.90   164        5.17

ggplot(ibutton.df.temp.dailylow, aes(habitat, low)) +
  geom_boxplot()

# # A tibble: 2 x 4
# habitat  meandailylow     n sd.dailylow
# <chr>           <dbl> <int>       <dbl>
#   1 Hypolith         7.98   154        5.35
# 2 Surface          5.65   154        5.03

####### low temp stats #######
pwc.ibutton.df.temp.dailylow <- ibutton.df.temp.dailylow %>% ungroup() %>% rstatix::t_test(low ~ habitat, paired = TRUE)

pwc.ibutton.df.temp.dailylow

# # A tibble: 1 x 8
# .y.   group1   group2     n1    n2 statistic    df        p
# * <chr> <chr>    <chr>   <int> <int>     <dbl> <dbl>    <dbl>
#   1 low   Hypolith Surface   164   164      23.6   163 1.69e-54

anova.ibutton.df.temp.dailylow <-  ibutton.df.temp.dailylow %>% ungroup() %>% 
  anova_test(dv=low, wid=date, within=habitat)

get_anova_table(anova.ibutton.df.temp.dailylow)

# Visualization: box plots with p-values
pwc.ibutton.df.temp.dailylow <- pwc.ibutton.df.temp.dailylow %>% add_xy_position(x = "habitat")




# bxp.ibutton.df.temp.dailylow + 
#   stat_pvalue_manual(pwc.ibutton.df.temp.dailylow, tip.length = 0, hide.ns = TRUE) +
#   labs(
#     subtitle = get_test_label(anova.ibutton.df.temp.dailylow, detailed = FALSE),
#     caption = get_pwc_label(pwc.ibutton.df.temp.dailylow)
#   ) 




####### RH ########

##### probability density function for bins of RH ####
ibutton.df.RH %>% 
  mutate(date = date(Date.Time)) %>% 
  group_by(habitat) %>% 
  ggplot() + 
  # geom_density(aes(x=Value, fill = habitat)) +
  geom_histogram(aes(x=Value, fill = habitat), binwidth = 10, color = "black") +
  facet_wrap(~habitat) +
  scale_fill_manual(name = "Microhabitat", values = cols) +
  theme_minimal() + 
  xlab("Relative Humidity (%)") +
  ylab("Density") 

# plot differences instead
ibutton.df.RH %>% 
  mutate(date = date(Date.Time)) %>% 
  dplyr::filter(habitat == "Surface") -> surface.RH

ibutton.df.RH %>% 
  mutate(date = date(Date.Time)) %>% 
  dplyr::filter(habitat == "Hypolithic") -> hypolithic.RH

# combine differences into a df with Date.Time and diff 
diff <- hypolithic.RH$Value - surface.RH$Value
Date.Time <- ibutton.df.RH$Date.Time[1:length(diff)]

RHdiff <- data.frame("diff" = diff,
                       "Date.Time" = Date.Time
)


# density function of RH differences

RHdiff %>% 
  mutate(date = date(Date.Time)) %>% 
  ggplot() + 
  # geom_density(aes(x=Value, fill = habitat)) +
  geom_histogram(aes(y=..count..,x=diff), binwidth = 10, color = "black", fill = RH) +
  # scale_fill_manual(name = "Microhabitat", values = cols) +
  theme_minimal() + 
  xlab("Difference in relative humidity (hypolithic - surface, %)") +
  ylab("Count") -> p.RHdiff

p.RHdiff

pdf.RHdiff <- (ggplot_build(p.RHdiff))$data[[1]]


# density function of RH differences with fewer bins to quantify hours of more favorable conditions

RHdiff %>% 
  mutate(date = date(Date.Time)) %>% 
  ggplot() + 
  # geom_density(aes(x=Value, fill = habitat)) +
  geom_histogram(aes(y=..count..,x=diff), binwidth = 10, color = "black", fill = RH) +
  # scale_fill_manual(name = "Microhabitat", values = cols) +
  theme_minimal() + 
  xlab("Difference in relative humidity (hypolithic - surface, %)") +
  ylab("Count") -> p.RHdiff.fewbins

p.RHdiff.fewbins

pdf.RHdiff.fewbins <- (ggplot_build(p.RHdiff.fewbins))$data[[1]]

# filter out and sum the points that RH is more than 5% higher in hypos
increasedRH <- pdf.RHdiff.fewbins %>% dplyr::filter(x > 0) 
increasedRH <- sum(increasedRH$count)

# filter out and sum the points that RH in hypos is within 5% of surface
nochangeRH <- pdf.RHdiff.fewbins %>% dplyr::filter(x == 0) 
nochangeRH <- sum(nochangeRH$count)

# filter out and sum the points that RH is more than 5% less in hypos
decreasedRH <- pdf.RHdiff.fewbins %>% dplyr::filter(x < 0) 
decreasedRH <- sum(decreasedRH$count)

increased_pct <- (increasedRH / (increasedRH + nochangeRH + decreasedRH))*100
# [1] 51.39591
nochange_pct <- (nochangeRH / (increasedRH + nochangeRH + decreasedRH))*100
# [1] 30.23132
decreased_pct <- (decreasedRH / (increasedRH + nochangeRH + decreasedRH))*100
# [1] 18.37277


# density function of RH differences with even fewer bins to quantify hours of more favorable conditions

RHdiff %>% 
  mutate(date = date(Date.Time)) %>% 
  ggplot() + 
  # geom_density(aes(x=Value, fill = habitat)) +
  geom_histogram(aes(y=..count..,x=diff), binwidth = 20, color = "black", fill = RH) +
  # scale_fill_manual(name = "Microhabitat", values = cols) +
  theme_minimal() + 
  xlab("Difference in relative humidity (hypolithic - surface, %)") +
  ylab("Count") -> p.RHdiff.fewerbins

p.RHdiff.fewerbins

pdf.RHdiff.fewerbins <- (ggplot_build(p.RHdiff.fewerbins))$data[[1]]

# filter out and sum the points that RH is more than 10% higher in hypos
increasedRH <- pdf.RHdiff.fewerbins %>% dplyr::filter(x > 0) 
increasedRH <- sum(increasedRH$count)

# filter out and sum the points that RH in hypos is within 10% of surface
nochangeRH <- pdf.RHdiff.fewerbins %>% dplyr::filter(x == 0) 
nochangeRH <- sum(nochangeRH$count)

# filter out and sum the points that RH is more than 10% less in hypos
decreasedRH <- pdf.RHdiff.fewerbins %>% dplyr::filter(x < 0) 
decreasedRH <- sum(decreasedRH$count)

increased_pct <- (increasedRH / (increasedRH + nochangeRH + decreasedRH))*100
# [1] 43.8979
nochange_pct <- (nochangeRH / (increasedRH + nochangeRH + decreasedRH))*100
# [1] 44.85509
decreased_pct <- (decreasedRH / (increasedRH + nochangeRH + decreasedRH))*100
# [1] 11.24701

# add columns that estimate the number of minutes in each bin
# these data were collected every hour so each point in the pdf reperesents an hour
# however, if there are 2 points in a bin, it may be only 1 hour 
# so all counts/bin have to have 1 subtracted from it
# (all bins have at least 2 points)
pdf.RHdiff %>% mutate(hours = count - 1) -> pdf.RHdiff



#### plot RHdiff ####

# color lines based on day and night for RH buffering hypothesis
RHdiff$Date <- date(RHdiff$Date.Time)
# pull out hour and add one because data were logged at the :59 minute of each our
RHdiff$Hour <- (hour(RHdiff$Date.Time))+1

RHdiff %>% mutate(daynight = ifelse(Hour >= 6 & Hour < 18, "day","night")) -> RHdiff
RHdiff %>% mutate(daynight_color = ifelse(daynight == "day", "\'blue\'", "\'black\'")) -> RHdiff

ggplot(data = RHdiff) +
  geom_line(size = 0.2, aes(x = Date.Time, y = diff, color = daynight, group = 1)) +
  scale_color_manual(values = c("skyblue", "midnightblue"),labels = c("Day", "Night"), name = "") +
  theme_light() + 
  xlab("") +
  ylab("Difference in relative humidity (hypolithic - surface, %)") 

#### pull out the high RH from each day, for each habitat ####
ibutton.df.RH %>% 
  mutate(date = date(Date.Time)) %>% 
  group_by(date, habitat) %>% 
  summarize(high = max(Value)) -> ibutton.df.RH.dailyhigh

head(ibutton.df.RH.dailyhigh)

ibutton.df.RH.dailyhigh %>% 
  group_by(habitat) %>% 
  summarize(meandailyhigh = mean(high), n = n(), sd.dailyhigh = sd(high)) -> ibutton.df.RH.dailyhigh.mean
head(ibutton.df.RH.dailyhigh.mean)

# # A tibble: 2 x 4
# habitat  meandailyhigh     n sd.dailyhigh
# <chr>            <dbl> <int>        <dbl>
#   1 Hypolith          70.4   164         34.6
# 2 Surface           70.8   164         27.4

ggplot(ibutton.df.RH.dailyhigh, aes(habitat, high)) +
  geom_boxplot()

# high RH stats 
pwc.ibutton.df.RH.dailyhigh <- ibutton.df.RH.dailyhigh %>% ungroup() %>% rstatix::t_test(high ~ habitat, paired = TRUE)

pwc.ibutton.df.RH.dailyhigh
# # A tibble: 1 x 12
# .y.   group1   group2     n1    n2 statistic    df     p y.position groups     xmin  xmax
# <chr> <chr>    <chr>   <int> <int>     <dbl> <dbl> <dbl>      <dbl> <list>    <int> <int>
#   1 high  Hypolith Surface   164   164    -0.337   163 0.736        100 <chr [2]>     1     2

anova.ibutton.df.RH.dailyhigh <-  ibutton.df.RH.dailyhigh %>% ungroup() %>% 
  anova_test(dv=high, wid=date, within=habitat)

get_anova_table(anova.ibutton.df.RH.dailyhigh)

# Visualization: box plots with p-values
pwc.ibutton.df.RH.dailyhigh <- pwc.ibutton.df.RH.dailyhigh %>% add_xy_position(x = "habitat")








# bxp.ibutton.df.RH.dailyhigh <- ggboxplot(
#   ibutton.df.RH.dailyhigh, x = "habitat", y = "high",
#   color = "habitat", palette = "Accent",
#   add = "point"
# )
# bxp.ibutton.df.RH.dailyhigh
# 
# bxp.ibutton.df.RH.dailyhigh + 
#   stat_pvalue_manual(pwc.ibutton.df.RH.dailyhigh, tip.length = 0, hide.ns = TRUE) +
#   labs(
#     subtitle = get_test_label(anova.ibutton.df.RH.dailyhigh, detailed = FALSE),
#     caption = get_pwc_label(pwc.ibutton.df.RH.dailyhigh)
#   )






###### pull out the low RH from each day, for each habitat ######
ibutton.df.RH %>% 
  mutate(date = date(Date.Time)) %>% 
  group_by(date, habitat) %>% 
  summarize(low = min(Value)) -> ibutton.df.RH.dailylow

head(ibutton.df.RH.dailylow)

ibutton.df.RH.dailylow %>% 
  group_by(habitat) %>% 
  summarize(meandailylow = mean(low), n = n(), sd.dailylow = sd(low)) -> ibutton.df.RH.dailylow.mean
head(ibutton.df.RH.dailylow.mean)

# # A tibble: 2 x 4
# habitat  meandailylow     n sd.dailylow
# <chr>           <dbl> <int>       <dbl>
#   1 Hypolith         62.5   164        39.2
# 2 Surface          32.5   164        30.6

ggplot(ibutton.df.RH.dailylow, aes(habitat, low)) +
  geom_boxplot()

# low RH stats 
pwc.ibutton.df.RH.dailylow <- ibutton.df.RH.dailylow %>% ungroup() %>% rstatix::t_test(low ~ habitat, paired = TRUE)

pwc.ibutton.df.RH.dailylow

# # A tibble: 1 x 12
# .y.   group1   group2     n1    n2 statistic    df        p y.position groups     xmin  xmax
# <chr> <chr>    <chr>   <int> <int>     <dbl> <dbl>    <dbl>      <dbl> <list>    <int> <int>
#   1 low   Hypolith Surface   164   164      13.1   163 2.84e-27        100 <chr [2]>     1     2

anova.ibutton.df.RH.dailylow <-  ibutton.df.RH.dailylow %>% ungroup() %>% 
  anova_test(dv=low, wid=date, within=habitat)

get_anova_table(anova.ibutton.df.RH.dailylow)

# Visualization: box plots with p-values
pwc.ibutton.df.RH.dailylow <- pwc.ibutton.df.RH.dailylow %>% add_xy_position(x = "habitat")





# bxp.ibutton.df.RH.dailylow <- ggboxplot(
#   ibutton.df.RH.dailylow, x = "habitat", y = "low",
#   color = "habitat", palette = "Accent",
#   add = "point"
# )
# bxp.ibutton.df.RH.dailylow
# 
# bxp.ibutton.df.RH.dailylow + 
#   stat_pvalue_manual(pwc.ibutton.df.RH.dailylow, tip.length = 0, hide.ns = TRUE) +
#   labs(
#     subtitle = get_test_label(anova.ibutton.df.RH.dailylow, detailed = FALSE),
#     caption = get_pwc_label(pwc.ibutton.df.RH.dailylow)
#   )

###### FIGURES ######


sigfontsize <- 6
meanfontsize <- 4.25
xtitlefontsize <- 18
ytitlefontsize <- 18
xaxisfontsize <- 14
yaxisfontsize <- 14



bxp.ibutton.df.temp.dailyhigh <- ggboxplot(
  ibutton.df.temp.dailyhigh, x = "habitat", y = "high",
  fill = "habitat", palette = "Accent",
  #add = "boxplot",add.params = list(fill = "white"),
  ylab = expression("Mean daily high temperature " ( degree~C)),  
  xlab = "Microhabitat", 
  font.x = xtitlefontsize,
  font.y = ytitlefontsize
)  +
  font("x.text", size = xaxisfontsize) +
  font("y.text", size = yaxisfontsize) +
  stat_compare_means(aes(label = ..p.signif..), 
                     paired = TRUE, method = "t.test", 
                     label.y = 45.5, 
                     label.x.npc = 0.45, 
                     size = sigfontsize
  ) +
  rremove("legend") +
  stat_summary(fun.data = function(x) data.frame(y=50, label = paste("Mean =", format(round(mean(x), digits = 1), nsmall = 1))), geom="text", size = meanfontsize) +
  geom_jitter(position=position_jitter(width=.07, height=0), alpha = 0.3, size = 2)


#bxp.ibutton.df.temp.dailyhigh



bxp.ibutton.df.temp.dailylow <- ggboxplot(
  ibutton.df.temp.dailylow, x = "habitat", y = "low",
  fill = "habitat", palette = "Accent",
  #add = "boxplot",add.params = list(fill = "white"),
  ylab = expression("Mean daily low temperature " ( degree~C)),  
  xlab = "Microhabitat", 
  font.x = xtitlefontsize,
  font.y = ytitlefontsize
)  +
  font("x.text", size = xaxisfontsize) +
  font("y.text", size = yaxisfontsize) +
  stat_compare_means(aes(label = ..p.signif..), 
                     paired = TRUE, method = "t.test", 
                     label.y = 45, 
                     label.x.npc = 0.45, 
                     size = sigfontsize
  ) +
  rremove("legend") +
  stat_summary(fun.data = function(x) data.frame(y=50, label = paste("Mean =", format(round(mean(x), digits = 1), nsmall = 1))), geom="text", size = meanfontsize) +
  geom_jitter(position=position_jitter(width=.07, height=0), alpha = 0.3, size = 2)


#bxp.ibutton.df.temp.dailylow


# # figure 2
# figure2 <- (bxp.ibutton.df.temp.dailylow + bxp.ibutton.df.temp.dailyhigh)
# 
# 
# pdf("paper/Figure 2.pdf") 
# 
# figure2
# 
# dev.off()



bxp.ibutton.df.RH.dailyhigh <- ggboxplot(
  ibutton.df.RH.dailyhigh, x = "habitat", y = "high",
  fill = "habitat", palette = "Accent",
  #add = "boxplot",add.params = list(fill = "white"),
  ylab = expression("Mean daily high relative humidity (%)"),
  xlab = "Microhabitat", 
  font.x = xtitlefontsize,
  font.y = ytitlefontsize
)  +
  font("x.text", size = xaxisfontsize) +
  font("y.text", size = yaxisfontsize) +
  stat_compare_means(aes(label = ..p.signif..), 
                     paired = TRUE, method = "t.test", 
                     label.y = 102.5, 
                     label.x.npc = 0.45, 
                     size = sigfontsize
  ) +
  rremove("legend") +
  stat_summary(fun.data = function(x) data.frame(y=107, label = paste("Mean =", format(round(mean(x), digits = 1), nsmall = 1))), geom="text", size = meanfontsize)+
  geom_jitter(position=position_jitter(width=.07, height=0), alpha = 0.3, size = 2) + 
  scale_y_continuous(breaks=seq(0,100,20))
                                                                                            


#bxp.ibutton.df.RH.dailyhigh





bxp.ibutton.df.RH.dailylow <- ggboxplot(
  ibutton.df.RH.dailylow, x = "habitat", y = "low",
  fill = "habitat", palette = "Accent",
  #add = "boxplot",add.params = list(fill = "white"),
  ylab = expression("Mean daily low relative humidity (%)"),
  xlab = "Microhabitat", 
  font.x = xtitlefontsize,
  font.y = ytitlefontsize
)  +
  font("x.text", size = xaxisfontsize) +
  font("y.text", size = yaxisfontsize) +
  stat_compare_means(aes(label = ..p.signif..), 
                     paired = TRUE, method = "t.test", 
                     label.y = 101, 
                     label.x.npc = 0.45, 
                     size = sigfontsize
  ) +
  rremove("legend") +
  stat_summary(fun.data = function(x) data.frame(y=107, label = paste("Mean =", format(round(mean(x), digits = 1), nsmall = 1))), geom="text", size = meanfontsize)+
  geom_jitter(position=position_jitter(width=.07, height=0), alpha = 0.3, size = 2)  + 
  scale_y_continuous(breaks=seq(0,100,20))


#bxp.ibutton.df.RH.dailylow

# # figure 3
# figure3 <- (bxp.ibutton.df.RH.dailylow + bxp.ibutton.df.RH.dailyhigh)
# 
# 
# 
# pdf("paper/Figure 3.pdf") 
# 
# figure3
# 
# dev.off()

