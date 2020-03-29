
library(ggplot2)
library(tidyverse)
library(rstatix)
library(ggpubr)
library(gplots)
library(dplyr)
library(infer)
library(cowplot)


####### LEAF DENSITY  #######
setwd("/Users/jennaekwealor/Documents/3dmoss/hypoliths/wrightwood/")
density.data <- as.data.frame(read.csv("leaf-density.csv"))
density.data <- na.omit(density.data)
 
density.data %>% 
shapiro_test(leaf.density)

# # A tibble: 1 x 3
# variable     statistic       p
# <chr>            <dbl>   <dbl>
#   1 leaf.density     0.886 0.00230

# p < 0.05, not normal, must use wilcox test

pwc.density <- density.data %>% 
  wilcox_test(leaf.density ~ habitat)

pwc.density

# # A tibble: 1 x 7
# .y.          group1     group2     n1    n2 statistic      p
# * <chr>        <chr>      <chr>   <int> <int>     <dbl>  <dbl>
#   1 leaf.density hypolithic surface    10    23        52 0.0125

pwc.density <- pwc.density %>% add_xy_position(x = "habitat")

density.data %>% 
    group_by(habitat) %>%
    summarize(mean = mean(leaf.density), sd = sd(leaf.density), n = n())

# # A tibble: 2 x 4
# habitat     mean    sd     n
# <fct>      <dbl> <dbl> <int>
#   1 hypolithic  16.5  10.4    10
# 2 surface     28.7  15.8    23



bxp.density.data <- ggboxplot(density.data, x = "habitat", y = "leaf.density",
                              color = "habitat", palette = "Accent",
                              add = "point",
                              )
bxp.density.data + 
  stat_pvalue_manual(pwc.density, tip.length = 0, hide.ns = FALSE, label = "p") +
  labs(caption = get_pwc_label(pwc.density)) +
  theme(legend.position = "none") 


####### STEM LENGTHS  #######
length.data <- as.data.frame(read.csv("stem-length.csv"))

length.data <- length.data %>% 
  gather(., habitat, length) %>% 
  na.omit()

length.data %>% 
  shapiro_test(length) 

# # A tibble: 1 x 3
# variable statistic        p
# <chr>        <dbl>    <dbl>
#   1 length       0.805 3.57e-20


  hist(length.data$length)


pwc.length <- length.data %>% 
  wilcox_test(length ~ habitat)

pwc.length

# # A tibble: 1 x 7
# .y.    group1     group2     n1    n2 statistic             p
# * <chr>  <chr>      <chr>   <int> <int>     <dbl>         <dbl>
#   1 length hypolithic surface    50   299     11390 0.00000000306

pwc.length <- pwc.length %>% add_xy_position(x = "habitat")

bxp.length.data <- ggboxplot(length.data, x = "habitat", y = "length",
                              color = "habitat", palette = "Accent",
                              add = "point",
)
bxp.length.data + 
  stat_pvalue_manual(pwc.length, tip.length = 0, hide.ns = FALSE, label = "p") +
  labs(caption = get_pwc_label(pwc.length)) +
  theme(legend.position = "none") 




####### COMMUNITY COMPOSITION  #######

community.data <- as.data.frame(read.csv("community.csv"))

community.data %>% 
  infer::chisq_test(species ~ habitat) 

# 
# # # A tibble: 1 x 3
# # statistic chisq_df p_value
# # <dbl>    <int>   <dbl>
#   1      11.0        2 0.00403

chisq.test(community.data$habitat, community.data$species) 
#
# Pearson's Chi-squared test
# 
# data:  community.data$habitat and community.data$species
# X-squared = 11.026, df = 2, p-value = 0.004033
observed_stat <- chisq.test(community.data$habitat, community.data$species)$stat

community.data %>% select(habitat, species) %>% 
  mutate(permutation_1 = sample(species),
         permutation_2 = sample(species))

community.data %>% 
  specify(species ~ habitat)  %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 10000, type = "permute") %>% 
  calculate(stat = "Chisq") %>% 
  visualize() +
  geom_vline(aes(xintercept = observed_stat), color = "red") # add a vertical line for gss data

community.data %>% 
  specify(species ~ habitat)  %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 10000, type = "permute") %>% 
  calculate(stat = "Chisq") %>% 
  summarise(p_val = mean(stat > observed_stat)) 

# # A tibble: 1 x 1
# p_val
# <dbl>
#   1 0.0013

community.data %>% select(habitat, species) %>% 
  ggplot() +
  geom_bar(aes(x = habitat, fill = species), position = "fill")

# community.data %>% 
#   select(species, habitat) %>%
#   group_by(species, habitat) %>%
#   summarise(n = n()) %>%
#   mutate(prop = n / sum(n))

####### QUARTZ THICKNESS #######

quartz.data <- as.data.frame(read.csv("quartz-thickness.csv"))
mean(quartz.data$thickness)
# 25.63889
sd(quartz.data$thickness)
# 14.88762
