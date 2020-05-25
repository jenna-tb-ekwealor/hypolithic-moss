
library(ggplot2)
library(tidyverse)
library(rstatix)
library(ggpubr)
library(infer)
setwd("/Users/jennaekwealor/Documents/3dmoss/hypolithic-moss/wrightwood/")


sigfontsize <- 6
meanfontsize <- 4.25
xtitlefontsize <- 18
ytitlefontsize <- 18
xaxisfontsize <- 14
yaxisfontsize <- 14


####### QUARTZ THICKNESS #######

quartz.data <- as.data.frame(read.csv("quartz-thickness.csv"))
mean(quartz.data$thickness)
# 25.63889
sd(quartz.data$thickness)
# 14.88762

####### COMMUNITY COMPOSITION  #######

community.data <- as.data.frame(read.csv("community.csv"))  %>% 
  select (Habitat, Species) %>% na.omit()
community.data$Habitat <- factor(community.data$Habitat, levels = c("Hypolithic", "Surface"))


community.data.table <- table(community.data$Habitat, community.data$Species)
community.data.table %>% rstatix::fisher_test()

# # A tibble: 1 x 3
# n       p p.signif
# * <int>   <dbl> <chr>   
#   1    53 0.00257 **   

community.data %>% count(Species, Habitat) 


####### SHOOT LENGTH #######
length.data <- as.data.frame(read.csv("stem-length.csv"))
length.data <- length.data %>% tidyr::gather(Habitat, Length, Surface:Hypolithic) %>% select (Habitat, Length) %>% na.omit()
length.data$Habitat <- factor(length.data$Habitat, levels = c("Hypolithic", "Surface"))


# shapiro_test(length.data$length) # not normal
# hist(length.data$length)

pwc.length <- length.data %>% 
  wilcox_test(Length ~ Habitat)

pwc.length
# # A tibble: 1 x 7
# .y.    group1     group2     n1    n2 statistic             p
# * <chr>  <chr>      <chr>   <int> <int>     <dbl>         <dbl>
#   1 length hypolithic surface    50   299     11390 0.00000000306



bxp.length.data <- ggboxplot(
  length.data, x = "Habitat", y = "Length",
  fill = "Habitat", palette = "Accent",
  #add = "boxplot",add.params = list(fill = "white"),
  ylab = "Shoot length (mm)",
  xlab = "Microhabitat", 
  font.x = xtitlefontsize,
  font.y = ytitlefontsize
)  +
  font("x.text", size = xaxisfontsize) +
  font("y.text", size = yaxisfontsize) +
  stat_compare_means(aes(label = ..p.signif..), 
                     paired = FALSE, method = "wilcox.test", 
                     label.y = 6, 
                     label.x.npc = 0.45, 
                     size = sigfontsize
  ) +
  rremove("legend") +
  stat_summary(fun.data = function(x) data.frame(y=6.5, label = paste("Mean =", format(round(mean(x), digits = 1), nsmall = 1))), geom="text", size = meanfontsize)+
  geom_jitter(position=position_jitter(width=.07, height=0), alpha = 0.3, size = 2) 


pdf("paper/Figure 4.pdf") 

figure4 <- bxp.length.data
figure4

dev.off()





####### LEAF DENSITY  #######
density.data <- as.data.frame(read.csv("leaf-density.csv"))
density.data <- na.omit(density.data)

#shapiro_test(density.data$leaf.density) # not normal
#hist(density.data$leaf.density)

pwc.density <- density.data %>% 
  wilcox_test(leaf.density ~ habitat)

pwc.density
# 
# # A tibble: 1 x 7
# .y.          group1     group2     n1    n2 statistic      p
# * <chr>        <chr>      <chr>   <int> <int>     <dbl>  <dbl>
#   1 leaf.density hypolithic surface    10    23        52 0.0125



bxp.density.data <- ggboxplot(
  density.data, x = "habitat", y = "leaf.density",
  fill = "habitat", palette = "Accent",
  #add = "boxplot",add.params = list(fill = "white"),
  ylab = "Leaf density (leaves/mm)",
  xlab = "Microhabitat", 
  font.x = xtitlefontsize,
  font.y = ytitlefontsize
)  +
  font("x.text", size = xaxisfontsize) +
  font("y.text", size = yaxisfontsize) +
  stat_compare_means(aes(label = ..p.signif..), 
                     paired = FALSE, method = "wilcox.test", 
                     label.y = 60, 
                     label.x.npc = 0.45, 
                     size = sigfontsize
  ) +
  rremove("legend") +
  stat_summary(fun.data = function(x) data.frame(y=65, label = paste("Mean =", format(round(mean(x), digits = 1), nsmall = 1))), geom="text", size = meanfontsize)+
  geom_jitter(position=position_jitter(width=.07, height=0), alpha = 0.3, size = 2) 




pdf("paper/Figure 5.pdf") 

figure5 <- bxp.density.data
figure5

dev.off()


