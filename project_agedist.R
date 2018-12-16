# finalproject_agedistribution
library(cowplot)
library(sampling)
library(stats)
library(tidyverse)  
library(ggplot2)
#install.packages("ggmosaic")
setwd("/Users/jenniferwartick/Documents/Boston University/cs544 Found Analytics/Project/clean-data")  # Change this to your path
s2017 <- read.csv(file = "2017-fCC-New-Coders-Survey-Data.csv",
                  header = TRUE,
                  na.strings = NA,
                  stringsAsFactors = FALSE) %>% tibble::as.tibble()


# only select columns of interest

s2017 <- s2017 %>%
  filter(CountryLive == "United States of America", 
         IsSoftwareDev == 0, !is.na(Age) )%>%
  select(Age)


par(mfrow=c(1,2))
age.pop<-table(s2017$Age)

# barplot of distribution
barplot(age.pop, main = "Age Distribution", 
        ylab="# of respondents", xlab="Age (years)",
        ylim = c(0,300))

# density histogram
hist(s2017$Age, prob = TRUE, main = "Density of Ages",
     xlab="Age (years)")

# mean and standard deviation:
age.mean <- mean(s2017$Age)
age.sd <- sd(s2017$Age)

getXbar <- function(data, samples, sample.size) {
  xbar <- numeric(samples)
  for (i in 1: samples){
    s <- srswor(sample.size, length(data))
    xbar[i] <- mean(data[s != 0])
  }
  return(xbar)
}

# Draw 1000 samples of size 10
xbar1 <- getXbar(s2017$Age, 1000, 10)
hist(xbar1, prob=TRUE, ylim = c(0, 0.3), xlim = c(15, 40))
xbar1.mean <- mean(xbar1)
xbar1.sd <- sd(xbar1)

# 1000 samples of size 30
xbar2 <- getXbar(s2017$Age, 1000, 30)
hist(xbar2, prob=TRUE, ylim = c(0,0.3), xlim = c(15, 40))
xbar2.mean <- mean(xbar2)
xbar2.sd <- sd(xbar2)

# means:
age.mean
xbar1.mean
xbar2.mean

# sd:
age.sd
xbar1.sd
xbar2.sd

# Interpretation: The means across all three populations were nearly identicaly with some slight deviation within a point. However, the standard deviations between the three had significant differences with the sd decreasing as our sampling population increased, meaning the the precision of the sample mean was increasing with fewer outliers. Looking at the density distributions of these histograms also supports the Central Limit Theorem as they both show the normal distribution that becomes more precise (higher density in the center) as our sample size increases.