# finalproject_agedistribution
library(cowplot)
library(stats)
library(tidyverse)  
library(riverplot)
library(ggplot2)
library(ggmosaic)
#install.packages("ggmosaic")
setwd("/Users/jenniferwartick/Documents/Boston University/cs544 Found Analytics/Project/clean-data")  # Change this to your path
s2017 <- read.csv(file = "2017-fCC-New-Coders-Survey-Data.csv",
                  header = TRUE,
                  na.strings = NA,
                  stringsAsFactors = FALSE) %>% tibble::as.tibble()


# only select columns of interest

s2017 <- s2017 %>%
  select(Age)


# Get rid of NAs
s2017 <- filter(s2017,!is.na(Age) )


age.pop<-table(s2017$Age)
barplot(age.pop, main = "Age Distribution")