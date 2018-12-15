## Sampling with genders and ages
library(cowplot)
library(sampling)
library(stats)
library(tidyverse)  
library(riverplot)
library(ggplot2)
library(ggmosaic)

#####
# Set directory and import data
####
setwd("/Users/jenniferwartick/Documents/Boston University/cs544 Found Analytics/Project/clean-data")  # Change this to your path
s2017 <- read.csv(file = "2017-fCC-New-Coders-Survey-Data.csv",
                  header = TRUE,
                  na.strings = NA,
                  stringsAsFactors = FALSE) %>% tibble::as.tibble()


#######
# Clean Data
######
# merge agender, genderqueer, trans into single bucket category

table(s2017$Gender) #before
s2017$Gender[s2017$Gender=="agender" | s2017$Gender== "genderqueer" | s2017$Gender=="trans"] <- "non-binary/genderqueer"

# some responses are EXTREME outliers that skew graphs visually. I will be be excluding all expected earnings above 250k and current Income about 300k
length(s2017$Gender) # 18175
s2017 <- s2017 %>%
  select(Gender, Income, ExpectedEarning) %>%
  filter(!is.na(Gender), !is.na(Income), !is.na(ExpectedEarning), 
         ExpectedEarning < 250000) 
  


#####
# Sampling by Gender
#####
# simple random sampling without replacement
# sample size of 200

N <- length(s2017$Gender)
n <- 20 # sample size
s1 <- srswor(n, N)

sample1 <- s2017[s1 != 0,]
head(sample1)

### systematic sampling:
k <- ceiling(N/n)
k <- floor(N/n)
k

# random item from first gorup
r <- sample(k, 1)
r

# create a seq of sample by k, up to samples taken = 200 (=n)
s2 <- seq(r, by=k, length=n)
head(s2)
sample2 <- s2017[s2,]


## Stratified sampling
# order by gender
s2017 <- s2017[order(s2017$Gender),]

freq <- table(s2017$Gender)
freq

# proportionate to Gender
st.sizes <- n * freq / sum(freq)
st.sizes

st.1 <-strata(s2017, stratanames = c("Gender"),
              size = ceiling(st.sizes), method = "srswor",
              description = TRUE) # ceiling otherwise rounds to zero
st.1
sample3 <- s2017[st.1$ID_unit,]



#Distributions
plot.gender_expected <- ggplot(s2017, aes(x=s2017$Gender, y=(s2017$ExpectedEarning/1000)))+
  geom_point()

plot.sample1 <- ggplot(sample1, aes(x=sample1$Gender, y=(sample1$ExpectedEarning/1000))) +
  geom_point()
sample1

plot.sample2 <- ggplot(sample2, aes(x=sample2$Gender, y=(sample2$ExpectedEarning/1000))) + 
  geom_point()

plot.sample3 <- ggplot(sample3, aes(x=sample3$Gender, y=(sample3$ExpectedEarning/1000))) + 
  geom_point()

s2017.summary <- s2017 %>%
  group_by(Gender) %>%
  summarise_all(funs(mean))
s2017.summary

sample1.summary <- sample1 %>%
  group_by(Gender) %>%
  summarise_all(funs(mean))
sample1.summary

sample2.summary <- sample2 %>%
  group_by(Gender) %>%
  summarise_all(funs(mean))
sample2.summary

sample3.summary <- sample3 %>%
  group_by(Gender) %>%
  summarise_all(funs(mean))
sample3.summary


