## Sampling with genders and ages
library(cowplot)
library(sampling)
library(stats)
library(tidyverse)  
library(ggplot2)

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

# some responses are EXTREME outliers that skew graphs visually. 
ExpectedByAge <- s2017 %>%
  select(Age, ExpectedEarning) %>%
  filter(!is.na(Age), !is.na(ExpectedEarning))
ExpectedByAge

# Expected earning by age
plot.expected_earn_by_age <- ggplot(ExpectedByAge, aes(
  x=ExpectedByAge$Age, y=(ExpectedByAge$ExpectedEarning/1000)
)) +
  geom_point() + ylab("Expected Earning at first Developer job") +
  xlab("Age") + ggtitle("Expected Earnings at \nfirst developer role by age")
plot.expected_earn_by_age

#summary
ExpectedByAge.summary <- ExpectedByAge %>%
  select(Age, ExpectedEarning) %>%
  group_by(Age) %>%
  summarise_all(funs(mean))
ExpectedByAge.summary


# Because of the above visuals, I will be excluding expected earnings above 250k

length(s2017$Gender) # 18175
s2017 <- s2017 %>%
  filter(!is.na(Gender), !is.na(Income), !is.na(ExpectedEarning), 
         ExpectedEarning < 250000, 
         CountryLive == "United States of America", 
         IsSoftwareDev == 0) %>%
  select(Gender, Income, ExpectedEarning)
  
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
plot.gender_expected <- ggplot(s2017, aes(x=s2017$Gender, 
                                          y=(s2017$ExpectedEarning/1000)))+
  geom_point() + ylab("per $1k") + xlab("Gender") +
  ggtitle("Expected Earnings of entire \n Pop by Gender")

plot.sample1 <- ggplot(sample1, aes(x=sample1$Gender, 
                                    y=(sample1$ExpectedEarning/1000))) +
  geom_point() + ylab("per $1k") + xlab("Gender") +
  ggtitle("Expected Earnings of sample1 \n by Gender")
sample1

plot.sample2 <- ggplot(sample2, aes(x=sample2$Gender, 
                                    y=(sample2$ExpectedEarning/1000))) + 
  geom_point() + ylab("per $1k") + xlab("Gender") +
  ggtitle("Expected Earnings of sample2 \nby Gender")

plot.sample3 <- ggplot(sample3, aes(x=sample3$Gender, 
                                    y=(sample3$ExpectedEarning/1000))) + 
  geom_point() + ylab("per $1k") + xlab("Gender") +
  ggtitle("Expected Earnings of sample3 \nby Gender")

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

plot_grid(plot.gender_expected, plot.sample1, plot.sample2,
          plot.sample3, 
          labels="AUTO", ncol = 2, align = 'v')


