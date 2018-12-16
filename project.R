library(cowplot)
library(stats)
library(tidyverse)  
library(ggplot2)

setwd("/Users/jenniferwartick/Documents/Boston University/cs544 Found Analytics/Project/clean-data")  # Change this to your path


###########
## 01 Categorical ----
###########
s2017 <- read.csv(file = "2017-fCC-New-Coders-Survey-Data.csv",
                  header = TRUE,
                  na.strings = NA,
                  stringsAsFactors = FALSE) %>% tibble::as.tibble()


# only select columns of interest

s2017 <- s2017 %>%
  select(Age, Gender, Income, IsSoftwareDev, JobRoleInterest, 
         JobInterestBackEnd, JobInterestDataEngr, JobInterestDevOps, 
         JobInterestFrontEnd, JobInterestFullStack, JobInterestGameDev, 
         JobInterestInfoSec, JobInterestMobile, JobInterestOther, 
         JobInterestProjMngr, JobInterestUX, CountryLive)


# only looking at those living in the USA who are not currently working as Software Developers and have specified a Gender
s2017 <- filter(s2017, CountryLive == "United States of America", 
                IsSoftwareDev == 0,
                !is.na(Gender) )

# merge agender, genderqueer, trans into single bucket category
table(s2017$Gender) #before
s2017$Gender[s2017$Gender=="agender" | s2017$Gender== "genderqueer" | s2017$Gender=="trans"] <- "non-binary/genderqueer"

glimpse(s2017)
gender.pop<-table(s2017$Gender)

# For use later:
age.pop<-table(s2017$Age)
barplot(gender.pop, main = "Gender Distribution")
barplot(age.pop, main = "Age Distribution")



# analyze gender break down of jobrole interest in 2017
# replace NAs with 0s
s2017$JobInterestBackEnd[is.na(s2017$JobInterestBackEnd)] <- 0
s2017$JobInterestDataEngr[is.na(s2017$JobInterestDataEngr)] <- 0
s2017$JobInterestDevOps[is.na(s2017$JobInterestDevOps)] <- 0
s2017$JobInterestFrontEnd[is.na(s2017$JobInterestFrontEnd)] <- 0
s2017$JobInterestFullStack[is.na(s2017$JobInterestFullStack)] <- 0
s2017$JobInterestGameDev[is.na(s2017$JobInterestGameDev)] <- 0
s2017$JobInterestInfoSec[is.na(s2017$JobInterestInfoSec)] <- 0
s2017$JobInterestMobile[is.na(s2017$JobInterestMobile)] <- 0
#s2017$JobInterestOther[is.na(s2017$JobInterestOther)] <- 0 # text, not 0/1
s2017$JobInterestProjMngr[is.na(s2017$JobInterestProjMngr)] <- 0
s2017$JobInterestUX[is.na(s2017$JobInterestUX)] <- 0


RolesByGender <- s2017 %>%
  select(Gender, JobInterestBackEnd, JobInterestDataEngr, 
         JobInterestDevOps, JobInterestFrontEnd, JobInterestFullStack, 
         JobInterestGameDev, JobInterestInfoSec,JobInterestMobile, 
         JobInterestProjMngr, JobInterestUX)

# get summary
RolesByGender<-RolesByGender %>%
  group_by(Gender) %>%
  summarise_all(funs(sum))
RolesByGender

# care more about proportion within gender population than across all respondents
i <- 1
for(col in RolesByGender){
  if(col[1]!="female"){
    # females: 1274
    col[1] <- col[1]/gender.pop[[1]]
    # males: 3381
    col[2] <- col[2]/gender.pop[[2]]
    # non-binary / genderqueer: 94
    col[3] <- col[3]/gender.pop[[3]]
    RolesByGender[i]=col
  }
  i <- i + 1
}
RolesByGender

## BACKEND
plot.backend<- ggplot(RolesByGender) + 
  geom_col(aes(x=RolesByGender$Gender, 
               y=RolesByGender$JobInterestBackEnd,
               fill=RolesByGender$Gender)) + 
  ylab("Proportion of interest") + xlab("Gender") + 
  ylim(c(0,0.45)) + ggtitle("BackEnd") +
  scale_fill_manual(values= c("yellow2", "seagreen3", "mediumvioletred"),
                    guide=FALSE) +
  coord_flip() 

## DATAENGR
plot.dataengr<- ggplot(RolesByGender) + 
  geom_col(aes(x=RolesByGender$Gender, 
               y=RolesByGender$JobInterestDataEngr,
               fill=RolesByGender$Gender)) + 
  ylab("Proportion of interest") + xlab("Gender") + 
  ylim(c(0,0.45)) + ggtitle("DataEngr") +
  scale_fill_manual(values= c("yellow2", "seagreen3", "mediumvioletred"),
                    guide=FALSE) +
  coord_flip() 

## DEVOPS
plot.devops<-ggplot(RolesByGender) + 
  geom_col(aes(x=RolesByGender$Gender, 
               y=RolesByGender$JobInterestDevOps,
               fill=RolesByGender$Gender)) + 
  ylab("Proportion of interest") + xlab("Gender") + 
  ylim(c(0,0.45)) + ggtitle("DevOps") +
  scale_fill_manual(values= c("yellow2", "seagreen3", "mediumvioletred"),
                    guide=FALSE) +
  coord_flip() 

## FRONTEND
plot.frontend<- ggplot(RolesByGender) + 
  geom_col(aes(x=RolesByGender$Gender, 
               y=RolesByGender$JobInterestFrontEnd,
               fill=RolesByGender$Gender)) + 
  ylab("Proportion of interest") + xlab("Gender") + 
  ylim(c(0,0.45)) + ggtitle("FrontEnd") +
  scale_fill_manual(values= c("yellow2", "seagreen3", "mediumvioletred"),
                    guide=FALSE) +
  coord_flip() 

## FULLSTACK This one seems to have stopped working, unsure why
plot.fullstack<- ggplot(RolesByGender) + 
  geom_col(aes(x=RolesByGender$Gender,
               y=c(RolesByGender$JobInterestFullStack),
               fill=RolesByGender$Gender)) + 
  ylab("Proportion of interest") + xlab("Gender") + 
  ylim(c(0,0.45)) + ggtitle("FullStack") +
  scale_fill_manual(values= c("yellow2", "seagreen3", "mediumvioletred"),
                    guide=FALSE) +
  coord_flip() 

## GAMEDEV
plot.gamedev <- ggplot(RolesByGender) + 
  geom_col(aes(x=RolesByGender$Gender, 
               y=RolesByGender$JobInterestGameDev,
               fill=RolesByGender$Gender)) + 
  ylab("Proportion of interest") + xlab("Gender") + 
  ylim(c(0,0.45)) + ggtitle("GameDev") +
  scale_fill_manual(values= c("yellow2", "seagreen3", "mediumvioletred"),
                    guide=FALSE) +
  coord_flip() 

## INFOSEC
plot.infosec<- ggplot(RolesByGender) + 
  geom_col(aes(x=RolesByGender$Gender, 
               y=RolesByGender$JobInterestInfoSec,
               fill=RolesByGender$Gender)) + 
  ylab("Proportion of interest") + xlab("Gender") + 
  ylim(c(0,0.45)) + ggtitle("InfoSec") +
  scale_fill_manual(values= c("yellow2", "seagreen3", "mediumvioletred"),
                    guide=FALSE) +
  coord_flip() 

## MOBILE
plot.mobile<- ggplot(RolesByGender) + 
  geom_col(aes(x=RolesByGender$Gender, 
               y=RolesByGender$JobInterestMobile,
               fill=RolesByGender$Gender)) + 
  ylab("Proportion of interest") + xlab("Gender") + 
  ylim(c(0,0.45)) + ggtitle("Mobile") +
  scale_fill_manual(values= c("yellow2", "seagreen3", "mediumvioletred"),
                    guide=FALSE) +
  coord_flip() 

## PROJMNGR
plot.projmngfr <- ggplot(RolesByGender) + 
  geom_col(aes(x=RolesByGender$Gender, 
               y=RolesByGender$JobInterestProjMngr,
               fill=RolesByGender$Gender)) + 
  ylab("Proportion of interest") + xlab("Gender") + 
  ylim(c(0,0.45)) + ggtitle("ProjectManager") +
  scale_fill_manual(values= c("yellow2", "seagreen3", "mediumvioletred"),
                    guide=FALSE) +
  coord_flip() 

## UX
plot.ux <- ggplot(RolesByGender) + 
  geom_col(aes(x=RolesByGender$Gender, 
               y=RolesByGender$JobInterestUX,
               fill=RolesByGender$Gender)) + 
  ylab("Proportion of interest") + xlab("Gender") + 
  ylim(c(0,0.45)) + ggtitle("UX") +
  scale_fill_manual(values= c("yellow2", "seagreen3", "mediumvioletred"),
                    guide=FALSE) +
  coord_flip() 


## print plots 
#pdf("plot1.pdf")

plot_grid(plot.backend, plot.dataengr, plot.devops,
          plot.frontend, plot.fullstack, 
          labels="AUTO", ncol = 1, align = 'v')
plot_grid(plot.gamedev,
          plot.infosec, plot.mobile, plot.projmngfr, plot.ux, 
          labels="AUTO", ncol = 1, align = 'v')

##########
# 02 Numerical ----
##########
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


##########
## 03 Sampling ----
##########
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