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
  select(Age, Gender, HasChildren, ChildrenNumber, CommuteTime, CityPopulation, 
         CountryCitizen, CountryLive, EmploymentField, EmploymentFieldOther,
         EmploymentStatus, EmploymentStatusOther, ExpectedEarning,
         HoursLearning, Income, IsEthnicMinority, IsReceiveDisabilitiesBenefits,
         IsSoftwareDev, JobPref, JobRoleInterest, JobInterestBackEnd,
         JobInterestDataEngr, JobInterestDevOps, JobInterestFrontEnd,
         JobInterestFullStack, JobInterestGameDev, JobInterestInfoSec,
         JobInterestMobile, JobInterestOther, JobInterestProjMngr,
         JobInterestUX, JobPref,
         JobWherePref, MaritalStatus, SchoolDegree, SchoolMajor)


# only looking at those living in the USA who are not currently working as Software Developers and have specified a Gender
s2017 <- filter(s2017, CountryLive == "United States of America", 
                IsSoftwareDev == 0,
                !is.na(Gender) )

# merge agender, genderqueer, trans into single bucket category

table(s2017$Gender) #before
s2017$Gender[s2017$Gender=="agender" | s2017$Gender== "genderqueer" | s2017$Gender=="trans"] <- "non-binary/genderqueer"


s2017 #4,790 x 35 extra 10 variables due to 2017 survey job interest Q split up
gender.pop<-table(s2017$Gender)



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

# care more about proportion of gender population than absolute numbers
i <- 1
for(row in RolesByGender){
  if(row[1]!="female"){
    # females: 1274
    row[1] <- row[1]/gender.pop[[1]]
    # males: 3381
    row[2] <- row[2]/gender.pop[[2]]
    # non-binary / genderqueer: 94
    row[3] <- row[3]/gender.pop[[3]]
    RolesByGender[i]=row
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
  ylim(c(0,0.4)) + ggtitle("BackEnd") +
  scale_fill_manual(values= c("yellow2", "seagreen3", "mediumvioletred"),
                    guide=FALSE) +
  coord_flip() 

## DATAENGR
plot.dataengr<- ggplot(RolesByGender) + 
  geom_col(aes(x=RolesByGender$Gender, 
               y=RolesByGender$JobInterestDataEngr,
               fill=RolesByGender$Gender)) + 
  ylab("Proportion of interest") + xlab("Gender") + 
  ylim(c(0,0.4)) + ggtitle("DataEngr") +
  scale_fill_manual(values= c("yellow2", "seagreen3", "mediumvioletred"),
                    guide=FALSE) +
  coord_flip() 

## DEVOPS
plot.devops<-ggplot(RolesByGender) + 
  geom_col(aes(x=RolesByGender$Gender, 
               y=RolesByGender$JobInterestDevOps,
               fill=RolesByGender$Gender)) + 
  ylab("Proportion of interest") + xlab("Gender") + 
  ylim(c(0,0.4)) + ggtitle("DevOps") +
  scale_fill_manual(values= c("yellow2", "seagreen3", "mediumvioletred"),
                    guide=FALSE) +
  coord_flip() 

## FRONTEND
plot.frontend<- ggplot(RolesByGender) + 
  geom_col(aes(x=RolesByGender$Gender, 
               y=RolesByGender$JobInterestFrontEnd,
               fill=RolesByGender$Gender)) + 
  ylab("Proportion of interest") + xlab("Gender") + 
  ylim(c(0,0.4)) + ggtitle("FrontEnd") +
  scale_fill_manual(values= c("yellow2", "seagreen3", "mediumvioletred"),
                    guide=FALSE) +
  coord_flip() 

## FULLSTACK
plot.fullstack<- ggplot(RolesByGender) + 
  geom_col(aes(x=RolesByGender$Gender, 
               y=RolesByGender$JobInterestDevOps,
               fill=RolesByGender$Gender)) + 
  ylab("Proportion of interest") + xlab("Gender") + 
  ylim(c(0,0.4)) + ggtitle("DevOps") +
  scale_fill_manual(values= c("yellow2", "seagreen3", "mediumvioletred"),
                    guide=FALSE) +
  coord_flip() 

## GAMEDEV
plot.gamedev <- ggplot(RolesByGender) + 
  geom_col(aes(x=RolesByGender$Gender, 
               y=RolesByGender$JobInterestGameDev,
               fill=RolesByGender$Gender)) + 
  ylab("Proportion of interest") + xlab("Gender") + 
  ylim(c(0,0.4)) + ggtitle("GameDev") +
  scale_fill_manual(values= c("yellow2", "seagreen3", "mediumvioletred"),
                    guide=FALSE) +
  coord_flip() 

## INFOSEC
plot.infosec<- ggplot(RolesByGender) + 
  geom_col(aes(x=RolesByGender$Gender, 
               y=RolesByGender$JobInterestInfoSec,
               fill=RolesByGender$Gender)) + 
  ylab("Proportion of interest") + xlab("Gender") + 
  ylim(c(0,0.4)) + ggtitle("InfoSec") +
  scale_fill_manual(values= c("yellow2", "seagreen3", "mediumvioletred"),
                    guide=FALSE) +
  coord_flip() 

## MOBILE
plot.mobile<- ggplot(RolesByGender) + 
  geom_col(aes(x=RolesByGender$Gender, 
               y=RolesByGender$JobInterestMobile,
               fill=RolesByGender$Gender)) + 
  ylab("Proportion of interest") + xlab("Gender") + 
  ylim(c(0,0.4)) + ggtitle("Mobile") +
  scale_fill_manual(values= c("yellow2", "seagreen3", "mediumvioletred"),
                    guide=FALSE) +
  coord_flip() 

## PROJMNGR
plot.projmngfr <- ggplot(RolesByGender) + 
  geom_col(aes(x=RolesByGender$Gender, 
               y=RolesByGender$JobInterestProjMngr,
               fill=RolesByGender$Gender)) + 
  ylab("Proportion of interest") + xlab("Gender") + 
  ylim(c(0,0.4)) + ggtitle("ProjectManager") +
  scale_fill_manual(values= c("yellow2", "seagreen3", "mediumvioletred"),
                    guide=FALSE) +
  coord_flip() 

## UX
plot.ux <- ggplot(RolesByGender) + 
  geom_col(aes(x=RolesByGender$Gender, 
               y=RolesByGender$JobInterestUX,
               fill=RolesByGender$Gender)) + 
  ylab("Proportion of interest") + xlab("Gender") + 
  ylim(c(0,0.4)) + ggtitle("UX") +
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

#####
# Age vs Current Income vs Expected Income 
####

IncomeByAge <- s2017 %>%
  select(Age, Income, ExpectedEarning) %>%
  filter(!is.na(Age), !is.na(Income), !is.na(ExpectedEarning))
IncomeByAge


## only want those that have a value for all three
IncomeByAge.avg_expectations <- IncomeByAge %>%
  group_by(Age) %>%
  summarise_all(funs(mean))
  
IncomeByAge
plot.income_by_age <- ggplot(IncomeByAge, aes(
  x=IncomeByAge$Age, y=(IncomeByAge$Income/1000))) +
  geom_point() + ylab("Last Year's Income (per $1k)") +
  xlab("Age")
plot.income_by_age
IncomeByAge$Income