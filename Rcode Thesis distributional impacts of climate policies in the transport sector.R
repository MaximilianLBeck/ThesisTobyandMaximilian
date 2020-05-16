#######################
# Thesis Toby and Max #
#######################

###If something is confusing or unclear, please don't hesitate to email tobias.l.bernstein@gmail.com 
#We will happily answer any questions to the best of our knowledge

##general structure of the code:
#Naming in the script follows original research design which is set out in work packages: A1-A4. A5 was added on at a later stage
#A1 is the estimation regression in order to get fuel efficiency values in the Mobilitaet in Deutschland dataset
#A2 evaluates the distributional impacts of carbon pricing - a motor vehicle tax and a revenue equivalent inefficiency tax
#A3 evaluates different revenue recycling schemes - a commuter allowance, lump sum, and for households with poor access to public transport
#A4 evaluates combustion bans in cities and the corresponding opportunity costs using the regional subsets in MiD
#A5 models a feebate scheme akin to the French one before the recent reforms and a continuous one based on Zachariadis and Clerides (2015)

##general functioning commands
#increase memory limit if r can't allocate vector size max memory limit (can be looked up with 'memory.size(max=TRUE)' (commented out for now)
#memory.limit(size = 18866)

#Clears Global Environment# (commented out for now)
#rm(list = ls())

#SET working directory !! You have to put your own working directory in here :)
setwd("c://Users/")

#libraries
install.packages("plyr")
install.packages("ggplot2")
install.packages("stargazer")
install.packages("texreg","dynlm","plm","lmtest","gmodels","car")
install.packages("pastecs")
install.packages("caret")
install.packages("fastDummies")
install.packages("broom")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("Stack")
install.packages("expss")
install.packages("questionr")
install.packages("formattable")
install.packages("data.table")
install.packages("tidyr")
install.packages("scales")
library(foreign)
library(ggplot2)
library(stargazer)
library(texreg)
library(dynlm)
library(plm)
library(lmtest)
library(gmodels)
library(car)
library(caret)
library(fastDummies)
library(broom)
library(plyr)
library(dplyr)
library(tidyverse)
library(Stack)
library(expss)
library(questionr)
library(data.table)
library(formattable)
library(tidyr)
library(scales)
#loading datasets for work package A1
#Loading MOP Data
dat_TANK <- read.csv2('TANK18.csv', sep=";", stringsAsFactors = FALSE)
dat_HH <- read.csv2('HH17.csv', sep=";", stringsAsFactors = FALSE)
# rename varibale
dat_TANK$ID <- dat_TANK$IDHH
dat_MOP <- merge(dat_HH,dat_TANK,by="ID")

#A1 Start####
#Data cleaning and recoding of variables

#recode to numercial, fix VERB
typeof(dat_MOP$VERB)
# create a new variable VERB_n, which is numeric
dat_MOP$VERB_n <- as.numeric(dat_MOP$VERB)
is.numeric(dat_TANK$VERB)
is.numeric(dat_MOP$VERB_n)
summary(dat_MOP$VERB_n)

# Recode variables and inspect

##VERB_n
plot(dat_MOP$VERB_n)
hist(dat_MOP$VERB_n, breaks = 100, col = "blue")
##KMJAHR
plot(dat_MOP$KMJAHR)
hist(dat_MOP$KMJAHR, breaks = 100)
##BAUJAHR
plot(dat_MOP$BAUJAHR)
hist(dat_MOP$BAUJAHR, breaks = 100)
##EINKO
plot(dat_MOP$EINKO)
hist(dat_MOP$EINKO, breaks = 10, col="blue")
# very few observations for low income groups
## SEGMENT
dat_MOP$SEGMENT
CrossTable(dat_MOP$SEGMENT)
# --> create dummies - in the regression analysis the first category alphabetically is the baseline for MOP we drop Motorhome as it is not in MiD and only 7 obersvations so not of great import
dat_MOP$SEGMENT_ =  NA
dat_MOP$SEGMENT_ = ifelse(dat_MOP$SEGMENT == 1, 'mini',
                             ifelse(dat_MOP$SEGMENT == 2, 'small',
                                    ifelse(dat_MOP$SEGMENT == 3, 'compact',
                                           ifelse(dat_MOP$SEGMENT == 4, 'middle.class',
                                                  ifelse(dat_MOP$SEGMENT == 5, 'upper.middle.class',
                                                         ifelse(dat_MOP$SEGMENT == 6, 'upper.class',
                                                                ifelse(dat_MOP$SEGMENT == 7, 'all.terrain',
                                                                       ifelse(dat_MOP$SEGMENT == 8, 'sport',
                                                                              ifelse(dat_MOP$SEGMENT == 9, 'minivan',
                                                                                     ifelse(dat_MOP$SEGMENT == 10, 'large.van',
                                                                                            ifelse(dat_MOP$SEGMENT == 11, 'utility',
                                                                                                   ifelse(dat_MOP$SEGMENT == 13, 'suv', NA))))))))))))
## ANTRIEB
dat_MOP$ANTRIEB
hist(dat_MOP$ANTRIEB, breaks = 10)
# create new ANTRIEB_ variable, which will provide dummies in the regression output, Hydrogen/methanol is dropped as it is not in MiD and only one observation
dat_MOP$ANTRIEB_=  NA
dat_MOP$ANTRIEB_= ifelse(dat_MOP$ANTRIEB == 1, 'diesel',
                             ifelse(dat_MOP$ANTRIEB == 2,'petrol',
                                    ifelse(dat_MOP$ANTRIEB == 3, 'nat.Gas',
                                           ifelse(dat_MOP$ANTRIEB == 4, 'hybrid',
                                                  ifelse(dat_MOP$ANTRIEB == 5, 'electric',NA)))))

#regression analysis
# Model 4 (was the fourth model built, highesta adjusted R2)
#saving coefficients to mod4 as it is used for estimation later

mod4 <- lm(VERB_n ~ SEGMENT_ + KMJAHR + BAUJAHR + ANTRIEB_+ EINKO, dat=dat_MOP)
summary(mod4)


## MiD ##

# import MiD data and merge the two datasets (n rows = n cars)
dat_MiD_Auto <- read_delim('MiD2017_Autos.csv',delim = ";")
dat_MiD_HH <- read_delim('MiD2017_Haushalte.csv', delim=";")
dat_MiD <- left_join(dat_MiD_Auto, dat_MiD_HH, by = "H_ID")

#Set both H_EINK_0RM and fuel_eff to NA if H_EINK_0RM = 0
dat_MiD$H_EINK_0RM <- ifelse(dat_MiD$H_EINK >0, dat_MiD$H_EINK, NA)

#create ordinal variable for income groups
#income groups match income groups in MOP - necessary for estimation
dat_MiD$inc_gr =  NA
dat_MiD$inc_gr = ifelse(dat_MiD$H_EINK_0RM < 500, 1,
                 ifelse(dat_MiD$H_EINK_0RM >= 500 & dat_MiD$H_EINK_0RM < 1000, 2,
                 ifelse(dat_MiD$H_EINK_0RM >= 1000 & dat_MiD$H_EINK_0RM < 1500, 3,
                 ifelse(dat_MiD$H_EINK_0RM >= 1500 & dat_MiD$H_EINK_0RM < 2000, 4,
                 ifelse(dat_MiD$H_EINK_0RM >= 2000 & dat_MiD$H_EINK_0RM < 2500, 5,
                 ifelse(dat_MiD$H_EINK_0RM >= 2500 & dat_MiD$H_EINK_0RM < 3000, 6,
                 ifelse(dat_MiD$H_EINK_0RM >= 3000 & dat_MiD$H_EINK_0RM < 3500, 7,
                 ifelse(dat_MiD$H_EINK_0RM >= 3500 & dat_MiD$H_EINK_0RM < 4000, 8,
                 ifelse(dat_MiD$H_EINK_0RM >= 4000 & dat_MiD$H_EINK_0RM < 5000, 9,
                 ifelse(dat_MiD$H_EINK_0RM >= 5000, 10, NA))))))))))
# creating dummies for Segment in MiD - in the regression analysis the first category alphabetically is the baseline- the factor categories match MOP
dat_MiD$SEGMENT_ =  NA
dat_MiD$SEGMENT_ = ifelse(dat_MiD$seg_kba == 1, 'Mini',
                   ifelse(dat_MiD$seg_kba == 2, 'Small',
                   ifelse(dat_MiD$seg_kba == 3, 'compact',
                   ifelse(dat_MiD$seg_kba == 4, 'Middle.class',
                   ifelse(dat_MiD$seg_kba == 5, 'upper.middle.class',
                                                       ifelse(dat_MiD$seg_kba == 6, 'upper.class',
                                                              ifelse(dat_MiD$seg_kba == 7, 'suv',
                                                                     ifelse(dat_MiD$seg_kba == 8, 'all.terrain',
                                                                            ifelse(dat_MiD$seg_kba == 9, 'Sport',
                                                                                   ifelse(dat_MiD$seg_kba == 10, 'minivan',
                                                                                          ifelse(dat_MiD$seg_kba == 11, 'large.van',
                                                                                                 ifelse(dat_MiD$seg_kba == 12, 'utility', NA))))))))))))
CrossTable(dat_MiD$SEGMENT_)
#Creating dummies for fuel type in MiD - diesel and petrol in the list are swapped in the order from MOP dataset - it is just how they are recorded in the individual datasets
dat_MiD$ANTRIEB_=  NA
dat_MiD$ANTRIEB_= ifelse(dat_MiD$A_ANTRIEB == 1, 'Petrol',
                           ifelse(dat_MiD$A_ANTRIEB == 2,'Diesel',
                                  ifelse(dat_MiD$A_ANTRIEB == 3, 'Nat.Gas',
                                         ifelse(dat_MiD$A_ANTRIEB == 4, 'Hybrid',
                                                ifelse(dat_MiD$A_ANTRIEB == 5, 'Electric',NA)))))

# create dummy variables one by one
# mini
dat_MiD$seg_mini <- NA
dat_MiD$seg_mini = ifelse(dat_MiD$seg_kba == 1, 1,
                          ifelse(dat_MiD$seg_kba == 2, 0,
                                 ifelse(dat_MiD$seg_kba == 3, 0,
                                        ifelse(dat_MiD$seg_kba == 4, 0,
                                               ifelse(dat_MiD$seg_kba == 5, 0,
                                                      ifelse(dat_MiD$seg_kba == 6, 0,
                                                             ifelse(dat_MiD$seg_kba == 7, 0,
                                                                    ifelse(dat_MiD$seg_kba == 8, 0,
                                                                           ifelse(dat_MiD$seg_kba == 9, 0,
                                                                                  ifelse(dat_MiD$seg_kba == 10, 0,
                                                                                         ifelse(dat_MiD$seg_kba == 11, 0,
                                                                                                ifelse(dat_MiD$seg_kba == 12, 0, NA))))))))))))
CrossTable(dat_MiD$seg_mini)
CrossTable(dat_MiD$SEGMENT_)
#small
dat_MiD$seg_small <- NA
dat_MiD$seg_small = ifelse(dat_MiD$seg_kba == 1, 0,
                           ifelse(dat_MiD$seg_kba == 2, 1,
                                  ifelse(dat_MiD$seg_kba == 3, 0,
                                         ifelse(dat_MiD$seg_kba == 4, 0,
                                                ifelse(dat_MiD$seg_kba == 5, 0,
                                                       ifelse(dat_MiD$seg_kba == 6, 0,
                                                              ifelse(dat_MiD$seg_kba == 7, 0,
                                                                     ifelse(dat_MiD$seg_kba == 8, 0,
                                                                            ifelse(dat_MiD$seg_kba == 9, 0,
                                                                                   ifelse(dat_MiD$seg_kba == 10, 0,
                                                                                          ifelse(dat_MiD$seg_kba == 11, 0,
                                                                                                 ifelse(dat_MiD$seg_kba == 12, 0, NA))))))))))))
CrossTable(dat_MiD$seg_small)
#compact
dat_MiD$seg_compact <- NA
dat_MiD$seg_compact = ifelse(dat_MiD$seg_kba == 1, 0,
                             ifelse(dat_MiD$seg_kba == 2, 0,
                                    ifelse(dat_MiD$seg_kba == 3, 1,
                                           ifelse(dat_MiD$seg_kba == 4, 0,
                                                  ifelse(dat_MiD$seg_kba == 5, 0,
                                                         ifelse(dat_MiD$seg_kba == 6, 0,
                                                                ifelse(dat_MiD$seg_kba == 7, 0,
                                                                       ifelse(dat_MiD$seg_kba == 8, 0,
                                                                              ifelse(dat_MiD$seg_kba == 9, 0,
                                                                                     ifelse(dat_MiD$seg_kba == 10, 0,
                                                                                            ifelse(dat_MiD$seg_kba == 11, 0,
                                                                                                   ifelse(dat_MiD$seg_kba == 12, 0, NA))))))))))))
CrossTable(dat_MiD$seg_compact)
#Middle class
dat_MiD$seg_middle.class <- NA
dat_MiD$seg_middle.class = ifelse(dat_MiD$seg_kba == 1, 0,
                                  ifelse(dat_MiD$seg_kba == 2, 0,
                                         ifelse(dat_MiD$seg_kba == 3, 0,
                                                ifelse(dat_MiD$seg_kba == 4, 1,
                                                       ifelse(dat_MiD$seg_kba == 5, 0,
                                                              ifelse(dat_MiD$seg_kba == 6, 0,
                                                                     ifelse(dat_MiD$seg_kba == 7, 0,
                                                                            ifelse(dat_MiD$seg_kba == 8, 0,
                                                                                   ifelse(dat_MiD$seg_kba == 9, 0,
                                                                                          ifelse(dat_MiD$seg_kba == 10, 0,
                                                                                                 ifelse(dat_MiD$seg_kba == 11, 0,
                                                                                                        ifelse(dat_MiD$seg_kba == 12, 0, NA))))))))))))
CrossTable(dat_MiD$seg_middle.class)
#upper middle class
dat_MiD$seg_upper.middle.class <- NA
dat_MiD$seg_upper.middle.class = ifelse(dat_MiD$seg_kba == 1, 0,
                                        ifelse(dat_MiD$seg_kba == 2, 0,
                                               ifelse(dat_MiD$seg_kba == 3, 0,
                                                      ifelse(dat_MiD$seg_kba == 4, 0,
                                                             ifelse(dat_MiD$seg_kba == 5, 1,
                                                                    ifelse(dat_MiD$seg_kba == 6, 0,
                                                                           ifelse(dat_MiD$seg_kba == 7, 0,
                                                                                  ifelse(dat_MiD$seg_kba == 8, 0,
                                                                                         ifelse(dat_MiD$seg_kba == 9, 0,
                                                                                                ifelse(dat_MiD$seg_kba == 10, 0,
                                                                                                       ifelse(dat_MiD$seg_kba == 11, 0,
                                                                                                              ifelse(dat_MiD$seg_kba == 12, 0, NA))))))))))))
CrossTable(dat_MiD$seg_upper.middle.class)
#upper class
dat_MiD$seg_upper.class <- NA
dat_MiD$seg_upper.class = ifelse(dat_MiD$seg_kba == 1, 0,
                                 ifelse(dat_MiD$seg_kba == 2, 0,
                                        ifelse(dat_MiD$seg_kba == 3, 0,
                                               ifelse(dat_MiD$seg_kba == 4, 0,
                                                      ifelse(dat_MiD$seg_kba == 5, 0,
                                                             ifelse(dat_MiD$seg_kba == 6, 1,
                                                                    ifelse(dat_MiD$seg_kba == 7, 0,
                                                                           ifelse(dat_MiD$seg_kba == 8, 0,
                                                                                  ifelse(dat_MiD$seg_kba == 9, 0,
                                                                                         ifelse(dat_MiD$seg_kba == 10, 0,
                                                                                                ifelse(dat_MiD$seg_kba == 11, 0,
                                                                                                       ifelse(dat_MiD$seg_kba == 12, 0, NA))))))))))))
CrossTable(dat_MiD$seg_upper.class)
#sport
dat_MiD$seg_suv <- NA
dat_MiD$seg_suv = ifelse(dat_MiD$seg_kba == 1, 0,
                         ifelse(dat_MiD$seg_kba == 2, 0,
                                ifelse(dat_MiD$seg_kba == 3, 0,
                                       ifelse(dat_MiD$seg_kba == 4, 0,
                                              ifelse(dat_MiD$seg_kba == 5, 0,
                                                     ifelse(dat_MiD$seg_kba == 6, 0,
                                                            ifelse(dat_MiD$seg_kba == 7, 1,
                                                                   ifelse(dat_MiD$seg_kba == 8, 0,
                                                                          ifelse(dat_MiD$seg_kba == 9, 0,
                                                                                 ifelse(dat_MiD$seg_kba == 10, 0,
                                                                                        ifelse(dat_MiD$seg_kba == 11, 0,
                                                                                               ifelse(dat_MiD$seg_kba == 12, 0, NA))))))))))))
CrossTable(dat_MiD$seg_suv)
#all terrain
dat_MiD$seg_all.terrain <- NA
dat_MiD$seg_all.terrain = ifelse(dat_MiD$seg_kba == 1, 0,
                                 ifelse(dat_MiD$seg_kba == 2, 0,
                                        ifelse(dat_MiD$seg_kba == 3, 0,
                                               ifelse(dat_MiD$seg_kba == 4, 0,
                                                      ifelse(dat_MiD$seg_kba == 5, 0,
                                                             ifelse(dat_MiD$seg_kba == 6, 0,
                                                                    ifelse(dat_MiD$seg_kba == 7, 0,
                                                                           ifelse(dat_MiD$seg_kba == 8, 1,
                                                                                  ifelse(dat_MiD$seg_kba == 9, 0,
                                                                                         ifelse(dat_MiD$seg_kba == 10, 0,
                                                                                                ifelse(dat_MiD$seg_kba == 11, 0,
                                                                                                       ifelse(dat_MiD$seg_kba == 12, 0, NA))))))))))))
CrossTable(dat_MiD$seg_all.terrain)
#sport
dat_MiD$seg_sport <- NA
dat_MiD$seg_sport = ifelse(dat_MiD$seg_kba == 1, 0,
                           ifelse(dat_MiD$seg_kba == 2, 0,
                                  ifelse(dat_MiD$seg_kba == 3, 0,
                                         ifelse(dat_MiD$seg_kba == 4, 0,
                                                ifelse(dat_MiD$seg_kba == 5, 0,
                                                       ifelse(dat_MiD$seg_kba == 6, 0,
                                                              ifelse(dat_MiD$seg_kba == 7, 0,
                                                                     ifelse(dat_MiD$seg_kba == 8, 0,
                                                                            ifelse(dat_MiD$seg_kba == 9, 1,
                                                                                   ifelse(dat_MiD$seg_kba == 10, 0,
                                                                                          ifelse(dat_MiD$seg_kba == 11, 0,
                                                                                                 ifelse(dat_MiD$seg_kba == 12, 0, NA))))))))))))
CrossTable(dat_MiD$seg_sport)
#minivan
dat_MiD$seg_minivan <- NA
dat_MiD$seg_minivan = ifelse(dat_MiD$seg_kba == 1, 0,
                             ifelse(dat_MiD$seg_kba == 2, 0,
                                    ifelse(dat_MiD$seg_kba == 3, 0,
                                           ifelse(dat_MiD$seg_kba == 4, 0,
                                                  ifelse(dat_MiD$seg_kba == 5, 0,
                                                         ifelse(dat_MiD$seg_kba == 6, 0,
                                                                ifelse(dat_MiD$seg_kba == 7, 0,
                                                                       ifelse(dat_MiD$seg_kba == 8, 0,
                                                                              ifelse(dat_MiD$seg_kba == 9, 0,
                                                                                     ifelse(dat_MiD$seg_kba == 10, 1,
                                                                                            ifelse(dat_MiD$seg_kba == 11, 0,
                                                                                                   ifelse(dat_MiD$seg_kba == 12, 0, NA))))))))))))
CrossTable(dat_MiD$seg_minivan)
#large van
dat_MiD$seg_large.van <- NA
dat_MiD$seg_large.van = ifelse(dat_MiD$seg_kba == 1, 0,
                               ifelse(dat_MiD$seg_kba == 2, 0,
                                      ifelse(dat_MiD$seg_kba == 3, 0,
                                             ifelse(dat_MiD$seg_kba == 4, 0,
                                                    ifelse(dat_MiD$seg_kba == 5, 0,
                                                           ifelse(dat_MiD$seg_kba == 6, 0,
                                                                  ifelse(dat_MiD$seg_kba == 7, 0,
                                                                         ifelse(dat_MiD$seg_kba == 8, 0,
                                                                                ifelse(dat_MiD$seg_kba == 9, 0,
                                                                                       ifelse(dat_MiD$seg_kba == 10, 0,
                                                                                              ifelse(dat_MiD$seg_kba == 11, 1,
                                                                                                     ifelse(dat_MiD$seg_kba == 12, 0, NA))))))))))))
CrossTable(dat_MiD$seg_large.van)
#utilities
dat_MiD$seg_utility <- NA
dat_MiD$seg_utility = ifelse(dat_MiD$seg_kba == 1, 0,
                        ifelse(dat_MiD$seg_kba == 2, 0,
                               ifelse(dat_MiD$seg_kba == 3, 0,
                                      ifelse(dat_MiD$seg_kba == 4, 0,
                                             ifelse(dat_MiD$seg_kba == 5, 0,
                                                    ifelse(dat_MiD$seg_kba == 6, 0,
                                                           ifelse(dat_MiD$seg_kba == 7, 0,
                                                                  ifelse(dat_MiD$seg_kba == 8, 0,
                                                                         ifelse(dat_MiD$seg_kba == 9, 0,
                                                                                ifelse(dat_MiD$seg_kba == 10, 0,
                                                                                       ifelse(dat_MiD$seg_kba == 11, 0,
                                                                                              ifelse(dat_MiD$seg_kba == 12, 1, NA))))))))))))
CrossTable(dat_MiD$seg_utility)
# create dummy variables one by one for fuel types to match MOP
dat_MiD$fuel_Petrol <- NA
dat_MiD$fuel_Petrol = ifelse(dat_MiD$A_ANTRIEB == 1, 1,
                             ifelse(dat_MiD$A_ANTRIEB == 2, 0,
                                    ifelse(dat_MiD$A_ANTRIEB == 3, 0,
                                           ifelse(dat_MiD$A_ANTRIEB == 4, 0,
                                                  ifelse(dat_MiD$A_ANTRIEB == 5, 0, NA)))))
dat_MiD$fuel_Diesel <- NA
dat_MiD$fuel_Diesel= ifelse(dat_MiD$A_ANTRIEB == 1, 0,
                            ifelse(dat_MiD$A_ANTRIEB == 2, 1,
                                   ifelse(dat_MiD$A_ANTRIEB == 3, 0,
                                          ifelse(dat_MiD$A_ANTRIEB == 4, 0,
                                                 ifelse(dat_MiD$A_ANTRIEB == 5, 0, NA)))))
dat_MiD$fuel_Nat.Gas <- NA
dat_MiD$fuel_Nat.Gas = ifelse(dat_MiD$A_ANTRIEB == 1, 0,
                              ifelse(dat_MiD$A_ANTRIEB == 2, 0,
                                     ifelse(dat_MiD$A_ANTRIEB == 3, 1,
                                            ifelse(dat_MiD$A_ANTRIEB == 4, 0,
                                                   ifelse(dat_MiD$A_ANTRIEB == 5, 0, NA)))))
dat_MiD$fuel_Hybrid <- NA
dat_MiD$fuel_Hybrid = ifelse(dat_MiD$A_ANTRIEB == 1, 0,
                             ifelse(dat_MiD$A_ANTRIEB == 2, 0,
                                    ifelse(dat_MiD$A_ANTRIEB == 3, 0,
                                           ifelse(dat_MiD$A_ANTRIEB == 4, 1,
                                                  ifelse(dat_MiD$A_ANTRIEB == 5, 0, NA)))))
dat_MiD$fuel_Electric <- NA
dat_MiD$fuel_Electric = ifelse(dat_MiD$A_ANTRIEB == 1, 0,
                               ifelse(dat_MiD$A_ANTRIEB == 2, 0,
                                      ifelse(dat_MiD$A_ANTRIEB == 3, 0,
                                             ifelse(dat_MiD$A_ANTRIEB == 4, 0,
                                                    ifelse(dat_MiD$A_ANTRIEB == 5, 1, NA)))))

# drop invalid observations (provided as "9999") for A_BAUJ (construction year) this was their default value in the survey for no information given
dat_MiD$A_BAUJ[dat_MiD$A_BAUJ == 9999] <- NA 

# drop invalid observations (provided as "999994" or "999999") for A_JAHRESFL (annual mileage) this was their default value in the survey for no information given or not recorded
dat_MiD$A_JAHRESFL[dat_MiD$A_JAHRESFL > 250000] <- NA 
hist(dat_MiD$A_JAHRESFL)
summary(dat_MiD$A_JAHRESFL)
sum(with(dat_MiD, A_JAHRESFL >= 100000))
#drop invalid observations (provided as "999999") for H_EINK_0RM - default value for no information collected
dat_MiD$H_EINK_0RM[dat_MiD$H_EINK_0RM > 9000] <- NA 

# we don't drop HH with more than five cars (following Levinson's (2019) example)
# but instead drop more than 3 because MiD does not collect data on >3 cars p. HH
sum(with(dat_MiD, H_ANZAUTO.x >= 4))
dat_MiD$H_ANZAUTO.x[dat_MiD$H_ANZAUTO.x > 3] <- NA
dat_MiD$H_ANZAUTO.y[dat_MiD$H_ANZAUTO.y > 3] <- NA
dat_MiD <- dat_MiD[!(is.na(dat_MiD$H_ANZAUTO.x)) ,]

## Estimating dependent variable fuel efficiency in MiD
# extract coefficients from model 4
mod4$coefficients
coeffs_mod4 <- summary(mod4)$coefficients
coeffs_mod4

# new column for fuel effieciency ("fuel_eff")
dat_MiD$fuel_eff <- NA
dat_MiD$fuel_eff = ifelse(is.na(dat_MiD$H_EINK_0RM),NA,(coeffs_mod4[1,1] + (dat_MiD$seg_compact*coeffs_mod4[2,1] + dat_MiD$seg_large.van*coeffs_mod4[3,1]
                                        + dat_MiD$seg_middle.class*coeffs_mod4[4,1] + dat_MiD$seg_mini*coeffs_mod4[5,1]
                                        + dat_MiD$seg_minivan*coeffs_mod4[6,1] + dat_MiD$seg_small*coeffs_mod4[7,1]
                                        + dat_MiD$seg_sport*coeffs_mod4[8,1] + dat_MiD$seg_suv*coeffs_mod4[9,1]
                                        + dat_MiD$seg_upper.class*coeffs_mod4[10,1] + dat_MiD$seg_upper.middle.class*coeffs_mod4[11,1]
                                        + dat_MiD$seg_utility*coeffs_mod4[12,1] + dat_MiD$A_JAHRESFL*coeffs_mod4[13,1]
                                        + dat_MiD$A_BAUJ*coeffs_mod4[14,1] + dat_MiD$fuel_Hybrid*coeffs_mod4[15,1]
                                        + dat_MiD$fuel_Nat.Gas*coeffs_mod4[16,1] + dat_MiD$fuel_Petrol*coeffs_mod4[17,1]+ dat_MiD$inc_gr * coeffs_mod4 [18,1])))
dat_MiD$fuel_eff
summary(dat_MiD$fuel_eff)
# make negative observations NA 
dat_MiD$fuel_eff <- ifelse(dat_MiD$fuel_eff > 0, dat_MiD$fuel_eff, NA)
summary(dat_MiD$fuel_eff)

#histogram to check distribution
ggplot(data=dat_MiD, aes(x=fuel_eff)) + geom_histogram(binwidth = .001,colour="#778899") + scale_x_continuous(limits = c(0,15), breaks = c(0,5,10,15)) +
  xlab("\nlitres/100km") +ylab("Frequency\n") + ggtitle("") + theme(plot.title=element_text(color="black", size = 10))+
  theme(axis.text.x = element_text(angle = 0, hjust = .5, size = 12), axis.text.y=element_text(size=12), axis.title = element_text(size=12)) 
#ggsave(path = "Boxplot, graphs and figures", filename = "distribution of estimated fuel efficiency MiD.png")

#A1 End####
#A2 Start#####
# using numbers from UBA (https://www.umweltbundesamt.de/sites/default/files/medien/1968/publikationen/co2_emission_factors_for_fossil_fuels_correction.pdf)
# for the carbon content of petrol and diesel; Petrol: 
#we use the emission factor for 'super' (page 32); Diesel:taking the mean of emission factor (third column page 34 of publication) diesel summer and winter
# using https://www.bdbe.de/daten/umrechnung-und-formeln for the density of diesel and petrol- the second number
#CP55 Start=====
## Step1: annual costs CP 55 ##
## Calculating € per liter costs ##
tax_liter_petrol_55 <- 55*3.186*0.00075
tax_liter_diesel_55 <- 55*3.1665*0.00084

#annual cost carbon price
dat_MiD$acost_car_cp55 <- NA
dat_MiD$acost_car_cp55 <- (dat_MiD$A_JAHRESFL/100)*dat_MiD$fuel_eff*tax_liter_diesel_55*dat_MiD$fuel_Diesel +
  (dat_MiD$A_JAHRESFL/100)*dat_MiD$fuel_eff*tax_liter_petrol_55*dat_MiD$fuel_Petrol

#calculate revenue equivalent tax on inefficiency and add as columnto MiD
ineff_tax_cp55 <- (sum(dat_MiD$acost_car_cp55, na.rm = T))/(sum(dat_MiD$fuel_eff, na.rm = T))
dat_MiD$acost_ineff_cp55 <- dat_MiD$fuel_eff*ineff_tax_cp55

#group cars by household
g_HH <- group_by(dat_MiD, H_ID)

#add df for CP55
df_cp55 <- summarise(g_HH,
                     h_total_cost_cp55 = sum(acost_car_cp55),
                     h_inco = unique(H_EINK_0RM))
#adding decile categorical value to DF and making decile NA if income is 0 
df_cp55 <- mutate(df_cp55, decile =  ntile(df_cp55$h_inco,10))
df_cp55 <- mutate(df_cp55, h_inc.share = (h_total_cost_cp55/(h_inco*12))*100)

#creating label 'legend' to stack by
df_cp55$legend = "Carbon price €55"

#add df for ineff tax equivalent to cp50
df_ineff_cp55 <- summarise(g_HH,
                         h_total_cost_ineff_cp55 = sum(acost_ineff_cp55),
                         h_inco = unique(H_EINK_0RM))
df_ineff_cp55 <- mutate(df_ineff_cp55, decile = ntile(df_ineff_cp55$h_inco,10))
df_ineff_cp55 <- mutate(df_ineff_cp55, h_inc.share = (h_total_cost_ineff_cp55/(h_inco*12))*100)

#creating label 'legend' to stack by
df_ineff_cp55$legend = "Revenue equivalent inefficiency tax"

#stacking the DF5s for comparison
df_cp55_stack <- NA
df_cp55_stack <- Stack(df_cp55, df_ineff_cp55)

#making decile categorical for boxplot
df_cp55_stack$decile <- factor(df_cp55_stack$decile)

#check income cutoffs for deciles, these will be the same for all further analysis but might be slightly influenced by NAs
tapply(df_cp55_stack$h_inco, df_cp55_stack$decile, summary)

#boxplot comparing the policies (\n in title creates linebreak)
ggplot(df_cp55_stack %>% filter(!is.na(decile)), aes(y = h_inc.share, x = decile)) + geom_boxplot(aes(fill =legend), outlier.shape = NA) +
  scale_y_continuous(limits = c(0,1.5))  + scale_x_discrete(breaks=c("1","2","3","4","5","6","7","8","9","10"), labels=c("1 \n Poorest \n 10%", "2", "3","4", "5", "6", "7", "8", "9", "10 \n Richest \n 10%")) +
  xlab("Income decile") + ylab("Share of household income (%)\n") +
  #ggtitle("Distributional incidence of €55 carbon price \nand revenue equivalent inefficiency tax") +
  theme(plot.title=element_text(color="black", size = 12), axis.text.x = element_text(angle = 0, hjust = .5, size = 18), axis.text.y=element_text(size=18),axis.title = element_text(size=20)) + 
  theme(legend.title = element_blank()) + theme(legend.position = "bottom") + theme(legend.text=element_text(size=20)) + 
  scale_fill_manual(values = c("#B2473E","#27646D"))
#saving to a specific folder
#ggsave(path = "Boxplot, graphs and figures", filename = "CP55_comparison.png")
#CP55 End====
#CP25 Start ----
## Step1: annual costs CP 25 ##
## Calculating € per liter costs ##
tax_liter_petrol_25 <- 25*3.186*0.00075
tax_liter_diesel_25 <- 25*3.1665*0.00084

#annual cost carbon price
dat_MiD$acost_car_cp25 <- NA
dat_MiD$acost_car_cp25 <- (dat_MiD$A_JAHRESFL/100)*dat_MiD$fuel_eff*tax_liter_diesel_25*dat_MiD$fuel_Diesel +
  (dat_MiD$A_JAHRESFL/100)*dat_MiD$fuel_eff*tax_liter_petrol_25*dat_MiD$fuel_Petrol

#calculate tax on inefficiency and add as columnto MiD
ineff_tax_cp25 <- (sum(dat_MiD$acost_car_cp25, na.rm = T))/(sum(dat_MiD$fuel_eff, na.rm = T))
dat_MiD$acost_ineff_cp25 <- dat_MiD$fuel_eff*ineff_tax_cp25

#group cars by household
g_HH <- group_by(dat_MiD, H_ID)

#add df for CP25
df_cp25 <- summarise(g_HH,
                     h_total_cost_cp25 = sum(acost_car_cp25),
                     h_inco = unique(H_EINK_0RM))
#adding decile categorical value to DF and making decile NA if income is 0 
df_cp25 <- mutate(df_cp25, decile = ntile(df_cp25$h_inco,10))
df_cp25 <- mutate(df_cp25, h_inc.share = (h_total_cost_cp25/(h_inco*12))*100)

#creating label 'legend' to stack by
df_cp25$legend = "Carbon price €25"

#add df for ineff tax equivalent to cp25
df_ineff_cp25 <- summarise(g_HH,
                           h_total_cost_ineff_cp25 = sum(acost_ineff_cp25),
                           h_inco = unique(H_EINK_0RM))
df_ineff_cp25 <- mutate(df_ineff_cp25, decile = ntile(df_ineff_cp25$h_inco,10))
df_ineff_cp25 <- mutate(df_ineff_cp25, h_inc.share = (h_total_cost_ineff_cp25/(h_inco*12))*100)
#creating label 'legend' to stack by
df_ineff_cp25$legend = "Revenue equivalent inefficiency tax"

#stacking the DF25s for comparison
df_cp25_stack <- NA
df_cp25_stack <- Stack(df_cp25, df_ineff_cp25)
#making decile categorical for boxplot
df_cp25_stack$decile <- factor(df_cp25_stack$decile)

#boxplot comparing the policies (for cp25 y scale changed to 0,.75)
ggplot(df_cp25_stack %>% filter(!is.na(decile)), aes(y = h_inc.share, x = decile)) + geom_boxplot(aes(fill =legend), outlier.shape = NA) +
  scale_y_continuous(limits = c(0,.75), breaks = c(0,.25,.5,.75))  + scale_x_discrete(breaks=c("1","2","3","4","5","6","7","8","9","10"), labels=c("1 \n Poorest \n 10%", "2", "3","4", "5", "6", "7", "8", "9", "10 \n Richest \n 10%")) +
  xlab("Income decile") + ylab("Share of household income (%)\n") +
  #ggtitle("Distributional incidence of €55 carbon price \nand revenue equivalent inefficiency tax") +
  theme(plot.title=element_text(color="black", size = 12), axis.text.x = element_text(angle = 0, hjust = .5, size = 18), axis.text.y=element_text(size=18),axis.title = element_text(size=20)) + 
  theme(legend.title = element_blank()) + theme(legend.position = "bottom") + theme(legend.text=element_text(size=20)) + 
  scale_fill_manual(values = c("#B2473E","#27646D"))
#saving to a specific folder
#ggsave(path = "Boxplot, graphs and figures", filename = "CP25_comparison.png")
#CP25 End----
##CP100 Start =====
## Step1: annual costs CP 100 ##
## Calculating € per liter costs ##
tax_liter_petrol_100 <- 100*3.186*0.00075
tax_liter_diesel_100 <- 100*3.1665*0.00084

#annual cost carbon price
dat_MiD$acost_car_cp100 <- NA
dat_MiD$acost_car_cp100 <- (dat_MiD$A_JAHRESFL/100)*dat_MiD$fuel_eff*tax_liter_diesel_100*dat_MiD$fuel_Diesel +
  (dat_MiD$A_JAHRESFL/100)*dat_MiD$fuel_eff*tax_liter_petrol_100*dat_MiD$fuel_Petrol

#calculate tax on inefficiency and add as columnto MiD
ineff_tax_cp100 <- (sum(dat_MiD$acost_car_cp100, na.rm = T))/(sum(dat_MiD$fuel_eff, na.rm = T))
dat_MiD$acost_ineff_cp100 <- dat_MiD$fuel_eff*ineff_tax_cp100

#group cars by household
g_HH <- group_by(dat_MiD, H_ID)

#add df for CP100
df_cp100 <- summarise(g_HH,
                     h_total_cost_cp100 = sum(acost_car_cp100),
                     h_inco = unique(H_EINK_0RM))
#adding decile categorical value to DF and making decile NA if income is 0 
df_cp100 <- mutate(df_cp100, decile = ntile(df_cp100$h_inco,10))
df_cp100 <- mutate(df_cp100, h_inc.share = (h_total_cost_cp100/(h_inco*12))*100)

#creating label 'legend' to stack by
df_cp100$legend = "Carbon price €100"

#add df for ineff tax equivalent to cp100
df_ineff_cp100 <- summarise(g_HH,
                           h_total_cost_ineff_cp100 = sum(acost_ineff_cp100),
                           h_inco = unique(H_EINK_0RM))
df_ineff_cp100 <- mutate(df_ineff_cp100, decile = ntile(df_ineff_cp100$h_inco,10))
df_ineff_cp100 <- mutate(df_ineff_cp100, h_inc.share = (h_total_cost_ineff_cp100/(h_inco*12))*100)
#creating label 'legend' to stack by
df_ineff_cp100$legend = "Revenue equivalent inefficiency tax"

#stacking the DF100s for comparison
df_cp100_stack <- NA
df_cp100_stack <- Stack(df_cp100, df_ineff_cp100)
#making decile categorical for boxplot
df_cp100_stack$decile <- factor(df_cp100_stack$decile)

#boxplot comparing the policies (for CP100 y scale changed to 0,3)
ggplot(df_cp100_stack %>% filter(!is.na(decile)), aes(y = h_inc.share, x = decile)) + geom_boxplot(aes(fill =legend), outlier.shape = NA) +
  scale_y_continuous(limits = c(0,3), breaks = c(0,1,2,3))  + scale_x_discrete(breaks=c("1","2","3","4","5","6","7","8","9","10"), labels=c("1 \n Poorest \n 10%", "2", "3","4", "5", "6", "7", "8", "9", "10 \n Richest \n 10%")) +
  xlab("Income decile") + ylab("Share of household income (%)\n") +
  #ggtitle("Distributional incidence of €55 carbon price \nand revenue equivalent inefficiency tax") +
  theme(plot.title=element_text(color="black", size = 12), axis.text.x = element_text(angle = 0, hjust = .5, size = 18), axis.text.y=element_text(size=18),axis.title = element_text(size=20)) + 
  theme(legend.title = element_blank()) + theme(legend.position = "bottom") + theme(legend.text=element_text(size=20)) + 
  scale_fill_manual(values = c("#B2473E","#27646D"))
#saving to a specific folder
#ggsave(path = "Boxplot, graphs and figures", filename = "CP100_comparison.png")
##CP100 End =====
#Motor Vehicle Tax start -----
#Calculating tCO2/l Content for Petrol and Diesel (values taken from above - sources are provided there)
tCO2_per_l_petrol <- 3.186*0.00075
tCO2_per_l_diesel <- 3.1665*0.00084
##calculating grams per CO2 using fuel efficiency
# getting fuel efficiency into liters/km
dat_MiD$fuel_eff_lkm <- dat_MiD$fuel_eff/100
#calculation notes: fuel_eff divided by 100 to get the number in l/km; and multipled by 1 mil to get it in grams (necessary for cost attribution)
dat_MiD$gramsco2km_untrimmed <- ((dat_MiD$fuel_eff_lkm *tCO2_per_l_diesel*dat_MiD$fuel_Diesel) +
  (dat_MiD$fuel_eff_lkm*tCO2_per_l_petrol*dat_MiD$fuel_Petrol))*1000000

dat_MiD$gramsco2km <- ifelse(dat_MiD$gramsco2km_untrimmed > 0 & dat_MiD$gramsco2km_untrimmed < 250, dat_MiD$gramsco2km_untrimmed, NA)
#summary below shows that actual fuel efficiencies are slightly less efficient than officially reported ones (no surprise - so adjustment takes place in a second round of comparison)
summary(dat_MiD$gramsco2km)
#histogram to check distribution
ggplot(data=dat_MiD, aes(x=gramsco2km)) + geom_histogram(binwidth = 5,colour="#FF9999") + xlab("grams CO2/km") +ylab("frequency") + ggtitle("Distribution of grams CO2/km") + theme(plot.title=element_text(color="black", size = 10))
#ggsave(path = "Boxplot, graphs and figures", filename = "histogram_gramsCO2_km.png")

#calculate taxable grams
dat_MiD$taxable_grams_with_negative <- ifelse(dat_MiD$A_BAUJ < 2009, 0, ifelse(dat_MiD$A_BAUJ > 2008 | dat_MiD$A_BAUJ < 2012, dat_MiD$gramsco2km - 120, ifelse( dat_MiD$A_BAUJ > 2011 | dat_MiD$A_BAUJ < 2014, dat_MiD$gramsco2km - 110, ifelse(dat_MiD$A_BAUJ > 2013, dat_MiD$gramsco2km - 95, 0))))
#Making sure all values are positive
dat_MiD$taxable_grams <- ifelse(dat_MiD$taxable_grams_with_negative > 0, dat_MiD$taxable_grams_with_negative, 0)
dat_MiD$motor_vehicle_tax <- dat_MiD$taxable_grams*2
hist(dat_MiD$motor_vehicle_tax)

#Aggregating motor vehicle tax at household level
#group cars by household
g_HH <- group_by(dat_MiD, H_ID)

#create df for motor vehicle tax and add to carbon price of 55
df_motor_vehicle_tax <- summarise(g_HH,
                     h_inco = unique(H_EINK_0RM),
                     h_total_cost_motor_vehicle_tax =sum(motor_vehicle_tax))
#adding decile categorical value to DF and making decile NA if income is 0 
df_motor_vehicle_tax <- mutate(df_motor_vehicle_tax, decile =  ntile(df_motor_vehicle_tax$h_inco,10))
#adding variable for household share of income
df_motor_vehicle_tax <- mutate(df_motor_vehicle_tax, h_inc.share = (h_total_cost_motor_vehicle_tax/(h_inco*12))*100)

#label 'legend' for combined tax
df_motor_vehicle_tax$legend = "Motor Vehicle Tax"

#making decile categorical for boxplot
df_motor_vehicle_tax$decile <- factor(df_motor_vehicle_tax$decile)

#boxplot motor vehicle tax (\n in title creates linebreak)
ggplot(df_motor_vehicle_tax %>% filter(!is.na(decile)), aes(y = h_inc.share, x = decile)) + geom_boxplot(aes(fill =legend), outlier.shape = NA) +
  scale_y_continuous(limits = c(0,1.25)) + scale_x_discrete(limits= c(1:10)) + xlab("income decile") + ylab("Share of household income (%)") +
  #ggtitle("Distributional incidence of the current german motor vehicle tax \n(not accounting for weight of car costs)") +
  theme(plot.title=element_text(color="black", size = 10), axis.text.x = element_text(angle = 0, hjust = 1, size = 8), axis.text.y=element_text(size=8)) + theme(legend.title = element_blank()) + theme(legend.position = "bottom")  + scale_fill_manual(values = c("#8FBC8B"))
#
ggsave(path = "Boxplot, graphs and figures", filename = "Distributional incidence of current German motor vehicle tax.png")

###Adjusting thresholds for the motor vehicle tax to reflect actual averages as ours are inflate

#Rescaling dat_MiD$gramsco2km so that it matches the averages we have from the KBA for those years
dat_MiD$gramsco2km_adjusted <- dat_MiD$gramsco2km
dat_MiD$gramsco2km_adjusted <- rescale(dat_MiD$gramsco2km_adjusted, to = c(0,210))
summary(dat_MiD$gramsco2km_adjusted)
#getting rid of the negatives
dat_MiD$gramsco2km_adjusted <- ifelse(dat_MiD$gramsco2km_adjusted > 0, dat_MiD$gramsco2km_adjusted, 0)
#rerunning steps above for the analysis
#checking distribution
ggplot(data=dat_MiD, aes(x=gramsco2km_adjusted)) + geom_histogram(binwidth = 5,colour="#778899") + xlab("grams CO2/km") +ylab("frequency") + ggtitle("Distribution of grams CO2/km") + theme(plot.title=element_text(color="black", size = 10))+xlim(c = 75,210)
#ggsave(path = "Boxplot, graphs and figures", filename = "adjusted_histogram_gramsCO2_km.png")

#calculate taxable grams
dat_MiD$taxable_grams_with_negative_adjusted <- ifelse(dat_MiD$A_BAUJ < 2009, 0, ifelse(dat_MiD$A_BAUJ > 2008 | dat_MiD$A_BAUJ < 2012, dat_MiD$gramsco2km_adjusted - 120, ifelse( dat_MiD$A_BAUJ > 2011 | dat_MiD$A_BAUJ < 2014, dat_MiD$gramsco2km_adjusted - 110, ifelse(dat_MiD$A_BAUJ > 2013, dat_MiD$gramsco2km_adjusted - 95, 0))))
#Making sure all values are positive
dat_MiD$taxable_grams_adjusted <- ifelse(dat_MiD$taxable_grams_with_negative_adjusted > 0, dat_MiD$taxable_grams_with_negative_adjusted, 0)
dat_MiD$motor_vehicle_tax_adjusted <- dat_MiD$taxable_grams_adjusted*2

#Aggregating motor vehicle tax at household level
#group cars by household
g_HH <- group_by(dat_MiD, H_ID)

#create df for motor vehicle tax and add to carbon price of 55
df_motor_vehicle_tax_adjusted <- summarise(g_HH,
                                  h_inco = unique(H_EINK_0RM),
                                  h_total_cost_motor_vehicle_tax_adjusted =sum(motor_vehicle_tax_adjusted))
#adding decile categorical value to DF and making decile NA if income is 0 
df_motor_vehicle_tax_adjusted <- mutate(df_motor_vehicle_tax_adjusted, decile =  ntile(df_motor_vehicle_tax_adjusted$h_inco,10))

#adding variable for the increased tax amount to the df - MULTIPLYING BY 2.5 TO CREATE A TAX OF 5 EUROS PER GRAM
df_motor_vehicle_tax_adjusted$h_total_cost_motor_vehicle_tax_adjusted_increase <-df_motor_vehicle_tax_adjusted$h_total_cost_motor_vehicle_tax_adjusted*2.5

#adding variables for household share of income (one for current rate and one for the suspected increase)
df_motor_vehicle_tax_adjusted <- mutate(df_motor_vehicle_tax_adjusted, h_inc.share = (h_total_cost_motor_vehicle_tax_adjusted/(h_inco*12))*100)
df_motor_vehicle_tax_adjusted <- mutate(df_motor_vehicle_tax_adjusted, h_inc.share_increase = (h_total_cost_motor_vehicle_tax_adjusted_increase/(h_inco*12))*100)
#label 'legend' for combined tax
df_motor_vehicle_tax_adjusted$legend = "Motor Vehicle Tax (adjusted mean)"

#making decile categorical for boxplot
df_motor_vehicle_tax_adjusted$decile <- factor(df_motor_vehicle_tax_adjusted$decile)

#boxplot motor vehicle tax adjusted (\n in title creates linebreak)
ggplot(df_motor_vehicle_tax_adjusted %>% filter(!is.na(decile)), aes(y = h_inc.share, x = decile)) + geom_boxplot(aes(fill =legend), outlier.shape = NA) +
  scale_y_continuous(limits = c(0,.5)) + xlab("Income decile") + ylab("Share of household income (%)\n") + scale_x_discrete(breaks=c("1","2","3","4","5","6","7","8","9","10"), labels=c("1 \n Poorest \n 10%", "2", "3","4", "5", "6", "7", "8", "9", "10 \n Richest \n 10%")) +
   #ggtitle("Distributional incidence of the current german motor vehicle tax adjusted \n(not accounting for weight of car costs)") + 
  theme(plot.title=element_text(color="black", size = 10), axis.text.x = element_text(angle = 0, hjust = .5, size = 18), axis.text.y=element_text(size=18)) + theme(legend.title = element_blank(), axis.title = element_text(size=20))+ theme(legend.position = "none") + scale_fill_manual(values = c("#2B7C80"))
#ggsave(path = "Boxplot, graphs and figures", filename = "Distributional incidence of current German motor vehicle tax adjusted.png") 

#boxplot motor vehicle tax adjusted INCREASE (the increase shows the price move from 2 to 5 euros per gram over the respective limits)
ggplot(df_motor_vehicle_tax_adjusted %>% filter(!is.na(decile)), aes(y = h_inc.share_increase, x = decile)) + geom_boxplot(aes(fill =legend), outlier.shape = NA) +
  scale_y_continuous(limits = c(0,1.25)) + scale_x_discrete(limits= c(1:10)) + xlab("income decile") + ylab("Share of household income (%)") +
  #ggtitle("Distributional incidence of the current german motor vehicle tax adjusted + increased tax to €5 \n(not accounting for weight of car costs)") +
  theme(plot.title=element_text(color="black", size = 10), axis.text.x = element_text(angle = 0, hjust = 1, size = 8), axis.text.y=element_text(size=8)) + theme(legend.title = element_blank())+ theme(legend.position = "bottom") + scale_fill_manual(values = c("#E6E6FA"))
#ggsave(path = "Boxplot, graphs and figures", filename = "Distributional incidence of current German motor vehicle tax adjusted increased tax to €5.png") 

###comparing the unadjusted and adjusted boxplots
#Stacking adjusted with sCheck
df_mvt_compare <- NA
df_mvt_compare <- Stack(df_motor_vehicle_tax_adjusted, df_motor_vehicle_tax)

#boxplot motor vehicle tax adjusted and no adjustment (comparison 1)
ggplot(df_mvt_compare %>% filter(!is.na(decile)), aes(y = h_inc.share, x = decile)) + geom_boxplot(aes(fill =legend), outlier.shape = NA) +
  scale_y_continuous(limits = c(0,1)) + scale_x_discrete(limits= c(1:10)) + xlab("Income decile") + ylab("Share of household income (%)") +
  theme(plot.title=element_text(color="black", size = 10), axis.text.x = element_text(angle = 0, hjust = 1, size = 8), axis.text.y=element_text(size=8)) + theme(legend.title = element_blank())+ theme(legend.position = "bottom") + scale_fill_manual(values = c("#8FBC8B","#008B8B"))
#ggsave(path = "Boxplot, graphs and figures", filename = "Distributional incidence of current German motor vehicle tax adjusted and unadjusted - comparison.png")

###Hypothetical evaluation of how the policy would play out 10 years from now
##checking to see how vehicle year plays out by decile
##we do this to see how the policy would have to adjusted to reflect the distribution of ownership
#first adding decile to dat_MiD 
dat_MiD<- mutate(dat_MiD, decile =  ntile(dat_MiD$H_EINK_0RM,10))
df_car_distribution <- dat_MiD[c("A_BAUJ", "decile")]
df_car_distribution$decile <- as.factor(df_car_distribution$decile)
tapply(df_car_distribution$A_BAUJ, df_car_distribution$decile, summary)
ggplot(df_car_distribution %>% filter(!is.na(decile)), aes(x=decile, y=A_BAUJ)) + geom_boxplot(fill=c("#808080") ,outlier.shape=NA) +scale_y_continuous(limits = c(1985,2020)) +
  ylab("car construction year") + xlab ("income decile")
#ggsave(path = "Boxplot, graphs and figures", filename = "Car age by decile.png") 

#load data from old dataset to see how the distribution looked ten years ago and then shift the threshlds accordingly (this set is the car info one and also has income)
#This is from the previous rendition of MiD (2008)
load("v.r")
#getting rid of high numbers that represented non-answers in the dataset
v$HHEINK <- ifelse(v$HHEINK < 16, v$HHEINK, NA)

#checking income cutoffs for deciles
tapply(dat_MiD$H_EINK_0RM, dat_MiD$decile, summary)

#re-order HHEINK from 2008 MiD to closely match decile groups in 2017 MiD

v$decile <-               ifelse(v$HHEINK<=3,1,
                          ifelse(v$HHEINK==4,2,
                          ifelse(v$HHEINK==5,3,
                          ifelse(v$HHEINK==6,4,
                          ifelse(v$HHEINK==7,5,
                          ifelse(v$HHEINK==8,6,
                          ifelse(v$HHEINK==9,7,
                          ifelse(v$HHEINK==10,8,
                          ifelse(v$HHEINK>=11 & v$HHEINK <=13,9,
                          ifelse(v$HHEINK>=14,10,NA))))))))))
#new decile groups: 1. 0-1500; 2. 1500-2000; 3. 2000-2600; 4. 2600-3000; 5. 3000-3600; 6. 3600-4000; 7. 4000-4600; 8. 4600-5000; 9. 5000-6600; 10 >6600

#creating lists for table outputs
MiD_2008_cars <- tapply(v$VBAUJ, v$decile, summary)
MiD_2017_cars <- tapply(df_car_distribution$A_BAUJ, df_car_distribution$decile, summary)

#creatingdf for 2008 cars to compare with 2017
df_car_distribution_2008 <- v[c("VBAUJ", "decile")]

#adding legends to both for stacking
df_car_distribution$legend      = "MiD 2017"
df_car_distribution_2008$legend = "MiD 2008"

#renaming build variable to match MiD for boxplot comparison
df_car_distribution_2008 <- rename.variable(df_car_distribution_2008, "VBAUJ", "A_BAUJ")
#decile as factor for 2008
df_car_distribution_2008$decile <- as.factor(df_car_distribution_2008$decile)

#stacking
df_car_comparison <- NA
df_car_comparison <- Stack(df_car_distribution, df_car_distribution_2008)

#plotting the two
ggplot(df_car_comparison %>% filter(!is.na(decile)), aes(x=decile, y=A_BAUJ)) + geom_boxplot(aes(fill =legend),outlier.shape=NA) + 
  scale_y_continuous(limits = c(1985,2020)) + ylab("car construction year") + xlab ("Income decile") +scale_fill_manual(values = c("#8779a1", "#9f709f")) + 
  theme(legend.position = "bottom") + theme(legend.title = element_blank())
#ggsave(path = "Boxplot, graphs and figures", filename = "Car year by decile - Comparison MiD 2008 and 2017.png")


summary(df_car_distribution_2008$A_BAUJ)
summary(df_car_distribution$A_BAUJ)

#shifting the policy - on average cars were 7 years older in MiD 2008 than in MiD 2017 - creating new construction year variable that shifts the year by 7
#Ostensibily how the policy will look in 9 years (not accounting for gains in efficiency)
dat_MiD$A_BAUJ_sensitivity = dat_MiD$A_BAUJ + 7
#calculate taxable grams
dat_MiD$taxable_grams_adjusted_sCheck_with_negative <- ifelse(dat_MiD$A_BAUJ_sensitivity < 2009, 0, ifelse(dat_MiD$A_BAUJ_sensitivity > 2008 | dat_MiD$A_BAUJ_sensitivity < 2012, dat_MiD$gramsco2km_adjusted - 120, ifelse( dat_MiD$A_BAUJ_sensitivity > 2011 | dat_MiD$A_BAUJ_sensitivity < 2014, dat_MiD$gramsco2km_adjusted - 110, ifelse(dat_MiD$A_BAUJ_sensitivity > 2013, dat_MiD$gramsco2km_adjusted - 95, 0))))
#Making sure all values are positive
dat_MiD$taxable_grams_adjusted_sCheck <- ifelse(dat_MiD$taxable_grams_adjusted_sCheck_with_negative > 0, dat_MiD$taxable_grams_adjusted_sCheck_with_negative, 0)
dat_MiD$motor_vehicle_tax_adjusted_sCheck <- dat_MiD$taxable_grams_adjusted_sCheck*2

#Aggregating motor vehicle tax at household level
#group cars by household
g_HH <- group_by(dat_MiD, H_ID)

#create df for motor vehicle tax
df_motor_vehicle_tax_adjusted_sCheck <- summarise(g_HH,
                                                  h_inco = unique(H_EINK_0RM),
                                                  h_total_cost_motor_vehicle_tax_adjusted =sum(motor_vehicle_tax_adjusted_sCheck))
#adding decile categorical value to DF and making decile NA if income is 0 
df_motor_vehicle_tax_adjusted_sCheck <- mutate(df_motor_vehicle_tax_adjusted_sCheck, decile =  ntile(df_motor_vehicle_tax_adjusted_sCheck$h_inco,10))

#adding variables for household share of income (one for current rate and one for the suspected increase)
df_motor_vehicle_tax_adjusted_sCheck <- mutate(df_motor_vehicle_tax_adjusted_sCheck, h_inc.share = (h_total_cost_motor_vehicle_tax_adjusted/(h_inco*12))*100)
#label 'legend' for combined tax
df_motor_vehicle_tax_adjusted_sCheck$legend = "MVT in 10 years"

#making decile categorical for boxplot
df_motor_vehicle_tax_adjusted_sCheck$decile <- factor(df_motor_vehicle_tax_adjusted_sCheck$decile)

#boxplot motor vehicle tax adjusted sensitivity check 
ggplot(df_motor_vehicle_tax_adjusted_sCheck %>% filter(!is.na(decile)), aes(y = h_inc.share, x = decile)) + geom_boxplot(aes(fill =legend), outlier.shape = NA) +
  scale_y_continuous(limits = c(0,1)) + scale_x_discrete(breaks=c("1","2","3","4","5","6","7","8","9","10"), labels=c("1 \n Poorest \n 10%", "2", "3","4", "5", "6", "7", "8", "9", "10 \n Richest \n 10%"))  + xlab("Income decile") + ylab("Share of household income (%)\n") +
  theme(plot.title=element_text(color="black", size = 10), axis.text.x = element_text(angle = 0, hjust = .5, size = 8), axis.text.y=element_text(size=8)) + theme(legend.title = element_blank())+ theme(legend.position = "none") + scale_fill_manual(values = c("#75B6B9"))
#ggsave(path = "Boxplot, graphs and figures", filename = "Distributional incidence of current German motor vehicle tax adjusted - sensitivity check (potential future scenario).png")

#Stacking adjusted with sCheck
df_mvt_compare2 <- NA
df_mvt_compare2 <- Stack(df_motor_vehicle_tax_adjusted, df_motor_vehicle_tax_adjusted_sCheck)

#boxplot motor vehicle tax adjusted and sensitivity check (comparison 2)
ggplot(df_mvt_compare2 %>% filter(!is.na(decile)), aes(y = h_inc.share, x = decile)) + geom_boxplot(aes(fill =legend), outlier.shape = NA) +
  scale_y_continuous(limits = c(0,1)) + scale_x_discrete(breaks=c("1","2","3","4","5","6","7","8","9","10"), labels=c("1 \n Poorest \n 10%", "2", "3","4", "5", "6", "7", "8", "9", "10 \n Richest \n 10%")) +
  xlab("Income decile") + ylab("Share of household income (%)\n") + theme(plot.title=element_text(color="black", size = 12), axis.text.x = element_text(angle = 0, hjust = .5, size = 18), axis.text.y=element_text(size=18), axis.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 20)) + theme(legend.title = element_blank())+ theme(legend.position = "bottom") + scale_fill_manual(values = c("#2B7C80","#75B6B9"))
#ggsave(path = "Boxplot, graphs and figures", filename = "Distributional incidence of current German motor vehicle tax adjusted and sensitivit check - comparison.png")


#Motor Vehicle Tax end -----
#A2 End#####
#A3 Start######
###Calculate Incidence for CP of different prices
#creating column with values for total cost of cp55 only for households being evaluated; the sum of total costs is divided
#by the number if households, which are affected by the cp55
recycle_value_cp55 <- NA
recycle_value_cp55 <- sum(df_cp55$h_total_cost_cp55,na.rm = TRUE)/sum(!is.na(df_cp55$h_inc.share))
##New df for cp55_r
df_cp55_r <- summarise(g_HH,
                       h_total_cost_cp55 = sum(acost_car_cp55),
                       h_inco = unique(H_EINK_0RM))
#adding decile categorical value to DF and making decile NA if income is 0 
df_cp55_r <- mutate(df_cp55_r, decile = ntile(df_cp55_r$h_inco,10))
#creating label 'legend' to stack by
df_cp55_r$legend = "Lump sum"
#new column for total household costs after revenue is recycled
df_cp55_r$h_total_cost_cp55_recycled <- NA
df_cp55_r$h_total_cost_cp55_recycled <- df_cp55$h_total_cost_cp55-recycle_value_cp55
# new h_inc.share
df_cp55_r <- mutate(df_cp55_r, h_inc.share = (h_total_cost_cp55_recycled/(h_inco*12))*100)
#making decile categorical for boxplot
df_cp55_r$decile <- factor(df_cp55_r$decile)
#stacking cp55 with cp55_r (including ineeficiency)
df_cp55_r1_stack <- NA
df_cp55_r1_stack <- Stack(df_cp55_stack, df_cp55_r)
#Creating boxplot for cp55 (tax, inefficiency tax and tax with recycling)
ggplot(df_cp55_r1_stack %>% filter(!is.na(decile)), aes(y = h_inc.share, x = decile)) + geom_boxplot(aes(fill =legend), outlier.shape = NA) +
  scale_y_continuous(limits = c(-1.75,1.5), breaks = c(-1.5,-1,-0.5,0,0.5,1,1.5)) + scale_x_discrete(limits= c(1:10)) + xlab("Income decile") + ylab("Share of household income (%)") +
  #ggtitle("Distributional incidence of €55 carbon price \nwith and without revenue recycling \nand revenue equivalent inefficiency tax") +
  theme(plot.title=element_text(color="black", size = 10), axis.text.x = element_text(angle = 0, hjust = 1, size = 8), axis.text.y=element_text(size=8)) + theme(legend.position = "bottom")
#saving to a specific folder
#ggsave(path = "Boxplot, graphs and figures", filename = "CP55_taxrecycled_all.png")
##stacking with only cp55 and cp55_r
df_cp55_r2_stack <- Stack(df_cp55, df_cp55_r)
#boxplot with recycled number 
ggplot(df_cp55_r2_stack %>% filter(!is.na(decile)), aes(y = h_inc.share, x = decile)) + geom_boxplot(aes(fill =legend), outlier.shape = NA) +
  scale_y_continuous(limits = c(-1.75,1.5), breaks = c(-1.5,-1,-0.5,0,0.5,1,1.5)) + scale_x_discrete(limits= c(1:10)) + xlab("Income decile") + ylab("Share of household income (%)") +
  #ggtitle("Distributional incidence of €55 carbon price \nwith and without revenue recycling") +
  theme(plot.title=element_text(color="black", size = 10), axis.text.x = element_text(angle = 0, hjust = 1, size = 8), axis.text.y=element_text(size=8)) + theme(legend.position = "bottom") + 
  theme(legend.text=element_text(size=7)) + theme(legend.title = element_blank()) + 
  scale_fill_manual(values = c("#E7674B", "#3CB371"))
#saving to a specific folder
#ggsave(path = "Boxplot, graphs and figures", filename = "CP55_tax w and without recycled.png")
##stacking with only ineff_cp55 and cp55_r
df_cp55_r3_stack <- Stack(df_ineff_cp55, df_cp55_r)
#boxplot with recycled number 
ggplot(df_cp55_r3_stack %>% filter(!is.na(decile)), aes(y = h_inc.share, x = decile)) + geom_boxplot(aes(fill =legend), outlier.shape = NA) +
  scale_y_continuous(limits = c(-1.75,1.5), breaks = c(-1.5,-1,-0.5,0,0.5,1,1.5)) + scale_x_discrete(limits= c(1:10)) + xlab("Income decile") + ylab("Share of household income (%)") +
  #ggtitle("Distributional incidence of €55 carbon price \nwith revenue recycling and revenue equivalent inefficiency tax") +
  theme(plot.title=element_text(color="black", size = 10), axis.text.x = element_text(angle = 0, hjust = 1, size = 8), axis.text.y=element_text(size=8)) + theme(legend.position = "bottom") +
  theme(legend.text=element_text(size=7)) + theme(legend.title = element_blank()) + 
  scale_fill_manual(values = c("#B0E0E6", "#3CB371"))
#saving to a specific folder
#ggsave(path = "Boxplot, graphs and figures", filename = "CP55_tax recycled and ineff.png")
###Explore Other ways of redistributing money.
#Import MiD_Personen which has info on commuters, join that with the MiD set we created above
dat_MiD_Personen <- read_delim('MiD2017_Personen.csv', delim = ";")
#creating a df on individuals with only relevant variables from MiD_Personen
df_indiv <- distinct(dat_MiD_Personen, HP_ID, H_ID, P_ID, P_VAUTO, P_NUTZ_AUTO, HP_SEX, P_PENDLER, P_RBW_KM, anzkm, quali_opnv)
#correct separators and numeric in "anzkm" by creating new variable "km_day"
df_indiv$km_day <- as.numeric(gsub(",", ".", df_indiv$anzkm))
#drop invalid observations (higher than 1555) for km_day
df_indiv$km_day[df_indiv$km_day > 1555] <- NA 
#"Entfernungspauschale" (commuter allowance): 0.3€ per km
#commuter allowance new in the climate package 0.38€ as of km 21 from 2024-2026 the years modelled for the CP, so an additional 0.08€
df_indiv$comm_al_n <- NA
df_indiv$comm_al_n = ifelse(df_indiv$km_day >= 21, (df_indiv$km_day-20) * 0.08 * 220,
                          ifelse(df_indiv$km_day < 21, 0, NA))
#left joining df_indiv with dat_MiD
dat_A3 <- left_join(dat_MiD, df_indiv, by = "H_ID")

#group by household
dat_A3_g <- group_by(dat_A3, H_ID)

#removing duplicate cars by household
dat_A3_g_rm <- dat_A3_g %>% distinct(H_ID, A_ID, .keep_all = TRUE)

#new data frame for analysis with commuter allowance
df_A3 <- summarise(dat_A3_g_rm,
                      h_total_cost_cp55_comm = sum(acost_car_cp55) - sum(comm_al_n),
                      h_inco = unique(H_EINK_0RM))
#adding decile categorical value to DF and making decile NA if income is 0 
df_A3 <- mutate(df_A3, decile =  ntile(df_A3$h_inco,10))
df_A3 <- mutate(df_A3, h_inc.share = (h_total_cost_cp55_comm/(h_inco*12))*100)
#making decile categorical for boxplot
df_A3$decile <- factor(df_A3$decile)
#creating label 'legend' to stack by
df_A3$legend = "Commuter allowance"

#plot the costs for commuters with the 0.35€ commuter allowance as of 21st km
ggplot(df_A3 %>% filter(!is.na(decile)), aes(y = h_inc.share, x = decile)) + geom_boxplot(aes(fill =legend), outlier.shape = NA) +
  scale_y_continuous(limits = c(-1.75,2.25), breaks = c(-1.5,-1,-0.5,0,0.5,1,1.5)) + scale_x_discrete(limits= c(1:10)) + xlab("Income decile") + ylab("Share of household income (%)") +
  #ggtitle("Distributional incidence of €55 carbon price \nwith revenue recycling through increase commuter allowance") +
  theme(plot.title=element_text(color="black", size = 10), axis.text.x = element_text(angle = 0, hjust = 1, size = 8), axis.text.y=element_text(size=8))  + theme(legend.position = "bottom") +
  theme(legend.text=element_text(size=7)) + theme(legend.title = element_blank()) + 
  scale_fill_manual(values = c("#4682B4"))
#saving to a specific folder
#ggsave(path = "Boxplot, graphs and figures", filename = "CP55_tax and commuter allowance.png")

#Recycling carbon price revenues only to households who have poor access to public transport
##New df for cp55_r_opnv
df_cp55_r_opnv <- summarise(dat_A3_g_rm,
                       h_total_cost_cp55 = sum(acost_car_cp55),
                       h_inco = unique(H_EINK_0RM),
                       access_public_transport= unique(quali_opnv))
#adding decile categorical value to DF and making decile NA if income is 0 
df_cp55_r_opnv <- mutate(df_cp55_r_opnv, decile = ntile(df_cp55_r_opnv$h_inco,10))
#creating label 'legend' to stack by
df_cp55_r_opnv$legend = "Lump sum PPTA"

#setting households who have good public transport access to NA (1 and 2 are very bad and bad access respectively)
df_cp55_r_opnv$access_public_transport <- ifelse(df_cp55_r_opnv$access_public_transport <=2,df_cp55_r_opnv$access_public_transport, NA )

#crosstab check of NAs - the .x is an artefact of the df merging process - might not be applicable to all runs
CrossTable(dat_MiD$quali_opnv.x)
sum(is.na(dat_MiD$quali_opnv.x))

#creating new recycle value only accountng for households that have poor access to public transport (HHs coded 1&2 have poor access)
recycle_value_cp55_opnv <- NA
recycle_value_cp55_opnv <- sum(df_cp55$h_total_cost_cp55,na.rm = TRUE)/sum((df_cp55_r_opnv$access_public_transport <=2),na.rm = TRUE)

#new column for total household costs after revenue is recycled
df_cp55_r_opnv$h_total_cost_recycled_revenue_public_transport <- NA
df_cp55_r_opnv$h_total_cost_recycled_revenue_public_transport <- ifelse(!is.na(df_cp55_r_opnv$access_public_transport),df_cp55_r_opnv$h_total_cost_cp55-recycle_value_cp55_opnv, df_cp55_r_opnv$h_total_cost_cp55)
# new h_inc.share
df_cp55_r_opnv <- mutate(df_cp55_r_opnv, h_inc.share = (h_total_cost_recycled_revenue_public_transport/(h_inco*12))*100)
#making decile categorical for boxplot
df_cp55_r_opnv$decile <- factor(df_cp55_r_opnv$decile)

#plot for revenue recycling to households with poor access to public transport - added new legend position text at end of command
ggplot(df_cp55_r_opnv %>% filter(!is.na(decile)), aes(y = h_inc.share, x = decile)) + geom_boxplot(aes(fill =legend), outlier.shape = NA) +
  scale_y_continuous(limits = c(-2.5,2), breaks = c(-2.5, -2, -1.5,-1,-0.5,0,0.5,1,1.5,2)) + scale_x_discrete(limits= c(1:10)) + xlab("Income decile") + ylab("Share of household income (%)") +
  #ggtitle("Distributional incidence of €55 carbon price \nwith revenue recycling to households \nwho have poor acess to public transport") +
  theme(plot.title=element_text(color="black", size = 10), axis.text.x = element_text(angle = 0, hjust = 1, size = 8), axis.text.y=element_text(size=8)) + theme(legend.position = "bottom") +
  theme(legend.text=element_text(size=7)) + theme(legend.title = element_blank()) + 
  scale_fill_manual(values = c("#3CB371"))
#saving
#ggsave(path = "Boxplot, graphs and figures", filename = "CP55_tax recycled to households with poor public transport access.png")

##robustness check of impact of public transport access recycling scheme by recycling instead only initial lumpsum value to those with poor access to public transport
##New df for cp55_r_opnv
df_cp55_r_opnv_check <- summarise(dat_A3_g_rm,
                            h_total_cost_cp55 = sum(acost_car_cp55),
                            h_inco = unique(H_EINK_0RM),
                            access_public_transport= unique(quali_opnv))
#adding decile categorical value to DF and making decile NA if income is 0 
df_cp55_r_opnv_check <- mutate(df_cp55_r_opnv_check, decile = ntile(df_cp55_r_opnv_check$h_inco,10))

#creating label 'legend' to stack by
df_cp55_r_opnv_check$legend = "public transport same recycling as lump sum"

#setting households who have good public transport access to NA (1 and 2 are very bad and bad access respectively)
df_cp55_r_opnv_check$access_public_transport <- ifelse(df_cp55_r_opn_checkv$access_public_transport <=2,df_cp55_r_opnv_check$access_public_transport, NA )

#new column for total household costs after revenue is recycled
df_cp55_r_opnv_check$h_total_cost_recycled_revenue_public_transport <- NA
df_cp55_r_opnv_check$h_total_cost_recycled_revenue_public_transport <- df_cp55_r_opnv_check$h_total_cost_recycled_revenue_public_transport <- ifelse(df_cp55_r_opnv_check$access_public_transport <=2,df_cp55_r_opnv_check$h_total_cost_cp55-201.191, df_cp55_r_opnv_check$h_total_cost_cp55)
# new h_inc.share
df_cp55_r_opnv_check <- mutate(df_cp55_r_opnv_check, h_inc.share = (h_total_cost_recycled_revenue_public_transport/(h_inco*12))*100)
#making decile categorical for boxplot
df_cp55_r_opnv_check$decile <- factor(df_cp55_r_opnv_check$decile)

#stacking opnv and check
df_stack_opnv_robustness <-Stack(df_cp55_r_opnv_check, df_cp55_r)

#plot for revenue recycling to households with poor access to public transport - added new legend position text at end of command
ggplot(df_stack_opnv_robustness %>% filter(!is.na(decile)), aes(y = h_inc.share, x = decile)) + geom_boxplot(aes(fill =legend), outlier.shape = NA) +
  scale_y_continuous(limits = c(-3,2.5), breaks = c(-3,-2.5, -2, -1.5,-1,-0.5,0,0.5,1,1.5,2,2.5)) + scale_x_discrete(limits= c(1:10)) + xlab("Income decile") + ylab("Share of household income (%)") +
  #ggtitle("Distributional incidence of €55 carbon price \nwith revenue recycling to households \nwho have poor acess to public transport - ROBUSTNESS CHECK") +
  theme(plot.title=element_text(color="black", size = 10), axis.text.x = element_text(angle = 0, hjust = 1, size = 8), axis.text.y=element_text(size=8)) + theme(legend.position = "bottom") +
  theme(legend.text=element_text(size=7)) + theme(legend.title = element_blank()) + 
  scale_fill_manual(values = c("#778899", "#3CB371"))
#saving
#ggsave(path = "Boxplot, graphs and figures", filename = "CP55_tax recycled to all hh or with poor public transport access.png")

###comparing policies of pendler pauschale, no public transport acess and lumpsum

#combining stacked DFs since it wouldn't allow to stack more than two DFs at a time
df_stack_com_and_opnv <-Stack(df_A3, df_cp55_r_opnv)
#stacking the two individually stacked dfs for comparison
df_cp55_r4_stack <- Stack(df_cp55_r2_stack, df_stack_com_and_opnv)
#boxplot
ggplot(df_cp55_r4_stack %>% filter(!is.na(decile)), aes(y = h_inc.share, x = decile)) + geom_boxplot(aes(fill =legend), outlier.shape = NA) +
  scale_y_continuous(limits = c(-3,3), breaks = c(-3,-2,-1,0,1,2,3)) +  
  scale_x_discrete(limits = c (1:10), labels=c("1 \n Poorest \n 10%", "2", "3","4", "5", "6", "7", "8", "9", "10 \n Richest \n 10%")) + xlab("Income decile") + ylab("Share of household income (%)\n") +
  #ggtitle("Distributional incidence of €55 carbon price and different recycling methods") +
  theme(plot.title=element_text(color="black", size = 12), axis.text.x = element_text(angle = 0, hjust = .5, size = 18), axis.text.y=element_text(size=18),axis.title = element_text(size=20)) + theme(legend.position = "bottom") + 
  theme(legend.text=element_text(size=20)) + theme(legend.title = element_blank()) + theme(axis.title.x =element_text(size=20)) + theme(axis.title.y = element_text(size=20)) +
  scale_fill_manual(values = c("#B2473E", "#EAB364", "#ACBD78", "#A4CABC")) +  geom_hline(yintercept = 0, linetype="dotted", color = "black", size=0.5)
#saving to a specific folder
#ggsave(path = "Boxplot, graphs and figures", filename = "CP55_tax w and without recycled compiled.png")
#A3 End######
#A4 Start#######

#Import MiD_Wege which has info on extra time spent travelling with public transport and where people live
dat_MiD_Wege <- read_delim('MiD2017_Wege.csv', delim=";")
#left joining our merged MiD dataset with MiD_Wege to create new merged DF

dat_MiD_A4 <- left_join(dat_MiD, dat_MiD_Wege, by = "H_ID")

#getting rid of large values for anzerw18 (max. 4 adults per HH)

dat_MiD_A4$wegmin <- ifelse(dat_MiD_A4$anzerw18 >= 5, NA, dat_MiD_A4$wegmin)
dat_MiD_A4$adults_adj <- ifelse(dat_MiD_A4$anzerw18 >= 5, NA, dat_MiD_A4$anzerw18)

#create a dummy for persons who go to work

dat_MiD_A4$employed <- ifelse(dat_MiD_A4$HP_BERUF == 1, 1, 
                              ifelse(dat_MiD_A4$HP_BERUF == 2,0,NA))

#creating new variable based on values given for extra time taken to travel by public transport and whether they are driving a car to start with (hvm = hauptverkehrsmittel and option 4 is whether they are driving their own car)

#getting rid of large values assigned to min_altern_opnv in the code plan due to NA (they count up until 3720 minutes but to remove outliers,
#we will only look at ones where it is maximum 2 hours = 120 mins)
dat_MiD_A4$min_altern_opnv_trim <- NA
dat_MiD_A4$min_altern_opnv_trim <- ifelse(dat_MiD_A4$min_altern_opnv >= 121, NA, dat_MiD_A4$min_altern_opnv)
#doing the same trimming for variable wegmin (duration of journey); this is the variable against which min_altern_opnv needs to be compared
dat_MiD_A4$wegmin_trim <- NA
dat_MiD_A4$wegmin_trim <- ifelse(dat_MiD_A4$wegmin >= 121, NA, dat_MiD_A4$wegmin)
#creating extra travel time variable now with the Other variables ready (also adding zweck to make sure they are going to work 1 = arbeit  )
dat_MiD_A4 <- mutate(dat_MiD_A4, extra_travel_time = (min_altern_opnv_trim-wegmin_trim))
dat_MiD_A4$extra_travel_time <- ifelse(dat_MiD_A4$hvm ==4|dat_MiD_A4$hvm ==3 & dat_MiD_A4$zweck ==1, dat_MiD_A4$extra_travel_time, NA)

#preparing weg_min and altern_opnv for further analysis (limiting to commute and only private vehicles)
dat_MiD_A4$travel_work_car <- ifelse(dat_MiD_A4$hvm ==4|dat_MiD_A4$hvm ==3 & dat_MiD_A4$zweck ==1, dat_MiD_A4$wegmin_trim, NA)
dat_MiD_A4$altern_public_trans <- ifelse(dat_MiD_A4$hvm ==4|dat_MiD_A4$hvm ==3 & dat_MiD_A4$zweck ==1, dat_MiD_A4$min_altern_opnv_trim, NA)
#calculating both for a month
dat_MiD_A4$travel_work_car_month <- dat_MiD_A4$travel_work_car * 2 * 20
dat_MiD_A4$altern_public_trans_month <- dat_MiD_A4$altern_public_trans * 2 * 20

# checking distribution
ggplot(data=dat_MiD_A4, aes(x=extra_travel_time)) + geom_histogram(binwidth = 1,colour="#FF9999", fill = "#D44A46") + xlab("Extra minutes with public transport") +ylab("Frequency") + 
  #ggtitle("Extra time that would be spent travelling using public transport instead of using a private vehicle") + 
  theme(plot.title=element_text(color="black", size = 10), axis.text.x = element_text(size = 12), axis.title = element_text(size=12), axis.text.y=element_text(size=12))

##opportunity cost calculation

#Household income in minutes - there are 43,200 minutes in a month of 30 days (multiplied by 2 as time is valuated by half)
dat_MiD_A4$h_inco_minutes <- dat_MiD_A4$H_EINK_0RM/(43200*2)

#cost of the car ban multiplied by 2 for two trips per day and by 20 as there are roughly 20 workdays per month
dat_MiD_A4$car_ban_cost <- dat_MiD_A4$h_inco_minutes * dat_MiD_A4$extra_travel_time * 2 * 20

#extra travel time aggregated over one month
dat_MiD_A4$extra_travel_time_month <- dat_MiD_A4$extra_travel_time * 2 * 20

##grouping by household
dat_A4_g <- group_by(dat_MiD_A4, H_ID)

#removing duplicate person entries by household
dat_A4_g_rm <- dat_A4_g %>% distinct(H_ID, P_ID, .keep_all = TRUE)

##new data frame for analysis of combustion engine ban
df_A4 <- summarise(dat_A4_g_rm,
                   additional_time = sum(extra_travel_time),
                   additional_time_month = sum(extra_travel_time_month),
                   h_cost_timeloss = sum(car_ban_cost),
                   h_inco = unique(H_EINK_0RM),
                   state = unique(BLAND),
                   regiostar17 = unique(RegioStaR17),
                   commute_car_month = sum(travel_work_car_month),
                   altern_public_trans_month = sum(altern_public_trans_month),
                   adults_hh = unique(adults_adj),
                   ad_employed = sum(employed))

##preparing x variables
df_A4 <- mutate(df_A4, decile = ntile(df_A4$h_inco,10))
df_A4$decile <- factor(df_A4$decile)

#look at distribution of employed adults per decile
tapply(df_A4$ad_employed,df_A4$decile,summary)

#turn state into factor
df_A4$state <- factor(df_A4$state)

#add variables for states names and abbreviations
df_A4$state_name <- df_A4$state
df_A4$state_name <- revalue(df_A4$state_name, c("1"="Schleswig-Holstein","2"="Hamburg","3"="Lower Saxony","4"="Bremen","5"="North Rhine-Westphalia",
                                                "6"="Hesse","7"="Rhineland-Palatinate","8"="Baden-Württemberg","9"="Bavaria","10"="Saarland",
                                                "11"="Berlin","12"="Brandenburg","13"="Mecklenburg-Western Pomerania","14"="Saxony","15"="Saxony-Anhalt",
                                                "16"="Thuringia"))

df_A4$state_abb <- df_A4$state
df_A4$state_abb <- revalue(df_A4$state_abb, c("1"="SH","2"="HH","3"="NI","4"="HB","5"="NW",
                                              "6"="HE","7"="RP","8"="BW","9"="BY","10"="SL",
                                              "11"="BE","12"="BB","13"="MV","14"="SN","15"="ST","16"="TH"))
df_A4$state <- factor(df_A4$state)

##calculate cost as income share of households
df_A4 <- mutate(df_A4, h_inc.share = (h_cost_timeloss/h_inco)*100)

###analysis of all metropolitan areas in Germany, creating one df for each regional type within the area

#Metropolis
df_A4_Metropolis <- select(filter(df_A4, regiostar17 == 111),
                           c(H_ID,state,decile,state_name,state_abb,regiostar17,additional_time,additional_time_month,h_inco,h_cost_timeloss,h_inc.share,commute_car_month,altern_public_trans_month,adults_hh,ad_employed))
df_A4_Metropolis$legend = "Metropolis"
#Large city
df_A4_large_city <- select(filter(df_A4, regiostar17 == 112),
                           c(H_ID,state,decile,state_name,state_abb,regiostar17,additional_time,additional_time_month,h_inco,h_cost_timeloss,h_inc.share,commute_car_month,altern_public_trans_month,adults_hh,ad_employed))
df_A4_large_city$legend = "Large city"
#Medium-sized town
df_A4_medium_sized_town <- select(filter(df_A4, regiostar17 == 113),
                                  c(H_ID,state,decile,state_name,state_abb,regiostar17,additional_time,additional_time_month,h_inco,h_cost_timeloss,h_inc.share,commute_car_month,altern_public_trans_month,adults_hh,ad_employed))
df_A4_medium_sized_town$legend = "Medium-sized town"
#Urban region
df_A4_urban_region <- select(filter(df_A4, regiostar17 == 114),
                             c(H_ID,state,decile,state_name,state_abb,regiostar17,additional_time,additional_time_month,h_inco,h_cost_timeloss,h_inc.share,commute_car_month,altern_public_trans_month,adults_hh,ad_employed))
df_A4_urban_region$legend = "Urban region"
#Rural region
df_A4_rural_region <- select(filter(df_A4, regiostar17 == 115),
                             c(H_ID,state,decile,state_name,state_abb,regiostar17,additional_time,additional_time_month,h_inco,h_cost_timeloss,h_inc.share,commute_car_month,altern_public_trans_month,adults_hh,ad_employed))
df_A4_rural_region$legend = "Rural region"

#stacking the datasets
df_A4_stack1 <- Stack(df_A4_Metropolis,df_A4_large_city)
df_A4_stack2 <- Stack(df_A4_stack1,df_A4_medium_sized_town)
df_A4_stack3 <- Stack(df_A4_stack2,df_A4_urban_region)
df_A4_stack <- Stack(df_A4_stack3,df_A4_rural_region)

#show distribution of time loss in metropolitan areas
ggplot(data=df_A4_stack, aes(x=additional_time)) + geom_histogram(binwidth = 1,colour="#FF9999") + xlab("\nExtra minutes with public transport") +ylab("Frequency\n") + 
  #ggtitle("Extra time that would be spent travelling using public transport instead of using a private vehicle") + 
  theme(plot.title=element_text(color="black", size = 10)) + xlim(c = -150,200) + 
  geom_vline(xintercept = 0, linetype="dotted", color = "black", size=0.5)
#ggsave(path = "Boxplot, graphs and figures", filename = "Comb.ban_timeloss_met.png")

#show distribution of time loss in metropolitan areas for a month
ggplot(data=df_A4_stack, aes(x=additional_time_month)) + geom_histogram(binwidth = 10,colour="#FF9999", fill = "#D44A46") + xlab("\nExtra minutes with public transport") +ylab("Frequency\n") + 
  #ggtitle("Extra time that would be spent travelling using public transport instead of using a private vehicle") + 
  theme(plot.title=element_text(color="black", size = 10)) + xlim(c = -3000,6000) + 
  geom_vline(xintercept = 0, linetype="dotted", color = "black", size=0.75)+
  theme(plot.title=element_text(color="black", size = 10), axis.text.x = element_text(size = 18), axis.title = element_text(size=20), axis.text.y=element_text(size=18))
#ggsave(path = "Boxplot, graphs and figures", filename = "Comb.ban_timeloss_met_month.png")

#order labels to get the outputs in the right order
df_A4_stack$legend <- factor(df_A4_stack$legend, levels=c("Metropolis", "Large city",  "Medium-sized town","Urban region","Rural region"))

#plot timeloss across deciles
ggplot(df_A4_stack %>% filter(!is.na(decile)), aes(y = additional_time, x = decile)) + geom_boxplot(aes(fill =legend), outlier.shape = NA) +
  scale_y_continuous(limits = c(-75,150)) + scale_x_discrete() + xlab("Income decile") + ylab("time loss (minutes)") +
  #ggtitle("Time loss through combustion engine ban \nfor different regions in metropolitan area") +
  theme(plot.title=element_text(color="black", size = 10), axis.text.x = element_text(angle = 0, hjust = 1, size = 8), axis.text.y=element_text(size=8)) + theme(legend.position = "bottom") +
  theme(legend.text=element_text(size=7)) + theme(legend.title = element_blank()) + 
  scale_fill_manual(values = c("deeppink4","red3","coral3","coral2","coral1"))

#plot the distribution across deciles
ggplot(df_A4_stack %>% filter(!is.na(decile)), aes(y = h_inc.share, x = decile)) + geom_boxplot(aes(fill =legend), outlier.shape = NA) +
  scale_y_continuous(limits = c(-3,6)) + 
  scale_x_discrete(limits = c (1:10), labels=c("1 \n Poorest \n 10%", "2", "3","4", "5", "6", "7", "8", "9", "10 \n Richest \n 10%")) + xlab("Income decile") + ylab("Share of household income (%)\n") +
  #ggtitle("Distributional incidence of a combustion ban \nfor different regions in metropolitan area") +
  theme(plot.title=element_text(color="black", size = 10), axis.text.x = element_text(angle = 0, hjust = .5, size = 18), axis.text.y=element_text(size=18), axis.title = element_text(size=20)) + theme(legend.position = "bottom") +
  theme(legend.text=element_text(size=20)) + theme(legend.title = element_blank()) + 
  scale_fill_manual(values = c("deeppink4","red3","coral3","coral2","coral1"))
#ggsave(path = "Boxplot, graphs and figures", filename = "Comb.ban_met_dist.png")

##show how this affects cities vs. Other

df_A4_stack_city_rest <- df_A4_stack
df_A4_stack_city_rest$legend <- revalue(df_A4_stack_city_rest$legend, c("Medium-sized town"="Other","Urban region"="Other","Rural region"="Other"))

#boxplot comparing city vs the rest of the regions - share of income
ggplot(df_A4_stack_city_rest %>% filter(!is.na(decile)), aes(y = h_inc.share, x = decile)) + geom_boxplot(aes(fill =legend), outlier.shape = NA) +
  scale_y_continuous(limits = c(-3,6)) + scale_x_discrete() + xlab("Income decile") + ylab("Share of household income (%)") +
  #ggtitle("Distributional incidence of a combustion ban \nfor different regions in metropolitan area") +
  theme(plot.title=element_text(color="black", size = 10), axis.text.x = element_text(angle = 0, hjust = 1, size = 8), axis.text.y=element_text(size=8)) + theme(legend.position = "bottom") +
  theme(legend.text=element_text(size=7)) + theme(legend.title = element_blank()) + 
  scale_fill_manual(values = c("deeppink4","red3","coral1"))
#ggsave(path = "Boxplot, graphs and figures", filename = "Comb.ban_met_cityrest_dist.png")

##show how this affects rural vs. Other

df_A4_stack_rural_rest <- df_A4_stack
df_A4_stack_rural_rest$legend <- revalue(df_A4_stack_rural_rest$legend, c("Metropolis"="Other","Large city"="Other","Medium-sized town"="Other","Urban region"="Other"))

#boxplot comparing rural vs the rest of the regions - share of income
ggplot(df_A4_stack_rural_rest %>% filter(!is.na(decile)), aes(y = h_inc.share, x = decile)) + geom_boxplot(aes(fill =legend), outlier.shape = NA) +
  scale_y_continuous(limits = c(-3,6)) + scale_x_discrete() + xlab("Income decile") + ylab("Share of household income (%)") +
  #ggtitle("Distributional incidence of a combustion ban \nfor different regions in metropolitan area") +
  theme(plot.title=element_text(color="black", size = 10), axis.text.x = element_text(angle = 0, hjust = 1, size = 8), axis.text.y=element_text(size=8)) + theme(legend.position = "bottom") +
  theme(legend.text=element_text(size=7)) + theme(legend.title = element_blank()) + 
  scale_fill_manual(values = c("red","coral1"))
#ggsave(path = "Boxplot, graphs and figures", filename = "Comb.ban_met_ruralrest_dist.png")

#rural vs rest only time loss
ggplot(df_A4_stack_rural_rest %>% filter(!is.na(decile)), aes(y = additional_time, x = decile)) + geom_boxplot(aes(fill =legend), outlier.shape = NA) +
  scale_y_continuous(limits = c(-75,150)) + scale_x_discrete() + xlab("Income decile") + ylab("time loss (minutes)") +
  #ggtitle("Time loss through combustion engine ban \nfor different regions in metropolitan area") +
  theme(plot.title=element_text(color="black", size = 10), axis.text.x = element_text(angle = 0, hjust = 1, size = 8), axis.text.y=element_text(size=8)) + theme(legend.position = "bottom") +
  theme(legend.text=element_text(size=7)) + theme(legend.title = element_blank()) + 
  scale_fill_manual(values = c("red","coral1"))

##initial distribution of travel time to work and alternative time with opnv - rural vs rest

#boxplot of time taken to commute (car) rural vs rest
ggplot(df_A4_stack_rural_rest %>% filter(!is.na(decile)), aes(y = commute_car_month, x = decile)) + geom_boxplot(aes(fill =legend), outlier.shape = NA) +
  scale_y_continuous(limits = c(0,5000)) + scale_x_discrete() + xlab("Income decile") + ylab("commuting time, using private vehicle (minutes)") +
  #ggtitle("Time loss through combustion engine ban \nfor different regions in metropolitan area") +
  theme(plot.title=element_text(color="black", size = 10), axis.text.x = element_text(angle = 0, hjust = 1, size = 8), axis.text.y=element_text(size=8)) + theme(legend.position = "bottom") +
  theme(legend.text=element_text(size=7)) + theme(legend.title = element_blank()) + 
  scale_fill_manual(values = c("red","coral1"))
#ggsave(path = "Boxplot, graphs and figures", filename = "Combustion ban_rural rest_travel time private vehicle.png")

#boxplot of time taken to taken to take public transport instead of car = rural vs rest
ggplot(df_A4_stack_rural_rest %>% filter(!is.na(decile)), aes(y = altern_public_trans_month, x = decile)) + geom_boxplot(aes(fill =legend), outlier.shape = NA) +
  scale_y_continuous(limits = c(0,8000)) + 
  scale_x_discrete(limits = c (1:10), labels=c("1 \n Poorest \n 10%", "2", "3","4", "5", "6", "7", "8", "9", "10 \n Richest \n 10%")) + xlab("Income decile") + ylab("Commuting time using public transport (minutes)\n") +
  #ggtitle("Distributional incidence of a combustion ban \nfor different regions in metropolitan area") +
  theme(plot.title=element_text(color="black", size = 10), axis.text.x = element_text(angle = 0, hjust = .5, size = 18), axis.text.y=element_text(size=18), axis.title = element_text(size=20)) + theme(legend.position = "bottom") +
  theme(legend.text=element_text(size=20)) + theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("red","coral1"))
#ggsave(path = "Boxplot, graphs and figures", filename = "Combustion ban_travel time pt.png")

##initial distribution of travel time to work  - all regional groups
ggplot(df_A4_stack %>% filter(!is.na(decile)), aes(y = commute_car_month, x = decile)) + geom_boxplot(aes(fill =legend), outlier.shape = NA) +
  scale_y_continuous(limits = c(0,5000)) + scale_x_discrete(limits = c (1:10), labels=c("1 \n Poorest \n 10%", "2", "3","4", "5", "6", "7", "8", "9", "10 \n Richest \n 10%")) + 
  xlab("\nIncome decile") + ylab("Commuting time using private vehicle (minutes)\n") +
  #ggtitle("Time loss through combustion engine ban \nfor different regions in metropolitan area") +
  theme(plot.title=element_text(color="black", size = 10), axis.text.x = element_text(angle = 0, hjust = .5, size = 18), axis.text.y=element_text(size=18), axis.title = element_text(size=20)) + theme(legend.position = "bottom") +
  theme(legend.text=element_text(size=20)) + theme(legend.title = element_blank()) + 
  scale_fill_manual(values = c("deeppink4","red3","coral3","coral2","coral1"))
#ggsave(path = "Boxplot, graphs and figures", filename = "Combustion ban_travel time private vehicle.png")

#same but with alternative time with opnv
ggplot(df_A4_stack %>% filter(!is.na(decile)), aes(y = altern_public_trans_month, x = decile)) + geom_boxplot(aes(fill =legend), outlier.shape = NA) +
  scale_y_continuous(limits = c(0,8000)) + scale_x_discrete() + xlab("Income decile") + ylab("commuting time using public transport (minutes)") +
  #ggtitle("Time loss through combustion engine ban \nfor different regions in metropolitan area") +
  theme(plot.title=element_text(color="black", size = 10), axis.text.x = element_text(angle = 0, hjust = 1, size = 8), axis.text.y=element_text(size=8)) + theme(legend.position = "bottom") +
  theme(legend.text=element_text(size=7)) + theme(legend.title = element_blank()) + 
  scale_fill_manual(values = c("deeppink4","red3","coral3","coral2","coral1"))
#ggsave(path = "Boxplot, graphs and figures", filename = "Combustion ban_ruralOther_travel time pt.png")

## calculate total cost to all households from combustion ban in metropolitan areas
#Not included in thesis, but we were interested in the aggregate societal costs from a combustion ban, interesting but didn't fit into theme of distributional impacts
monthly_total_cost_ban = sum(df_A4_stack$h_cost_timeloss,na.rm = TRUE)
monthly_cost_ban_perhh = monthly_total_cost_ban/(sum(!is.na(df_A4_stack$h_cost_timeloss)))
annual_total_cost_ban = monthly_total_cost_ban*12
annual_cost_ban_perhh = monthly_cost_ban_perhh*12
sum(!is.na(df_A4_stack$h_cost_timeloss))

##checking to see underlying causes driving effects of higher opportunity costs for wealthier households
## regression analysis of effect of number of adults in hh on household income
mod_A4 <- lm(h_inco ~ adults_hh, dat=df_A4_stack)
summary(mod_A4)

## sensitivity check for only households with two working adults
df_A4_adult2 <- select(filter(df_A4_stack, ad_employed == 2),c(additional_time,h_inco,h_inc.share,decile,legend,regiostar17,state,state_name,ad_employed))

#plot the distribution across deciles
ggplot(df_A4_adult2 %>% filter(!is.na(decile)), aes(y = h_inc.share, x = decile)) + geom_boxplot(aes(fill =legend), outlier.shape = NA) +
  scale_y_continuous(limits = c(-3,6)) + scale_x_discrete() + xlab("Income decile") + ylab("Share of household income (%)") +
  #ggtitle("Distributional incidence of a combustion ban \nfor different regions in metropolitan area") +
  theme(plot.title=element_text(color="black", size = 10), axis.text.x = element_text(angle = 0, hjust = 1, size = 8), axis.text.y=element_text(size=8)) + theme(legend.position = "bottom") +
  theme(legend.text=element_text(size=7)) + theme(legend.title = element_blank()) + 
  scale_fill_manual(values = c("deeppink4","red3","coral3","coral2","coral1"))

df_A4_adult2 %>% group_by(decile) %>% summarise(non_na_count = sum(!is.na(ad_employed)))

## same analysis for only rural vs rest

df_A4_adult2_rr = df_A4_adult2
df_A4_adult2_rr$legend <- revalue(df_A4_adult2$legend, c("Metropolis"="Other","Large city"="Other","Medium-sized town"="Other","Urban region"="Other"))

#plot the distribution across deciles
ggplot(df_A4_adult2 %>% filter(!is.na(decile)), aes(y = h_inc.share, x = decile)) + geom_boxplot(aes(),fill = c("#66A5AD"), outlier.shape = NA) +
  scale_y_continuous(limits = c(-3,6)) + scale_x_discrete(breaks=c(1:10), labels=c("1 \n Poorest \n 10%", "2", "3","4", "5", "6", "7", "8", "9", "10 \n Richest \n 10%")) + xlab("Income decile") + ylab("Share of household income (%)\n")   + xlab("Income decile") + ylab("Share of household income (%)\n") +
  #ggtitle("Distributional incidence of a combustion ban \nfor different regions in metropolitan area") +
  theme(plot.title=element_text(color="black", size = 10), axis.text.x = element_text(angle = 0, hjust = 1, size = 18), axis.text.y=element_text(size=18), axis.title = element_text(size = 20)) + theme(legend.position = "bottom") +
  theme(legend.text=element_text(size=7)) + theme(legend.title = element_blank()) + stat_summary(fun.y=mean, geom="point", aes(), position = position_dodge(width = .75))
#ggsave(path = "Boxplot, graphs and figures", filename = "Combustion ban_sensitivity.png")

tapply(df_A4_adult2$h_inc.share,df_A4_adult2$decile, summary)

## sensitivity check only on time loss and at inidividual level
#we wanted to see if trends held true with only individual, i.e. persons from richer households had different patterns, holding the amount of people in the household constant (i.e. only evaluating individuals) and only looking at time loss

#removing duplicate person entries by household - there were more than one entry per person ID sometimes
dat_A4_i <- dat_MiD_A4 %>% distinct(H_ID, P_ID, .keep_all = TRUE)

df_A4_i <- select(dat_A4_i,H_ID,BLAND,RegioStaR17,extra_travel_time,extra_travel_time_month,H_EINK_0RM,travel_work_car,travel_work_car_month,altern_public_trans_month,adults_adj)
#change variable names
setnames(df_A4_i, old = c('BLAND','RegioStaR17','extra_travel_time','extra_travel_time_month','H_EINK_0RM','travel_work_car','travel_work_car_month','adults_adj'), 
         new = c('state','regiostar17','additional_time','additional_time_month','h_inco','commute_car_journey','commute_car_month','adults_hh'))

##preparing x variables
df_A4_i <- mutate(df_A4_i, decile = ntile(df_A4_i$h_inco,10))
df_A4_i$decile <- factor(df_A4_i$decile)

#turn state into factor to allow for further analysis
df_A4_i$state <- factor(df_A4_i$state)

#add variables for states names and abbreviations
df_A4_i$state_name <- df_A4_i$state
df_A4_i$state_name <- revalue(df_A4_i$state_name, c("1"="Schleswig-Holstein","2"="Hamburg","3"="Lower Saxony","4"="Bremen","5"="North Rhine-Westphalia",
                                                "6"="Hesse","7"="Rhineland-Palatinate","8"="Baden-Württemberg","9"="Bavaria","10"="Saarland",
                                                "11"="Berlin","12"="Brandenburg","13"="Mecklenburg-Western Pomerania","14"="Saxony","15"="Saxony-Anhalt",
                                                "16"="Thuringia"))

df_A4_i$state_abb <- df_A4_i$state
df_A4_i$state_abb <- revalue(df_A4_i$state_abb, c("1"="SH","2"="HH","3"="NI","4"="HB","5"="NW",
                                              "6"="HE","7"="RP","8"="BW","9"="BY","10"="SL",
                                              "11"="BE","12"="BB","13"="MV","14"="SN","15"="ST","16"="TH"))
df_A4_i$state <- factor(df_A4_i$state)

##analysis of all metropolitan areas in Germany, creating one df for each regional type within the area

#Metropolis
df_A4_i_Metropolis <- select(filter(df_A4_i, regiostar17 == 111),
                           c(H_ID,state,decile,state_name,state_abb,regiostar17,additional_time,additional_time_month,h_inco,commute_car_journey,commute_car_month,altern_public_trans_month,adults_hh))
df_A4_i_Metropolis$legend = "Metropolis"
#Large city
df_A4_i_large_city <- select(filter(df_A4_i, regiostar17 == 112),
                           c(H_ID,state,decile,state_name,state_abb,regiostar17,additional_time,additional_time_month,h_inco,commute_car_journey,commute_car_month,altern_public_trans_month,adults_hh))
df_A4_i_large_city$legend = "Large city"
#Medium-sized town
df_A4_i_medium_sized_town <- select(filter(df_A4_i, regiostar17 == 113),
                            c(H_ID,state,decile,state_name,state_abb,regiostar17,additional_time,additional_time_month,h_inco,commute_car_journey,commute_car_month,altern_public_trans_month,adults_hh))
df_A4_i_medium_sized_town$legend = "Medium-sized town"
#Urban region
df_A4_i_urban_region <- select(filter(df_A4_i, regiostar17 == 114),
                             c(H_ID,state,decile,state_name,state_abb,regiostar17,additional_time,additional_time_month,h_inco,commute_car_journey,commute_car_month,altern_public_trans_month,adults_hh))
df_A4_i_urban_region$legend = "Urban region"
#Rural region
df_A4_i_rural_region <- select(filter(df_A4_i, regiostar17 == 115),
                             c(H_ID,state,decile,state_name,state_abb,regiostar17,additional_time,additional_time_month,h_inco,commute_car_journey,commute_car_month,altern_public_trans_month,adults_hh))
df_A4_i_rural_region$legend = "Rural region"

#stacking the datasets
df_A4_i_stack1 <- Stack(df_A4_i_Metropolis,df_A4_i_large_city)
df_A4_i_stack2 <- Stack(df_A4_i_stack1,df_A4_i_medium_sized_town)
df_A4_i_stack3 <- Stack(df_A4_i_stack2,df_A4_i_urban_region)
df_A4_i_stack <- Stack(df_A4_i_stack3,df_A4_i_rural_region)

df_A4_i_stack$legend <- factor(df_A4_i_stack$legend, levels=c("Metropolis", "Large city",  "Medium-sized town","Urban region","Rural region"))

#plot timeloss across deciles per individual and day
ggplot(df_A4_i_stack %>% filter(!is.na(decile)), aes(y = additional_time, x = decile)) + geom_boxplot(aes(), outlier.shape = NA, fill = c("#459194")) +
  scale_y_continuous(limits = c(-40,85), breaks = c(-25,0,25,50,75)) + scale_x_discrete(breaks=c("1","2","3","4","5","6","7","8","9","10"), labels=c("1 \n Poorest \n 10%", "2", "3","4", "5", "6", "7", "8", "9", "10 \n Richest \n 10%")) +
  xlab("Income decile") + ylab("Time loss per journey (minutes)\n") +
  #ggtitle("Time loss through combustion engine ban \nfor different regions in metropolitan area") +
  theme(plot.title=element_text(color="black", size = 10), axis.text.x = element_text(angle = 0, hjust = .5, size = 18), axis.text.y=element_text(size=18),axis.title = element_text(size=20)) + theme(legend.position = "bottom") +
  theme(legend.text=element_text(size=7)) + theme(legend.title = element_blank()) + stat_summary(fun.y=mean, geom="point", aes(), position = position_dodge(width = .75))
#ggsave(path = "Boxplot, graphs and figures", filename = "Combustion ban_sensitivity_timeloss indiv.png")

#plot timeloss across deciles per individual and day
ggplot(df_A4_i_stack %>% filter(!is.na(decile)), aes(y = additional_time, x = decile)) + geom_boxplot(aes(fill =legend), outlier.shape = NA) +
  scale_y_continuous(limits = c(-40,90)) + scale_x_discrete() + xlab("Income decile") + ylab("time loss (minutes)") +
  #ggtitle("Time loss through combustion engine ban \nfor different regions in metropolitan area") +
  theme(plot.title=element_text(color="black", size = 10), axis.text.x = element_text(angle = 0, hjust = 1, size = 8), axis.text.y=element_text(size=8)) + theme(legend.position = "bottom") +
  theme(legend.text=element_text(size=7)) + theme(legend.title = element_blank()) + 
  scale_fill_manual(values = c("deeppink4","red3","coral3","coral2","coral1"))

ggplot(df_A4_i_stack, aes(y = additional_time, x = h_inco)) + geom_point(size = 0.1) + geom_smooth(method = "lm")

#plot timeloss across deciles per individual and month
ggplot(df_A4_i_stack %>% filter(!is.na(decile)), aes(y = additional_time_month, x = decile)) + geom_boxplot(aes(fill =legend), outlier.shape = NA) +
  scale_y_continuous(limits = c(-2000,4000)) + scale_x_discrete() + xlab("Income decile") + ylab("time loss (minutes)") +
  #ggtitle("Time loss through combustion engine ban \nfor different regions in metropolitan area") +
  theme(plot.title=element_text(color="black", size = 10), axis.text.x = element_text(angle = 0, hjust = 1, size = 8), axis.text.y=element_text(size=8)) + theme(legend.position = "bottom") +
  theme(legend.text=element_text(size=7)) + theme(legend.title = element_blank()) + 
  scale_fill_manual(values = c("deeppink4","red3","coral3","coral2","coral1"))
#get rid of scientific notation (makes outputs easier to read)
options(scipen = 999)
#running linear models to see what is driving time loss
mod_A4_i_1 <- lm(additional_time ~ h_inco, dat=df_A4_i_stack)
summary(mod_A4_i_1)

mod_A4_i_2 <- lm(additional_time ~ h_inco + legend, dat=df_A4_i_stack)
summary(mod_A4_i_2)

mod_A4_i_3 <- lm(commute_car_journey ~ h_inco, dat=df_A4_i_stack)
summary(mod_A4_i_3)

#plot commuting time across deciles per individual and day
ggplot(df_A4_i_stack %>% filter(!is.na(decile)), aes(y = commute_car_journey, x = decile)) + geom_boxplot(aes(), outlier.shape = NA, fill = c("dodgerblue2")) +
  scale_y_continuous(limits = c(0,90)) + scale_x_discrete() + xlab("Income decile") + ylab("commuting time (minutes)") +
  #ggtitle("Time loss through combustion engine ban \nfor different regions in metropolitan area") +
  theme(plot.title=element_text(color="black", size = 10), axis.text.x = element_text(angle = 0, hjust = 1, size = 8), axis.text.y=element_text(size=8)) + theme(legend.position = "bottom") +
  theme(legend.text=element_text(size=7)) + theme(legend.title = element_blank()) + stat_summary(fun.y=mean, geom="point", aes(), position = position_dodge(width = .75))

#plot additional time across deciles per individual and day
ggplot(df_A4_i_stack %>% filter(!is.na(decile)), aes(y = additional_time, x = legend)) + geom_boxplot(fill = c("deeppink4","red3","coral3","coral2","coral1"), outlier.shape = NA, fill = c("dodgerblue2")) +
  scale_y_continuous(limits = c(-50,100)) + scale_x_discrete() + ylab("Time loss per journey (minutes)\n") +
  #ggtitle("Time loss through combustion engine ban \nfor different regions in metropolitan area") +
  theme(plot.title=element_text(color="black", size = 10), axis.text.x = element_text(angle = 45, hjust = 1, size = 18), axis.text.y=element_text(size=18),axis.title = element_text(size=20)) + theme(legend.position = "bottom") +
  theme(legend.text=element_text(size=7)) + theme(legend.title = element_blank()) + stat_summary(fun.y=mean, geom="point", aes(), position = position_dodge(width = .75))+
  theme(axis.title.x = element_blank())
#ggsave(path = "Boxplot, graphs and figures", filename = "Combustion ban_sensitivity_timeloss indiv_per settlement.png")

#A4 End#######
#A5 Start ########
#Creating a simplified bonus/malus system
summary(dat_MiD$gramsco2km)
ggplot(data=dat_MiD, aes(x=gramsco2km)) + geom_histogram(binwidth = 10,colour="#FF9999") + xlab("grams CO2/km") +ylab("frequency") + ggtitle("Distribution of grams CO2/km") + theme(plot.title=element_text(color="black", size = 10))
tapply(dat_MiD$gramsco2km, dat_MiD$A_BAUJ, summary)
#checking to see if there is enough data to do an analysis of >=2014
count_if(dat_MiD$A_BAUJ>=2014,dat_MiD$A_BAUJ)

#making variable grams of co2km for just 2014 cars and newer - as this is when the 95gram rule applied for the KFZ Steuer
#and ostensibly car owners were therefore making decisions under the same policy frameworks
dat_MiD$gramsco2km_2014_plus <- ifelse(dat_MiD$A_BAUJ >= 2014, dat_MiD$gramsco2km, NA)
#checking distribution (if text is set really big it is because I was working with the monitor so when it saves and shrinks the text would get small Otherwise)
#also put xlim to 75 to get a more normal looking graph
ggplot(data=dat_MiD, aes(x=gramsco2km_2014_plus)) + geom_histogram(binwidth = 3,colour="#C4DFE6", fill = "#003B46") + xlab(expression(Grams~CO[2]/km)) +ylab("Frequency") + ggtitle("") +xlim(c(75,250))  +
theme(plot.title=element_text(color="black", size = 10), axis.text.x = element_text(size = 18), axis.title = element_text(size=18), axis.text.y=element_text(size=18))
#ggsave(path = "Boxplot, graphs and figures", filename = "distribution of 2014 cars and newer.png")
summary(dat_MiD$gramsco2km_2014_plus)
#creating new df that is much smaller to facilitate the bonus malus calculations

#putting gramsco2km 2014 in bins
dat_MiD <- mutate(dat_MiD,gramsco2km_bonus_malus_grouped_target_value = case_when(gramsco2km_2014_plus >= 226 & gramsco2km_2014_plus <= 250 ~ "9",
                                                                                  gramsco2km_2014_plus >= 201 & gramsco2km_2014_plus <= 225 ~ "8",
                                                                                  gramsco2km_2014_plus >= 176 & gramsco2km_2014_plus <= 200 ~ "7",
                                                                                  gramsco2km_2014_plus >= 146 & gramsco2km_2014_plus <= 175 ~ "6",
                                                                                  gramsco2km_2014_plus >= 116 & gramsco2km_2014_plus <= 145 ~ "5",
                                                                                  gramsco2km_2014_plus >= 96  & gramsco2km_2014_plus <= 115 ~ "4",
                                                                                  gramsco2km_2014_plus >= 76  & gramsco2km_2014_plus <= 95  ~ "3",
                                                                                  gramsco2km_2014_plus >= 50  & gramsco2km_2014_plus <= 75  ~ "2",
                                                                                  gramsco2km_2014_plus >=  0  & gramsco2km_2014_plus <= 49  ~ "1"))
CrossTable(dat_MiD$gramsco2km_bonus_malus_grouped_target_value)


#efficient groups are too small - redoing it just to center around mean no target value
dat_MiD <- mutate(dat_MiD,gramsco2km_bonus_malus_grouped_no_target = case_when(   gramsco2km_2014_plus >= 230.0001 & gramsco2km_2014_plus <= 250 ~ "9",
                                                                                  gramsco2km_2014_plus >= 215.0001 & gramsco2km_2014_plus <= 230 ~ "8",
                                                                                  gramsco2km_2014_plus >= 200.0001 & gramsco2km_2014_plus <= 215 ~ "7",
                                                                                  gramsco2km_2014_plus >= 190.0001 & gramsco2km_2014_plus <= 200 ~ "6",
                                                                                  gramsco2km_2014_plus >= 160.0001 & gramsco2km_2014_plus <= 190 ~ "5",
                                                                                  gramsco2km_2014_plus >= 140.0001 & gramsco2km_2014_plus <= 160 ~ "4",
                                                                                  gramsco2km_2014_plus >= 100.0001 & gramsco2km_2014_plus <= 140 ~ "3",
                                                                                  gramsco2km_2014_plus >= 75.0001  & gramsco2km_2014_plus <= 100 ~ "2",
                                                                                  gramsco2km_2014_plus >=  0     & gramsco2km_2014_plus <= 75  ~ "1"))
CrossTable(dat_MiD$gramsco2km_bonus_malus_grouped_no_target)
#creating payment steps - values were chosen through inputting the first 3 steps to each side and then performing goal-seek to bring the sum to 0 in excel to determine the last step
dat_MiD$feebate <- 0
dat_MiD$feebate <- ifelse(dat_MiD$gramsco2km_bonus_malus_grouped_no_target==9, dat_MiD$feebate +  1800, 
                   ifelse(dat_MiD$gramsco2km_bonus_malus_grouped_no_target==8, dat_MiD$feebate +  1600,
                   ifelse(dat_MiD$gramsco2km_bonus_malus_grouped_no_target==7, dat_MiD$feebate +  1400,
                   ifelse(dat_MiD$gramsco2km_bonus_malus_grouped_no_target==6, dat_MiD$feebate +  1200,
                   ifelse(dat_MiD$gramsco2km_bonus_malus_grouped_no_target==5, dat_MiD$feebate -    0,
                   ifelse(dat_MiD$gramsco2km_bonus_malus_grouped_no_target==4, dat_MiD$feebate -  1200,
                   ifelse(dat_MiD$gramsco2km_bonus_malus_grouped_no_target==3, dat_MiD$feebate -  1400,
                   ifelse(dat_MiD$gramsco2km_bonus_malus_grouped_no_target==2, dat_MiD$feebate -  1600,
                   ifelse(dat_MiD$gramsco2km_bonus_malus_grouped_no_target==1, dat_MiD$feebate -  1800,
                   NA)))))))))
#Checking to see if the policy is more or less revenue neutral
CrossTable(dat_MiD$feebate)
sum(dat_MiD$feebate,na.rm=TRUE)
# -> Yes, only taking €16000 more in than giving out (peanuts compared to overall money exchange of >11,000,000)
#grouping by household 
g_HH <- group_by(dat_MiD, H_ID)
#adding new simple DF for feebate analysis
df_bonus_malus <- summarise(g_HH,
                            h_inco = unique(H_EINK_0RM),
                            h_total_cost_feebate =sum(feebate))
#adding decile
df_bonus_malus <- mutate(df_bonus_malus, decile = ntile(df_bonus_malus$h_inco,10))
#adding legend for ggplot
df_bonus_malus$legend = "Step scheme"
#making decile categorical for boxplot
df_bonus_malus$decile <- factor(df_bonus_malus$decile)

#boxplot bonus malus 
ggplot(df_bonus_malus %>% filter(!is.na(decile)), aes(y = h_total_cost_feebate, x = decile)) + geom_boxplot(aes(fill =legend), outlier.shape = NA) +
  scale_y_continuous(limits = c(-4000,4000),breaks = c(-4000,-2000,0,2000,4000)) + scale_x_discrete(limits= c(1:10)) + xlab("Income decile") + ylab("Household Feebate costs (€)") +
  ggtitle("") + theme(plot.title=element_text(color="black", size = 10),
  axis.text.x = element_text(angle = 0, hjust = 1, size = 8), axis.text.y=element_text(size=8)) + theme(legend.title = element_blank())+ theme(legend.position = "bottom") +
  scale_fill_manual(values = c("#DDA0DD"))
#ggsave(path = "Boxplot, graphs and figures", filename = "Household feebate costs by decile.png") 

##Looking to see how this would translate as share of hosuehold income
#ading household income variable
df_bonus_malus <- mutate(df_bonus_malus, h_inc.share = (h_total_cost_feebate/(h_inco*12))*100)

#boxplot
ggplot(df_bonus_malus %>% filter(!is.na(decile)), aes(y = h_inc.share, x = decile)) + geom_boxplot(aes(fill =legend), outlier.shape = NA) +
  scale_y_continuous(limits = c(-15,15),breaks = c(-15,-10,-5,0,5,10,15)) + scale_x_discrete(limits= c(1:10)) + xlab("Income decile") + ylab("Share of household income (%)") +
  ggtitle("") + theme(plot.title=element_text(color="black", size = 10),
  axis.text.x = element_text(angle = 0, hjust = 1, size = 8), axis.text.y=element_text(size=8)) + theme(legend.title = element_blank())+ theme(legend.position = "bottom") +
  scale_fill_manual(values = c("#DDA0DD"))
#ggsave(path = "Boxplot, graphs and figures", filename = "Feebates as share of income.png")

#creating step graph plot
ggplot(data=dat_MiD, aes(x=gramsco2km_2014_plus)) + geom_step(aes(gramsco2km_2014_plus, y=feebate)) + scale_y_continuous(limits = c(-2000,2000), breaks = c(-2000.,-1500,-1000,-500,0,500,1000,1500,2000)) +xlab(expression(Grams~CO[2]/km)) + ylab("Feebate amount (€)\n") +
  theme(plot.title=element_text(color="black", size = 12), axis.text.x = element_text(angle = 0, hjust = .5, size = 18), axis.text.y=element_text(size=18),axis.title = element_text(size=20))
#ggsave(path = "Boxplot, graphs and figures", filename = "Feebate policy step graph.png")
###creating alternative feebate system with marginal taxation
#creating marginal tax, Tax amount chosen as 40 eur per gram of CO2 deviation from pivot point (chosen as this equates costs (overall money paid out and in both equal ~(-)11,000,000) to Other policy making them comparable) - pivot point is set at 175.445 to create a revenue neutral tax
dat_MiD$feebate_margin <- NA
dat_MiD$feebate_margin <- 40*(dat_MiD$gramsco2km_2014_plus-175.445)
#checking to see if policy is revenue neutral
dat_MiD$feebate_margin <- as.numeric(dat_MiD$feebate_margin)
sum(dat_MiD$feebate_margin,na.rm = TRUE)
#Yes -> Gov pays out around 2000 more than they take in, getting ths number to 0 would require more than 4 decimals at pivot point
#simple line plot shows linear function
ggplot(data=dat_MiD, aes(x=gramsco2km_2014_plus))+ geom_line(aes(x=gramsco2km_2014_plus, y=feebate_margin)) + scale_y_continuous(limits = c(-8000, 4000),breaks = c(-8000,-6000,-4000,-2000,0,2000,4000)) +xlab(expression(Grams~CO[2]/km)) + ylab("Feebate amount (€)\n") +
  theme(plot.title=element_text(color="black", size = 12), axis.text.x = element_text(angle = 0, hjust = .5, size = 18), axis.text.y=element_text(size=18),axis.title = element_text(size=20))
#ggsave(path = "Boxplot, graphs and figures", filename = "continuous feebate scheme.png")
#grouping by household 
g_HH <- group_by(dat_MiD, H_ID)
#adding new simple DF for feebate analysis
df_bonus_malus_marginal <- summarise(g_HH,
                                     h_inco = unique(H_EINK_0RM),
                                     h_total_cost_feebate =sum(feebate_margin))
#adding decile
df_bonus_malus_marginal <- mutate(df_bonus_malus_marginal, decile = ntile(df_bonus_malus_marginal$h_inco,10))
#adding legend for ggplot
df_bonus_malus_marginal$legend = "Continuous scheme"
#making decile categorical for boxplot
df_bonus_malus_marginal$decile <- factor(df_bonus_malus_marginal$decile)

#boxplot bonus malus 
ggplot(df_bonus_malus_marginal %>% filter(!is.na(decile)), aes(y = h_total_cost_feebate, x = decile)) + geom_boxplot(aes(fill =legend), outlier.shape = NA) +
  scale_y_continuous(limits = c(-4000,4000), breaks = c(-4000,-2000,-0,2000,4000)) + scale_x_discrete(limits= c(1:10)) + xlab("Income decile") + ylab("Household Feebate costs (€)") +
  ggtitle("") + theme(plot.title=element_text(color="black", size = 10),
  axis.text.x = element_text(angle = 0, hjust = 1, size = 8), axis.text.y=element_text(size=8)) + theme(legend.title = element_blank())+ theme(legend.position = "none") +
  scale_fill_manual(values = c("#D8BFD8"))
#ggsave(path = "Boxplot, graphs and figures", filename = "Household feebate costs by decile (marginal system).png") 

##Looking to see how this would translate as share of hosuehold income
#adding household income variable
df_bonus_malus_marginal <- mutate(df_bonus_malus_marginal, h_inc.share = (h_total_cost_feebate/(h_inco*12))*100)

#boxplot of marginal feebate scheme
ggplot(df_bonus_malus_marginal %>% filter(!is.na(decile)), aes(y = h_inc.share, x = decile)) + geom_boxplot(aes(fill =legend), outlier.shape = NA) +
  scale_y_continuous(limits = c(-20,10),breaks = c(-20,-15,-10,-5,0,5,10)) + scale_x_discrete(limits= c(1:10)) + xlab("Income decile") + ylab("Share of household income (%)") +
  ggtitle("") + theme(plot.title=element_text(color="black", size = 10),
  axis.text.x = element_text(angle = 0, hjust = 1, size = 8), axis.text.y=element_text(size=8)) + theme(legend.title = element_blank())+ theme(legend.position = "none") +
  scale_fill_manual(values = c("#D8BFD8"))
#ggsave(path = "Boxplot, graphs and figures", filename = "Feebates as share of income (marginal system).png")

#stacking the two scehemes for comparison
df_stack_feebate <-Stack(df_bonus_malus, df_bonus_malus_marginal)
#plot comparing household share of income
ggplot(df_stack_feebate %>% filter(!is.na(decile)), aes(y = h_inc.share, x = decile)) + geom_boxplot(aes(fill =legend), outlier.shape = NA) + stat_summary(fun.y=mean, geom="point", aes(group=legend), position = position_dodge(width = .75)) +
  scale_y_continuous(limits = c(-20,10),breaks = c(-20,-15,-10,-5,0,5,10)) + scale_x_discrete(breaks=c("1","2","3","4","5","6","7","8","9","10"), labels=c("1 \n Poorest \n 10%", "2", "3","4", "5", "6", "7", "8", "9", "10 \n Richest \n 10%")) + xlab("Income decile") + ylab("Share of household income (%)\n")  +
  ggtitle("") + theme(plot.title=element_text(color="black", size = 10),
  axis.text.x = element_text(angle = 0, hjust = .5, size = 18), axis.text.y=element_text(size=18), axis.title = element_text(size=20)) + theme(legend.text = element_text(size = 16)) + theme(legend.title = element_blank())+ theme(legend.position = "bottom") +
  scale_fill_manual(values = c("#07575B","#66A5AD")) +  geom_hline(yintercept = 0, linetype="dotted", color = "black", size=0.5)
#ggsave(path = "Boxplot, graphs and figures", filename = "Step-continuous comparison % of income.png")

#plot comparing aggregate households costs
ggplot(df_stack_feebate %>% filter(!is.na(decile)), aes(y = h_total_cost_feebate, x = decile)) + geom_boxplot(aes(fill =legend), outlier.shape = NA) + stat_summary(fun.y=mean, geom="point", aes(group=legend), position = position_dodge(width = .75)) +
  scale_y_continuous(limits = c(-4000,4000), breaks = c(-4000,-2000,0,2000,4000)) + scale_x_discrete(breaks=c("1","2","3","4","5","6","7","8","9","10"), labels=c("1 \n Poorest \n 10%", "2", "3","4", "5", "6", "7", "8", "9", "10 \n Richest \n 10%")) + xlab("Income decile") + ylab("Household Feebate costs (€)\n")  +
  ggtitle("") + theme(plot.title=element_text(color="black", size = 10),
  axis.text.x = element_text(angle = 0, hjust = .5, size = 18), axis.text.y=element_text(size=18), axis.title = element_text(size=20)) + theme(legend.text = element_text(size = 16)) + theme(legend.title = element_blank())+ theme(legend.position = "bottom") +
  scale_fill_manual(values = c("#07575B","#66A5AD")) + geom_hline(yintercept = 0, linetype="dotted", color = "black", size=0.5)
#ggsave(path = "Boxplot, graphs and figures", filename = "Step-continuous comparison houeshold costs.png")

#revaluing and re-arranging a little so that the data can be used to make a bar chart of electric vehicle ownership by decile with geom_bar
tapply(dat_MiD$A_ANTRIEB==5, dat_MiD$decile, summary)
dat_MiD$antrieb_e <- NA
dat_MiD$antrieb_e <- ifelse(dat_MiD$A_ANTRIEB==5,dat_MiD$A_ANTRIEB, NA)
dat_MiD$antrieb_e <- as.factor(dat_MiD$antrieb_e)
dat_MiD$antrieb_e <- revalue(dat_MiD$antrieb_e, c("5"="1"))

#bar graph for electric vehicle ownership by decile
dat_MiD$decile_antrieb_e <- ifelse(!is.na(dat_MiD$antrieb_e),dat_MiD$decile, NA)
ggplot(data=dat_MiD,  aes(x=decile_antrieb_e)) +
  geom_bar(stat="count", alpha =.75, fill=c("#4682B4"))  + scale_y_continuous(limits = c(0,80), breaks = c(0,20,40,60,80))  +xlab("Income decile") + ylab("Number of electric vehicles\n") + 
  scale_x_discrete(limits = c (1:10), labels=c("1 \n Poorest \n 10%", "2", "3","4", "5", "6", "7", "8", "9", "10 \n Richest \n 10%")) +
  theme(axis.text.x = element_text(angle = 0, hjust = .5, size = 18), axis.text.y=element_text(size=18), axis.title = element_text(size=20))
#ggsave(path = "Boxplot, graphs and figures", filename = "Distribution of electric cars by decile.png")
#A5 End ########

#If you're reading this, you read through our entire code and we applaud you for making it this far.
#We were first time coders so we hope it was clear enough and not too messy.
#We hope the code worked for you, you were able to adapt it to other research needs,
#and that you were able to enjoy the fruits of our labour.