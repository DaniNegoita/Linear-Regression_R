
library(haven)
library(tidyverse)
library(dplyr)
library(ggplot2)

#Sub-directories
dir.create("original")
dir.create("working")
dir.create("scripts")
dir.create("plots")

#Checking working directory
getwd()

#Loading the European Values Study data (Wave 5)
evs <- read_dta("original/ZA7500_v5-0-0.dta")
View(evs)

#Subsetting the variables of interest only for the Italian observations

# Age -> Respondent’s age 
# v225 -> Respondent’s gender identity
# v243_r -> Respondent’s educational level
# v261_ppp -> Household income corrected for the Purchasing Power Parity. categories 
#measured in x1000

names(evs)

evs$country #Italy is coded as 380

evs_italy <- evs %>%
filter(country==380) %>%
select(age,v225,v243_r,v261_ppp) 


#Rename variables
evs_italy <- rename(evs_italy, gndr = v225, educ = v243_r, income = v261_ppp)

#Save the working dataset
save(evs_italy,file = "working/evs_italy.RData")
rm(evs)
------------------------------------------#Data processing#--------------------

                                  #Recode missing values#

unique(evs_italy$age)
evs_italy$age[evs_italy$age>=82] <- NA #recode as missing people that are 82+ old
sum(is.na(evs_italy$age)) #87 cases

unique(evs_italy$gndr) #no missing observations

unique(evs_italy$educ)
evs_italy$educ[evs_italy$educ == -2] <- NA
sum(is.na(evs_italy$educ)) #13 cases

unique(evs_italy$income)
evs_italy$income[evs_italy$income == -2 | evs_italy$income == -1] <- NA
sum(is.na(evs_italy$income)) #647

sum(is.na(evs_italy)) #747 cases in total


                                                #Listwise deletion#

evs_italy <- na.omit(evs_italy) #1556 complete cases


                                     #Data Types#

sapply(evs_italy,class)


                                #Setting to factor Gender and Education
evs_italy$gndr <- factor(evs_italy$gndr,
                         levels = c(1,2),
                         labels = c("Male", "Female"))
class(evs_italy$gndr) #Nominal

evs_italy$educ <- factor(evs_italy$educ,
                         levels = c(1,2,3),
                         labels = c("Lower", "Medium", "Higher"))     

                      
class(evs_italy$educ) #Ordinal

evs_italy$age <- as.numeric(evs_italy$age)
evs_italy$income <- as.numeric(evs_italy$income)



------------------------##Exploratory Data Analysis#----------------
#Frequency table income
table(evs_italy$income)
 #The occurring values are 0.689, 1.139, 1.499, 1.858,2.158, 2.577,
#3.117, 3.566, 5.139, 6.083

#Density plot
plot1 <- ggplot(evs_italy,aes(x=income)) +
  geom_density(linetype = "solid",colour = "brown4", linewidth = 0.9)+
  scale_x_discrete(name ="Income", 
                  limits=c("0.689","1.139","1.499","1.858","2.158", "2.577",
                  "3.117", "3.566", "5.139", "6.083")) +
       theme(axis.text.x = element_text(angle=45),
        panel.background = element_rect(fill = "white"),
          axis.line = element_line(linewidth = 0.5, linetype = "solid", colour = "dimgray")) +
      labs(title = "Income distribution among Italian respondents", 
           y= "")
 plot1 
 ggsave("plots/income_distribution.png", width = 12, height = 8) 
#Heavily right skewed, unimodal distribution: most of the frequencies pile up at the lower end  of the income scale. The small bumps around 2.158 and 2.577 euros
# just indicative of of a few cases that have such incomes (most likely outliers).
 
#Multivariate plot
plot2 <- ggplot(evs_italy, aes(x= educ, y=income)) +
  geom_boxplot(colour = "brown4") +
  facet_wrap(~gndr) +
  scale_y_discrete(name ="Income", 
                   limits=c("0.689","1.139","1.499","1.858","2.158", "2.577",
                            "3.117", "3.566", "5.139", "6.083")) +
  theme(axis.text.x = element_text(angle=45),
        panel.background = element_rect(fill = "white", colour = "grey32",
                                        linewidth  = 0.9, linetype = 1),
        axis.line = element_line(linewidth  = 0.5, linetype = "solid", colour = "dimgray")) +
        ggtitle("Income by educational level across gender categories") +
        labs(x="Education") + 
  theme(plot.title = element_text(size = 10),
                axis.title.x = element_text(size = 8),
                axis.title.y = element_text(size = 8))
plot2
ggsave("plots/income_gndr_educ.png", width = 12, height = 8)

#The median household income is higher among highly educated respondents, followed by medium and low educated.There does not appear to be any significant differences
# across the genders. The distributions are slightly right skewed (= low income) for low- and medium educated men and women. The opposite
# can be observed for highly educated men and women (left skewed = high income). Interestingly, a few outliers detected on the right hand 
#of the distribution (= high income values) for all subgroups, exception made for highly educated men. 


plot3 <- ggplot(evs_italy, aes(x= age, y=income)) +
  geom_smooth(colour = "brown4", span= 0.2, se=FALSE) +
  geom_point()+
  facet_grid(educ~gndr) +
  scale_y_discrete(name ="Income", 
                   limits=c("0.689","1.139","1.499","1.858","2.158", "2.577",
                            "3.117", "3.566", "5.139", "6.083")) +
  theme(axis.text.x = element_text(angle=45),
        panel.background = element_rect(fill = "white", colour = "grey32",
                                        linewidth  = 0.9, linetype = 1),
        axis.line = element_line(size = 0.5, linetype = "solid", colour = "dimgray")) +
  ggtitle("Income by educational level, age, and gender") +
  labs(x="Age") + 
  theme(plot.title = element_text(size = 10),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8))

plot3
ggsave("plots/income_gndr_educ_age.png", width = 12, height = 8)
#Income varies greatly across educational levels (especially, high vs. medium & low) and by age. No differences between men and women. 

                     #LINEAR REGRESSION#

#Grand mean centering Age for a substantive interpretation of the constant
mean(evs_italy$age) #overall sample mean age is 51 years old

evs_italy$age_c <-  evs_italy$age - mean(evs_italy$age)
round(mean(evs_italy$age_c), 5) #practically 0

#Bivariate regression
reg1 <-  lm(income~educ,data = evs_italy)
summary(reg1)
# Intercept -> this is the average monthly household income for a low educated respondent
# Medium and highly educated respondents earn, on average, 656€ and 1443€ more compared to the low educated. 
#The R2 explains 16% of the variance of income in the sample

#Adding gender and age
reg2 <-  lm(income~educ + gndr + age_c,data = evs_italy)
summary(reg2)

#The intercept slightly decreases and stays statistically significant: this is the household income for a low educated man of 50 years old
#The magnitude of the parameters for medium and highly educated increase compared to the previous model: this might be due to the introduction of a suppressor variable.
# Women earn on average less but this result is not bolstered by statistical significance.
# For each year of difference in age, income is estimated to differ by 0.006 euros between 2 randomly selected individuals in the sample.
#The R2 slightly increases to 17%.


#Limitations
# Average age in the sample is 51 years old
# Underrepresentation of highly educated respondents
# Potential model misspecification 


