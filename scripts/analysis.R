
  
   
library(tidyverse)
refactored_wages <- wages <- read_csv("~/Wages/data/wages.csv")

skimr::skim(wages)
  

# Exploratory Data Analysis

 
head(wages)
  
# wages by union
wages |> 
  group_by(union, gender) |> 
  summarize(avg_wage = mean(wage)) |> 
  ggplot(aes(reorder(union, avg_wage), avg_wage, colour = union)) +
  geom_boxplot() +
  labs(
    x = "Union",
    y = "Average Wage",
    title = "Average Wages By Union"
  )

# wages by occupation
wages |> 
  group_by(occupation, gender) |> 
  summarize(avg_wage = mean(wage)) |> 
  ggplot(aes(reorder(occupation, avg_wage), avg_wage, colour = occupation)) +
  geom_boxplot() +
  labs(
    x = "Occupation",
    y = "Average Wage",
    title = "Average wage by occupation"
  )
  

 
#wages by gender
wages |> 
  group_by(gender, occupation) |> 
  summarize(avg_wage = mean(wage)) |> 
  ggplot(aes(reorder(gender, avg_wage), avg_wage, colour = gender)) +
  geom_boxplot() +
  labs(
    x = "Gender",
    y = "Average Wage",
    title = "Average Wage By Gender"
  )


#wages by sector
wages |> 
  group_by(sector, gender) |> 
  summarize(avg_wage = mean(wage)) |> 
  ggplot(aes(reorder(sector, avg_wage), avg_wage, colour = sector)) +
  geom_boxplot() +
  labs(
    x = "Sector",
    y = "Average Wage",
    title = "Average Wages By Sector"
  )

#wages by ethnicity
wages |> 
  group_by(gender, ethnicity) |> 
  summarize(avg_wage = mean(wage)) |> 
  ggplot(aes(reorder(ethnicity, avg_wage), avg_wage, colour = ethnicity)) +
  geom_boxplot() +
  labs(
    x = "Ethnicity",
    y = "Average Wage",
    title = "Average Wages By Ethnicity"
  )

  

 
#wages by married
wages |> 
  group_by(gender, married) |> 
  summarize(avg_wage = mean(wage)) |> 
  ggplot(aes(reorder(married, avg_wage), avg_wage, colour = married)) +
  geom_boxplot() +
  labs(
    x = "Marital Status",
    y = "Average Wage",
    title = "Average Wage By Marital Status"
  )
  

 
#correlation between numeric variables 
library(corrr)
wages_cor <- wages |> 
  select(1:4) |> 
  correlate()
wages_cor
  

 
rplot(wages_cor)
  

# Statistical Analysis

#t-test on married workers
t.test(wage ~ married, data = wages)
#t-test on gender
t.test(wage ~ gender, data = wages)
#t-test on region
t.test(wage ~ region, data = wages)

#anova on ethnicity
summary(aov(wage ~ ethnicity, data = wages))
#anova on occupation
summary(aov(wage ~ occupation, data = wages))
#anova on sector
summary(aov(wage ~ sector, data = wages))
# SEM Analysis
 
#Data cleaning for regression
refactored_wages$gender <- recode(refactored_wages$gender, 
                                  female = 0, 
                                  male = 1)
refactored_wages$ethnicity <- recode(refactored_wages$ethnicity, 
                                     hispanic = 1, 
                                     cauc = 2,
                                     other = 3)
refactored_wages$region <- recode(refactored_wages$region, 
                                  other = 0, 
                                  south = 1)
refactored_wages$occupation <- recode(refactored_wages$occupation, 
                                      worker = 1, 
                                      management = 2,
                                      sales = 3,
                                      office = 4,
                                      services = 5,
                                      technical = 6)
refactored_wages$sector <- recode(refactored_wages$sector, 
                                  manufacturing = 1, 
                                  other= 2,
                                  construction = 3)
refactored_wages$married <- recode(refactored_wages$married, 
                                   no = 0, 
                                   yes = 1)
refactored_wages$union <- recode(refactored_wages$union, 
                                 no = 0, 
                                 yes = 1)

  
#Multiple Linear Regression
summary(lm(wage ~ education + experience + ethnicity + region + gender + occupation + sector + married + union, refactored_wages))

