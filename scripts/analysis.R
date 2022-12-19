
  
#library installed
library(tidyverse)
library(patchwork)
library(corrr)

refactored_wages <- wages <- read_csv("~/Wages/data/wages.csv")

skimr::skim(wages)
  

# Exploratory Data Analysis

#Descriptive Statistics

prop.table(table(wages$ethnicity))
prop.table(table(wages$region))
prop.table(table(wages$gender))
prop.table(table(wages$occupation))
prop.table(table(wages$sector))
prop.table(table(wages$union))
prop.table(table(wages$married))

head(wages)

#Distribution of wage
q1 <- wages |> 
  ggplot(aes(wage)) +
  geom_density() +
  labs(
    x = "Wage",
    title = "Wages"
  )

#Distribution of experience
q2 <- wages |> 
  ggplot(aes(experience)) +
  geom_density() +
  labs(
    x = "Experience",
    title = "Experience"
  )

#Distribution of education
q3 <- wages |> 
  ggplot(aes(education)) +
  geom_density() +
  labs(
    x = "'Education",
    title = "Education"
  )

#Distribution of age
q4 <- wages |> 
  ggplot(aes(age)) +
  geom_density() +
  labs(
    x = "Age",
    title = "Age"
  )

#combined plots of variables distribution
q0 <- (q1 + q2)/(q3 + q4)
  
# wages by union
p1 <- wages |> 
  group_by(union) |> 
  summarize(avg_wage = mean(wage)) |> 
  ggplot(aes(reorder(union, avg_wage), avg_wage, fill = union)) +
  geom_col() +
  labs(
    x = "Union",
    y = "Average Wage",
    title = "Union"
  )

# wages by occupation
p2 <- wages |> 
  group_by(occupation, gender) |> 
  summarize(avg_wage = mean(wage)) |> 
  ggplot(aes(reorder(occupation, avg_wage), avg_wage, fill = occupation)) +
  geom_col() +
  labs(
    x = "Occupation",
    y = "Average Wage",
    title = "Occupation"
  )
  

 
#wages by gender
p3 <- wages |> 
  group_by(gender, occupation) |> 
  summarize(avg_wage = mean(wage)) |> 
  ggplot(aes(reorder(gender, avg_wage), avg_wage, fill = gender)) +
  geom_col() +
  labs(
    x = "Gender",
    y = "Average Wage",
    title = "Gender"
  )


#wages by sector
p4 <- wages |> 
  group_by(sector, gender) |> 
  summarize(avg_wage = mean(wage)) |> 
  ggplot(aes(reorder(sector, avg_wage), avg_wage, fill = sector)) +
  geom_col() +
  labs(
    x = "Sector",
    y = "Average Wage",
    title = "Sector"
  )

#wages by ethnicity
p5 <- wages |> 
  group_by(gender, ethnicity) |> 
  summarize(avg_wage = mean(wage)) |> 
  ggplot(aes(reorder(ethnicity, avg_wage), avg_wage, fill = ethnicity)) +
  geom_col() +
  labs(
    x = "Ethnicity",
    y = "Average Wage",
    title = "Ethnicity"
  )

  

 
#wages by married
p6 <- wages |> 
  group_by(gender, married) |> 
  summarize(avg_wage = mean(wage)) |> 
  ggplot(aes(reorder(married, avg_wage), avg_wage, fill = married)) +
  geom_col() +
  labs(
    x = "Marital Status",
    y = "Average Wage",
    title = "Marital Status"
  )
  

#wages by region
p7 <- wages |> 
  group_by(region, gender) |> 
  summarize(avg_wage = mean(wage)) |> 
  ggplot(aes(reorder(region, avg_wage), avg_wage, fill = region)) +
  geom_col() +
  labs(
    x = "Region",
    y = "Average Wage",
    title = "Region"
  )


#combined plots of ethnicity distribution
p0 <- (p1 + p2)/(p3 + p7)/(p5 + p6)
 
#correlation between numeric variables 

wages_cor <- wages |> 
  select(1:4) |> 
  correlate()
wages_cor
  
#correlaation plot
rplot(wages_cor)
  

# Statistical Analysis

#t-test on married workers
t.test(wage ~ married, data = wages)
#t-test on gender
t.test(wage ~ gender, data = wages)
#t-test on region
t.test(wage ~ region, data = wages)
#t-test on union
t.test(wage ~ union, data = wages)

#anova on ethnicity, occupation and sector
summary(aov(wage ~ ethnicity + occupation + sector, data = wages))
 
#Data cleaning for regression analysis

#normalization of predictor variable.
refactored_wages$wage <- sqrt(refactored_wages$wage)

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

