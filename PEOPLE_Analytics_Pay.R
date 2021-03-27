library(readr)
library(dplyr)
library(skimr)
library(ggplot2)
library(infer)
library(broom)

# Compensation Analysis: ----

## Data
pay <- read_csv("Data/fair_pay_data.csv")

## Exploratory Data Analysis 

# - summary
pay %>% skimr::skim()
pay %>% glimpse()
pay %>% count(department)
pay %>% count(job_level)
pay %>% count(new_hire)

### Visual: Avg Salary By New Hire
pay %>% 
  group_by(new_hire) %>% 
  summarise(avg_salary = mean(salary)) %>% 
  ggplot(aes(new_hire, avg_salary)) +
  geom_col() + geom_hline(yintercept = 74142) +
  ggtitle("Are new hires being paid more ?")
# - Stat test: 
pay %>% t_test(salary ~ new_hire)  # Significant

### Visual: Count New Hire By Job Level
pay %>% 
  ggplot(aes(new_hire, fill = job_level)) +
  geom_bar(position = "fill")

### Visual: Avg Salary By New Hire ~ Controlling for Job Level
pay %>% 
  group_by(new_hire, job_level) %>% 
  summarise(avg_salary = mean(salary)) %>% 
  ggplot(aes(new_hire, avg_salary)) +
  geom_col() +
  facet_wrap(~ job_level) +
  ggtitle("Are new hires being paid more ~ Contolling for Job Level ?")
# - stat test: Hourly
pay %>% filter(job_level == "Hourly") %>% t_test(salary ~ new_hire) # Not Significant
# - stat test: JobLevel
lm(salary ~ new_hire + job_level, data = pay) %>% tidy()
# -- New Hire: Not Significant
# -- JobLevel = Manager: Significant
# -- JObLevel = Salaried: Significant


# Market Survey: ----

## Data
md <- read_csv("Data/MarketSurveyData.csv")
# - clean
md <- md %>% select(Job_ID, Job_Title, 
                    Survey_1_Hourly = Survey_1, Survey_2_Hourly = Survey_2, Survey_3_Hourly = Survey_3,
                    Sample_Size_1 = SS_Survey_1, Sample_Size_2 = SS_Survey_2, Sample_Size_3 = SS_Survey_3)

## Exploratory Data Analysis
# - summary
md %>% skimr::skim()
md %>% glimpse()

# - Age Data
# -- 1: Identify annual wage/salary budget growth projections ( 3.1% )
abg <- 0.031
# -- 2: Monthly Growth Projection ( abg / 12 )
mbg <- abg / 12
# -- 3: Calculate Aging Factor ( 7 months )
af <-  ( mbg * 7 ) + 1
# -- 4: Age Data
md <- md %>% 
  mutate(Survey_1_Aged = Survey_1_Hourly * af,
         Survey_2_Aged = Survey_2_Hourly * af,
         Survey_3_Aged = Survey_3_Hourly * af)

# - Feature Engineering
md <- md %>% 
  mutate(Weighted_Mean = ( ( Survey_1_Aged * Sample_Size_1 ) +
                             ( Survey_2_Aged * Sample_Size_2 ) +
                             ( Survey_3_Aged * Sample_Size_3 ) ) /
           ( Sample_Size_1 + Sample_Size_2 + Sample_Size_3))


# Compa Ratio: ---- 

## Data
personalData <- read_csv("Data/PData.csv")
compData <- read_csv("Data/CData.csv")
# - leftjoin
mergeData <- left_join(personalData, compData, by = "EmployeeID")
# - clean: Date Format
mergeData <- mergeData %>% mutate(DateofHire = as.Date(DateofHire, format = "%m/%d/%Y"))

## Exploratory Data Analysis
# - summary
mergeData %>% skimr::skim()
mergeData %>% count(Supervisor)
mergeData %>% count(Sex)
mergeData %>% count(Race)
mergeData %>% count(Ethnicity)
mergeData %>% count(Disability)
mergeData %>% count(Veteran)
# - feature Engineering:
mergeData <- mergeData %>% 
  mutate(TotalPay2016 = BaseAnnualPay2016 + VariablePay2016,
         CompaRatio = ( TotalPay2016 / median(TotalPay2016) ) * 100,
         Tenure = as.numeric(as.Date("2018-12-31") - DateofHire),
         Tenure_Yrs = Tenure / 365)
# - visaul: Compa Ratio By Tenure
mergeData %>% 
  ggplot(aes(Tenure_Yrs, CompaRatio)) +
  geom_point(alpha = 0.3) + 
  geom_hline(yintercept = 100, color = "red") +
  ggtitle("Compa Ratio ~ Tenure")


# Market Pay Line: ----

## Data
mp <- read_csv("Data/MarketPayLine.csv")
# - clean: Readable Names
mp <- mp %>% mutate(Job_Family = ifelse(Job_Family == "OA", "Office", "Nurse"))

## Exploratory Data Analysis
# - summary
mp %>% skimr::skim()
mp %>% glimpse()
mp %>% count(Job_Family)

# - visual: Market Pay
mp %>% 
  ggplot(aes(Points, Pay, color = Job_Title)) +
  geom_point() +
  ggtitle("Market Pay")
# - stat tests: 
lm(Pay ~ Points, data = mp) %>% glance() %>% select(r.squared)
lm(Pay ~ Points, data = mp) %>% plot(which = 1)
lm(Pay ~ Points, data = mp) %>% plot(which = 4)
lm(Pay ~ poly(Points, 2), data = mp) %>% glance() %>% select(r.squared)
lm(Pay ~ poly(Points, 2), data = mp) %>% plot(which = 1)
lm(Pay ~ poly(Points, 2), data = mp) %>% plot(which = 4)

## Model
# - fit
mod <- lm(Pay ~ poly(Points, 2), data = mp)

# - visual: Market Pay Line
mod %>% augment() %>% 
  mutate(fit = .fitted,
         min_Pay = fit / ( 1 + .40/2 ),
         max_Pay = fit / ( 1 - .40 )) %>% 
  left_join(mp, by = "Pay") %>% 
  ggplot(aes(Points, Pay)) +
  geom_point() +
  geom_line(aes(Points, fit), color = "green") +
  geom_ribbon(aes(ymin = min_Pay, ymax = max_Pay),
              color = "grey", alpha = 0.25) +
  ggtitle("Market Pay Line")


# Pay Determinants: ----

# Data
pd <- read_csv("Data/PayDeterminants.csv")
# - clean: Factors
pd <- pd %>% 
  mutate(educ = factor(educ),
         sex = factor(sex),
         race = factor(race))

# Exploratory Data Analysis
# - summary
pd %>% skimr::skim()
pd %>% glimpse()

## Visual: Raise By Education (Protected Feature)
pd %>% 
  ggplot(aes(perf, raise)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE,
              aes(color = educ)) +
  ggtitle("Performance ~ Raise (Education)")
# - stat test
pd %>% lm(raise ~ perf + educ, data = .) %>% tidy()

## Visual: Raise By Sex (Protected Feature)
pd %>% 
  ggplot(aes(perf, raise)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE,
              aes(color = sex)) +
  ggtitle("Performance ~ Raise (Sex)")
# - stat test
pd %>% lm(raise ~ perf + sex, data = .) %>% tidy()

## Visual: Raise By Raise (Protected Feature)
pd %>% 
  ggplot(aes(perf, raise)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE,
              aes(color = race)) +
  ggtitle("Performance ~ Raise (Race)")
# - stat test:
pd %>% lm(raise ~ perf + race, data = .) %>% tidy()

# Modeling
# - fit
mod <- lm(raise ~ perf + educ, data = pd)
mod_Protected_Class <- lm(raise ~ perf + educ + race + sex, data = pd)
# - performance
mod %>% glance() %>% select(r.squared)
mod_Protected_Class %>% glance() %>% select(r.squared)
anova(mod, mod_Protected_Class) %>% tidy()

# Incremental Variance Explained
mod_Protected_Class %>% glance() %>% select(r.squared) -
  mod %>% glance() %>% select(r.squared)
