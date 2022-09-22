#
# 	R0000100	CASEID	                IDENTIFICATION CODE
#
#       R0304410	ROSENBERG_ESTEEM_SCORE	SELF-ESTEEM SCORE
#
#       R4531700	Q3-31	                R EVER ATTEND HEAD START PROGRAM
#       R4531800	Q3-32	                R ATTEND ANY PRE-SCHOOL (DID NOT ATTEND HEAD START PROGRAM)
#
#       R0214800	SAMPLE_SEX	        SEX OF R
#       R0001800	FAM-6	                AREA OF RESIDENCE AT AGE 14 URBAN/RURAL?
library(tidyverse)
# library(lfe)
library(skimr)
library(stargazer)

## load data
source('preschool-economics-happiness.R')

new_data <- tibble::tibble(new_data)

## basic checks
length(unique(new_data$R0000100)) == nrow(new_data)

## format empirical sample
new_data <- new_data %>%
    dplyr::select(
        # id variables
        i = R0000100,
        # key outcome variables
        self_esteem = R0304410,
        # key explanatory variables
        head_start = R4531700,
        pre_school = R4531800,
        # covariates
        sex = R0214800, # 1 = male, 2 = female
        urban = R0001800 # 1 = city, 2, 3 = non-city
    ) %>%
    dplyr::mutate(
        male = ifelse(sex == 1, 1, 0),
        urban = ifelse(urban == 1, 1, 0)
    )

print(new_data)

## descriptive stats 
skimr::skim(new_data)

## estimation

ols1 <- lm(self_esteem ~ head_start, new_data)
ols2 <- lm(self_esteem ~ head_start + male + urban, new_data)
ols3 <- lm(self_esteem ~ head_start*male + urban, new_data)
ols4 <- lm(self_esteem ~ head_start*urban + male, new_data)

### idea: estimate model on subsets

## results
stargazer::stargazer(ols1, ols2, ols3, ols4, title="Results", align=TRUE, type="text")
