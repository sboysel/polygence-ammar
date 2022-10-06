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
#
#       R0006500 HGC-MOTHER HIGHEST GRADE COMPLETED BY R'S MOTHER
#       R0007900 HGC-FATHER HIGHEST GRADE COMPLETED BY R'S FATHER
#
#        2006 Survey Variables:
#
#       G0226400 WELFARE-AMT-2006 TOTAL AMOUNT AFDC, FOOD STAMPS OR OTH WELFARE/SSI RECEIVED DURING CAL YR 2006
#       T0899810 ROSENBERG_ESTEEM_SCORE SELF-ESTEEM SCORE
#       T0987600 FAMSIZE FAMILY SIZE
#       T0987800 TNFI_TRUNC TOTAL NET FAMILY INCOME IN PAST CALENDAR YEAR KEY (TRUNCATED)
#       T0987900 POVSTATUS FAMILY POVERTY STATUS IN 2005
#       T0988500 MARSTAT-KEY MARITAL STATUS
#       T0989300 WKSUEMP-PCY NUMBER OF WEEKS UNEMPLOYED IN PAST CALENDAR YEAR KEY
#       T0990400 URBAN-RURAL IS R'S CURRENT RESIDENCE URBAN/RURAL?
#       T0001000 HH1-1 TYPE OF RESIDENCE R IS LIVING IN

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
        # self_esteem = T0899810,
        # key explanatory variables
        head_start = R4531700,
        pre_school = R4531800,
        # covariates
        sex = R0214800, # 1 = male, 2 = female
        urban = R0001800, # 1 = city, 2, 3 = non-city
        married = T0988500,
        educ_mother = R0006500,
        educ_father = R0007900,
        welfare = G0226400,
        fam_size = T0987600,
        fam_income = T0987800,
        fam_poverty = T0987900,
        unemployed = T0989300
        # residence_type = T0001000
    ) %>%
    dplyr::mutate(
        male = ifelse(sex == 1, 1, 0),
        urban = ifelse(urban == 1, 1, 0),
        married = ifelse(married >= 1, 1, 0),
        unemployed = ifelse(unemployed >= 1, 1, 0)
    )

print(new_data)

## descriptive stats 
skimr::skim(new_data)

## estimation

print("Effect of Head Start on self-reported Self Esteem")

ols1 <- lm(self_esteem ~ head_start, new_data)
ols2 <- lm(self_esteem ~ head_start + male + urban + unemployed, new_data)
ols3 <- lm(self_esteem ~ head_start + male + urban + unemployed + married + fam_size, new_data)
ols4 <- lm(self_esteem ~ head_start + male + urban + unemployed + married + fam_size + educ_mother + educ_father, new_data)
ols5 <- lm(self_esteem ~ head_start + male + urban + unemployed + married + fam_size + educ_mother + educ_father + welfare + fam_income + fam_poverty, new_data)
stargazer::stargazer(ols1, ols2, ols3, ols4, ols5, title="Results", align=TRUE, type="text")

print("Effect of Preschool on self-reported Self Esteem")

ols1 <- lm(self_esteem ~ pre_school, new_data)
ols2 <- lm(self_esteem ~ pre_school + male + urban + unemployed, new_data)
ols3 <- lm(self_esteem ~ pre_school + male + urban + unemployed + married + fam_size, new_data)
ols4 <- lm(self_esteem ~ pre_school + male + urban + unemployed + married + fam_size + educ_mother + educ_father, new_data)
ols5 <- lm(self_esteem ~ pre_school + male + urban + unemployed + married + fam_size + educ_mother + educ_father + welfare + fam_income + fam_poverty, new_data)
stargazer::stargazer(ols1, ols2, ols3, ols4, ols5, title="Results", align=TRUE, type="text")

# drop welfare variable

print("Effect of Head Start on self-reported Self Esteem (omit welfare variable)")

ols1 <- lm(self_esteem ~ head_start, new_data)
ols2 <- lm(self_esteem ~ head_start + male + urban + unemployed, new_data)
ols3 <- lm(self_esteem ~ head_start + male + urban + unemployed + married + fam_size, new_data)
ols4 <- lm(self_esteem ~ head_start + male + urban + unemployed + married + fam_size + educ_mother + educ_father, new_data)
ols5 <- lm(self_esteem ~ head_start + male + urban + unemployed + married + fam_size + educ_mother + educ_father + fam_income + fam_poverty, new_data)
stargazer::stargazer(ols1, ols2, ols3, ols4, ols5, title="Results", align=TRUE, type="text")

print("Effect of Preschool on self-reported Self Esteem (omit welfare variable)")

ols1 <- lm(self_esteem ~ pre_school, new_data)
ols2 <- lm(self_esteem ~ pre_school + male + urban + unemployed, new_data)
ols3 <- lm(self_esteem ~ pre_school + male + urban + unemployed + married + fam_size, new_data)
ols4 <- lm(self_esteem ~ pre_school + male + urban + unemployed + married + fam_size + educ_mother + educ_father, new_data)
ols5 <- lm(self_esteem ~ pre_school + male + urban + unemployed + married + fam_size + educ_mother + educ_father + fam_income + fam_poverty, new_data)
stargazer::stargazer(ols1, ols2, ols3, ols4, ols5, title="Results", align=TRUE, type="text")

### idea: interactions or estimate model on subsets

estim_head_start <- function(data) {
    ols1 <- lm(self_esteem ~ head_start, data)
    ols2 <- lm(self_esteem ~ head_start + male + urban + unemployed, data)
    ols3 <- lm(self_esteem ~ head_start + male + urban + unemployed + married + fam_size, data)
    ols4 <- lm(self_esteem ~ head_start + male + urban + unemployed + married + fam_size + educ_mother + educ_father, data)
    ols5 <- lm(self_esteem ~ head_start + male + urban + unemployed + married + fam_size + educ_mother + educ_father + fam_income + fam_poverty, data)
    stargazer::stargazer(ols1, ols2, ols3, ols4, ols5, title="Results", align=TRUE, type="text")
}

estim_pre_school <- function(data) {
    ols1 <- lm(self_esteem ~ pre_school, data)
    ols2 <- lm(self_esteem ~ pre_school + male + urban + unemployed, data)
    ols3 <- lm(self_esteem ~ pre_school + male + urban + unemployed + married + fam_size, data)
    ols4 <- lm(self_esteem ~ pre_school + male + urban + unemployed + married + fam_size + educ_mother + educ_father, data)
    ols5 <- lm(self_esteem ~ pre_school + male + urban + unemployed + married + fam_size + educ_mother + educ_father + fam_income + fam_poverty, data)
    stargazer::stargazer(ols1, ols2, ols3, ols4, ols5, title="Results", align=TRUE, type="text")
}

print("Effect of Head Start on self-reported Self Esteem (male / female interaction)")

ols1 <- lm(self_esteem ~ head_start, new_data)
ols2 <- lm(self_esteem ~ head_start*male + urban + unemployed, new_data)
ols3 <- lm(self_esteem ~ head_start*male + urban + unemployed + married + fam_size, new_data)
ols4 <- lm(self_esteem ~ head_start*male + urban + unemployed + married + fam_size + educ_mother + educ_father, new_data)
ols5 <- lm(self_esteem ~ head_start*male + urban + unemployed + married + fam_size + educ_mother + educ_father + fam_income + fam_poverty, new_data)
stargazer::stargazer(ols1, ols2, ols3, ols4, ols5, title="Results", align=TRUE, type="text")

print("Effect of Preschool on self-reported Self Esteem (male / female interaction)")

ols1 <- lm(self_esteem ~ pre_school, new_data)
ols2 <- lm(self_esteem ~ pre_school*male + urban + unemployed, new_data)
ols3 <- lm(self_esteem ~ pre_school*male + urban + unemployed + married + fam_size, new_data)
ols4 <- lm(self_esteem ~ pre_school*male + urban + unemployed + married + fam_size + educ_mother + educ_father, new_data)
ols5 <- lm(self_esteem ~ pre_school*male + urban + unemployed + married + fam_size + educ_mother + educ_father + fam_income + fam_poverty, new_data)
stargazer::stargazer(ols1, ols2, ols3, ols4, ols5, title="Results", align=TRUE, type="text")

# consider 2006 self esteem in a separate model
# interpolation from 1980->2006
# there's a 1994 self esteem question
