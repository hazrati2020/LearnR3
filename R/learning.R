
# LOAD PACKAGES -----------------------------------------------------------

library(tidyverse)

library(NHANES)

# Looking at data ---------------------------------------------------------

glimpse(NHANES)

select(NHANES, Age)
select(NHANES, starts_with("BP"))
select(NHANES, contains("Age"))

# Create smaller nhanse dataset -------------------------------------------

NHANES_small <- select(NHANES, Age, Gender, BMI, Diabetes, Education)
NHANES_small <- rename(NHANES_small, sex=Gender)

# trying ------------------------------------------------------------------

colnames(NHANES_small)

NHANES_small %>%
    colnames()

NHANES_small %>%
    select(Education) %>%
    rename(educationlevel= Education)


NHANES_small %>%
    select(bp_sys_ave_educationlevel)

# Filtering ---------------------------------------------------------------

NHANES_small %>%
    filter(BMI >= "25")

# Combining logical operator ----------------------------------------------

NHANES_small %>%
    filter(BMI >= 25 & Age == 20)
# arrange data

NHANES_small %>%
    arrange(Education, Age)

# Transform data ----------------------------------------------------------


NHANES_small %>%
    mutate(Age=Age*12,
           logged_BMI=log(BMI))


NHANES_small %>%
    mutate(old=if_else(Age>=30, "yes","No"))
