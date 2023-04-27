
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



# 1. BMI between 20 and 40 with diabetes
nhanes_small %>%
    # Format should follow: variable >= number or character
    filter(___ >= ___ & ___ <= ___ & ___ == ___)

# Pipe the data into mutate function and:
nhanes_modified <- nhanes_small %>% # Specifying dataset
    mutate(
        # 2. Calculate mean arterial pressure
        ___ = ___,
        # 3. Create young_child variable using a condition
        ___ = if_else(___, "Yes", "No")
    )

nhanes_modified


# new section for practice ------------------------------------------------




# 1. BMI between 20 and 40 with diabetes
NHANES_small %>%
    # Format should follow: variable >= number or character
    filter(BMI >= 20 & BMI <= 40 & Education=="some college")

# Pipe the data into mutate function and:
NHANES_modified <- NHANES_small %>% # dataset
    mutate(
        mean_arterial_pressure = ((2 * bp_dia_ave) + bp_sys_ave) / 3,
        young_child = if_else(age < 6, "Yes", "No")
    )

nhanes_modified

nhanes_modified


# cREATING SUMMUARY STATS -------------------------------------------------

NHANES_small %>%
    summarise(max_BMI=max(BMI,na.rm = TRUE), min_BMI = min(BMI, na.rm = TRUE))

NHANES_small %>%
    group_by(Diabetes) %>%
    summarise(mean_age=mean(Age, na.rm=TRUE), mean_BMI=mean(BMI,na.rm=TRUE))


#saving data
readr::write_csv(NHANES_small, here::here("data/nhanes_smaall.csv"))

title: "reproducible kdoc"
author: "Hossein Koskon"
format: docx
editor: visual


# visualization -----------------------------------------------------------

''

