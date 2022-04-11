
library(tidyverse)
library(dplyr)
library(lubridate)
library(writexl)
library(readxl)
library(deeplr)
library(assertthat)
source("functions.R")
#library(devtools)
#install_github("impact-initiatives/cleaninginspectoR")

#### Data cleaning ####

# load data
data <- read_xlsx("input/raw_data.xlsx") %>% rename(uuid = "_uuid")
names(data) <- names(data) %>% str_replace_all(c("/"=".")) %>% tolower()

raw_data <- data

# load tool
survey <- read_xlsx("input/tool.xlsx") %>% 
  mutate(name = tolower(name)) %>% 
  filter(name %in% names(data))
choices <- read_xlsx("input/tool.xlsx", 2)

# define question types
questions_so <- survey$name[grepl("select_one", survey$type)]
questions_sm <- survey$name[grepl("select_multiple", survey$type)]
questions_text <- survey$name[grepl("text", survey$type)]
questions_int <- survey$name[grepl("integer", survey$type)] %>% append(c("calc_ukrainian","calc_third_party"))
questions_cat <- append(questions_so, questions_sm)
questions_other <- grep("_specify|_explain_why|_other", names(data), value = T)
questions_sm_num <- data %>% select(contains(questions_sm) & !any_of(c(questions_sm, questions_other))) %>% names()
questions_num <- append(questions_int, questions_sm_num)

# make sure that all integer columns are numerical and text and categoricals character
data[questions_int] <- lapply(data[questions_int], as.numeric)
data[c(questions_cat, questions_other)] <- lapply(data[c(questions_cat, questions_other)], as.character)

### clean data using cleaning log

clog_change <- read_xlsx("input/cleaning_log.xlsx")
clog_change <- cleaninglog(clog_change$uuid, 
                           clog_change$question.name, 
                           clog_change$new.value,  
                           clog_change$changed, 
                             "uuid")

clog_change_cat <- clog_change %>% filter(!variables %in% questions_num)
clog_change_cat$new_values <- as.character(clog_change_cat$new_values)
clog_change_num <- clog_change %>% filter(variables %in% questions_num)

clog_deletion <- read_xlsx("input/cleaning_log.xlsx", 2)

# delete surveys based on deletion log
data <- data %>% filter(!uuid %in% clog_deletion$uuid)

# change values using change log
data <- clog_clean(data, clog_change_cat)
data <- clog_clean(data, clog_change_num)

### run checks

# check for surveys without consent
uuids_no_consent <- data$uuid[data$consent != "yes"] %>% print

# check for duplicated uuids
dup_uuid <- data$uuid[duplicated(data$uuid)] %>% print

# check for surveys for the same center (it should only be one per center)
dup_centre_id <- data$centre_id[duplicated(data$centre_id)] %>% print


data <- data %>% mutate(CHECK_dup_center_id = case_when(centre_id %in% dup_centre_id ~ "Duplicated centre ID"),
                        CHECK_no_consent = case_when(uuid %in% uuids_no_consent ~ "No consent")
                        )

# check time
# Initializing variables
time_min <- 5
time_max <- NA

# declaring the function
time_check <- function(df, time_min, time_max){
  df <- df %>% mutate(interview_duration = difftime(as.POSIXct(ymd_hms(end)), as.POSIXct(ymd_hms(start)), units = "mins"),
                      CHECK_interview_duration = case_when(
                        interview_duration < time_min ~ "Interview time less than five minutes"
                        #interview_duration > time_max ~ "Interview time too long",
                      )
  )
  return(df)
}

# Applying the function to data frame
data <- time_check(data , time_min, time_max)

# Don't check time when the center doesnt need anything.
data$CHECK_interview_duration[which(data$need_center == "no")] <- NA

sum(data$CHECK_interview_duration == "Interview time less than five minutes", na.rm = T)
boxplot(data$interview_duration)

# check for outliers

# 999 (meaning don't know)
checks_outlier <- data_frame(index =  NA_integer_,
                 value = NA_character_,
                 variable = NA_character_,
                 has_issue = NA,                  
                 issue_type = NA_character_,
                 )

s <- 1
for(i in 1:nrow(data)) {
  for(c in 1:nrow(data)) {
    
    if(!is.na(data[i,c]) && data[i,c] == 999) {
    checks_outlier[s,"index"] <- row_number(data[i,c])
    checks_outlier[s,"value"] <- "999"
    checks_outlier[s,"variable"] <- names(data[i,c])
    checks_outlier[s,"has_issue"] <- TRUE
    checks_outlier[s,"issue_type"] <- "distribution outlier"
    s <- s + 1
    }
  }
}
  
outliers <- cleaninginspectoR::find_outliers(data) %>% filter(!variable %in% c("centre_id", "X_id")) %>% rbind(checks_outlier)  %>% print
outliers %>% write_xlsx(paste0("output/outliers_", Sys.Date(), ".xlsx"))

# check for logical inconsistencies between number of people needing items and people at center
data <- data  %>% mutate(CHECK_number_people_items = case_when(center_need_sleeping_item_unit > center_ind | 
                                                                 center_need_clothes_unit > center_ind |
                                                                 center_need_older_pwd_item_unit > center_ind ~ "Number of people in need exceeding number of people at centre"
                                                               )
)

sum(data$CHECK_number_people_items == "Number of people in need exceeding number of people at centre", na.rm = T)

clog_old <- data %>% filter(CHECK_number_people_items == "Number of people in need exceeding number of people at centre") %>% 
  select(uuid,
         center_need_sleeping_item_unit,
         center_need_clothes_unit,
         center_need_older_pwd_item_unit) %>% 
  pivot_longer(-uuid, names_to = "question.name", values_to = "old.value")

# replace number of people estimate when they exceed people at center with number of people at center
data2 <- data  %>% mutate(center_need_sleeping_item_unit = case_when(center_need_sleeping_item_unit > center_ind ~  center_ind,
                                                                    TRUE ~ center_need_sleeping_item_unit),
                         center_need_clothes_unit = case_when(center_need_clothes_unit > center_ind ~  center_ind,
                                                              TRUE ~ center_need_clothes_unit),
                         center_need_older_pwd_item_unit = case_when(center_need_older_pwd_item_unit > center_ind ~ center_ind,
                                                                     TRUE ~ center_need_older_pwd_item_unit)
)

clog_new <- data2 %>% filter(CHECK_number_people_items == "Number of people in need exceeding number of people at centre") %>% 
  select(uuid,
         center_need_sleeping_item_unit,
         center_need_clothes_unit,
         center_need_older_pwd_item_unit) %>% 
  pivot_longer(-uuid, names_to = "question.name", values_to = "new.value")

clog_logical <- left_join(clog_old, clog_new, by = c("uuid", "question.name")) %>% filter(old.value != new.value)

clog_logical %>% write_xlsx(paste0("output/clog_logical_", Sys.Date(), ".xlsx"))

# export raw dataset with macro data checks
checks <- data %>% select(CHECK_no_consent,
                          CHECK_dup_center_id,
                          CHECK_interview_duration, 
                          uuid)
data_checks <- left_join(raw_data, checks, by = "uuid")
data_checks %>% 
  write_xlsx(paste0("output/MDA_RAC_raw_data_with_macro_checks_", Sys.Date(), ".xlsx"))

### make modifications & additions to data

data  <- data  %>% filter(consent == "yes")
data <- data %>% filter(!duplicated(uuid))

# replace NAs with 0s in integer vars
questions_int_no_age <- questions_int[!questions_int %in% c("child_0_2_number", "child_2_6_number", "child_7_11_number", "child_12_18_number", "demo_elderly", "how_many_are_children_2_18_years_old")]
data[questions_int_no_age][is.na(data[questions_int_no_age])] <- 0

# # exclude 0s from categorical columns
# data[c(questions_cat, questions_text)][data[c(questions_cat, questions_text)]== "0"] <- NA_character_
# data[questions_other][data[questions_other]== "0"] <- NA_character_

# translate "other" responses 

pivot_longer_other <- function(df, vars_other) {
  
  require(tidyverse)
  
  list <- list()
  
  uuid <- "uuid"
  
  vars_other <- append(vars_other, uuid)
  
  other_data <- df[,vars_other]
  
  for (i in 1:nrow(other_data)) { 
    
    if(rowSums(is.na(other_data[i,])) < (ncol(other_data)-1)) {
      rdata <- other_data[i,] %>% pivot_longer(!uuid, 
                                               names_to = "variable",
                                               values_to = "original", 
                                               values_drop_na = T)
      
      list[[i]] <- rdata
      
    }
  } 
  
  other_data_long <- do.call(rbind, list)
  
  dup <- other_data_long[,c("uuid","variable")] 
  
  other_data_long <- other_data_long[!duplicated(dup) & 
                                       !duplicated(dup, fromLast=TRUE),]
  return(other_data_long)
  
} 

other_data_long <- pivot_longer_other(data, questions_other)

# add authorization key from deepl api account
my_key <- "5c72704c-1df8-72c3-3162-cb15d34de739:fx"
langs <- deeplr::available_languages2(my_key)
as.data.frame(langs)
deeplr::usage2(my_key)

other_data_long$translation <- translate2(
  text = other_data_long$original,
  source_lang = "RO",
  target_lang = "EN",
  auth_key = my_key
)

other_data_long %>% write_xlsx(paste0("output/other_response_translations_", Sys.Date(), ".xlsx"))

for (i in 1:nrow(data)) {
  for(c in 1:ncol(data)) {
  cname <- names(data[c])
  if(cname %in% unique(other_data_long$variable))  {
  m <- match(paste0(data$uuid[i], "/", cname) , paste0(other_data_long$uuid, "/", other_data_long$variable))
  if(!is.na(m)){
    {
      data[i,c] <- other_data_long$translation[m]
    }
  }
  }
  }
}

data[c("uuid",questions_other)]

# set NA for staff when number of staff 0 and there are people hosted
data$how_many_staff_does_entre_currently_have[which(data$how_many_staff_does_entre_currently_have == 0 &
                                                              data$center_ind > 0 )] <- NA_integer_

# calculate number of staff per hosted person
data <- data %>% mutate(how_many_staff_per_people_hosted = how_many_staff_does_entre_currently_have / center_ind)
data["how_many_staff_per_people_hosted"][data["how_many_staff_per_people_hosted"] == Inf] <- NA
data["how_many_staff_per_people_hosted"][data["how_many_staff_per_people_hosted"] == NaN] <- NA
questions_int <- append(questions_int, "how_many_staff_per_people_hosted")

# add 0s and no so that overall % of centers per need status is calculated
vars_set_overall <- survey$name[which(survey$change_to_overall_percent == "yes")]
vars_set_no <- vars_set_overall[vars_set_overall %in% questions_so]
vars_set_zero_temp <- vars_set_overall[vars_set_overall %in% questions_sm]

vars_set_zero <- data %>% select(contains(vars_set_zero_temp)) %>% names
vars_set_zero <- vars_set_zero[!vars_set_zero %in% c(questions_sm, questions_other)]
data[vars_set_no][is.na(data[, vars_set_no])] <- "no"
data[vars_set_zero ][is.na(data[, vars_set_zero ])] <- 0

# add total of children 2-18 where there is a breakdown
data <- data %>% mutate(how_many_are_children_2_18_years_old = case_when(!is.na(child_2_6_number) ~ child_2_6_number + child_7_11_number + child_12_18_number,
                                                                          is.na(child_2_6_number)  ~ how_many_are_children_2_18_years_old),
                         )
# add seperate total individual variable for breakdown
data$center_ind_breakdown <- data$center_ind

# set NAs for all three age variable when one of them is NA
vars_age <- c("center_ind_breakdown", "child_0_2_number", "how_many_are_children_2_18_years_old", "demo_elderly")
data[vars_age]
data <- data %>% mutate(set_na_age = case_when(rowSums(across(all_of(vars_age), ~ is.na(.))) > 0 ~"yes"))
data[which(data$set_na_age == "yes"), vars_age] <- NA
data[vars_age]

# calculate percentages of demographic groups per center
data <- data %>% mutate(perc_0_2 = child_0_2_number / center_ind,
                        perc_2_18 = how_many_are_children_2_18_years_old / center_ind,
                        perc_65_plus = demo_elderly / center_ind,
                        perc_2_6 = ifelse(!is.na(child_2_6_number), child_2_6_number / how_many_are_children_2_18_years_old, NA),
                        perc_7_11 = ifelse(!is.na(child_7_11_number), child_7_11_number / how_many_are_children_2_18_years_old, NA),
                        perc_12_18 = ifelse(!is.na(child_12_18_number), child_12_18_number / how_many_are_children_2_18_years_old, NA)
)

# add building type when building type was already known before
building_type <- read_xlsx("input/building_type.xlsx") %>% select(-label)
building_type$centre_id <- as.character(building_type$centre_id )
data <- left_join(data, building_type, by = "centre_id")
data  <- data  %>% mutate(what_type_of_building_is_the_c = ifelse(is.na(what_type_of_building_is_the_c), what_type_of_building_is_the_c_new, what_type_of_building_is_the_c))
data$centre_id[which(is.na(data$what_type_of_building_is_the_c))]

# exclude not needed variables
vars_clean <- survey$name[which(survey$clean_dataset == "yes")] %>% append(c("how_many_staff_per_people_hosted", "center_ind_breakdown", "perc_0_2", "perc_2_18", "perc_65_plus", "perc_2_6", "perc_7_11", "perc_12_18"))
data <- data %>% select(uuid, contains(vars_clean)) %>% select(-what_type_of_building_is_the_c_new)

# export clean dataset without checks
data %>% write_xlsx(paste0("output/MDA_RAC_clean_data_unlabelled_", Sys.Date(), ".xlsx"))
data %>% write_xlsx("input/clean_data.xlsx")

# export clean dataset with labels
data_en <- from_xml_tolabel(db = data,
                         choices = choices,
                         survey = survey,
                         choices_label = "label::English",
                         survey_label = "label::English")

data_ro <- from_xml_tolabel(db = data,
                            choices = choices,
                            survey = survey,
                            choices_label = "label::Romanian",
                            survey_label = "label::Romanian")

data_en %>% write_xlsx(paste0("output/MDA_RAC_clean_data_labelled_EN_", Sys.Date(), ".xlsx"))
data_ro %>% write_xlsx(paste0("output/MDA_RAC_clean_data_labelled_RO_", Sys.Date(), ".xlsx"))
