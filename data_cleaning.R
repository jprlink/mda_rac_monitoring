
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
data <- read_xlsx("input/raw_data.xlsx") %>% rename(uuid = "_uuid", index = "_index")
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
questions_other <- questions_other[!questions_other %in% c(questions_int, questions_cat)]
questions_sm_num <- data %>% select(contains(questions_sm) & !any_of(c(questions_sm, questions_other))) %>% names()
questions_num <- append(questions_int, questions_sm_num)
questions_note <- grep("_note$", names(data), value = T) 
questions_note <- questions_note [!questions_note %in% c("calc_check_1_note", "calc_check_2_note")]

# make sure that all integer columns are numerical and text and categoricals character
data[questions_int] <- lapply(data[questions_int], as.numeric)
data[c(questions_cat, questions_other, questions_note)] <- lapply(data[c(questions_cat, questions_other, questions_note)], as.character)

### clean data using cleaning log

clog_change <- read_xlsx("input/cleaning_log.xlsx")
clog_change$index <- as.character(clog_change$index)
clog_change$id <- paste0(clog_change$index, "-/-",clog_change$question.name)

# save responses that should not be changed
clog_ids_no_change <- clog_change$id[clog_change$changed == "FALSE"] %>% unique()
clog_ids_all <- clog_change$id %>% unique()
clog_ids_dup <- clog_change$id[duplicated(clog_change$id)] %>% unique()
values_no_change <- clog_change$old.value[clog_change$changed == "FALSE"] %>% unique()

# check if there are any duplicated cleaning log entries
clog_change %>% filter(id %in% clog_ids_dup) %>%
  write_xlsx(paste0("output/MDA_RAC_dup_clog_entries_", Sys.Date(), ".xlsx"))

clog_change <- cleaninglog(clog_change$index, 
                           clog_change$question.name, 
                           clog_change$new.value,  
                           clog_change$changed, 
                             "index")

clog_change$variables[!clog_change$variables %in% names(data)]

clog_change_cat <- clog_change %>% filter(!variables %in% questions_num)
clog_change_cat$new_values <- as.character(clog_change_cat$new_values)
clog_change_cat$variables[!clog_change_cat$variables %in% names(data)]

clog_change_num <- clog_change %>% filter(variables %in% questions_num)
clog_change_num$new_values <- as.numeric(clog_change_num$new_values)

clog_deletion <- read_xlsx("input/cleaning_log.xlsx", 2)

# delete surveys based on deletion log
data <- data %>% filter(!index %in% clog_deletion$index)

# change values using change log
data <- clog_clean(data, clog_change_cat)
data <- clog_clean(data, clog_change_num)

### run checks

# check for surveys without consent
index_no_consent <- data$index[data$consent != "yes"] %>% print

# check for duplicated uuids
dup_uuid <- data$uuid[duplicated(data$uuid)] %>% print

# check for surveys for the same center (it should only be one per center)
dup_centre_id <- data$centre_id[duplicated(data$centre_id)] %>% print

data <- data %>% mutate(CHECK_dup_uuid = case_when(uuid %in% dup_uuid ~ "Duplicated UUID"),
                        CHECK_no_consent = case_when(index %in% index_no_consent ~ "No consent"),
                        CHECK_dup_center_id = case_when(centre_id %in% dup_centre_id ~ "Duplicated centre ID")
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
data$CHECK_interview_duration[which(data$need_center == "no" | data$consent == "no")] <- NA

sum(data$CHECK_interview_duration == "Interview time less than five minutes", na.rm = T)
boxplot(data$interview_duration)

# export raw dataset with macro data checks
checks <- data %>% select(CHECK_dup_uuid,
                          CHECK_no_consent,
                          CHECK_dup_center_id,
                          CHECK_interview_duration, 
                          uuid, 
                          index)

data_checks <- left_join(raw_data, checks, by = c("uuid", "index"))
data_checks %>% 
  write_xlsx(paste0("output/MDA_RAC_raw_data_with_macro_checks_", Sys.Date(), ".xlsx"))

# check for outliers

list_outliers <- list()

for(c in 1:ncol(data)) {
  
  if(names(data[c]) %in% questions_int){
    
  cname <- names(data[c])
  col_ind <- grep("^uuid|^index", colnames(data)) %>% append(c)
  out <- boxplot.stats(as.vector(unlist(data[c])))$out
  out_ind <- which(as.vector(unlist(data[c])) %in% c(out))
  
  if(length(out_ind) >0) {
    
    df_outlier <- data[out_ind, col_ind] %>%
     pivot_longer(cols = all_of(cname), 
                 names_to =  "question.name",
                 values_to = "old.value") %>% 
      mutate(old.value = as.character(old.value))
    
  list_outliers[[c]] <- df_outlier
  }
  }
}

list_outliers <- list_outliers[lapply(list_outliers,length)>0]
clog_outliers  <- do.call(bind_rows, list_outliers) %>% 
  filter(!paste0(index, "-/-", question.name) %in% clog_ids_no_change) %>% 
  mutate(issue = "Normal distribution outlier",
         feedback	= NA,
         changed = NA,
         new.value = NA
         ) %>%
  select(uuid, index, question.name, issue, feedback, changed, old.value, new.value) %>%
  print

# check for logical inconsistencies between number of people needing items and people at center
data <- data  %>% mutate(CHECK_number_people_items = case_when(center_need_sleeping_item_unit > center_ind | 
                                                                 center_need_clothes_unit > center_ind |
                                                                 center_need_older_pwd_item_unit > center_ind ~ "Number of people in need exceeding number of people at centre"
                                                               )
)

sum(!is.na(data$CHECK_number_people_items))

clog_items_old <- data %>% filter(!is.na(CHECK_number_people_items)) %>% 
  select(uuid,
         index,
         center_need_sleeping_item_unit,
         center_need_clothes_unit,
         center_need_older_pwd_item_unit) %>% 
  pivot_longer(-c("uuid", "index"), names_to = "question.name", values_to = "old.value")

# replace number of people estimate when they exceed people at center with number of people at center
data2 <- data  %>% mutate(center_need_sleeping_item_unit = case_when(center_need_sleeping_item_unit > center_ind ~  center_ind,
                                                                    TRUE ~ center_need_sleeping_item_unit),
                         center_need_clothes_unit = case_when(center_need_clothes_unit > center_ind ~  center_ind,
                                                              TRUE ~ center_need_clothes_unit),
                         center_need_older_pwd_item_unit = case_when(center_need_older_pwd_item_unit > center_ind ~ center_ind,
                                                                     TRUE ~ center_need_older_pwd_item_unit)
)

clog_items_new <- data2 %>% filter(!is.na(CHECK_number_people_items)) %>% 
  select(uuid,
         index,
         center_need_sleeping_item_unit,
         center_need_clothes_unit,
         center_need_older_pwd_item_unit) %>% 
  pivot_longer(-c("uuid", "index"), names_to = "question.name", values_to = "new.value")

clog_items <- left_join(clog_items_old, clog_items_new, by = c("index", "uuid", "question.name")) %>% 
  filter(old.value != new.value) %>%
  mutate(issue = "Logical inconsistencies between people in need of items and people at center",
         feedback	= NA,
         changed = NA
  ) %>%
  select(uuid, index, question.name, issue, feedback, changed, old.value, new.value) %>%
  print

# check for logical inconsistencies between total of gender disaggregation and people at center
data <- data  %>% mutate(CHECK_gender_disaggregation = case_when((!is.na(how_many_are_women) & is.na(how_many_are_other) & how_many_are_women + how_many_are_men != center_ind) |
                                                                   (!is.na(how_many_are_women) & !is.na(how_many_are_other) & how_many_are_women + how_many_are_men + how_many_are_other != center_ind)  ~ "Total of gender disaggregation does not equal number of people at centre" 
)
)

sum(!is.na(data$CHECK_gender_disaggregation), na.rm = T)

clog_gender_old <- data %>% filter(!is.na(CHECK_gender_disaggregation)) %>% 
  select(uuid,
         index,
         how_many_are_women,
         how_many_are_men,
         how_many_are_other) %>% 
  pivot_longer(-c("uuid", "index"), names_to = "question.name", values_to = "old.value")

# replace number of disaggregation with NA
data3 <- data  %>% mutate(how_many_are_women = case_when(!is.na(CHECK_gender_disaggregation) ~ NA_real_ ,
                                                         is.na(CHECK_gender_disaggregation) ~ how_many_are_women),
                          how_many_are_men = case_when(!is.na(CHECK_gender_disaggregation) ~ NA_real_ ,
                                                         is.na(CHECK_gender_disaggregation) ~ how_many_are_men),
                          how_many_are_other = case_when(!is.na(CHECK_gender_disaggregation) ~ NA_real_ ,
                                                         is.na(CHECK_gender_disaggregation) ~ how_many_are_other)
)

clog_gender_new <- data3 %>% filter(!is.na(CHECK_gender_disaggregation)) %>% 
  select(uuid,
         index,
         how_many_are_women,
         how_many_are_men,
         how_many_are_other) %>% 
  pivot_longer(-c("uuid", "index"), names_to = "question.name", values_to = "new.value")

clog_gender <- left_join(clog_gender_old, clog_gender_new, by = c("index", "uuid", "question.name")) %>% 
  filter(!is.na(old.value)) %>%
  mutate(issue = "Logical inconsistencies between total of gender disaggregation and people at center",
         feedback	= NA,
         changed = NA
  ) %>%
  select(uuid, index, question.name, issue, feedback, changed, old.value, new.value) %>%
  print

# check for NAs for age and gender variables when there are no people at the centre

data4 <- data
questions_nas_demo_num <- c("how_many_are_men",
                            "how_many_are_women",
                            "how_many_are_other",
                            "child_0_2_number",
                        "child_2_6_number",
                        "child_7_11_number",
                        "child_12_18_number",
                        "how_many_are_children_2_18_years_old",
                        "demo_elderly",
                        "vuln_groups.plw",
                        "vuln_groups.solo_child",
                        "vuln_groups.disability",
                        "vuln_groups.medical_condition",
                        "vuln_groups.protection_need",
                        "vuln_groups.other"
)

questions_nas_demo_cat <- c("do_you_know_how_many_are_men",
                            "do_you_know_how_many_are_women",
                            "do_you_know_how_many_are_other",
                            "child_0_2",
                            "child_age_breakdown",
                            "do_you_know_how_many_are_child",
                            "elderly"
)

questions_demo_all <- c(questions_nas_demo_num, questions_nas_demo_cat,"vuln_groups.none")

data4 <- data4  %>% mutate(CHECK_NAs_when_no_people = case_when((rowSums(across(all_of(questions_demo_all), ~ is.na(.)))  > 0 |
                                                                rowSums(across(all_of(questions_nas_demo_cat), ~ . == "no"))  > 0 |
                                                                vuln_groups.none == 0)
                                                              & center_ind == 0  ~ "The number of people hosted is 0, so number of age groups should be the same." 
)
)

sum(!is.na(data4$CHECK_NAs_when_no_people), na.rm = T)

data4[questions_demo_all] <- lapply(data4[questions_demo_all], as.character)

clog_no_people_old <- data4 %>% filter(!is.na(CHECK_NAs_when_no_people)) %>% 
  select(uuid,
         index,
         all_of(questions_demo_all)) %>% 
  pivot_longer(-c("uuid", "index"), names_to = "question.name", values_to = "old.value")


clog_no_people <- clog_no_people_old %>% 
  mutate(new.value = case_when(question.name %in% questions_nas_demo_num ~ "0",
                               question.name %in% "vuln_groups.none" ~ "1",
                               question.name %in% questions_nas_demo_cat ~ "yes"
                               )
         ) %>% 
  filter(!old.value %in% new.value) %>%
  mutate(issue = "Logical inconsistencies between NAs in demographic disaggregation and people at center",
         feedback	= NA,
         changed = NA
  ) %>%
  select(uuid, index, question.name, issue, feedback, changed, old.value, new.value) %>%
  print

# merge cleaning log templates with logical issues

clog_logical <- rbind(clog_gender, clog_items, clog_no_people, clog_outliers)

clog_logical <- clog_logical %>% filter(!paste0(index, "-/-", question.name) %in% clog_ids_no_change)

clog_logical

clog_logical %>% write_xlsx(paste0("output/MDA_RAC_logical_issues_and_outliers_", Sys.Date(), ".xlsx"))

### make modifications & additions to data

# replace NAs with 0s in integer vars
questions_int_no_age_gender <- questions_int[!questions_int %in% c("how_many_are_women", "how_many_are_men", "how_many_are_other", "child_0_2_number", "child_2_6_number", "child_7_11_number", "child_12_18_number", "demo_elderly", "how_many_are_children_2_18_years_old")]
data[questions_int_no_age_gender][is.na(data[questions_int_no_age_gender])] <- 0

# # exclude 0s from categorical columns
# data[c(questions_cat, questions_text)][data[c(questions_cat, questions_text)]== "0"] <- NA_character_
# data[questions_other][data[questions_other]== "0"] <- NA_character_

# translate "other" responses and notes 
translated_responses <- pivot_longer_other(data, c(questions_other, questions_note)) %>% 
  left_join(data[c("uuid", "index")], by ="index") %>% 
  rename(question.name = variable, old.value = original)  %>% 
  mutate(issue = "Translation of text response", 
         feedback = NA,
         changed = NA
         ) %>%
  select(uuid, index, question.name, issue, feedback, changed, old.value) %>% 
  filter(!paste0(index, "-/-", question.name) %in% clog_ids_all)

# add authorization key from deepl api account
my_key <- "5c72704c-1df8-72c3-3162-cb15d34de739:fx"
langs <- deeplr::available_languages2(my_key)
as.data.frame(langs)
deeplr::usage2(my_key)

translated_responses$new.value <- translate2(
  text = translated_responses$old.value,
  source_lang = "RO",
  target_lang = "EN",
  auth_key = my_key
)

translated_responses %>% write_xlsx(paste0("output/MDA_RAC_translations_other_resp_and_notes_", Sys.Date(), ".xlsx"))

for (i in 1:nrow(data)) {
  for(c in 1:ncol(data)) {
  cname <- names(data[c])
  if(cname %in% unique(translated_responses$question.name))  {
  m <- match(paste0(data$index[i], "/", cname) , paste0(translated_responses$index, "/", translated_responses$question.name))
  if(!is.na(m)){
    {
      data[i,c] <- translated_responses$new.value[m]
    }
  }
  }
  }
}

#data[c("index",questions_other)]

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
data$center_ind_breakdown_age <- data$center_ind

# set NAs for all three age variable when one of them is NA
questions_nas_demo_num
vars_age <- c("center_ind_breakdown_age", "child_0_2_number", "how_many_are_children_2_18_years_old", "demo_elderly")
vars_age_all <- c("center_ind_breakdown_age", "child_0_2_number", "how_many_are_children_2_18_years_old", "demo_elderly", "child_2_6_number", "child_7_11_number", "child_12_18_number")
data[vars_age_all]
data <- data %>% mutate(set_na_age = case_when(rowSums(across(all_of(vars_age), ~ is.na(.))) > 0 ~"yes"))
data[which(data$set_na_age == "yes"), vars_age_all] <- NA
data[vars_age_all]

# calculate percentages of demographic groups per center
data <- data %>% mutate(perc_0_2 = child_0_2_number / center_ind,
                        perc_2_18 = how_many_are_children_2_18_years_old / center_ind,
                        perc_65_plus = demo_elderly / center_ind,
                        perc_2_6 = ifelse(!is.na(child_2_6_number), child_2_6_number / how_many_are_children_2_18_years_old, NA),
                        perc_7_11 = ifelse(!is.na(child_7_11_number), child_7_11_number / how_many_are_children_2_18_years_old, NA),
                        perc_12_18 = ifelse(!is.na(child_12_18_number), child_12_18_number / how_many_are_children_2_18_years_old, NA)
)

# add seperate total individual variable for breakdown of gender
data$center_ind_breakdown_gender <- data$center_ind

# set NAs for all gender variables when one of them is NA
vars_gender <- c("center_ind_breakdown_gender", "how_many_are_women", "how_many_are_men", "how_many_are_other")
data[vars_gender]
data <- data %>% mutate(set_na_gender = case_when(rowSums(across(all_of(c("how_many_are_women", "how_many_are_men")), ~ is.na(.))) > 0 ~"yes"))
data[which(data$set_na_gender == "yes"), vars_gender] <- NA
data[vars_gender]

if("what_type_of_building_is_the_c" %in% names(data)) {

# add building type when building type was already known before
building_type <- read_xlsx("input/building_type.xlsx") %>% select(-label)
building_type$centre_id <- as.character(building_type$centre_id )
data <- left_join(data, building_type, by = "centre_id")
data  <- data  %>% mutate(what_type_of_building_is_the_c = ifelse(is.na(what_type_of_building_is_the_c), what_type_of_building_is_the_c_new, what_type_of_building_is_the_c))
data$centre_id[which(is.na(data$what_type_of_building_is_the_c))]
data <- data %>% select(-c("what_type_of_building_is_the_c_new"))

}

# exclude data without consent or duplicated uuid
data  <- data  %>% filter(consent == "yes")
#data <- data %>% filter(!duplicated(uuid))

# exclude not needed variables
vars_clean <- survey$name[which(survey$clean_dataset == "yes")] %>% append(c("how_many_staff_per_people_hosted", "center_ind_breakdown_age", "perc_0_2", "perc_2_18", "perc_65_plus", "perc_2_6", "perc_7_11", "perc_12_18", "center_ind_breakdown_gender"))
data_clean_for_sharing <- data %>% select(uuid, index, contains(all_of(vars_clean))) %>% select(-c(all_of(questions_sm_num)))
data_clean <- data %>% select(uuid, index, contains(all_of(vars_clean)))

# export clean dataset without checks
data_clean %>% write_xlsx(paste0("output/MDA_RAC_clean_data_unlabelled_", Sys.Date(), ".xlsx"))
data_clean %>% write_xlsx("input/clean_data.xlsx")

# export clean dataset with labels (for sharing)
data_en <- from_xml_tolabel(db = data_clean_for_sharing,
                         choices = choices,
                         survey = survey,
                         choices_label = "label::English",
                         survey_label = "label::English")

data_ro <- from_xml_tolabel(db = data_clean_for_sharing,
                            choices = choices,
                            survey = survey,
                            choices_label = "label::Romanian",
                            survey_label = "label::Romanian")

data_en %>% write_xlsx(paste0("output/MDA_RAC_clean_data_labelled_EN_", Sys.Date(), ".xlsx"))
data_ro %>% write_xlsx(paste0("output/MDA_RAC_clean_data_labelled_RO_", Sys.Date(), ".xlsx"))
