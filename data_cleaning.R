
library(tidyverse)
library(dplyr)
library(lubridate)
library(writexl)
library(readxl)
library(deeplr)
#library(devtools)
#install_github("impact-initiatives/cleaninginspectoR")

#### Data cleaning ####

data <- read_xlsx("input/pre_cleaned_data.xlsx") %>% rename(uuid = "_uuid")
survey <- read_xlsx("input/tool.xlsx")

# add dummy disaggreation level
data$disag <- "total"

# exclude survey questions not included in data
survey <- survey %>% filter(`label::English` %in% names(data))

# define question types
questions_so <- survey$`label::English`[grepl("select_one", survey$type)]
questions_sm <- survey$`label::English`[grepl("select_multiple", survey$type)]
questions_text <- survey$`label::English`[grepl("text", survey$type)]
questions_int <- survey$`label::English`[grepl("integer", survey$type)]
questions_cat <- append(questions_so, questions_sm)
questions_num <- append(questions_int, questions_sm)
questions_other <- grep("If other, please specify|If not, may I ask you why", names(data), value = T)

# make sure that all integer columns are numerical and text and categoricals character
data[questions_int] <- lapply(data[questions_int], as.numeric)
data[c(questions_cat, questions_other)] <- lapply(data[c(questions_cat, questions_other)], as.character)

# exclude 0s from categorical columns
data[c(questions_cat, questions_text)][data[c(questions_cat, questions_text)]== "0"] <- NA_character_
data[questions_other][data[questions_other]== "0"] <- NA_character_

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

other_data_long

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

# exclude colums with only NAs
data <- data[, colSums(is.na(data)) != nrow(data)]

# replace NAs with 0s in integer vars
data[questions_int][is.na(data[questions_int])] <- 0

# set NA for staff when number of staff 0 and there are people hosted
data$`How many staff does the centre currently have?`[which(data$`How many staff does the centre currently have?` == 0 &
                                                              data$`How many people are currently being hosted at the centre?` > 0 )] <- NA_integer_

# calculate number of staff per hosted person
data <- data %>% mutate(`How many staff per people hosted?` = `How many staff does the centre currently have?` / `How many people are currently being hosted at the centre?`)
data["How many staff per people hosted?"][data["How many staff per people hosted?"] == Inf] <- NA
data["How many staff per people hosted?"][data["How many staff per people hosted?"] == NaN] <- NA
questions_int <- append(questions_int, "How many staff per people hosted?")

# add 0s and no so that overall % of centers per need status is calculated
vars_set_no <- c("Does this center need food products?",
                 "Has the centre received any food products since receiving approval to open?",
                 "Does this center need cooking and eating utensils?",
                 "Has the centre received any cooking and eating utensils since receiving approval to open?",  
                 "Does this center need sleeping items?", 
                 "Has the centre received any sleeping items since receiving approval to open?",
                 "Does this center need hygiene (personal care) items?",
                 "Has the centre received any  hygiene (personal care) items since receiving approval to open?",
                 "Does this center need cleaning items?",
                 "Has the centre received any cleaning items since receiving approval to open?",
                 "Does this center need baby and children products?",
                 "Has the centre received any baby and children products since receiving approval to open?",
                 "Does this center need clothes for children and adults?",
                 "Has the centre received any clothes for children and adults since receiving approval to open?",
                 "Does this center need first aid supplies?",
                 "Has the centre received any first aid supplies since receiving approval to open?",
                 "Does this center need supplies for people of age and people with disabilities?",
                 "Has the centre received any supplies for  people of age and people with disabilities since receiving approval to open?",
                 "Does this center need other appliances (Microwave, cooler, fridge, oven, speace heaters, boilers, washing machine, vacuum cleaner)?",
                 "Has the centre received any other appliances since receiving approval to open?"
)

vars_set_zero_temp <- c("If yes, which type of of food products does the center need?",
                        "If yes, which type of of food products has the center received?",
                        "If yes, which type of cooking and eating utensils does the center need?",
                        "If yes, which type of cooking and eating utensils has the center received?",
                        "If yes, which type of sleeping items does the center need?",
                        "If yes, which type of sleeping items has the center received?",
                        "If yes, which type of hygiene (personal care) items does the center need?",
                        "If yes, which type of hygiene (personal care) items has the center received?",
                        "If yes, which type of cleaning items does the center need?",
                        "If yes, which type of cleaning items has the center received?",
                        "If yes, which type of baby and children products does the center need?",
                        "If yes, which type of baby and children products has the center received?",
                        "If yes, which type of clothes for children and adults does the center need?",
                        "If yes, which type of clothes for children and adults has the center received?",
                        "If yes, which type of first aid supplies does the center need?",
                        "If yes, which type of first aid supplies has the center received?",
                        "If yes, which type of supplies for people of age and people with disabilities does the center need?",
                        "If yes, which type of supplies for people of age and people with disabilities has the center received?",
                        "If yes, which type of other appliances does the center need?",
                        "If yes, which type of appliances has the center received?"
)

vars_set_zero <- data %>% select(contains(vars_set_zero_temp)) %>% names
vars_set_zero <- vars_set_zero[!vars_set_zero %in% questions_cat]
data[vars_set_no][is.na(data[, vars_set_no])] <- "No"
data[vars_set_zero ][is.na(data[, vars_set_zero ])] <- 0

# delete surveys without consent
data  <- data  %>% filter(`My name is ${enumerator} and I am working with ANAS (National Agency for Social Assistance). We are in charge of conducting an assessment on the Refugee Accommodation Centers (RAC) in Moldova. The information you provide will help to inform the assistance that is provided to centers across Moldova. Today, the interview will last a bit longer, around 15 minutes, because we are gathering detailed information about the needs of the centers. The information you provide will only be shared in a strictly anonymized format. Your participation is voluntary and you can withdraw from the interview at any point. Do you have any questions? Are you willing to participate to this survey?` == "Yes")

# check for duplicated uuids
cleaninginspectoR::find_duplicates(data, duplicate.column.name = "uuid")
data <- data %>% filter(!duplicated(uuid))

# shows surveys for the same center (it should only be one per center)
cor_centers <- read_xlsx("input/correction_centers.xlsx")
data <- left_join(data, cor_centers)
sum(is.na(data$center_new))
sum(duplicated(data$`Center that is being assessed`))
data$`Center that is being assessed`[duplicated(data$`Center that is being assessed`)]
data <- data %>% mutate(`Center that is being assessed` = center_new)
sum(duplicated(data$`Center that is being assessed`))

# check time

time_missing <- read_xlsx("input/added_times.xlsx")
data <- data %>% select(-c("today", "start", "end")) %>% left_join(time_missing)

# Initializing variables
time_min <- 5
time_max <- NA

# declaring the function
time_check <- function(df, time_min, time_max){
  df <- df %>% mutate(interview_duration = difftime(as.POSIXct(ymd_hms(end)), as.POSIXct(ymd_hms(start)), units = "mins"),
                      CHECK_interview_duration = case_when(
                        interview_duration < time_min ~ "Too short",
                        interview_duration > time_max ~ "Too long",
                        TRUE ~ "Okay"
                      )
  )
  return(df)
}

# Applying the function to data frame
data <- time_check(data , time_min, time_max)

# Don't check time when the center doesnt need anything.
data$CHECK_interview_duration[which(data$`Does this center need anything?` == "No")] <- "Okay"

sum(data$CHECK_interview_duration == "Too short")
boxplot(data$interview_duration)

# check for outliers
cleaninginspectoR::find_outliers(data)

# check for logical inconsistencies between number of people needing items and people at center
data <- data  %>% mutate(CHECK_number_people_items = case_when(`For how many people does the center need sleeping items?` > `How many people are currently being hosted at the centre?` | 
                                                                   `For how many people does the center need clothes?` > `How many people are currently being hosted at the centre?` |
                                                                         `For how many older people and PWD does the center need supplies?` > `How many people are currently being hosted at the centre?` ~ "Check",
                                                               TRUE ~ "Okay")
)

sum(data$CHECK_number_people_items == "Check")
data %>% write_xlsx("output/data_checked.xlsx")
clog_old <- data %>% filter(CHECK_number_people_items == "Check") %>% 
  select(uuid,
         `For how many people does the center need sleeping items?`,
         `For how many people does the center need clothes?`,
         `For how many older people and PWD does the center need supplies?`) %>% 
  rename("For how many people does the center need sleeping items? - OLD" = "For how many people does the center need sleeping items?",
         "For how many people does the center need clothes? - OLD" = "For how many people does the center need clothes?",
         "For how many older people and PWD does the center need supplies? - OLD" = "For how many older people and PWD does the center need supplies?")

# replace number of people estimate when they exceed people at center with number of people at center
data <- data  %>% mutate(`For how many people does the center need sleeping items?` = case_when(`For how many people does the center need sleeping items?` > `How many people are currently being hosted at the centre?` ~  `How many people are currently being hosted at the centre?`,
                                                                 TRUE ~ `For how many people does the center need sleeping items?`),
                         `For how many people does the center need clothes?` = case_when(`For how many people does the center need clothes?` > `How many people are currently being hosted at the centre?` ~  `How many people are currently being hosted at the centre?`,
                                                                 TRUE ~ `For how many people does the center need clothes?`),
                         `For how many older people and PWD does the center need supplies?` = case_when(`For how many older people and PWD does the center need supplies?` > `How many people are currently being hosted at the centre?` ~ `How many people are currently being hosted at the centre?`,
                                                               TRUE ~ `For how many older people and PWD does the center need supplies?`)
)

clog_new <- data %>% filter(CHECK_number_people_items == "Check") %>% 
  select(uuid,
         `For how many people does the center need sleeping items?`,
         `For how many people does the center need clothes?`,
         `For how many older people and PWD does the center need supplies?`) %>% 
  rename("For how many people does the center need sleeping items? - NEW" = "For how many people does the center need sleeping items?",
         "For how many people does the center need clothes? - NEW" = "For how many people does the center need clothes?",
         "For how many older people and PWD does the center need supplies? - NEW" = "For how many older people and PWD does the center need supplies?")

clog_logical <- left_join(clog_old, clog_new, by = "uuid")
clog_logical %>% write_xlsx("output/clog_logical.xlsx")

# exclude colums with only NAs
data <- data[, colSums(is.na(data)) != nrow(data)]

# exclude sensitive/not-needed columns
cols_exclude <- c("start", 
                  "end", 
                  "deviceid",	
                  "audit",	
                  "audit_URL",	
                  "Enumerator first name",
                  "Name and phone number of the Respondent",
                  "Center that is being assessed",
                  "calc2",
                  "calc3",	
                  "calc4",	
                  "calc5",	
                  "_id",
                  "_submission_time",	
                  "_validation_status",	
                  "_notes",	"_status",	
                  "_submitted_by",	
                  "_tags",
                  "_index",
                  "__version__",
                  "_version_",
                  "_version__001",
                  "interview_duration",	
                  "CHECK_interview_duration",	
                  "CHECK_number_people_items",
                  "center_new"
)
data <- data[!names(data) %in% cols_exclude]

data <- data %>% select(uuid, today, c(1:ncol(data)))

data %>% write_xlsx(paste0("output/MDA_RAC_data_clean_", Sys.Date(), ".xlsx"))
data %>% write_xlsx("input/clean_data.xlsx")
