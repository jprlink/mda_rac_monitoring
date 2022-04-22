

library(tidyverse)
library(dplyr)
library(lubridate)
library(writexl)
library(readxl)


#### Data analysis ####

# load data
data <- read_xlsx("input/clean_data.xlsx")
data$disag <- "total" # add dummy strata for analysis

# exclude empty columns
data <- data[, colSums(is.na(data)) != nrow(data)]

# load tool
survey <- read_xlsx("input/tool.xlsx") %>% 
  mutate(name = tolower(name)) %>% 
  filter(name %in% names(data))
choices <- read_xlsx("input/tool.xlsx", 2)
  
# define question types
questions_so <- survey$name[grepl("select_one", survey$type)]
questions_so <- questions_so[!questions_so %in% "centre_id"]
questions_sm <- survey$name[grepl("select_multiple", survey$type)]
questions_text <- survey$name[grepl("text", survey$type)]
questions_int <- survey$name[grepl("integer", survey$type)] %>% append(c("calc_ukrainian","calc_third_party",
                                                                         "how_many_staff_per_people_hosted", "center_ind_breakdown_age",
                                                                         "center_ind_breakdown_gender",
                                                                         "perc_0_2",	"perc_2_18",	"perc_65_plus",	
                                                                         "perc_2_6",	"perc_7_11",	"perc_12_18"
                                                                         ))
questions_cat <- append(questions_so, questions_sm)
questions_other <- grep("_specify|_explain_why|_other", names(data), value = T)
questions_other <- questions_other[!questions_other %in% c(questions_int, questions_cat)]
questions_sm_num <- data %>% select(contains(questions_sm) & !any_of(c(questions_sm, questions_other))) %>% names()
questions_num <- append(questions_int, questions_sm_num)

# define cross-tabulate functions
getfreq <-function(var,data){
  df<-table(data[[var]]) %>% as.data.frame 
  names(df)[1]<-"var_x_level"
  df$Freq[which(df$var_x_level %in% "NA")] <- 0 # omit NAs
  df$perc<-df$Freq/sum(df$Freq)
  df$var_x<-var
  return(df)
}

crosstab <-function(var,disa,data){
  lapply(unique(data[[disa]]),function(x,var,data,disa){
    dt<-data[data[[disa]]==x,]
    df<-getfreq(var,dt)
    df$var_disa<-disa
    df$var_disa_level<-x
    return(df)
  },data=data,var=var,disa=disa) %>% bind_rows
}

# cross-tabulate select_one results
results_so <- lapply(questions_so,function(y,disa,data){
  crosstab(y,disa,data)
},disa="disag",data=data
) %>% bind_rows

# rename select one columns 
results_so <- results_so %>% 
  mutate(id = paste0(var_x, ".", var_x_level)) %>%
  rename(Result = perc
         ) %>%
  select(id, Result) %>% mutate(Type = "select_one")


# calculate select multiple results
results_sm_wide <- data %>% group_by(disag) %>% 
  summarize(across(.cols = all_of(questions_sm_num), .fns = ~ mean(.x, na.rm = TRUE)))

results_sm_wide <- results_sm_wide[!names(results_sm_wide) %in% c(questions_cat, questions_other, "disag")]

results_sm <- results_sm_wide %>% pivot_longer(everything(),
                                            names_to = "id",
                                            values_to = "Result") %>%
  mutate(Type = "select_multiple")

# calculate average results
results_mean_wide <- data %>% group_by(disag) %>% 
  summarize(across(.cols = all_of(questions_int), .fns = ~ mean(.x, na.rm = TRUE)))

results_mean_wide <- results_mean_wide[!names(results_mean_wide) %in% c(questions_cat, "disag")]

results_mean <- results_mean_wide %>% pivot_longer(everything(),
                                                 names_to = "id",
                                                 values_to = "Result") %>%
  mutate(Type = "average")

# calculate sum results
results_sum_wide <- data %>% group_by(disag) %>% 
  summarize(across(.cols = all_of(questions_int), .fns = ~ sum(.x, na.rm = TRUE)))

results_sum_wide <- results_sum_wide[!names(results_sum_wide) %in% c(questions_cat, "disag", "how_many_staff_per_people_hosted", 
                                                                         "perc_0_2",	"perc_2_18",	"perc_65_plus",	
                                                                         "perc_2_6",	"perc_7_11",	"perc_12_18")]

results_sum <- results_sum_wide %>% pivot_longer(everything(),
                                                           names_to = "id",
                                                           values_to = "Result") %>%
  mutate(Type = "sum")

# add age and gender breakdown
age_total <- sum(data$center_ind_breakdown_age, na.rm = T)
vars_age <- c("child_0_2_number", "how_many_are_children_2_18_years_old", "demo_elderly")
results_age <- results_sum %>% filter(id %in% vars_age) %>% 
  mutate(Result =  Result / age_total,
         Type = "select_one")

adult_results <- 1 - sum(results_age$Result)
results_age[4,"id"] <-  "adults_number"
results_age[4,"Result"] <-  adult_results
results_age[4,"Type"] <- "select_one"

gender_total <- sum(data$center_ind_breakdown_gender, na.rm = T)
vars_gender <- c("how_many_are_women", "how_many_are_men", "how_many_are_other")
results_gender <- results_sum %>% filter(id %in% vars_gender) %>% 
  mutate(Result =  Result / gender_total ,
        Type = "select_one")

# merge all results
results_all <- rbind(results_so,
                     results_sm,
                     results_mean,
                     results_sum,
                     results_age,
                     results_gender)

# attach labels
question_labels <- survey %>% 
  select(type, name, `label::English`, `label::Romanian`) %>% 
  rename(id = type, 
         dependent.var = name,
         "Question (EN)" = `label::English`,
         "Question (RO)" = `label::Romanian`)

choice_labels <- choices %>% 
  select(list_name, name, `label::English`, `label::Romanian`) %>% 
  rename(id = list_name, 
         dependent.var.value = name, 
         "Response (EN)" = `label::English`,
         "Response (RO)" = `label::Romanian`)

question_labels <- question_labels %>% filter(grepl("select_one|select_multiple|integer", id)) 
question_labels <- question_labels %>% 
  mutate(question_type = 
           case_when(grepl("select_one", id) ~ "select_one",
                     grepl("select_multiple", id) ~ "select_multiple",
                     grepl("integer", id) ~ "integer"
           )
  )

question_labels$id <- gsub("select_one |select_multiple ", "", question_labels$id)
question_choice_labels <- left_join(question_labels, choice_labels, by = "id") %>% select(-id) %>%
  mutate(id = paste0(dependent.var, ".", dependent.var.value))

question_choice_labels$id[which(question_choice_labels$question_type == "integer")] <- question_choice_labels$id[which(question_choice_labels$question_type == "integer")] %>% str_replace_all(c(".NA"=""))

results_all_labelled <- left_join(results_all, question_choice_labels, by = "id") 
results_all_labelled <- results_all_labelled  %>% 
  select(id, "Question (EN)", "Question (RO)", "Response (EN)", "Response (RO)", "Type", "Result")

# export results
results_all_labelled %>% write_xlsx(paste0("output/MDA_RAC_analysis_results_", Sys.Date(), ".xlsx"))

