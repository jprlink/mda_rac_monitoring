

library(tidyverse)
library(dplyr)
library(lubridate)
library(writexl)
library(readxl)


#### Data analysis ####

data <- read_xlsx("input/clean_data.xlsx")
data$disag <- "total" # add dummy strata for analysis

survey <- read_xlsx("input/tool.xlsx") %>% 
  mutate(tolower(name)) %>% 
  filter(name %in% names(data))
choices <- read_xlsx("input/tool.xlsx", 2)
  
# define question types
questions_so <- survey$name[grepl("select_one", survey$type)]
questions_so <- questions_so[!questions_so %in% "centre_id"]
questions_sm <- survey$name[grepl("select_multiple", survey$type)]
questions_text <- survey$name[grepl("text", survey$type)]
questions_int <- survey$name[grepl("integer", survey$type)] %>% append(c("calc_ukrainian","calc_third_party"))
questions_cat <- append(questions_so, questions_sm)
questions_num <- append(questions_int, questions_sm)
questions_other <- grep("_specify|_explain_why|_other", names(data), value = T)


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
  summarize(across(.cols = any_of(contains(questions_sm)), .fns = ~ mean(.x, na.rm = TRUE)))

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

results_sum_wide <- results_sum_wide[!names(results_sum_wide) %in% c(questions_cat, "How many staff per people hosted?", "disag")]

results_sum <- results_sum_wide %>% pivot_longer(everything(),
                                                           names_to = "id",
                                                           values_to = "Result") %>%
  mutate(Type = "sum")

# merge all results
results_all <- rbind(results_so,
                     results_sm,
                     results_mean,
                     results_sum)

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

results_all_labelled <- left_join(results_all, question_choice_labels, by = "id") 
results_all_labelled <- results_all_labelled  %>% 
  select(id, "Question (EN)", "Question (RO)", "Response (EN)", "Response (RO)", "Type", "Result")

# export results
results_all_labelled %>% write_xlsx(paste0("output/MDA_RAC_analysis_", Sys.Date(), ".xlsx"))

