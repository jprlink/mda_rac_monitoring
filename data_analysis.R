

library(tidyverse)
library(dplyr)
library(lubridate)
library(writexl)
library(readxl)


#### Data analysis ####

data <- read_xlsx("input/clean_data.xlsx")
survey <- read_xlsx("input/tool.xlsx") %>% filter(`label::English` %in% names(data))

# define question types
questions_so <- survey$`label::English`[grepl("select_one", survey$type)]
questions_sm <- survey$`label::English`[grepl("select_multiple", survey$type)]
questions_text <- survey$`label::English`[grepl("text", survey$type)]
questions_int <- survey$`label::English`[grepl("integer", survey$type)]
questions_cat <- append(questions_so, questions_sm)
questions_num <- append(questions_int, questions_sm)
questions_other <- grep("If other, please specify|If not, may I ask you why", names(data), value = T)

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
results_so <- results_so %>% rename(Frequency = Freq,
                                    Percentage = perc,
                                    Question = var_x,
                                    Response = var_x_level) %>%
  select(Question, Response, Frequency, Percentage)


# calculate select multiple results
results_sm_wide <- data %>% group_by(disag) %>% 
  summarize(across(.cols = any_of(contains(questions_sm)), .fns = ~ mean(.x, na.rm = TRUE)))

results_sm_wide <- results_sm_wide[!names(results_sm_wide) %in% c(questions_cat, "disag")]

results_sm <- results_sm_wide %>% pivot_longer(everything(),
                                            names_to = "Question/response",
                                            values_to = "Percentage")

# calculate average results
results_mean_wide <- data %>% group_by(disag) %>% 
  summarize(across(.cols = all_of(questions_int), .fns = ~ mean(.x, na.rm = TRUE)))

results_mean_wide <- results_mean_wide[!names(results_mean_wide) %in% c(questions_cat, "disag")]

results_mean <- results_mean_wide %>% pivot_longer(everything(),
                                                 names_to = "Question/response",
                                                 values_to = "Average")

# calculate sum results
results_sum_wide <- data %>% group_by(disag) %>% 
  summarize(across(.cols = all_of(questions_int), .fns = ~ sum(.x, na.rm = TRUE)))

results_sum_wide <- results_sum_wide[!names(results_sum_wide) %in% c(questions_cat, "How many staff per people hosted?", "disag")]

results_sum <- results_sum_wide %>% pivot_longer(everything(),
                                                           names_to = "Question/response",
                                                           values_to = "Sum")

results_so  %>% write_xlsx(paste0("output/MDA_RAC_analysis_select_one_", Sys.Date(), ".xlsx"))
results_sm %>% write_xlsx(paste0("output/MDA_RAC_analysis_select_multiple_", Sys.Date(), ".xlsx"))
results_sum %>% write_xlsx(paste0("output/MDA_RAC_analysis_sums_", Sys.Date(), ".xlsx"))
results_mean %>% write_xlsx(paste0("output/MDA_RAC_analysis_averages_", Sys.Date(), ".xlsx"))




