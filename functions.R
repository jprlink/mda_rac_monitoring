

re_create_sm <- function(df, survey, separator="."){
  multiple_choices <- filter(survey, str_detect(type, "(\\bselect_multiple\\b)"))$name
  for (m in 1:length(multiple_choices)) {
    choice_char_count <- nchar(multiple_choices[m]) + 2
    choices <- df %>% select(starts_with(paste0(multiple_choices[m], separator)))
    for (c in 1:length(choices)) {
      choice_name <- substring(names(choices[c]),choice_char_count)
      for (r in 1:nrow(choices)) {
        if (!is.na(choices[r,c]) & as.numeric(choices[[c]][r]) == 1 & choices[[c]][r] != "NC") {
          temp <- str_detect(df[[multiple_choices[m]]][r], choice_name)
          if(!is.na(temp) & !temp){
            df[r, multiple_choices[m]] <- paste(df[[multiple_choices[m]]][r], choice_name, sep = " ")
            df[r, multiple_choices[m]] <- gsub("NC ","",df[r, multiple_choices[m]])
          }
        }
      }
    }
  }
  return(df)
}

sm_label_toxml <- function(df, survey, separator="."){
  names(survey)<-gsub(":.*","",names(survey))
  multiple_choices <- filter(survey, str_detect(type, "(\\bselect_multiple\\b)"))$name
  for (m in 1:length(multiple_choices)) {
    choice_char_count <- nchar(multiple_choices[m]) + 2
    choices <- df %>% select(starts_with(paste0(multiple_choices[m], separator)))
    df[[multiple_choices[m]]]<-""
    for (c in 1:length(choices)) {
      choice_name <- substring(names(choices[c]),choice_char_count)
      for (r in 1:nrow(choices)) {
        if (!is.na(choices[r,c]) & as.numeric(choices[[c]][r]) == 1 & choices[[c]][r] != "NC") {
          if(df[[multiple_choices[m]]][r]==""){
            df[r, multiple_choices[m]] <- choice_name
            df[r, multiple_choices[m]] <- gsub("NC ","",df[r, multiple_choices[m]])
          } else{
            df[r, multiple_choices[m]] <- paste(df[[multiple_choices[m]]][r], choice_name, sep = " ")
            df[r, multiple_choices[m]] <- gsub("NC ","",df[r, multiple_choices[m]])
          }
        }
      }
    }
  }
  return(df)
}


sl_correction<-function(db,sl_definition,survey,sl_name="name",sl_condition="condition"){
  multiple_choices <- filter(survey, str_detect(type, "(\\bselect_multiple\\b)"))$name
  for (i in 1:nrow(sl_definition)) {
    qname<-sl_definition[[sl_name]][i]
    if(qname%in%names(db)){
      db[[qname]]<-ifelse(eval(parse(text=sl_definition[[sl_condition]][[i]]),envir = db),db[[qname]],"SL")
      if(qname%in%multiple_choices){
        sm<-names(db)[str_detect(names(db),paste0(qname,"[.]"))]
        for(j in 1:length(sm)){
          db[[sm[j]]]<-ifelse(db[[qname]]=="SL"&!is.na(db[[qname]]),"SL",db[[sm[j]]])
        }
      }
    }
  }
  return(db)
}

from_xml_tolabel<-function(db,choices,survey,choices_label,survey_label){
  multiple_choices <- filter(survey, str_detect(type, "(\\bselect_multiple\\b)"))$name
  chr_names<-db %>% select_if(~ !(all(is.na(.x)))) %>% select_if(~ is.character(.)) %>% names
  chr_names<-chr_names[!str_detect(chr_names,paste(paste0(multiple_choices,"."),collapse = "|"))]
  # names(choices)<-gsub(":.*","",names(choices))
  # names(survey)<-gsub(":.*","",names(survey))
  choice_labels <- choices[[choices_label]]
  survey_labels <- survey[[survey_label]]
  
  for (i in 1: length(chr_names)){
    if(chr_names[i]%in%multiple_choices){
      split_sm<-str_split(db[[chr_names[i]]]," ")
      db[[chr_names[i]]]<-lapply(split_sm, function(x)match(x, choices[["name"]])) %>% 
        lapply(.,function(x){ifelse(is.na(x),x,choice_labels[x])}) %>% lapply(., function(x)paste(x,collapse = " ")) %>% unlist
    } else{
      var_label <- match(db[[chr_names[i]]], choices[["name"]])
      db[[chr_names[i]]]<-ifelse(is.na(var_label),db[[chr_names[i]]],choice_labels[var_label])
    }
  }
  names(db)<-gsub(".*[.]","",names(db))
  label_indices<-match(names(db),survey[["name"]])
  names(db)<-ifelse(is.na(label_indices)|is.na(survey_labels[label_indices]),names(db),survey_labels[label_indices])
  choices_indices<-match(names(db),choices[["name"]])
  names(db)<-ifelse(is.na(choices_indices),names(db),choice_labels[choices_indices])
  return(db)
}








#' create standardised cleaninglog
#' @param ids vector of ids that can be matched with the dataset (i.e. uuids)
#' @param variabels vector of variable names (should match data column headers)
#' @param new_values vector of new (corrected) values. If no change should be made, this must be set to the original value
#' @param change vector of logical values: whether a row in the cleaninglog should change data or not (the cleaninglog may contain rows that turned out to be ok and not need any change). Should be the same length as the other vectors. Defaults to `TRUE`.
#' @param data_id_column_name the name of the column in the dataset with ids matching the values in `ids`
#' @param info (optional): a character vector with additional information about the cleaning log
#' @param name a vector name of the data check that produced the
#' @param ... additional vectors or data.frames to be added to the cleaning log.
#' @return a `clog` cleaninglog objects to be used with `clog_clean()` etc.
#' @export
cleaninglog <- function(ids, variables, new_values, change = TRUE, data_id_column_name,name = "default_cleaninglog_element_name",info=NULL,...) {
  
  assert_that(length(ids)==length(variables))
  assert_that(length(ids)==length(new_values))
  assert_that(length(ids)==length(new_values))
  assert_that(length(ids)==length(change))
  assert_that(length(ids)==length(name) | length(name)==1)
  assertthat::assert_that(assertthat::is.string(data_id_column_name))
  more_vars<-list(...)
  
  if(is.null(info)){info<-rep("",length(ids))}
  df<-tibble::tibble(ids=as.character(ids),variables=as.character(variables),new_values=new_values,change = as.logical(change), name = as.character(name),info=as.character(info))
  df<-do.call(c,list(df,more_vars)) %>% (tibble::as_tibble)
  attributes(df)$data_id_column_name<-data_id_column_name
  class(df)<-c("clog_cleaninglog",class(df))
  
  df
}




#' @export
print.clog_cleaninglog<-function(x){
  
  x_tibble<-tibble::as_tibble(x)
  print(x_tibble)
  # class(x_tibble)<-class(x_tibble)[-which(class(x_tibble)=="clog_cleaninglog")]
  # x_tibble[[attributes(x)$cleaninglog$issue_name_column]] %>% table %>% (knitr::kable) %>% print
  message(crayon::green(paste("cleaninglog with", nrow(x)," rows")))
  return(invisible(x))
}




change_df_by_variables_and_ids <-function(data, change_variables, change_ids, data_id_column,new_values, change){
  
  
  change_variables<-as.character(change_variables)
  change_ids<-as.character(change_ids)
  change <- as.logical(change)
  change[is.na(change)]<-TRUE
  data_ids<-data[[data_id_column]]
  data_ids<-as.character(data_ids)
  data_id_column<-make.names(data_id_column)
  
  names(data)<-make.names(names(data))
  change_variables<-make.names(change_variables)
  
  cl<-tibble::tibble(change_variables,change_ids,data_id_column,new_values,change)
  cl<-purrr::map(cl,function(x){if(is.factor(x)){as.character(x)};x}) %>% as_tibble
  
  non_unique_change_ids<-change_ids[change_ids %in% data_ids[duplicated(data_ids)]] %>% unique
  if(length(non_unique_change_ids)!=0){
    warning("some change_ids are not unique in the data ids - ignoring these.")
    cl<-cl[!(change_ids %in% non_unique_change_ids),]
    
  }
  
  unknown_ids<-(!(change_ids %in% data[[data_id_column]]))
  if(any(unknown_ids)){warning(paste(
    "ignoring", length(which(unknown_ids)), "cleaning log rows because ID not found in data"))
    cl<-cl[!unknown_ids,]
  }
  
  
  unknown_variables<-(!(cl$change_variables %in% names(data)))
  if(any(unknown_variables)){
    warning(paste(
      "ignoring", length(which(unknown_variables)), "cleaning log rows because variable name not found in data"))
    cl<-cl[!unknown_variables,]
    
  }
  cl_to_change<-cl[cl$change,]
  change_ids<-cl_to_change$change_ids
  change_variables<-cl_to_change$change_variables
  new_values<-cl_to_change$new_values
  change <- cl_to_change$change
  rows_to_change<-match(change_ids,data_ids)
  cols_to_change<-match(change_variables,colnames(data))
  
  old_values<-purrr:::pmap(list(rows_to_change,cols_to_change),function(row,col){
    data[row,col]
  }) %>% unlist
  
  change_value<-function(row,col,new){
    
    data[row,col]<<-new
  }
  mapply(change_value,rows_to_change,cols_to_change,new_values)
  
  changelog<-tibble(change_variables,change_ids,old_values,new_values,change,data_id_column)
  
  attributes(data)$changelog<-rbind(attributes(data)$changelog,changelog)
  class(data)<-c("clog_modified_data", class(data)) %>% unique
  data
}

#' @export
print.clog_modified_data<-function(x){
  log<-attributes(x)$changelog
  class(x)<-class(x)[class(x)!="clog_modified_data"]
  print(x)
  changed_value_to_value<-which(log$old_values!=log$new_values)
  changed_na<-which(is.na(log$old_values)!=is.na(log$new_values))
  num_changed<-length(c(changed_value_to_value,changed_na))
  cat(paste0(crayon::yellow(num_changed," values modified; show edits with clog_changelog()\n")))
}

#' make changes to raw dataset based on cleaninglog (see `cleaninglog()` )
#' @param df a data.frame / tibble
#' @param cleaninglog a cleaninglog object create with `cleaninglog()`
#' @return the original dataframe (`df`) modified based on the cleaning log.
#' @export
clog_clean<-function(df,cleaninglog){
  df<-tibble::as_tibble(df)
  attributes(df)$raw_data <- df
  df<-change_df_by_variables_and_ids(data = df,
                                     change_variables = cleaninglog$variables,
                                     change_ids = cleaninglog$ids,
                                     data_id_column = attributes(cleaninglog)$data_id_column_name,
                                     new_values = cleaninglog$new_values,
                                     change = cleaninglog$change)
  
  class(df)<-c("clog_modified_data", class(df)) %>% unique
  df
  
}


#' reverse changes applied based on cleaning log
#' @param df a data frame modified with `clog_clean()`
#' @return the original cleaning log applied in reverse.
#' @export
clog_reverse<-function(df){
  log<-attributes(df)$changelog
  df<-change_df_by_variables_and_ids(df,
                                     change_variables = log$change_variables,
                                     change_ids = log$change_ids,
                                     data_id_column = log$data_id_column[1],
                                     new_values = log$old_values,
                                     change = log$change)
  
  attributes(df)$changelog<-NULL
  # class(df)<-class(df)[class(df)!="clog_modified_data"]
  df
}

#' extract cleaninglog from dataset modified with `clog_clean()`
#' @param df a dataset modified with `clog_clean()`
#' @return the cleaninglog as it was applied to `df`
#' @export
clog_get_cleaninglog<-function(df){
  attributes(df)$changelog
}









