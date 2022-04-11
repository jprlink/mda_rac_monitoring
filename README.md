---
output:
  html_document: default
  pdf_document: default
---
# Instructions for using the R scripts data_cleaning and data_analysis of the project mda_rac_monitoring

Steps:

1) Download raw data from kobo, name it "raw_data" put it into the input folder. 

Important: Select only the latest version of the tool and xml values, not labels. And delete the two columns on the enumerator name and center contacts. These are not supposed to even be included in the raw data.

2) Name latest version of the tool, which was used for data collection, "tool" and put it into the input folder. 

Important: Make sure that you update the column "clean_dataset" in which you specify which variables should be included in the shareable version of the clean dataset. And update the column "change_to_overall_percent" in which you specify for which variables with skip logic NAs should be replaced with 0s/no - and hence results should be calculated for the overall population, not the subset.

3) Empty the old content of the file "cleaning_log" in the input folder and only keep the column names

4) Make sure that in the script "data_cleaning", the variable names included in the vectors questions_int, questions_other, questions_int_no_age, vars_age and vars_clean are updated if you make changes to the tool.

5) Run the script "data_cleaning"

6) Check the file "MDA_RAC_raw_data_with_checks_[date]" in the output folder:

In the last columns, you will find macro data checks for each survey:
a) CHECK_no_consent: No consent. Add survey to the tab "deletion" of the cleaning log.
b) CHECK_dup_center_id: Duplicated centre ID (there should be only one per round). if survey is indeed for same centre, add survey to the tab "deletion" of the cleaning log. If not, change the id in the "change" tab.
c) CHECK_interview_duration: interview duration below 5 minutes. If enumerator cannot provide a satisfying explanation, add survey to the tab "deletion" of the cleaning log.

Also, just to be sure, skim the raw data manually if you find any illogical values.

7) Check the output file "clog_logical_[date]": It shows values of variables where the number of people in need for three items exceed number of people at the centre. Add the changes to the "change" tab of the cleaning log. The structure of the file is already similar to the cleaning log, so you just need to copy paste.

8) Check the file "other_response_translations_[date]" in the output folder: It contains all translated "other" responses. Add all translations to the cleaning log tab "change". In addition, if translation corresponds to existing choices of the respective question, add the change to the "change" tab of the cleaning log. Make sure that the variable with the "other" text response (old value is the text response and new value is blank) is added to the cleaning log, as well as the binary variable with the old "other" choice (old value is 1 and new value is 0) and the binary variable with the new choice  (old value is 0 and new value is 1).

9) Check the file "outliers_[date]" in the output folder: Check if any of the outliers require recoding. If yes, add them to the "change" tab of the cleaning log.

Important: Make sure that all numerical values in the new.value column of the cleaning log are formatted as numerical and all character values formatted as character. 

10)Once the cleaning log is completed, run the script "data_cleaning" again. Now check the same outputs and verify whether any of the problematic issues (apart from translations of other responses) are still flagged. If yes, make sure they are addressed correctly in the cleaning log (change and deletion tabs) and then run the cleaning script again. 

Data cleaning is finished now and you will find the clean dataset "MDA_RAC_clean_data_..."", as unlabelled and two labelled (in EN and RO) versions in the output folder.

11) Make sure that in the script "data_analysis", the variable names included in the vectors questions_int and questions_other are updated if you make changes to the tool.

12) Run the script "data_analysis". 

Data analysis is finished now and you will find the analysis table "MDA_RAC_analysis_results_[date]"" in the output folder.




