library(tidyverse)
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_211')
library(XLConnect)
library(janitor)
library(RDCOMClient)
library(data.table)
library(lubridate)
# library(RDCOMServer)

# Read in district accountability file
dist_acct_total <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/district_accountability_file.csv") %>% 
  filter(!system %in% c(90, 960, 963))
# dist_acct_total <- read_csv("N:/ORP_accountability/projects/Andrew/Pre-Coding-2019/data/district_accountability_2019_example.csv")

# Read in district determination file
dist_determinations_total <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/district_designations.csv")

# Loop to district heat maps
district_list <- unique(dist_acct_total$system)

# district_list <- c(650)


for(district in district_list){

  dist_acct <- dist_acct_total %>% 
    filter(system == district)
  
  achievement_sub <- dist_acct %>% 
    filter(indicator == 'Achievement')
  
  dist_determination <- dist_determinations_total %>% 
    filter(system == district)
  
  if((dim(dist_determination)[1] != 0) & (dim(dist_acct)[1] != 0)){
    
    # Account for districts missing certain things
    if(!'Graduation Rate' %in% dist_acct$indicator) {
      dist_acct <- dist_acct %>% 
        bind_rows(data.frame("indicator" = 'Graduation Rate', "grade" = "All Grades", "subgroup"= "All Students", stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("indicator" = 'Graduation Rate', "grade" = "All Grades", "subgroup"= "English Learners with Transitional 1-4", stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("indicator" = 'Graduation Rate', "grade" = "All Grades", "subgroup"= "Black/Hispanic/Native American", stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("indicator" = 'Graduation Rate', "grade" = "All Grades", "subgroup"= "Students with Disabilities", stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("indicator" = 'Graduation Rate', "grade" = "All Grades", "subgroup"= "Economically Disadvantaged", stringsAsFactors = FALSE))
    }
    if(!'English Learners with Transitional 1-4' %in% dist_acct$subgroup) {
      dist_acct <- dist_acct %>% 
        bind_rows(data.frame("indicator" = 'Achievement', "grade" = "3rd through 5th", "subgroup"= "English Learners with Transitional 1-4", stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("indicator" = 'Achievement', "grade" = "6th through 8th", "subgroup"= "English Learners with Transitional 1-4", stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("indicator" = 'Achievement', "grade" = "9th through 12th", "subgroup"= "English Learners with Transitional 1-4", stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("indicator" = 'Graduation Rate', "grade" = "All Grades", "subgroup"= "English Learners with Transitional 1-4", stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("indicator" = 'Chronic Absenteeism', "grade" = "All Grades", "subgroup"= "English Learners with Transitional 1-4", stringsAsFactors = FALSE))
    }
    if(!'ELPA Growth Standard' %in% dist_acct$indicator) {
      dist_acct <- dist_acct %>% 
        bind_rows(data.frame("indicator" = 'ELPA Growth Standard', "grade" = "All Grades", "subgroup"= "English Learners with Transitional 1-4", stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("indicator" = 'ELPA Growth Standard', "grade" = "All Grades", "subgroup"= "All Students", stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("indicator" = 'ELPA Growth Standard', "grade" = "All Grades", "subgroup"= "Black/Hispanic/Native American", stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("indicator" = 'ELPA Growth Standard', "grade" = "All Grades", "subgroup"= "Students with Disabilities", stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("indicator" = 'ELPA Growth Standard', "grade" = "All Grades", "subgroup"= "Economically Disadvantaged", stringsAsFactors = FALSE))
    }
    if(!'9th through 12th' %in% achievement_sub$grade) {
      dist_acct <- dist_acct %>% 
        bind_rows(data.frame("indicator" = 'Achievement', "grade" = "9th through 12th", "subgroup"= "All Students", stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("indicator" = 'Achievement', "grade" = "9th through 12th", "subgroup"= "English Learners with Transitional 1-4", stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("indicator" = 'Achievement', "grade" = "9th through 12th", "subgroup"= "Black/Hispanic/Native American", stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("indicator" = 'Achievement', "grade" = "9th through 12th", "subgroup"= "Students with Disabilities", stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("indicator" = 'Achievement', "grade" = "9th through 12th", "subgroup"= "Economically Disadvantaged", stringsAsFactors = FALSE))
    }
    if(!'3rd through 5th' %in% achievement_sub$grade) {
      dist_acct <- dist_acct %>% 
        bind_rows(data.frame("indicator" = 'Achievement', "grade" = "3rd through 5th", "subgroup"= "All Students", stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("indicator" = 'Achievement', "grade" = "3rd through 5th", "subgroup"= "English Learners with Transitional 1-4", stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("indicator" = 'Achievement', "grade" = "3rd through 5th", "subgroup"= "Black/Hispanic/Native American", stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("indicator" = 'Achievement', "grade" = "3rd through 5th", "subgroup"= "Students with Disabilities", stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("indicator" = 'Achievement', "grade" = "3rd through 5th", "subgroup"= "Economically Disadvantaged", stringsAsFactors = FALSE))
    }
    if(!'6th through 8th' %in% achievement_sub$grade) {
      dist_acct <- dist_acct %>% 
        bind_rows(data.frame("indicator" = 'Achievement', "grade" = "6th through 8th", "subgroup"= "All Students", stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("indicator" = 'Achievement', "grade" = "6th through 8th", "subgroup"= "English Learners with Transitional 1-4", stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("indicator" = 'Achievement', "grade" = "6th through 8th", "subgroup"= "Black/Hispanic/Native American", stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("indicator" = 'Achievement', "grade" = "6th through 8th", "subgroup"= "Students with Disabilities", stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("indicator" = 'Achievement', "grade" = "6th through 8th", "subgroup"= "Economically Disadvantaged", stringsAsFactors = FALSE))
    }
    
    # Check if still missing something
    achievement_sub <- dist_acct %>% 
      filter(indicator == 'Achievement')
    
    grad_sub <- dist_acct %>% 
      filter(indicator == 'Graduation Rate')
    
    ca_sub <- dist_acct %>% 
      filter(indicator == 'Chronic Absenteeism')
    
    elpa_sub <- dist_acct %>% 
      filter(indicator == 'ELPA Growth Standard')
    
    if(!'English Learners with Transitional 1-4' %in% grad_sub$subgroup) {
      dist_acct <- dist_acct %>% 
        bind_rows(data.frame("indicator" = 'Graduation Rate', "grade" = "9th through 12th", "subgroup"= "English Learners with Transitional 1-4", stringsAsFactors = FALSE))
    }
    if(!'English Learners with Transitional 1-4' %in% achievement_sub$subgroup) {
      dist_acct <- dist_acct %>% 
        bind_rows(data.frame("indicator" = 'Achievement', "grade" = "3rd through 5th", "subgroup"= "English Learners with Transitional 1-4", stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("indicator" = 'Achievement', "grade" = "6th through 8th", "subgroup"= "English Learners with Transitional 1-4", stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("indicator" = 'Achievement', "grade" = "9th through 12th", "subgroup"= "English Learners with Transitional 1-4", stringsAsFactors = FALSE))
    }
    if(!'English Learners with Transitional 1-4' %in% ca_sub$subgroup) {
      dist_acct <- dist_acct %>% 
        bind_rows(data.frame("indicator" = 'Chronic Absenteeism', "grade" = "All Grades", "subgroup"= "English Learners with Transitional 1-4", stringsAsFactors = FALSE))
    }
    if(!'English Learners with Transitional 1-4' %in% elpa_sub$subgroup) {
      dist_acct <- dist_acct %>% 
        bind_rows(data.frame("indicator" = 'ELPA Growth Standard', "grade" = "All Grades", "subgroup"= "English Learners with Transitional 1-4", stringsAsFactors = FALSE))
    }
    # BHN
    if(!'Black/Hispanic/Native American' %in% grad_sub$subgroup) {
      dist_acct <- dist_acct %>% 
        bind_rows(data.frame("indicator" = 'Graduation Rate', "grade" = "9th through 12th", "subgroup"= "Black/Hispanic/Native American", stringsAsFactors = FALSE))
    }
    if(!'Black/Hispanic/Native American' %in% achievement_sub$subgroup) {
      dist_acct <- dist_acct %>% 
        bind_rows(data.frame("indicator" = 'Achievement', "grade" = "3rd through 5th", "subgroup"= "Black/Hispanic/Native American", stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("indicator" = 'Achievement', "grade" = "6th through 8th", "subgroup"= "Black/Hispanic/Native American", stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("indicator" = 'Achievement', "grade" = "9th through 12th", "subgroup"= "Black/Hispanic/Native American", stringsAsFactors = FALSE))
    }
    if(!'Black/Hispanic/Native American' %in% ca_sub$subgroup) {
      dist_acct <- dist_acct %>% 
        bind_rows(data.frame("indicator" = 'Chronic Absenteeism', "grade" = "All Grades", "subgroup"= "Black/Hispanic/Native American", stringsAsFactors = FALSE))
    }
    if(!'Black/Hispanic/Native American' %in% elpa_sub$subgroup) {
      dist_acct <- dist_acct %>% 
        bind_rows(data.frame("indicator" = 'ELPA Growth Standard', "grade" = "All Grades", "subgroup"= "Black/Hispanic/Native American", stringsAsFactors = FALSE))
    }
    # SWD
    if(!'Students with Disabilities' %in% grad_sub$subgroup) {
      dist_acct <- dist_acct %>% 
        bind_rows(data.frame("indicator" = 'Graduation Rate', "grade" = "9th through 12th", "subgroup"= "Students with Disabilities", stringsAsFactors = FALSE))
    }
    if(!'Students with Disabilities' %in% achievement_sub$subgroup) {
      dist_acct <- dist_acct %>% 
        bind_rows(data.frame("indicator" = 'Achievement', "grade" = "3rd through 5th", "subgroup"= "Students with Disabilities", stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("indicator" = 'Achievement', "grade" = "6th through 8th", "subgroup"= "Students with Disabilities", stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("indicator" = 'Achievement', "grade" = "9th through 12th", "subgroup"= "Students with Disabilities", stringsAsFactors = FALSE))
    }
    if(!'Students with Disabilities' %in% ca_sub$subgroup) {
      dist_acct <- dist_acct %>% 
        bind_rows(data.frame("indicator" = 'Chronic Absenteeism', "grade" = "All Grades", "subgroup"= "Students with Disabilities", stringsAsFactors = FALSE))
    }
    if(!'Students with Disabilities' %in% elpa_sub$subgroup) {
      dist_acct <- dist_acct %>% 
        bind_rows(data.frame("indicator" = 'ELPA Growth Standard', "grade" = "All Grades", "subgroup"= "Students with Disabilities", stringsAsFactors = FALSE))
    }
    # Economically Disadvantaged
    if(!'Economically Disadvantaged' %in% grad_sub$subgroup) {
      dist_acct <- dist_acct %>% 
        bind_rows(data.frame("indicator" = 'Graduation Rate', "grade" = "9th through 12th", "subgroup"= "Economically Disadvantaged", stringsAsFactors = FALSE))
    }
    if(!'Economically Disadvantaged' %in% achievement_sub$subgroup) {
      dist_acct <- dist_acct %>% 
        bind_rows(data.frame("indicator" = 'Achievement', "grade" = "3rd through 5th", "subgroup"= "Economically Disadvantaged", stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("indicator" = 'Achievement', "grade" = "6th through 8th", "subgroup"= "Economically Disadvantaged", stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("indicator" = 'Achievement', "grade" = "9th through 12th", "subgroup"= "Economically Disadvantaged", stringsAsFactors = FALSE))
    }
    if(!'Economically Disadvantaged' %in% ca_sub$subgroup) {
      dist_acct <- dist_acct %>% 
        bind_rows(data.frame("indicator" = 'Chronic Absenteeism', "grade" = "All Grades", "subgroup"= "Economically Disadvantaged", stringsAsFactors = FALSE))
    }
    if(!'Economically Disadvantaged' %in% elpa_sub$subgroup) {
      dist_acct <- dist_acct %>% 
        bind_rows(data.frame("indicator" = 'ELPA Growth Standard', "grade" = "All Grades", "subgroup"= "Economically Disadvantaged", stringsAsFactors = FALSE))
    }
    # All Students
    if(!'All Students' %in% grad_sub$subgroup) {
      dist_acct <- dist_acct %>% 
        bind_rows(data.frame("indicator" = 'Graduation Rate', "grade" = "9th through 12th", "subgroup"= "All Students", stringsAsFactors = FALSE))
    }
    if(!'All Students' %in% achievement_sub$subgroup) {
      dist_acct <- dist_acct %>% 
        bind_rows(data.frame("indicator" = 'Achievement', "grade" = "3rd through 5th", "subgroup"= "All Students", stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("indicator" = 'Achievement', "grade" = "6th through 8th", "subgroup"= "All Students", stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("indicator" = 'Achievement', "grade" = "9th through 12th", "subgroup"= "All Students", stringsAsFactors = FALSE))
    }
    if(!'All Students' %in% ca_sub$subgroup) {
      dist_acct <- dist_acct %>% 
        bind_rows(data.frame("indicator" = 'Chronic Absenteeism', "grade" = "All Grades", "subgroup"= "All Students", stringsAsFactors = FALSE))
    }
    if(!'All Students' %in% elpa_sub$subgroup) {
      dist_acct <- dist_acct %>% 
        bind_rows(data.frame("indicator" = 'ELPA Growth Standard', "grade" = "All Grades", "subgroup"= "All Students", stringsAsFactors = FALSE))
    }
    
    dist_acct <- (dist_acct %>% filter(indicator == 'Achievement') %>% arrange(grade, subgroup)) %>% 
      bind_rows(dist_acct %>% filter(indicator == 'Chronic Absenteeism') %>% arrange(subgroup)) %>% 
      bind_rows(dist_acct %>% filter(indicator == 'ELPA Growth Standard') %>% arrange(subgroup)) %>% 
      bind_rows(dist_acct %>% filter(indicator == 'Graduation Rate') %>% arrange(subgroup))
  
    file_base <- paste0(as.character(district), '_DistrictHeatMapFile_', as.character(month(Sys.Date(), label = TRUE)), as.character(day(Sys.Date())),'.xlsx')
    file_location <- "N:/ORP_accountability/projects/2019_district_accountability/heat maps/"
    raw_file_path <- paste0(file_location, file_base)
    
    # wb <- loadWorkbook("N:/ORP_accountability/projects/Andrew/Pre-Coding-2019/district_heat_maps/district_heat_map_template.xlsx", create = TRUE)
    # 
    # participation_rates <- readWorksheet(wb, sheet = 1, startRow = 1, endRow = 5, startCol = 0, endCol = 6) %>% 
    #   clean_names()
    
    file.copy("N:/ORP_accountability/projects/Andrew/Pre-Coding-2019/district_heat_maps/district_heat_map_template.xlsx", 
              raw_file_path)
    
    xlApp <- COMCreate("Excel.Application")
    
    wb    <- xlApp[["Workbooks"]]$Open(raw_file_path)
    
    # ============================ Participation Rate ==================================================
    participation_rate_sheet <- wb$Worksheets("Participation Rates")
    
    # change the value of a single cell
    # cell  <- sheet$Cells("C3")
    # cell[["Value"]] <- 3.1
    
    participation_df <- dist_acct %>% 
      filter(indicator %in% c('Achievement', 'Graduation Rate')) %>% 
      select(indicator:participation_rate) %>% 
      mutate(grade = if_else(indicator == 'Graduation Rate', 'ACT Participation', grade),
             participation_rate = participation_rate/100) %>% 
      select(-indicator) 
    
    if(!'English Learners with Transitional 1-4' %in% participation_df$subgroup) {
      participation_df <- participation_df %>% 
        bind_rows(data.frame("grade" = "3rd through 5th", "subgroup"= "English Learners with Transitional 1-4", stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("grade" = "6th through 8th", "subgroup"= "English Learners with Transitional 1-4", stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("grade" = "9th through 12th", "subgroup"= "English Learners with Transitional 1-4", stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("grade" = "ACT Participation", "subgroup"= "English Learners with Transitional 1-4", stringsAsFactors = FALSE)) %>% 
        arrange(grade, subgroup)
    }
    if(!'Students with Disabilities' %in% participation_df$subgroup) {
      participation_df <- participation_df %>% 
        bind_rows(data.frame("grade" = "3rd through 5th", "subgroup"= "Students with Disabilities", stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("grade" = "6th through 8th", "subgroup"= "Students with Disabilities", stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("grade" = "9th through 12th", "subgroup"= "Students with Disabilities", stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("grade" = "ACT Participation", "subgroup"= "Students with Disabilities", stringsAsFactors = FALSE)) %>% 
        arrange(grade, subgroup)
    }
    if(!'Economically Disadvantaged' %in% participation_df$subgroup) {
      participation_df <- participation_df %>% 
        bind_rows(data.frame("grade" = "3rd through 5th", "subgroup"= "Economically Disadvantaged", stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("grade" = "6th through 8th", "subgroup"= "Economically Disadvantaged", stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("grade" = "9th through 12th", "subgroup"= "Economically Disadvantaged", stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("grade" = "ACT Participation", "subgroup"= "Economically Disadvantaged", stringsAsFactors = FALSE)) %>% 
        arrange(grade, subgroup)
    }
    if(!'Black/Hispanic/Native American' %in% participation_df$subgroup) {
      participation_df <- participation_df %>% 
        bind_rows(data.frame("grade" = "3rd through 5th", "subgroup"= "Black/Hispanic/Native American", stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("grade" = "6th through 8th", "subgroup"= "Black/Hispanic/Native American", stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("grade" = "9th through 12th", "subgroup"= "Black/Hispanic/Native American", stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("grade" = "ACT Participation", "subgroup"= "Black/Hispanic/Native American", stringsAsFactors = FALSE)) %>% 
        arrange(grade, subgroup)
    }
    
    participation_df <- participation_df %>% 
      spread(subgroup, participation_rate)
    
    # participation_df[is.na(participation_df)] <- ''
    
    # Loop to add data to rows in participation rate sheet
    for(i in 2:5){
      cell <- paste0('B', as.character(i), ':', 'F', as.character(i))
      all_student_participation <- participation_rate_sheet$Range(cell)
      all_student_participation[["Value"]] <- as.numeric( participation_df[i-1,2:ncol(participation_df)])
    }
    
    na_df <- as.data.frame(which(is.na(participation_df), arr.ind=TRUE))
    if(dim(na_df)[1] != 0){
      for(i in 1:nrow(na_df)){
        na_row <- na_df$row[[i]]
        na_col <- na_df$col[[i]]
        excel_col <- case_when(
          na_col == 2 ~ 'B',
          na_col == 3 ~ 'C',
          na_col == 4 ~ 'D',
          na_col == 5 ~ 'E',
          na_col == 6 ~ 'F'
        )
        cell_loc <- paste0(excel_col, as.character(na_row+1), ':', excel_col, as.character(na_row+1))
        na_replacement <- participation_rate_sheet$Range(cell_loc)
        na_replacement[["Value"]] <- as.character("")
      }
    }
    
    # =============================== Final Determination ================================
    determination_sheet <- wb$Worksheets("Final Determination")
    
    achievement_avg <- determination_sheet$Range("B2:B2")
    achievement_avg[["Value"]] <- as.numeric(dist_determination$achievement_average[[1]])
    
    subgroup_avg <- determination_sheet$Range("B3:B3")
    subgroup_avg[["Value"]] <- as.numeric(dist_determination$subgroup_average[[1]])
    
    achievement_determination <- determination_sheet$Range("C2:C2")
    achievement_determination[["Value"]] <- as.character(dist_determination$achievement_determination[[1]])
    
    subgroup_determination <- determination_sheet$Range("C3:C3")
    subgroup_determination[["Value"]] <- as.character(dist_determination$subgroup_determination[[1]])
    
    overall_avg <- determination_sheet$Range("D2:D2")
    overall_avg[["Value"]] <- as.numeric(dist_determination$overall_average[[1]])
    
    overall_determination <- determination_sheet$Range("E2:E2")
    overall_determination[["Value"]] <- as.character(dist_determination$final_determination[[1]])
    
    # =================================== Achievement Pathway ===============================
    achievement_pathway_sheet <- wb$Worksheets("Achievement Pathway")
    
    achievement_path_df <- dist_acct %>% 
      filter(subgroup == 'All Students') %>% 
      select(indicator, grade, AMO_pathway, absolute_pathway, value_add_pathway, overall_score)
    
    for(i in 2:7){
      cell <- paste0('B', as.character(i), ':', 'E', as.character(i))
      achievement_pathway <- achievement_pathway_sheet$Range(cell)
      achievement_pathway[["Value"]] <- as.numeric(achievement_path_df[i-1,3:ncol(achievement_path_df)])
    }
    
    na_df_success <- as.data.frame(which(is.na(achievement_path_df), arr.ind=TRUE))
    if(dim(na_df_success)[1] != 0){
      for(i in 1:nrow(na_df_success)){
        na_row <- na_df_success$row[[i]]
        na_col <- na_df_success$col[[i]]
        excel_col <- case_when(
          na_col == 3 ~ 'B',
          na_col == 4 ~ 'C',
          na_col == 5 ~ 'D',
          na_col == 6 ~ 'E'
        )
        cell_loc <- paste0(excel_col, as.character(na_row+1), ':', excel_col, as.character(na_row+1))
        na_replacement <- achievement_pathway_sheet$Range(cell_loc)
        na_replacement[["Value"]] <- as.character("")
      }
    }
    
    # ============================== Subgroup Pathway ====================================
    subgroup_pathway_sheet <- wb$Worksheets("Subgroup Pathway")
    
    subgroup_path_df <- dist_acct %>% 
      filter(subgroup != 'All Students') 
    
    if (!'9th through 12th' %in% subgroup_path_df$grade) {
      subgroup_path_df <- subgroup_path_df %>% 
        bind_rows(data.frame("indicator" = "Achievement",'grade' = '9th through 12th', 'subgroup' = 'Black/Hispanic/Native American', 'overall_score' = NA, stringsAsFactors = FALSE))
    }
    if (!'Graduation Rate' %in% subgroup_path_df$indicator) {
      subgroup_path_df <- subgroup_path_df %>% 
        bind_rows(data.frame("indicator" = "Graduation Rate",'grade' = 'All Grades', 'subgroup' = 'Black/Hispanic/Native American', 'overall_score' = NA, stringsAsFactors = FALSE))
    }
    
    subgroup_path_df <- (subgroup_path_df %>% filter(indicator == 'Achievement') %>% arrange(grade, subgroup)) %>% 
      bind_rows(subgroup_path_df %>% filter(indicator == 'Chronic Absenteeism') %>% arrange(subgroup)) %>% 
      bind_rows(subgroup_path_df %>% filter(indicator == 'ELPA Growth Standard') %>% arrange(subgroup)) %>% 
      bind_rows(subgroup_path_df %>% filter(indicator == 'Graduation Rate') %>% arrange(subgroup))
    
    subgroup_path_df <- subgroup_path_df %>% 
      select(indicator, grade, subgroup, overall_score) %>% 
      mutate(
        grade = case_when(
          indicator == 'Chronic Absenteeism' ~ 'Absenteeism',
          indicator == 'ELPA Growth Standard' ~ 'ELPA',
          indicator == 'Graduation Rate' ~ 'Grad Rate',
          TRUE ~ grade
        )
      ) %>% 
      select(-indicator) %>% 
      spread(subgroup, overall_score)
    
    for(i in 2:7){
      cell <- paste0('B', as.character(i), ':', 'E', as.character(i))
      subgroup_pathway <- subgroup_pathway_sheet$Range(cell)
      subgroup_pathway[["Value"]] <- as.numeric(subgroup_path_df[i-1,2:ncol(subgroup_path_df)])
    }
    
    na_df_subgroup <- as.data.frame(which(is.na(subgroup_path_df), arr.ind=TRUE))
    if(dim(na_df_subgroup)[1] != 0){
      for(i in 1:nrow(na_df_subgroup)){
        na_row <- na_df_subgroup$row[[i]]
        na_col <- na_df_subgroup$col[[i]]
        excel_col <- case_when(
          na_col == 2 ~ 'B',
          na_col == 3 ~ 'C',
          na_col == 4 ~ 'D',
          na_col == 5 ~ 'E'
        )
        cell_loc <- paste0(excel_col, as.character(na_row+1), ':', excel_col, as.character(na_row+1))
        na_replacement <- subgroup_pathway_sheet$Range(cell_loc)
        na_replacement[["Value"]] <- as.character("")
      }
    }
    
    subgroup_avg <- dist_acct %>% 
      filter(subgroup != 'All Students') 
    
    if (!'9th through 12th' %in% subgroup_avg$grade) {
      subgroup_avg <- subgroup_avg %>% 
        bind_rows(data.frame("indicator" = "Achievement",'grade' = '9th through 12th', 'subgroup' = 'Black/Hispanic/Native American', 'overall_score' = NA, stringsAsFactors = FALSE))
    }
    if (!'Graduation Rate' %in% subgroup_avg$indicator) {
      subgroup_avg <- subgroup_avg %>% 
        bind_rows(data.frame("indicator" = "Graduation Rate",'grade' = 'All Grades', 'subgroup' = 'Black/Hispanic/Native American', 'overall_score' = NA, stringsAsFactors = FALSE))
    }
    
    subgroup_avg <- (subgroup_avg %>% filter(indicator == 'Achievement') %>% arrange(grade, subgroup)) %>% 
      bind_rows(subgroup_avg %>% filter(indicator == 'Chronic Absenteeism') %>% arrange(subgroup)) %>% 
      bind_rows(subgroup_avg %>% filter(indicator == 'ELPA Growth Standard') %>% arrange(subgroup)) %>% 
      bind_rows(subgroup_avg %>% filter(indicator == 'Graduation Rate') %>% arrange(subgroup))
    
    subgroup_avg <- subgroup_avg %>% 
      select(indicator, grade, subgroup, overall_score) %>% 
      mutate(
        grade = case_when(
          indicator == 'Chronic Absenteeism' ~ 'Absenteeism',
          indicator == 'ELPA Growth Standard' ~ 'ELPA',
          indicator == 'Graduation Rate' ~ 'Grad Rate',
          TRUE ~ grade
        )
      ) %>% 
      select(-indicator) %>%
      group_by(subgroup) %>%
      mutate(
        subgroup_avg = mean(overall_score, na.rm = TRUE)
      ) %>%
      ungroup() %>% 
      select(subgroup, subgroup_avg) %>% 
      distinct() %>% 
      spread(subgroup, subgroup_avg)
    
    subgroup_avg_range <- subgroup_pathway_sheet$Range("B8:E8")
    subgroup_avg_range[["Value"]] <- as.numeric(subgroup_avg[1,1:ncol(subgroup_avg)])
    
    na_df_subgroup_avg <- as.data.frame(which(is.na(subgroup_avg), arr.ind=TRUE))
    if(dim(na_df_subgroup_avg)[1] != 0){
      for(i in 1:nrow(na_df_subgroup_avg)){
        na_row <- na_df_subgroup_avg$row[[i]]
        na_col <- na_df_subgroup_avg$col[[i]]
        excel_col <- case_when(
          na_col == 1 ~ 'B',
          na_col == 2 ~ 'C',
          na_col == 3 ~ 'D',
          na_col == 4 ~ 'E'
        )
        cell_loc <- paste0(excel_col, as.character(8), ':', excel_col, as.character(8))
        na_replacement <- subgroup_pathway_sheet$Range(cell_loc)
        na_replacement[["Value"]] <- as.character("")
      }
    }
    
    # ============================ Individual Subgroup Heat Maps ===============================
    individual_subgroups <- wb$Worksheets("Individual Subgroup Heat Maps")
    
    individual_subgroups_df <- dist_acct %>% 
      filter(subgroup != 'All Students') 
    
    if (!'9th through 12th' %in% individual_subgroups_df$grade) {
      individual_subgroups_df <- individual_subgroups_df %>% 
        bind_rows(data.frame("indicator" = "Achievement",'grade' = '9th through 12th', 'subgroup' = 'Black/Hispanic/Native American', 'overall_score' = NA, stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("indicator" = "Achievement",'grade' = '9th through 12th', 'subgroup' = 'English Learners with Transitional 1-4', 'overall_score' = NA, stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("indicator" = "Achievement",'grade' = '9th through 12th', 'subgroup' = 'Economically Disadvantaged', 'overall_score' = NA, stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("indicator" = "Achievement",'grade' = '9th through 12th', 'subgroup' = 'Students with Disabilities', 'overall_score' = NA, stringsAsFactors = FALSE)) 
    }
    if (!'Graduation Rate' %in% individual_subgroups_df$indicator) {
      individual_subgroups_df <- individual_subgroups_df %>% 
        bind_rows(data.frame("indicator" = "Graduation Rate",'grade' = 'All Grades', 'subgroup' = 'Black/Hispanic/Native American', 'overall_score' = NA, stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("indicator" = "Graduation Rate",'grade' = 'All Grades', 'subgroup' = 'English Learners with Transitional 1-4', 'overall_score' = NA, stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("indicator" = "Graduation Rate",'grade' = 'All Grades', 'subgroup' = 'Economically Disadvantaged', 'overall_score' = NA, stringsAsFactors = FALSE)) %>% 
        bind_rows(data.frame("indicator" = "Graduation Rate",'grade' = 'All Grades', 'subgroup' = 'Students with Disabilities', 'overall_score' = NA, stringsAsFactors = FALSE)) 
    }
    
    individual_subgroups_df <- (individual_subgroups_df %>% filter(indicator == 'Achievement') %>% arrange(grade, subgroup)) %>% 
      bind_rows(individual_subgroups_df %>% filter(indicator == 'Chronic Absenteeism') %>% arrange(subgroup)) %>% 
      bind_rows(individual_subgroups_df %>% filter(indicator == 'ELPA Growth Standard') %>% arrange(subgroup)) %>% 
      bind_rows(individual_subgroups_df %>% filter(indicator == 'Graduation Rate') %>% arrange(subgroup))
    
    # middle_df <- (individual_subgroups_df %>% filter(subgroup == 'Black/Hispanic/Native American')) %>% 
    #   bind_rows(individual_subgroups_df %>% filter(subgroup == 'Economically Disadvantaged')) %>% 
    #   bind_rows(individual_subgroups_df %>% filter(subgroup == 'English Learners with Transitional 1-4')) %>% 
    #   bind_rows(individual_subgroups_df %>% filter(subgroup == 'Students with Disabilities'))
    
    individual_subgroups_df <- individual_subgroups_df %>% 
      mutate(
        content_area = case_when(
          indicator == 'Chronic Absenteeism' ~ 'Absenteeism',
          indicator == 'ELPA Growth Standard' ~ 'ELPA',
          indicator == 'Graduation Rate' ~ 'Grad Rate',
          TRUE ~ grade 
        )
      ) %>% 
      select(content_area, subgroup, AMO_pathway, absolute_pathway, value_add_pathway, overall_score) %>% 
      arrange(subgroup)
    

    
    
    for(i in 2:28){
      if(i < 8){
        cell <- paste0('C', as.character(i), ':', 'F', as.character(i))
        individual_subgroups_range <- individual_subgroups$Range(cell)
        individual_subgroups_range[["Value"]] <- as.numeric(individual_subgroups_df[i-1,3:ncol(individual_subgroups_df)])
        
        na_df_first <- as.data.frame(which(is.na(individual_subgroups_df[1:6,1:6]), arr.ind=TRUE))
        if(dim(na_df_first)[1] != 0){
          for(i in 1:nrow(na_df_first)){
            na_row <- na_df_first$row[[i]]
            na_col <- na_df_first$col[[i]]
            excel_col <- case_when(
              na_col == 3 ~ 'C',
              na_col == 4 ~ 'D',
              na_col == 5 ~ 'E',
              na_col == 6 ~ 'F'
            )
            cell_loc <- paste0(excel_col, as.character(na_row+1), ':', excel_col, as.character(na_row+1))
            na_replacement <- individual_subgroups$Range(cell_loc)
            na_replacement[["Value"]] <- as.character("")
          }
        }
      }
      if(i >8 & i < 15){
        cell <- paste0('C', as.character(i), ':', 'F', as.character(i))
        individual_subgroups_range <- individual_subgroups$Range(cell)
        individual_subgroups_range[["Value"]] <- as.numeric(individual_subgroups_df[i-2,3:ncol(individual_subgroups_df)])
        
        na_df_second <- as.data.frame(which(is.na(individual_subgroups_df[7:12,1:6]), arr.ind=TRUE))
        if(dim(na_df_second)[1] != 0){
          for(i in 1:nrow(na_df_second)){
            na_row <- na_df_second$row[[i]]
            na_col <- na_df_second$col[[i]]
            excel_col <- case_when(
              na_col == 3 ~ 'C',
              na_col == 4 ~ 'D',
              na_col == 5 ~ 'E',
              na_col == 6 ~ 'F'
            )
            cell_loc <- paste0(excel_col, as.character(na_row+8), ':', excel_col, as.character(na_row+8))
            na_replacement <- individual_subgroups$Range(cell_loc)
            na_replacement[["Value"]] <- as.character("")
          }
        }
      }
      if(i >15 & i < 22){
        cell <- paste0('C', as.character(i), ':', 'F', as.character(i))
        individual_subgroups_range <- individual_subgroups$Range(cell)
        individual_subgroups_range[["Value"]] <- as.numeric(individual_subgroups_df[i-3,3:ncol(individual_subgroups_df)])
        
        na_df_third <- as.data.frame(which(is.na(individual_subgroups_df[13:18,1:6]), arr.ind=TRUE))
        if(dim(na_df_third)[1] != 0){
          for(i in 1:nrow(na_df_third)){
            na_row <- na_df_third$row[[i]]
            na_col <- na_df_third$col[[i]]
            excel_col <- case_when(
              na_col == 3 ~ 'C',
              na_col == 4 ~ 'D',
              na_col == 5 ~ 'E',
              na_col == 6 ~ 'F'
            )
            cell_loc <- paste0(excel_col, as.character(na_row+15), ':', excel_col, as.character(na_row+15))
            na_replacement <- individual_subgroups$Range(cell_loc)
            na_replacement[["Value"]] <- as.character("")
          }
        }
      }
      if(i >22){
        cell <- paste0('C', as.character(i), ':', 'F', as.character(i))
        individual_subgroups_range <- individual_subgroups$Range(cell)
        individual_subgroups_range[["Value"]] <- as.numeric(individual_subgroups_df[i-4,3:ncol(individual_subgroups_df)])
        
        na_df_last <- as.data.frame(which(is.na(individual_subgroups_df[19:24,1:6]), arr.ind=TRUE))
        if(dim(na_df_last)[1] != 0){
          for(i in 1:nrow(na_df_last)){
            na_row <- na_df_last$row[[i]]
            na_col <- na_df_last$col[[i]]
            excel_col <- case_when(
              na_col == 3 ~ 'C',
              na_col == 4 ~ 'D',
              na_col == 5 ~ 'E',
              na_col == 6 ~ 'F'
            )
            cell_loc <- paste0(excel_col, as.character(na_row+22), ':', excel_col, as.character(na_row+22))
            na_replacement <- individual_subgroups$Range(cell_loc)
            na_replacement[["Value"]] <- as.character("")
          }
        }
      }
    }
    
    # wb$Save()                  # save the workbook
    # wb$SaveAS("N:/ORP_accountability/projects/Andrew/Pre-Coding-2019/district_heat_maps/test.xlsx")  # save as a new workbook
    wb$Save()
    xlApp$Quit()               # close Excel

  }
  
}
