#' Function to count distinct patients per site
#' @param tbl table with at least the cols site | [countcol] 
#' @param countcol the (quoted) name of the column to count individual ids
#' @param stepname value to insert into a "step" column 
#' @param stepnum value to insert into a "stepnum" column
#' @return table with step_number | attrition_step | site | num_pts
count_bysite <- function(tbl,
                         countcol="person_id",
                         stepname,
                         stepnum,
                         prev_log=NULL){
  
  total<-tbl%>%mutate(id:=!!sym(countcol))%>% #to align if there are different ids to count
    summarise(num_pts=n_distinct(id))%>%
    mutate(site='total')%>%collect()
  site_total<-tbl %>%
    mutate(id:=!!sym(countcol))%>%
    group_by(site)%>%
    summarise(num_pts=n_distinct(id)) %>%
    ungroup() %>%collect()%>%
    dplyr::union_all(total) %>%
    mutate(attrition_step:=!!stepname,
           step_number=stepnum)
  if(!is.null(prev_log)){
    rslt<-prev_log%>%
      dplyr::union_all(site_total)
  }else{rslt<-site_total}
  
  return(rslt)
}



get_conds <- function(codeset,
                      cohort,
                      min_date,
                      max_date,
                      min_age,
                      max_age,
                      cond_tbl = cdm_tbl('condition_occurrence'),
                      person_tbl = cdm_tbl('person')
) {
  
  #' Condition_concept_id
  conds <- cohort %>%
    distinct(person_id) %>%
    inner_join(cond_tbl, by=c('person_id')) %>%
    filter(as.Date(condition_start_date) >= as.Date(min_date)) %>%
    filter(as.Date(condition_start_date) <= as.Date(max_date)) %>%
    left_join(person_tbl %>% select(person_id, birth_date), by=c('person_id')) %>%
    mutate(age = date_diff('day', as.Date(birth_date), as.Date(condition_start_date))/365.25) %>%
    filter(age < max_age) %>%
    filter(age >= min_age) %>%
    inner_join(codeset %>% distinct(concept_id), by=c('condition_concept_id'='concept_id')) %>%
    distinct(person_id, condition_start_date)
  
  #' Condition_source_concept_id
  conds_source <- cohort %>%
    distinct(person_id) %>%
    inner_join(cond_tbl, by=c('person_id')) %>%
    filter(as.Date(condition_start_date) >= as.Date(min_date)) %>%
    filter(as.Date(condition_start_date) <= as.Date(max_date)) %>%
    left_join(person_tbl %>% select(person_id, birth_date), by=c('person_id')) %>%
    mutate(age = date_diff('day', as.Date(birth_date), as.Date(condition_start_date))/365.25) %>%
    filter(age < max_age) %>%
    filter(age >= min_age) %>%
    inner_join(codeset %>% distinct(concept_id), by=c('condition_source_concept_id'='concept_id')) %>%
    distinct(person_id, condition_start_date)
  
  #' Combine condition_concept_id and condition_source_concept_id information
  conds_all <- conds %>%
    full_join(conds_source,
              by=c('person_id','condition_start_date')) %>%
    distinct(person_id, condition_start_date) %>%
    group_by(person_id) %>%
    summarise(min_cond_date = min(as.Date(condition_start_date)),
              max_cond_date = max(as.Date(condition_start_date)),
              n_cond_dates=n_distinct(condition_start_date)) %>%
    ungroup()
  
  return(conds_all) 
}

#' Use this version for OP/ED visits
#' Version that looks at multiple visit dates rather than multiple condition dates
#' Is outputting a permanent table, 'conds_linked_visits_1st_min,' which must be removed later
get_conds_with_linked_visits <- function(codeset,
                                         cohort,
                                         visit_concept_list,
                                         min_date,
                                         max_date,
                                         min_age = 0L, #' Keep this at 0, because it's for identifying initial cohorts, not the outcomes
                                         max_age,
                                         cond_tbl = cdm_tbl('condition_occurrence'),
                                         visit_tbl = cdm_tbl('visit_occurrence'),
                                         person_tbl = cdm_tbl('person')
) {
  
  #' Filter visit table and condition table as far as possible
  visit_info <- cohort %>%
    distinct(person_id) %>%
    left_join(person_tbl %>% select(person_id, birth_date), by=c('person_id')) %>%
    inner_join(visit_tbl %>%
                 select(visit_occurrence_id, person_id, visit_start_date, visit_concept_id), by=c('person_id')) %>%
    filter(visit_concept_id %in% visit_concept_list) %>%
    filter(as.Date(visit_start_date) >= as.Date(min_date)) %>%
    filter(as.Date(visit_start_date) <= as.Date(max_date)) %>%
    mutate(age_visit = date_diff('day', as.Date(birth_date), as.Date(visit_start_date))/365.25) %>%
    filter(age_visit >= min_age) %>%
    filter(age_visit < max_age) %>%
    inner_join(cond_tbl %>%
                 select(condition_occurrence_id, condition_start_date, visit_occurrence_id,
                        condition_concept_id, condition_source_concept_id),
               by=c('visit_occurrence_id')) %>%
    mutate(age_cond = date_diff('day', as.Date(birth_date), as.Date(condition_start_date))/365.25) %>%
    filter(age_cond >= min_age) %>%
    filter(age_cond < max_age) %>%
    filter(as.Date(condition_start_date) >= as.Date(min_date)) %>%
    filter(as.Date(condition_start_date) <= as.Date(max_date))
  
  cond_info <- visit_info %>%
    inner_join(codeset %>% distinct(concept_id), by=c('condition_concept_id'='concept_id')) %>%
    distinct(person_id, condition_start_date, visit_start_date)
  
  cond_src_info <- visit_info %>%
    inner_join(codeset %>% distinct(concept_id), by=c('condition_source_concept_id'='concept_id')) %>%
    distinct(person_id, condition_start_date, visit_start_date)
  
  visit_cond_info <- cond_info %>%
    full_join(cond_src_info, by=c('person_id','condition_start_date','visit_start_date'))
  
  cond_date_info <- visit_cond_info %>%
    distinct(person_id, condition_start_date) %>%
    group_by(person_id) %>%
    summarise(min_cond_date = min(as.Date(condition_start_date)),
              n_cond_dates = n()) %>%
    ungroup() %>% as_tibble()
  
  visit_date_info <- visit_cond_info %>%
    distinct(person_id, visit_start_date) %>%
    arrange(person_id, as.Date(visit_start_date)) %>%
    group_by(person_id) %>%
    mutate(lag_visit_days = lag(visit_start_date),
           row_num = row_number(),
           n_visit_dates = n()) %>%
    ungroup() %>%
    mutate(best_row_num = case_when(n_visit_dates == 1L ~ 1L,
                                    n_visit_dates >= 1L ~ 2L)) %>%
    filter(as.numeric(row_num) == as.numeric(best_row_num)) %>% as_tibble()
  
  visit_cond_date_info <- visit_date_info %>%
    rename(min_idd_visit_date = lag_visit_days) %>%
    rename(second_min_idd_visit_date = visit_start_date) %>%
    left_join(cond_date_info, by=c('person_id')) %>%
    select(person_id, min_idd_visit_date, second_min_idd_visit_date,
           n_visit_dates, min_cond_date,  n_cond_dates) %>%
    collect()
  
  return(visit_cond_date_info) 
}



#' Use this version for IP visits
#' Version that looks at multiple visit dates rather than multiple condition dates
#' Is outputting a permanent table, 'conds_linked_visits_1st_min,' which must be removed later
get_ip_conds_with_linked_visits <- function(codeset,
                                            cohort,
                                            min_date,
                                            max_date,
                                            min_age = 0L, #' Keep this at 0, because it's for identifying initial cohorts, not the outcomes
                                            max_age,
                                            cond_tbl = cdm_tbl('condition_occurrence'),
                                            visit_tbl,
                                            person_tbl = cdm_tbl('person')
) {
  
  #' Filter visit table and condition table as far as possible
  visit_info <- cohort %>%
    distinct(person_id) %>%
    left_join(person_tbl %>% select(person_id, birth_date), by=c('person_id')) %>%
    inner_join(visit_tbl %>%
                 select(visit_occurrence_id, person_id, visit_start_date, visit_concept_id), by=c('person_id')) %>%
    filter(as.Date(visit_start_date) >= as.Date(min_date)) %>%
    filter(as.Date(visit_start_date) <= as.Date(max_date)) %>%
    mutate(age_visit = date_diff('day', as.Date(birth_date), as.Date(visit_start_date))/365.25) %>%
    filter(age_visit >= min_age) %>%
    filter(age_visit < max_age) %>%
    inner_join(cond_tbl %>%
                 select(condition_occurrence_id, condition_start_date, visit_occurrence_id,
                        condition_concept_id, condition_source_concept_id),
               by=c('visit_occurrence_id')) %>%
    mutate(age_cond = date_diff('day', as.Date(birth_date), as.Date(condition_start_date))/365.25) %>%
    filter(age_cond >= min_age) %>%
    filter(age_cond < max_age) %>%
    filter(as.Date(condition_start_date) >= as.Date(min_date)) %>%
    filter(as.Date(condition_start_date) <= as.Date(max_date))
  
  cond_info <- visit_info %>%
    inner_join(codeset %>% distinct(concept_id), by=c('condition_concept_id'='concept_id')) %>%
    distinct(person_id, condition_start_date, visit_start_date)
  
  cond_src_info <- visit_info %>%
    inner_join(codeset %>% distinct(concept_id), by=c('condition_source_concept_id'='concept_id')) %>%
    distinct(person_id, condition_start_date, visit_start_date)
  
  visit_cond_info <- cond_info %>%
    full_join(cond_src_info, by=c('person_id','condition_start_date','visit_start_date'))
  
  cond_date_info <- visit_cond_info %>%
    distinct(person_id, condition_start_date) %>%
    group_by(person_id) %>%
    summarise(min_cond_date = min(as.Date(condition_start_date)),
              n_cond_dates = n()) %>%
    ungroup() %>%
    as_tibble()
  
  visit_date_info <- visit_cond_info %>%
    distinct(person_id, visit_start_date) %>%
    arrange(person_id, as.Date(visit_start_date)) %>%
    group_by(person_id) %>%
    mutate(lag_visit_days = lag(visit_start_date),
           row_num = row_number(),
           n_visit_dates = n()) %>%
    ungroup() %>%
    mutate(best_row_num = case_when(n_visit_dates == 1L ~ 1L,
                                    n_visit_dates >= 1L ~ 2L)) %>%
    filter(as.numeric(row_num) == as.numeric(best_row_num)) %>%
    as_tibble()
  
  visit_cond_date_info <- visit_date_info %>%
    rename(min_idd_visit_date = lag_visit_days,
           second_min_idd_visit_date = visit_start_date) %>%
    left_join(cond_date_info, by=c('person_id')) %>%
    select(person_id, min_idd_visit_date, second_min_idd_visit_date,
           n_visit_dates, min_cond_date,  n_cond_dates) %>% collect()
  
  return(visit_cond_date_info) 
}



get_demographics <- function(cohort,
                             #latest_date,
                             min_study_date = as.Date('2009-01-01'),
                             max_study_date = as.Date('2024-12-31'),
                             #ip_visit_concept_list = c(9201L, 2000000048L, 2000000088L),
                             min_age = 0L,
                             max_age = 21L,
                             person_tbl = cdm_tbl('person'),
                             ip_visit_tbl,
                             visit_tbl = cdm_tbl('visit_occurrence')
) {
  
  latest_visit <- cohort %>%
    left_join(person_tbl %>% select(person_id, birth_date), by=c('person_id')) %>%
    inner_join(visit_tbl, by=c('person_id')) %>%
    filter(as.Date(visit_start_date) >= as.Date(min_study_date)) %>%
    filter(as.Date(visit_start_date) <= as.Date(max_study_date)) %>%
    mutate(age = date_diff('day', as.Date(birth_date), as.Date(visit_start_date))/365.25) %>%
    filter(age >= min_age) %>%
    filter(age < max_age) %>%
    group_by(person_id) %>%
    summarise(latest_any_visit_date = max(as.Date(visit_start_date))) %>%
    ungroup() #%>%
  # mutate(latest_any_visit_date = case_when(as.Date(latest_any_visit_date) > as.Date(latest_date) ~ as.Date(latest_date),
  #                                          TRUE ~ latest_any_visit_date))
  
  count_hosp_pre <- cohort %>%
    left_join(person_tbl %>% select(person_id, birth_date), by=c('person_id')) %>%
    inner_join(ip_visit_tbl, by=c('person_id')) %>%
    #filter(visit_concept_id %in% ip_visit_concept_list) %>%
    filter(as.Date(visit_start_date) >= as.Date(min_study_date)) %>%
    filter(as.Date(visit_start_date) <= as.Date(max_study_date)) %>%
    #filter(as.Date(visit_start_date) < as.Date(cond_date_fulfilled)) %>%
    filter(as.Date(visit_start_date) < as.Date(visit_date_fulfilled)) %>%
    mutate(age = date_diff('day', as.Date(birth_date), as.Date(visit_start_date))/365.25) %>%
    filter(age >= min_age) %>%
    filter(age < max_age) %>%
    #distinct(person_id, cond_date_fulfilled, visit_start_date) %>%
    distinct(person_id, visit_date_fulfilled, visit_start_date) %>%
    group_by(person_id) %>%
    summarise(count_hosp_pre = n()) %>%
    ungroup()
  
  count_hosp_post <- cohort %>%
    left_join(person_tbl %>% select(person_id, birth_date), by=c('person_id')) %>%
    inner_join(ip_visit_tbl, by=c('person_id')) %>%
    #filter(visit_concept_id %in% ip_visit_concept_list) %>%
    filter(as.Date(visit_start_date) >= as.Date(min_study_date)) %>%
    filter(as.Date(visit_start_date) <= as.Date(max_study_date)) %>%
    filter(as.Date(visit_start_date) >= as.Date(visit_date_fulfilled)) %>%
    #filter(as.Date(visit_start_date) >= as.Date(cond_date_fulfilled)) %>%
    mutate(age = date_diff('day', as.Date(birth_date), as.Date(visit_start_date))/365.25) %>%
    filter(age >= min_age) %>%
    filter(age < max_age) %>%
    #distinct(person_id, cond_date_fulfilled, visit_start_date) %>%
    distinct(person_id, visit_date_fulfilled, visit_start_date) %>%
    group_by(person_id) %>%
    summarise(count_hosp_post = n()) %>%
    ungroup()
  
  demog_cohort <- cohort %>%
    left_join(latest_visit, by=c('person_id')) %>%
    left_join(count_hosp_pre, by=c('person_id')) %>%
    left_join(count_hosp_post, by=c('person_id')) %>%
    mutate(count_hosp_pre = case_when(is.na(count_hosp_pre) ~ 0L,
                                      TRUE ~ count_hosp_pre)) %>%
    mutate(count_hosp_post = case_when(is.na(count_hosp_post) ~ 0L,
                                       TRUE ~ count_hosp_post)) %>%
    left_join(person_tbl %>%
                select(person_id, birth_date, site,
                       gender_concept_id, race_concept_id, ethnicity_concept_id, language_concept_id), by=c('person_id')) %>%
    mutate(age_at_visit_fulfilled = as.numeric(date_diff('day', as.Date(birth_date), as.Date(visit_date_fulfilled)))/365.25) %>%
    #mutate(age_at_cond_fulfilled = as.numeric(date_diff('day', as.Date(birth_date), as.Date(cond_date_fulfilled)))/365.25) %>%
    mutate(age_at_latest_any_visit_date = as.numeric(date_diff('day', as.Date(birth_date), as.Date(latest_any_visit_date)))/365.25) %>%
    # mutate(age_group_at_cond_fulfilled = case_when(age_at_cond_fulfilled < 0.5 ~ '1: Under 6 months',
    #                                                age_at_cond_fulfilled < 2L ~ '2: 6 months to <2 years',
    #                                                age_at_cond_fulfilled < 5L ~ '3: 2 years to <5 years',
    #                                                age_at_cond_fulfilled < 10L ~ '4: 5 years to <10 years',
    #                                                age_at_cond_fulfilled < 15L ~ '5: 10 years to <15 years',
    #                                                age_at_cond_fulfilled < 18L ~ '6: 15 years to <18 years',
    #                                                age_at_cond_fulfilled < 21L ~ '7: 18 years to <21 years',
    #                                                TRUE ~ 'Other'
    # )) %>%
    # mutate(year_at_cond_fulfilled = year(as.Date(cond_date_fulfilled))) %>%
    mutate(age_group_at_visit_fulfilled = case_when(age_at_visit_fulfilled < 5L ~ '1: <5 years',
                                                    age_at_visit_fulfilled < 8L ~ '2: 5 years to <8 years',
                                                    age_at_visit_fulfilled < 13L ~ '3: 8 years to <13 years',
                                                    age_at_visit_fulfilled < 21L ~ '4: 13 years to <21 years',
                                                    TRUE ~ 'Other'
    )) %>%
    mutate(year_at_visit_fulfilled = year(as.Date(visit_date_fulfilled))) %>%
    mutate(
      sex_cat=case_when(
        gender_concept_id==8507L ~ 'Male',
        gender_concept_id==8532L ~ 'Female',
        TRUE ~ 'Other/unknown/ambiguous')) %>%
    mutate(eth_cat=case_when(ethnicity_concept_id == 38003563L ~ 'Hispanic',
                             ethnicity_concept_id == 38003564L ~ 'Not_Hispanic',
                             ethnicity_concept_id %in% c(44814660L, 44814649L, 44814650L,
                                                         44814653L) ~ 'Refuse_Other_Unknown',
                             TRUE ~ 'Other/Unknown'
    )) %>%
    mutate(race_cat = case_when(race_concept_id == 8527L ~ 'White',
                                race_concept_id == 8516L ~ 'Black',
                                race_concept_id == 8515L ~ 'Asian',
                                race_concept_id == 8657L ~ 'American_Indian_Alaska_Native',
                                race_concept_id == 8557L ~ 'Native_Hawaiian_Other_PI',
                                race_concept_id == 38003615L ~ 'Middle_Eastern_North_African',
                                race_concept_id == 44814659L ~ 'Multiple_Race',
                                race_concept_id %in% c(44814650L, 44814653L,
                                                       44814649L, 44814660) ~ 'Unknown_Other_Refuse'
    )) %>%
    mutate(
      raceeth_cat=case_when(ethnicity_concept_id == 38003563L ~ 'Hispanic',
                            ethnicity_concept_id == 38003564L & race_concept_id == 8527L ~ 'NH_White',
                            ethnicity_concept_id == 38003564L & race_concept_id == 8516L ~ 'NH_Black/AA',
                            ethnicity_concept_id == 38003564L & race_concept_id == 8515L ~ 'NH_Asian',
                            ethnicity_concept_id == 38003564L & race_concept_id == 8657L ~ 'NH_American_Indian_Alaska_Native',
                            ethnicity_concept_id == 38003564L & race_concept_id == 8557L ~ 'NH_Native_Hawaiian_Other_PI',
                            ethnicity_concept_id == 38003564L & race_concept_id == 38003615L ~ 'NH_Middle_Eastern_North_African',
                            ethnicity_concept_id == 38003564L & race_concept_id == 44814659L ~ 'NH_Multiple_Race',
                            ethnicity_concept_id == 38003564L & race_concept_id %in% c(44814650L, 44814653L,
                                                                                       44814649L, 44814660) ~ 'Other/Unknown',
                            TRUE ~ 'Other/Unknown')) %>%
    mutate(
      raceeth_cat2=case_when(ethnicity_concept_id == 38003563L ~ 'Hispanic',
                             ethnicity_concept_id == 38003564L & race_concept_id == 8527L ~ 'NH_White',
                             ethnicity_concept_id == 38003564L & race_concept_id == 8516L ~ 'NH_Black/AA',
                             ethnicity_concept_id == 38003564L & race_concept_id == 8515L ~ 'NH_Asian',
                             ethnicity_concept_id == 38003564L & race_concept_id == 8657L ~ 'Multiple/Other/Unknown',
                             ethnicity_concept_id == 38003564L & race_concept_id == 8557L ~ 'Multiple/Other/Unknown',
                             ethnicity_concept_id == 38003564L & race_concept_id == 38003615L ~ 'Multiple/Other/Unknown',
                             ethnicity_concept_id == 38003564L & race_concept_id == 44814659L ~ 'Multiple/Other/Unknown',
                             ethnicity_concept_id == 38003564L & race_concept_id %in% c(44814650L, 44814653L,
                                                                                        44814649L, 44814660) ~ 'Multiple/Other/Unknown',
                             TRUE ~ 'Multiple/Other/Unknown')) %>%
    mutate(language_cat=case_when(language_concept_id == 4180186L ~ 'English',
                                  language_concept_id == 4182511L ~ 'Spanish',
                                  TRUE ~ 'Other/Unknown'
    ))
  
  return(demog_cohort)
}


#' Make function to mask sites randomly and generate file indicating the masked site list
mask_sites <- function(cohort_tbl,
                       seed=118,
                       person_tbl=cdm_tbl('person')
) {
  require(stringr)
  
  site_list <- cohort_tbl %>% distinct(person_id) %>%
    left_join(person_tbl %>% select(person_id, site), by=c('person_id')) %>%
    distinct(site) %>%
    collect()
  
  set.seed(seed)
  
  site_order <- sample(x = 1:length(site_list[[1]]),
                       replace = FALSE) %>%
    as_tibble()
  
  sites_randomized <- site_list %>%
    cbind(site_order) %>%
    mutate(site_mask = paste0('site_',str_pad(as.character(value),2,pad="0"))) %>%
    select(site, site_mask) %>%
    arrange(site_mask)
  
  return(sites_randomized)
}


#' Run demographics on full cohort of IDD and non-IDD groups
get_demographics_general <- function(cohort,
                                     #latest_date,
                                     min_study_date = as.Date('2009-01-01'),
                                     max_study_date = as.Date('2024-12-31'),
                                     #ip_visit_concept_list = c(9201L, 2000001532L, 2000000048L, 2000000088L),
                                     ip_visit_tbl,
                                     min_age = 5L,
                                     max_age = 21L,
                                     person_tbl = cdm_tbl('person'),
                                     visit_tbl = cdm_tbl('visit_occurrence')
) {
  
  latest_visit <- cohort %>%
    left_join(person_tbl %>% select(person_id, birth_date), by=c('person_id')) %>%
    inner_join(visit_tbl, by=c('person_id')) %>%
    filter(as.Date(visit_start_date) >= as.Date(min_study_date)) %>%
    filter(as.Date(visit_start_date) <= as.Date(max_study_date)) %>%
    mutate(age = date_diff('day', as.Date(birth_date), as.Date(visit_start_date))/365.25) %>%
    filter(age >= min_age) %>%
    filter(age < max_age) %>%
    group_by(person_id) %>%
    summarise(latest_any_visit_date = max(as.Date(visit_start_date))) %>%
    ungroup()
  
  count_hosp_all <- cohort %>%
    left_join(person_tbl %>% select(person_id, birth_date), by=c('person_id')) %>%
    inner_join(ip_visit_tbl, by=c('person_id')) %>%
    #filter(visit_concept_id %in% ip_visit_concept_list) %>%
    filter(as.Date(visit_start_date) >= as.Date(min_study_date)) %>%
    filter(as.Date(visit_start_date) <= as.Date(max_study_date)) %>%
    mutate(age = date_diff('day', as.Date(birth_date), as.Date(visit_start_date))/365.25) %>%
    filter(age >= min_age) %>%
    filter(age < max_age) %>%
    distinct(person_id, visit_start_date) %>%
    group_by(person_id) %>%
    summarise(min_hosp_date = min(as.Date(visit_start_date)),
              max_hosp_date = max(as.Date(visit_start_date)),
              count_hosps = n()) %>%
    ungroup()
  
  demog_cohort <- cohort %>%
    left_join(latest_visit, by=c('person_id')) %>%
    left_join(count_hosp_all, by=c('person_id')) %>%
    mutate(count_hosps = case_when(is.na(count_hosps) ~ 0L,
                                   TRUE ~ count_hosps)) %>%
    mutate(count_hosps_cat = case_when(count_hosps == 1L ~ '1 hospitalization',
                                       count_hosps == 2L ~ '2 hospitalizations',
                                       count_hosps > 2L ~ '>2 hospitalizations')) %>%
    left_join(person_tbl %>%
                select(person_id, birth_date, site,
                       gender_concept_id, race_concept_id, ethnicity_concept_id, language_concept_id), by=c('person_id')) %>%
    mutate(age_at_visit_fulfilled = as.numeric(date_diff('day', as.Date(birth_date), as.Date(visit_date_fulfilled)))/365.25) %>%
    mutate(age_at_latest_any_visit_date = as.numeric(date_diff('day', as.Date(birth_date), as.Date(latest_any_visit_date)))/365.25) %>%
    mutate(age_at_min_hosp_date = as.numeric(date_diff('day', as.Date(birth_date), as.Date(min_hosp_date)))/365.25) %>%
    mutate(age_at_max_hosp_date = as.numeric(date_diff('day', as.Date(birth_date), as.Date(max_hosp_date)))/365.25) %>%
    mutate(age_group_at_visit_fulfilled = case_when(age_at_visit_fulfilled < 5L ~ '1: <5 years',
                                                    age_at_visit_fulfilled < 8L ~ '2: 5 years to <8 years',
                                                    age_at_visit_fulfilled < 13L ~ '3: 8 years to <13 years',
                                                    age_at_visit_fulfilled < 21L ~ '4: 13 years to <21 years',
                                                    TRUE ~ 'Other'
    )) %>%
    mutate(age_group_at_min_hosp_date = case_when(age_at_min_hosp_date < 10L ~ '1: 5 years to <9 years',
                                                  age_at_min_hosp_date < 13L ~ '2: 9 years to <13 years',
                                                  age_at_min_hosp_date < 17L ~ '3: 13 years to <17 years',
                                                  age_at_min_hosp_date < 21L ~ '4: 17 years to <21 years',
                                                  TRUE ~ 'Other'
    )) %>%
    mutate(year_at_min_hosp_date = year(as.Date(min_hosp_date))) %>%
    mutate(
      sex_cat=case_when(
        gender_concept_id==8507L ~ 'Male',
        gender_concept_id==8532L ~ 'Female',
        TRUE ~ 'Other/unknown/ambiguous')) %>%
    mutate(eth_cat=case_when(ethnicity_concept_id == 38003563L ~ 'Hispanic',
                             ethnicity_concept_id == 38003564L ~ 'Not_Hispanic',
                             ethnicity_concept_id %in% c(44814660L, 44814649L, 44814650L,
                                                         44814653L) ~ 'Refuse_Other_Unknown',
                             TRUE ~ 'Other/Unknown'
    )) %>%
    mutate(race_cat = case_when(race_concept_id == 8527L ~ 'White',
                                race_concept_id == 8516L ~ 'Black',
                                race_concept_id == 8515L ~ 'Asian',
                                race_concept_id == 8657L ~ 'American_Indian_Alaska_Native',
                                race_concept_id == 8557L ~ 'Native_Hawaiian_Other_PI',
                                race_concept_id == 38003615L ~ 'Middle_Eastern_North_African',
                                race_concept_id == 44814659L ~ 'Multiple_Race',
                                race_concept_id %in% c(44814650L, 44814653L,
                                                       44814649L, 44814660) ~ 'Unknown_Other_Refuse'
    )) %>%
    mutate(
      raceeth_cat=case_when(ethnicity_concept_id == 38003563L ~ 'Hispanic',
                            ethnicity_concept_id == 38003564L & race_concept_id == 8527L ~ 'NH_White',
                            ethnicity_concept_id == 38003564L & race_concept_id == 8516L ~ 'NH_Black/AA',
                            ethnicity_concept_id == 38003564L & race_concept_id == 8515L ~ 'NH_Asian',
                            ethnicity_concept_id == 38003564L & race_concept_id == 8657L ~ 'NH_American_Indian_Alaska_Native',
                            ethnicity_concept_id == 38003564L & race_concept_id == 8557L ~ 'NH_Native_Hawaiian_Other_PI',
                            ethnicity_concept_id == 38003564L & race_concept_id == 38003615L ~ 'NH_Middle_Eastern_North_African',
                            ethnicity_concept_id == 38003564L & race_concept_id == 44814659L ~ 'NH_Multiple_Race',
                            ethnicity_concept_id == 38003564L & race_concept_id %in% c(44814650L, 44814653L,
                                                                                       44814649L, 44814660) ~ 'Other/Unknown',
                            TRUE ~ 'Other/Unknown')) %>%
    mutate(
      raceeth_cat2=case_when(ethnicity_concept_id == 38003563L ~ 'Hispanic',
                             ethnicity_concept_id == 38003564L & race_concept_id == 8527L ~ 'NH_White',
                             ethnicity_concept_id == 38003564L & race_concept_id == 8516L ~ 'NH_Black/AA',
                             ethnicity_concept_id == 38003564L & race_concept_id == 8515L ~ 'NH_Asian',
                             ethnicity_concept_id == 38003564L & race_concept_id == 8657L ~ 'Multiple/Other/Unknown',
                             ethnicity_concept_id == 38003564L & race_concept_id == 8557L ~ 'Multiple/Other/Unknown',
                             ethnicity_concept_id == 38003564L & race_concept_id == 38003615L ~ 'Multiple/Other/Unknown',
                             ethnicity_concept_id == 38003564L & race_concept_id == 44814659L ~ 'Multiple/Other/Unknown',
                             ethnicity_concept_id == 38003564L & race_concept_id %in% c(44814650L, 44814653L,
                                                                                        44814649L, 44814660) ~ 'Multiple/Other/Unknown',
                             TRUE ~ 'Multiple/Other/Unknown')) %>%
    mutate(language_cat=case_when(language_concept_id == 4180186L ~ 'English',
                                  language_concept_id == 4182511L ~ 'Spanish',
                                  TRUE ~ 'Other/Unknown'
    ))
  
  return(demog_cohort)
}

#' Make attrition table with patient-level and hospitalization-level counts
#' 7/17/2026
get_manual_attrition <- function(idd_attr_site = idd_attr_site,
                                 cohort = results_tbl('combined_cohort_demog'),
                                 elig_hosps = results_tbl('elig_hosps'),
                                 excl_sites = c('national')
) {
  
  output <- list()
  
  step1 <- idd_attr_site %>%
    filter(site == 'total') %>%
    filter(step_number == 1L) %>%
    select(num_pts) %>% pull()
  
  step2 <- idd_attr_site %>%
    filter(site == 'total') %>%
    filter(step_number == 2L) %>%
    select(num_pts) %>% pull()
  
  step3 <- idd_attr_site %>%
    filter(site == 'total') %>%
    filter(step_number == 3L) %>%
    select(num_pts) %>% pull()
  
  step4 <- cohort %>%
    distinct(person_id) %>%
    count() %>% pull()
  
  step5 <- cohort %>%
    filter(!site %in% excl_sites) %>%
    count() %>% pull()
  
  step1_desc <- idd_attr_site %>%
    filter(site == 'total') %>%
    filter(step_number == 1L) %>%
    select(attrition_step) %>% pull()
  
  step2_desc <- idd_attr_site %>%
    filter(site == 'total') %>%
    filter(step_number == 2L) %>%
    select(attrition_step) %>% pull()
  
  step3_desc <- idd_attr_site %>%
    filter(site == 'total') %>%
    filter(step_number == 3L) %>%
    select(attrition_step) %>% pull()
  
  step4_desc <- 'Patients meeting the intermediate IDD definition or with no evidence of IDD'
  
  step5_desc <- 'Patients from sites that were not excluded due to PDI data quality issues'
  
  att_desc <- c(step1_desc,
                step2_desc,
                step3_desc,
                step4_desc,
                step5_desc)
  
  att_cts <- c(step1,
               step2,
               step3,
               step4,
               step5)
  
  output$att_summary <- att_desc %>%
    cbind(att_cts) %>% as_tibble() %>%
    mutate(pct_prior = 100*as.numeric(att_cts)/as.numeric(lag(att_cts))) %>%
    rename_with(~ "description", 1)
  
  
  
  
  patients_idd <- cohort %>%
    filter(cohort == 'idd') %>%
    filter(!site %in% excl_sites) %>%
    distinct(person_id) %>%
    count() %>% pull()
  
  hospitalizations_idd <- elig_hosps %>%
    filter(cohort == 'idd') %>%
    filter(!site %in% excl_sites) %>%
    distinct(visit_occurrence_id) %>%
    count() %>% pull()
  
  patients_nonidd <- cohort %>%
    filter(cohort == 'non_idd') %>%
    filter(!site %in% excl_sites) %>%
    distinct(person_id) %>%
    count() %>% pull()
  
  hospitalizations_nonidd <- elig_hosps %>%
    filter(cohort == 'non_idd') %>%
    filter(!site %in% excl_sites) %>%
    distinct(visit_occurrence_id) %>%
    count() %>% pull()
  
  
  
  patient_counts <- c('Patients meeting the IDD or non-IDD definitions from non-excluded sites', patients_idd, patients_nonidd)
  hospitalization_counts <- c('Hospitalization records in patients meeting the IDD or non-IDD definitions from non-excluded sites', hospitalizations_idd, hospitalizations_nonidd)
  
  output$att_summary2 <- patient_counts %>%
    rbind(hospitalization_counts) %>%
    as_tibble() %>%
    rename(Description = V1,
           IDD_Definition = V2,
           No_Evidence_of_IDD = V3)
  
  return(output)
}

