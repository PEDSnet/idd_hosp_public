#' Check maximum visit dates for the database of interest
check_visit_dates <- 
cdm_tbl('visit_occurrence') %>%
  group_by(site) %>%
  summarise(min_visit_date = min(as.Date(visit_start_date)),
            max_visit_date = max(as.Date(visit_start_date))) %>%
  ungroup()

check_visit_dates

# --------------------------------------------------
# Study parameters
# --------------------------------------------------

current_db_version <- "v61"

base_start_date <- as.Date('1800-01-01')
idd_start_date <- as.Date('2009-01-01')
idd_age_min <- 0L

study_start_date <- as.Date('2016-01-01')
study_end_date <- as.Date('2025-12-31')
age_min <- 5L
age_max <- 21L
site_list <- c('nemours', 'national', 'chop',
              'colorado', 'cchmc', 'nationwide',
              'lurie', 'stanford', 'seattle', 'texas')

# --------------------------------------------------
# Get all "valid" patients: plausible birthdate, valid sex, ≥1 visit of any type, at sites of interest
# --------------------------------------------------

today <- Sys.Date()
min_birth <- today - lubridate::years(120)

person <- cdm_tbl("person") %>%
  filter(site %in% site_list) %>%
  select(
    person_id, site, birth_date,
    gender_concept_id, gender_concept_name,
    race_concept_id, race_concept_name,
    ethnicity_concept_id, ethnicity_concept_name) %>%
  filter(
    !is.na(birth_date),
    birth_date <= !!today,
    birth_date >= !!min_birth,
    gender_concept_id %in% c(8532, 8507))

visits <- cdm_tbl("visit_occurrence") %>%
  select(
    person_id, site, visit_occurrence_id,
    visit_concept_id, visit_concept_name,
    visit_start_date)

person_visits <- person %>%
  inner_join(visits, by = c('person_id','site'))

person_visit_ct <- person_visits %>% distinct(person_id) %>% count()

idd_attr_site <- count_bysite(tbl = person_visits,
                               countcol = "person_id",
                               stepname = paste0('Valid patients in PEDSnet db ',current_db_version, ' (birthdate, valid sex, >=1 visit)'),
                               stepnum = 1L)

# --------------------------------------------------
# Patients with an inpatient, outpatient or ED visit from 1/1/2009 - study end date,
# aged >=0 at time of visit
# --------------------------------------------------

#' Get inpatient visit_occurrence_ids
ip_visit_concepts <- c(9201L, 2000001532L, 2000000048L, 2000000088L)

nutrition_ip_visits <- cdm_tbl('visit_occurrence') %>%
  filter(visit_concept_id %in% c(2000001593L)) %>%
  filter(regexp_like(lower(visit_source_value), 'hospital encounter\\|inpatient')) %>%
  distinct(visit_occurrence_id)

ip_visit_ids <- cdm_tbl('visit_occurrence') %>%
  filter(visit_concept_id %in% ip_visit_concepts) %>%
  distinct(visit_occurrence_id) %>%
  full_join(nutrition_ip_visits, by=c('visit_occurrence_id'))

#' Get outpatient and ED visit_occurrence_ids
op_ed_visit_ids <- cdm_tbl('visit_occurrence') %>%
  filter(visit_concept_id %in% c(9202L, 581399L, 9203L)) %>%
  distinct(visit_occurrence_id)

#' Get all visit_occurrence_ids for patients aged >=0 to <21 from 2009-2025
all_visit_ids <- op_ed_visit_ids %>%
  full_join(ip_visit_ids, by=c('visit_occurrence_id')) %>%
  inner_join(person_visits, by=c('visit_occurrence_id')) %>%
  mutate(age_at_visit = date_diff('day',as.Date(birth_date), as.Date(visit_start_date))/365.25) %>%
  filter(age_at_visit >= idd_age_min) %>%
  filter(age_at_visit < age_max) %>%
  filter(as.Date(visit_start_date) >= idd_start_date) %>%
  filter(as.Date(visit_start_date) <= study_end_date)

all_visit_id_ct <- all_visit_ids %>% distinct(person_id) %>% count()

idd_attr_site <- count_bysite(tbl = all_visit_ids,
                              countcol = "person_id",
                              stepname = paste0('Patients with an IP/OP/ED visit from ',
                                                as.character(idd_start_date),' - ',as.character(study_end_date),
                                                ', aged >=', as.character(idd_age_min), ' to <',
                                                as.character(age_max), ' at time of visit'),
                              stepnum = 2L,
                              prev_log = idd_attr_site)

# --------------------------------------------------
# Patients with at least one hospitalization during the study period,
# aged 5 to <21 at time of hospitalization
# --------------------------------------------------

#' Join to person_visits
ip_visits <- all_visit_ids %>%
  inner_join(ip_visit_ids, by=c('visit_occurrence_id')) %>%
  filter(as.Date(visit_start_date) >= study_start_date) %>%
  filter(as.Date(visit_start_date) <= study_end_date) %>%
  mutate(age_at_visit = date_diff('day',as.Date(birth_date), as.Date(visit_start_date))/365.25) %>%
  filter(age_at_visit >= age_min) %>%
  filter(age_at_visit < age_max)

output_tbl(ip_visits, 'ip_visits') #' to be used later, to identify outcomes

#db=config('db_src')
#db_remove_table(db, dbplyr::in_schema(config('results_schema'), 'ip_visits'))

ip_visit_ct <- results_tbl('ip_visits') %>% distinct(person_id) %>% count()
  
idd_attr_site <- count_bysite(tbl = results_tbl('ip_visits'),
                              countcol = "person_id",
                              stepname = paste0('Patients with at least one hospitalization occurring during the study period: ',
                                                as.character(study_start_date), '-', as.character(study_end_date),
                                                ', aged ', as.character(age_min), ' to <', as.character(age_max)),
                              stepnum = 3L,
                              prev_log = idd_attr_site)

# --------------------------------------------------
# Summarise attrition results
# --------------------------------------------------

attrition_results <- ca_process(attrition_tbl=idd_attr_site,
                                multi_or_single_site = 'multi',
                                anomaly_or_exploratory = 'exploratory',
                                start_step_num = 1)

output_tbl(attrition_results,
           name='attrition_cohort')

# --------------------------------------------------
#' Specify input cohort for identifying patients with IDD:
#' patients from step 3 of attrition
#' (included sites, valid sex, valid birthdate, age in range, during study period,
#' with hospitalization)
# --------------------------------------------------

input_cohort <- results_tbl('ip_visits') %>% distinct(person_id)

# --------------------------------------------------
#' Implement the IDD definition components (note: using a broader cohort than the
#' cohort of patients who have inpatient visits, just in case we want to retain larger
#' cohorts and then subset later)
# --------------------------------------------------

# idd_codes <- read_csv('codesets/dx_idd.csv')
# output_tbl(idd_codes, 'dx_idd', .chunk_size=1000)

idd_codes <- results_tbl('dx_idd')

#output_tbl(idd_codes, 'idd_codes')

#' Look at all IDD code conditions for all patients
all_idd <- get_conds(codeset = idd_codes,
                     cohort = cdm_tbl('person'),
                     min_date = base_start_date,
                     max_date = study_end_date,
                     min_age = 0L,
                     max_age = 250L)

output_tbl(all_idd, 'all_idd_pats_ever')

# --------------------------------------------------
#' ED IDD visit, using visit_occurrence_id linked to condition_occurrence record
# --------------------------------------------------

ed_idd <- get_conds_with_linked_visits(codeset = idd_codes,
                                       cohort = input_cohort,
                                       visit_concept_list = c(9203L),
                                       min_date = idd_start_date,
                                       max_date = study_end_date,
                                       max_age = age_max)

output_tbl(ed_idd, 'ed_idd_pats', .chunk_size = 1000)

ed_idd_count <- results_tbl('ed_idd_pats') %>% count() %>% pull()

#db=config('db_src')
#db_remove_table(db, dbplyr::in_schema(config('results_schema'), 'cond_date_info'))

# --------------------------------------------------
#' Physician IDD visit (does this mean outpatient physician visit?),
#' using visit_occurrence_id linked to condition_occurrence record
# --------------------------------------------------

physician_idd <- get_conds_with_linked_visits(codeset = idd_codes,
                                              cohort = input_cohort,
                                              visit_concept_list = c(9202L, 581399L),
                                              min_date = idd_start_date,
                                              max_date = study_end_date,
                                              max_age = age_max)

output_tbl(physician_idd, 'physician_idd_pats', .chunk_size = 1000)

physician_idd_count <- results_tbl('physician_idd_pats') %>% count() %>% pull()

# db=config('db_src')
# db_remove_table(db, dbplyr::in_schema(config('results_schema'), 'visit_date_info'))

# --------------------------------------------------
#' Inpatient IDD visit, using visit_occurrence_id linked to condition_occurrence record
# --------------------------------------------------

#' Check to make sure dates in this table are consistent with how ED/OP visits are identified
ip_visit_input <- all_visit_ids %>%
  inner_join(ip_visit_ids, by=c('visit_occurrence_id'))

ip_idd <- get_ip_conds_with_linked_visits(codeset = idd_codes,
                                          cohort = input_cohort,
                                          visit_tbl = ip_visit_input,
                                          min_date = idd_start_date,
                                          max_date = study_end_date,
                                          max_age = age_max)

output_tbl(ip_idd, 'ip_idd_pats', .chunk_size = 1000)

ip_idd_count <- results_tbl('ip_idd_pats') %>% count() %>% pull()


# db=config('db_src')
# db_remove_table(db, dbplyr::in_schema(config('results_schema'), 'visit_date_info'))
# 
# db=config('db_src')
# db_remove_table(db, dbplyr::in_schema(config('results_schema'), 'input_cohort'))


# --------------------------------------------------
#' Implement the intermediate IDD definition: any inpatient visit, visits linked to conditions
#' Now prioritizing the visit_start_date rather than the condition_start_date
# --------------------------------------------------

intermediate_idd_pats <- results_tbl('ed_idd_pats') %>%
  select(person_id, min_idd_visit_date, second_min_idd_visit_date) %>%
  dplyr::union(results_tbl('ip_idd_pats') %>%
                 select(person_id, min_idd_visit_date, second_min_idd_visit_date)) %>%
  mutate(visit_date_fulfilled = case_when(!is.na(min_idd_visit_date) ~ min_idd_visit_date,
                                          is.na(min_idd_visit_date) ~ second_min_idd_visit_date)) %>% #' take the earliest date
  dplyr::union(results_tbl('physician_idd_pats') %>%
                 filter(n_visit_dates > 1L) %>%
                 filter(!is.na(min_idd_visit_date)) %>% #' here requiring that a patient have two separate condition dates, in addition to separate visits-- might consider dropping
                 mutate(visit_date_fulfilled = second_min_idd_visit_date)) %>%
  select(person_id, visit_date_fulfilled) %>%
  group_by(person_id) %>%
  summarise(visit_date_fulfilled=min(as.Date(visit_date_fulfilled))) %>%
  ungroup()

intermediate_idd_pats_count <- intermediate_idd_pats %>% count() %>% pull()

results_tbl('intermediate_idd_demog') %>%
  select(age_at_visit_fulfilled) %>%
  collect() %>%
  quantile(probs = seq(0, 1, 0.25), na.rm = TRUE)

ip_visit_tbl <- ip_visit_ids %>%
  left_join(cdm_tbl('visit_occurrence'), by=c('visit_occurrence_id'))

intermediate_idd_demog <- get_demographics(cohort = intermediate_idd_pats,
                                           min_study_date = study_start_date,
                                           max_study_date = study_end_date,
                                           ip_visit_tbl = ip_visit_tbl,
                                           min_age = 5L,
                                           max_age = 21L)

output_tbl(intermediate_idd_demog, 'intermediate_idd_demog')

################################################################################
#' Mask sites

sites_masked <- mask_sites(cohort_tbl = results_tbl('intermediate_idd_demog'),
                           seed=121825)

output_tbl(sites_masked, 'sites_masked')

################################################################################
################################################################################
################################################################################
#' Look at patients with no evidence of IDD

nonidd_pats <- cdm_tbl('person') %>% anti_join(results_tbl('all_idd_pats_ever'), by=c('person_id')) %>%
  inner_join(input_cohort %>% distinct(person_id), by=c('person_id'))

#' Get demographics for intermediate IDD and non-IDD cohorts
cohort_idd_add <- input_cohort %>%
  inner_join(intermediate_idd_pats, by=c('person_id'))

cohort_nonidd_add <- input_cohort %>%
  inner_join(nonidd_pats, by=c('person_id')) %>%
  distinct(person_id) %>%
  mutate(visit_date_fulfilled = as.Date(NA))

cohort_combined <- cohort_idd_add %>%
  mutate(cohort='idd') %>%
  full_join(cohort_nonidd_add, by=c('person_id','visit_date_fulfilled')) %>%
  mutate(cohort=case_when(cohort=='idd' ~ 'idd',
                          TRUE ~ 'non_idd'))

 output_tbl(cohort_combined, 'cohort_combined')

cohort_combined_demog <- get_demographics_general(cohort = results_tbl('cohort_combined'),
                                                 min_study_date = as.Date('2016-01-01'),
                                                 max_study_date = as.Date('2025-12-31'),
                                                 ip_visit_tbl = ip_visit_tbl,
                                                 min_age = 5L,
                                                 max_age = 21L)

output_tbl(cohort_combined_demog, 'combined_cohort_demog')

# ------------------------------------------------------------------------------
#' Make manual attrition table
# ------------------------------------------------------------------------------

manual_attrition <- get_manual_attrition(idd_attr_site = idd_attr_site,
                                        cohort = results_tbl('combined_cohort_demog'),
                                        elig_hosps = results_tbl('elig_hosps'),
                                        excl_sites = c('national'))

manual_attrition$att_summary %>% output_tbl('manual_attrition')
manual_attrition$att_summary2 %>% output_tbl('manual_attrition_by_idd_status')

















