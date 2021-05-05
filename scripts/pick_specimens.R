#' #######################################################################################
#' load libraries and set seed
#' #######################################################################################
library(tidyverse)
library(readxl)
library(tidybayes)
library(brms)
library(gt)
library(colorspace)

set.seed(16)


#' #######################################################################################
#' load data
#' #######################################################################################
readxl::read_xls("./data/pmt_donor_stool_swab_20210504.xls") %>%
  janitor::clean_names() %>%
  identity() -> dat
  
dat



#' #######################################################################################
#' select specimens from donors 5, 16, 22, 34, 38 (doses given) and 1 (most longitudinal)
#' #######################################################################################

#' view available specimens
dat %>%
  filter(external_participant_id %in% paste0("Donor ", c("001", "005", "016", "022", "034", "038"))) %>%
  select(external_participant_id, sample, sample_type, collection_date, status, location) %>%
  distinct() %>% 
  filter(grepl("CHOP",location) == FALSE) %>% #remove if already transferred to CHOP
  group_by(external_participant_id) %>%
  summarise(count = n(),
            start_date = min(collection_date, na.rm = TRUE),
            end_date = max(collection_date, na.rm = TRUE),
            ) %>%
  ungroup() %>%
  mutate_at(.vars = vars(contains("date")), .funs = ~ as.Date(.x, format = "%Y-%m-%d")) %>%
  mutate(timespan_days = as.numeric(end_date - start_date)) %>%
  gt()


#' check specimen selection
dat %>%
  filter(external_participant_id %in% paste0("Donor ", c("001", "005", "016", "022", "034", "038"))) %>%
  select(external_participant_id, sample, sample_type, collection_date, status, location) %>%
  distinct() %>% 
  filter(grepl("CHOP",location) == FALSE) %>% #remove if already transferred to CHOP
  group_by(external_participant_id) %>%
  # slice first and last
  slice(c(1,n())) %>%
  #
  summarise(count = n(),
            start_date = min(collection_date, na.rm = TRUE),
            end_date = max(collection_date, na.rm = TRUE),
  ) %>%
  ungroup() %>%
  mutate_at(.vars = vars(contains("date")), .funs = ~ as.Date(.x, format = "%Y-%m-%d")) %>%
  mutate(timespan_days = as.numeric(end_date - start_date)) %>%
  gt()


#' select specimens
dat %>%
  filter(external_participant_id %in% paste0("Donor ", c("001", "005", "016", "022", "034", "038"))) %>%
  select(external_participant_id, sample, sample_type, collection_date, status, location) %>%
  distinct() %>% 
  filter(grepl("CHOP",location) == FALSE & !is.na(location)) %>% #remove if already transferred to CHOP
  add_count(external_participant_id, name = "specimen_count") %>%
  mutate(first_last_vector = map(.x = specimen_count, .f = ~ c(1,unique(.x)))) %>%
  mutate(middle_vector = map(.x = specimen_count / 5, .f = ~ round(c(.x, .x*2, .x*3, .x*4)))) %>%
  mutate(slice_vector = map2(.x = first_last_vector, .y = middle_vector, .f = ~ c(.x,.y))) %>%
  #View()
  group_by(external_participant_id) %>%
  nest(-contains("vector")) %>%
  map2(.x = .$data, .y = .$slice_vector, .f = ~ slice(.data = .x, .y)) %>%
  bind_rows() %>%
  ungroup() %>%
  identity() -> dat_choice

dat_choice

dat_choice %>%
  count(external_participant_id)

dat_choice %>%
  select(-specimen_count) %>%
  write_csv("./tabs/extra_PMT_donor_specimens_for_16S.csv")


