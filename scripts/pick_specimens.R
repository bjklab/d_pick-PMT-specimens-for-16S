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



#' revise selection of specimens given limited to only 32 additional specimens: remove Donor 1, add 1 each from donors 16 & 22 (longest duration) 
dat %>%
  filter(external_participant_id %in% paste0("Donor ", c("005", "016", "022", "034", "038"))) %>%
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
  identity() -> dat_choice2

dat_choice2


dat %>%
  filter(!sample %in% dat_choice2$sample) %>%
  filter(external_participant_id %in% paste0("Donor ", c("016", "022"))) %>%
  select(external_participant_id, sample, sample_type, collection_date, status, location) %>%
  distinct() %>% 
  filter(grepl("CHOP",location) == FALSE & !is.na(location)) %>% #remove if already transferred to CHOP
  add_count(external_participant_id, name = "specimen_count") %>%
  mutate(first_last_vector = map(.x = specimen_count, .f = ~ c(1,unique(.x)))) %>%
  mutate(middle_vector = map(.x = specimen_count / 2, .f = ~ round(c(.x)))) %>%
  mutate(slice_vector = map2(.x = first_last_vector, .y = middle_vector, .f = ~ c(.x,.y))) %>%
  #View()
  group_by(external_participant_id) %>%
  nest(-contains("vector")) %>%
  map2(.x = .$data, .y = .$middle_vector, .f = ~ slice(.data = .x, .y)) %>%
  bind_rows() %>%
  ungroup() %>%
  bind_rows(dat_choice2) %>%
  arrange(external_participant_id, collection_date) %>%
  identity() -> dat_choice3

dat_choice3
  



dat_choice3 %>%
  count(external_participant_id)

dat_choice3 %>%
  select(-specimen_count) %>%
  write_csv("./tabs/extra_PMT_donor_specimens_for_16S_revised.csv")



#' replace two alread-sequenced donor 5 specimens

dat_choice3 %>%
  mutate(date = as.Date(collection_date)) %>%
  ggplot(data = .) + geom_point(aes(x = date, y = external_participant_id))


dat %>%
  filter(!sample %in% dat_choice3$sample) %>%
  filter(external_participant_id %in% paste0("Donor ", c("016", "022"))) %>%
  select(external_participant_id, sample, sample_type, collection_date, status, location) %>%
  distinct() %>% 
  filter(grepl("CHOP",location) == FALSE & !is.na(location)) %>% #remove if already transferred to CHOP
  add_count(external_participant_id, name = "specimen_count") %>%
  mutate(first_last_vector = map(.x = specimen_count, .f = ~ c(1,unique(.x)))) %>%
  mutate(middle_vector = map(.x = specimen_count / 3, .f = ~ round(c(.x)))) %>%
  mutate(slice_vector = map2(.x = first_last_vector, .y = middle_vector, .f = ~ c(.x,.y))) %>%
  #View()
  group_by(external_participant_id) %>%
  nest(-contains("vector")) %>%
  map2(.x = .$data, .y = .$slice_vector, .f = ~ slice(.data = .x, .y)) %>%
  bind_rows() %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  bind_rows(dat_choice3) %>%
  arrange(external_participant_id, collection_date) %>%
  identity() -> dat_choice4

dat_choice4 %>%
  count(external_participant_id)


dat_choice4 %>%
  mutate(date = as.Date(collection_date)) %>%
  ggplot(data = .) + geom_point(aes(x = date, y = external_participant_id), shape = 21)


dat_choice4 %>%
  select(-specimen_count) %>%
  write_csv("./tabs/FINAL_34_extra_PMT_donor_specimens_for_16S_revised.csv")



#' Emily and Laura request 4 additional specimens
dat %>%
  filter(!sample %in% dat_choice4$sample) %>%
  filter(external_participant_id %in% paste0("Donor ", c("034","038"))) %>%
  select(external_participant_id, sample, sample_type, collection_date, status, location) %>%
  distinct() %>% 
  filter(grepl("CHOP",location) == FALSE & !is.na(location)) %>% #remove if already transferred to CHOP
  add_count(external_participant_id, name = "specimen_count") %>%
  mutate(first_last_vector = map(.x = specimen_count, .f = ~ c(1,unique(.x)))) %>%
  mutate(middle_vector = map(.x = specimen_count / 3, .f = ~ round(c(.x,.x*2)))) %>%
  mutate(slice_vector = map2(.x = first_last_vector, .y = middle_vector, .f = ~ c(.x,.y))) %>%
  #View()
  group_by(external_participant_id) %>%
  nest(-contains("vector")) %>%
  #map2(.x = .$data, .y = .$slice_vector, .f = ~ slice(.data = .x, .y)) %>%
  map2(.x = .$data, .y = .$middle_vector, .f = ~ slice(.data = .x, .y)) %>%
  bind_rows() %>%
  ungroup() %>%
  bind_rows(dat_choice4) %>%
  arrange(external_participant_id, collection_date) %>%
  identity() -> dat_choice5

dat_choice5 %>%
  count(external_participant_id)

dat_choice5 %>%
  mutate(date = as.Date(collection_date)) %>%
  ggplot(data = .) + geom_point(aes(x = date, y = external_participant_id), shape = 21)


dat_choice5 %>%
  select(-specimen_count) %>%
  write_csv("./tabs/extra_PMT_donor_specimens_for_16S_updated_20210507.csv")

# just the new ones:
dat_choice5 %>%
  anti_join(dat_choice4, by = "sample") %>%
  pull(sample)
