###################################
#                                 #
#             Table 1             #
#                                 #
###################################

library(tidyverse)
library(survey)
library(srvyr)
library(ggplot2)

### setting the survey design

design <- adolescents %>% as_survey_design(id = verep, 
                                           strata = vestr,
                                           weights = wts4,
                                           nest = TRUE)

design.allyr  <-  adolescents %>% as_survey_design(id = verep, 
                                                   strata = vestr,
                                                   weights = analwt_c,
                                                   nest = TRUE)


### function to transform a proportion to percent
percent <- function(x) {
  x * 100
}


design %>% 
  group_by(med_po) %>%
  summarize(proportion = survey_mean(vartype = "ci"), 
            N = unweighted(n())) %>%
  mutate_if(is.numeric, percent) %>% 
  print(n = Inf)

design %>% 
  filter(!is.na(talkprob_r2)) %>%
  group_by(talkprob_r2) %>%
  summarize(proportion = survey_mean(vartype = "ci"), 
            N = unweighted(n())) %>%
  mutate_if(is.numeric, percent) %>% 
  print(n = Inf)

design %>% 
  group_by(med_po, sex) %>%
  summarize(proportion = survey_mean(vartype = "ci"), 
            N = unweighted(n())) %>%
  mutate_if(is.numeric, percent) %>% 
  print(n = Inf)

design %>% 
  filter(!is.na(talkprob_r2)) %>%
  group_by(talkprob_r2, sex) %>%
  summarize(proportion = survey_mean(vartype = "ci"), 
            N = unweighted(n())) %>%
  mutate_if(is.numeric, percent) %>% 
  print(n = Inf)

design %>% 
  group_by(med_po, race) %>%
  summarize(proportion = survey_mean(vartype = "ci"), 
            N = unweighted(n())) %>%
  mutate_if(is.numeric, percent) %>% 
  print(n = Inf)

design %>% 
  filter(!is.na(talkprob_r2)) %>%
  group_by(talkprob_r2, race) %>%
  summarize(proportion = survey_mean(vartype = "ci"), 
            N = unweighted(n())) %>%
  mutate_if(is.numeric, percent) %>% 
  print(n = Inf)

design %>% 
  group_by(med_po, county) %>%
  summarize(proportion = survey_mean(vartype = "ci"), 
            N = unweighted(n())) %>%
  mutate_if(is.numeric, percent) %>% 
  print(n = Inf)

design %>% 
  filter(!is.na(talkprob_r2)) %>%
  group_by(talkprob_r2, county) %>%
  summarize(proportion = survey_mean(vartype = "ci"), 
            N = unweighted(n())) %>%
  mutate_if(is.numeric, percent) %>% 
  print(n = Inf)

design %>% 
  group_by(med_po, bnghvymon_r) %>%
  summarize(proportion = survey_mean(vartype = "ci"), 
            N = unweighted(n())) %>%
  mutate_if(is.numeric, percent) %>% 
  print(n = Inf)

design %>% 
  filter(!is.na(talkprob_r2)) %>%
  group_by(talkprob_r2, bnghvymon_r) %>%
  summarize(proportion = survey_mean(vartype = "ci"), 
            N = unweighted(n())) %>%
  mutate_if(is.numeric, percent) %>% 
  print(n = Inf)

design %>% 
  group_by(med_po, illyr_r) %>%
  summarize(proportion = survey_mean(vartype = "ci"), 
            N = unweighted(n())) %>%
  mutate_if(is.numeric, percent) %>% 
  print(n = Inf)

design %>% 
  filter(!is.na(talkprob_r2)) %>%
  group_by(talkprob_r2, illyr_r) %>%
  summarize(proportion = survey_mean(vartype = "ci"), 
            N = unweighted(n())) %>%
  mutate_if(is.numeric, percent) %>% 
  print(n = Inf)


design %>% 
  filter(!is.na(anysedmf_r)) %>%
  group_by(med_po, anysedmf_r) %>%
  summarize(proportion = survey_mean(vartype = "ci"), 
            N = unweighted(n())) %>%
  mutate_if(is.numeric, percent) %>% 
  print(n = Inf)

design %>% 
  filter(!is.na(talkprob_r2) & !is.na(anysedmf_r)) %>%
  group_by(talkprob_r2, anysedmf_r) %>%
  summarize(proportion = survey_mean(vartype = "ci"), 
            N = unweighted(n())) %>%
  mutate_if(is.numeric, percent) %>% 
  print(n = Inf)

design %>% 
  filter(!is.na(suic_id)) %>%
  mutate(suic_id_fct = factor(suic_id)) %>%
  group_by(med_po, suic_id_fct) %>%
  summarize(proportion = survey_mean(vartype = "ci"), 
            N = unweighted(n())) %>%
  mutate_if(is.numeric, percent) %>% 
  print(n = Inf)

design %>% 
  filter(!is.na(talkprob_r2) & !is.na(suic_id)) %>%
  mutate(suic_id_fct = factor(suic_id)) %>%
  group_by(talkprob_r2, suic_id_fct) %>%
  summarize(proportion = survey_mean(vartype = "ci"), 
            N = unweighted(n())) %>%
  mutate_if(is.numeric, percent) %>% 
  print(n = Inf)

design %>% 
  filter(!is.na(suic_atp)) %>%
  mutate(suic_atp_fct = factor(suic_atp)) %>%
  group_by(med_po, suic_atp_fct) %>%
  summarize(proportion = survey_mean(vartype = "ci"), 
            N = unweighted(n())) %>%
  mutate_if(is.numeric, percent) %>% 
  print(n = Inf)

design %>% 
  filter(!is.na(talkprob_r2) & !is.na(suic_atp)) %>%
  mutate(suic_atp_fct = factor(suic_atp)) %>%
  group_by(talkprob_r2, suic_atp_fct) %>%
  summarize(proportion = survey_mean(vartype = "ci"), 
            N = unweighted(n())) %>%
  mutate_if(is.numeric, percent) %>% 
  print(n = Inf)
