##############################################
#                                            #
#                 Main Analysis              #
#                                            #
##############################################
library(tidyverse)
library(survey)
library(srvyr)
library(ggplot2)
library(emmeans)
library(broom)

### Setting up the survey design
design1  <- svydesign(id = ~ verep, 
                      strata = ~ vestr,
                      weights = ~ analwt_c, ### note the weights for regression models
                      data = adolescents,
                      nest = TRUE)

### Association between medical, nonmedical PO and SI
### Crude Model with the exposure only
m.si.exp <- svyglm(suic_id ~ med_po, design = design1, family = quasipoisson(link = "log"), data = adolescents)

tidy(m.si.exp, exponentiate = T, conf.int = T)

### Crude model with the effect modifier only
m.si.efm <- svyglm(suic_id ~ talkprob_r2, design = design1, family = quasipoisson(link = "log"), data = adolescents)

tidy(m.si.efm, exponentiate = T, conf.int = T)

### adjusted model no effect modifier
m.si.exp.adj <- svyglm(suic_id ~ med_po + talkprob_r + sex_r + year_r + race_r + county + bnghvymon_r + illyr_r + anysedmf, design = design1, family = quasipoisson(link = "log"), data = adolescents)

tidy(m.si.exp.adj, exponentiate = T, conf.int = T)[-c(4:18), ]



m.si <- svyglm(suic_id ~ med_po*talkprob_r2 + sex_r + year_r + race_r + county + bnghvymon_r + illyr_r + anysedmf, design = design1, family = quasipoisson(link = "log"), data = adolescents)

tidy(m.si, conf.int = T, exponentiate = T)[-c(5:18), ]



m.si_emm <- emmeans(m.si_emm, specs = ~ med_po : talkprob_r, 
                   infer = c(T, T), level = .95) ### you need to tell emmeans that there is an interaction between med_po and talk_prob.
                                                ### Now it should display the log odds differences (emmean column) between both med_po and talk_prob 

m.si_emm <- emmeans(m.si_emm, "med_po", by = "talkprob_r", 
                    infer = c(T, T), level = .95) ### you need to tell emmeans that there is an interaction between med_po and talk_prob.
### Now it should display the log odds differences (emmean column) between both med_po and talk_prob 


summary(m.si_emm, type = "response")


m11.m <- svyglm(suic_id ~ med_po*talkprob_r + year_r + race_r + county + bnghvymon_r + illyr_r + anysedmf, design = subset(design1, sex_r == "Male"), family = quasipoisson(link = "log"), data = adolescents)
tidy(m11.m, conf.int = T, exponentiate = T)

m11.f <- svyglm(suic_id ~ med_po*talkprob_r + year_r + race_r + county + bnghvymon_r + illyr_r + anysedmf, design = subset(design1, sex_r == "Female"), family = quasipoisson(link = "log"), data = adolescents)
tidy(m11.f, conf.int = T, exponentiate = T)