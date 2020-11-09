##############################################
#                                            #
#     Main Analysis for Suicidal Ideation    #
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

### Adjusted model exposure no effect modifier
m.si.exp.adj <- svyglm(suic_id ~ med_po + sex_r + year_r + race_r + county + bnghvymon_r + illyr_r + anysedmf, 
                       design = design1, family = quasipoisson(link = "log"), 
                       data = adolescents)

mainfx.exp <- tidy(m.si.exp.adj, conf.int = T, exponentiate = T)[-c(1, 4:18), -c(3:5)]

mainfx.exp ### Prevalence Ratio or Risk Ratio


### Adjusted model effect modifier no exposure
m.si.efm.adj <- svyglm(suic_id ~ talkprob_r2 + sex_r + year_r + race_r + county + bnghvymon_r + illyr_r + anysedmf, design = design1, family = quasipoisson(link = "log"), data = adolescents)

mainfx.efm <- tidy(m.si.efm.adj, conf.int = T, exponentiate = T)[-c(1, 3:18), -c(3:5)]

mainfx.efm


### MAIN EFFECTS TABLE

mainfx.tab <- rbind(mainfx.exp, mainfx.efm)

mainfx.tab


##################################################
#                                                #
#     Looking for interaction additive scale     #
#                                                #
##################################################


### Adjusted model exposure PO Use and social support as effect modifier
m.si <- svyglm(suic_id ~ med_po*talkprob_r2 + sex_r + year_r + race_r + county + bnghvymon_r + illyr_r + anysedmf, design = design1, family = quasipoisson(link = "log"), data = adolescents)

intfx <- tidy(m.si, conf.int = T, exponentiate = T)[-c(1, 5:18), -c(3:5)]

intfx


m.si_emm <- emmeans(m.si, specs = ~ med_po : talkprob_r2, 
                   infer = c(T, T), level = .95) ### you need to tell emmeans that there is an interaction between med_po and talk_prob.
                                                ### Now it should display the log odds differences (emmean column) between both med_po and talk_prob 


summary(m.si_emm, type = "response")

### IC for social support / SI

m.si_reg <- regrid(m.si_emm, transform = "response") ### this is the backtransformation step.

m.si_reg ### if you call this object, it shows you the risk differences (RD) of SI between med_po and talk_prob

ptable.si <- contrast(m.si_reg, method = "identity", reverse = T, infer = c(TRUE, TRUE)) ### p00, p01, p10, p11

ptable.si <- as_tibble(ptable.si) %>%
  mutate(p = c("p00", "p10", "p10", "p01", "p11", "p11"))

ptable.si

rd_table.si <- as_tibble(contrast(m.si_reg, method = "revpairwise", reverse = T, infer = c(TRUE, TRUE))) ### p11 - p10 and p01 - p00.

rd_table.si

rd_table.si <- rd_table.si[rd_table.si$contrast %in% c("No PO Use No one - No PO Use Someone", 
                                              "PY Medical Use Only No one - PY Medical Use Only Someone",
                                              "(PY Any Non-Medical Use No one) - (PY Any Non-Medical Use Someone)"), 
                     -c(3:4, 7:8)]

rd_table.si <- rd_table.si %>%
  mutate(p = c("p01 - p00", "p11 - p10", "p11 - p10"), 
         contrast = factor(contrast, levels = unique(contrast),
                           labels = c("RD(No PO Use, No Social Support - No PO Use, Some Social Support)", 
                                                "RD(PY Medical Use Only No one - PY Medical Use Only Someone)", 
                                                "RD(PY Any Non-Medical Use, No Social Support) - (PY Any Non-Medical Use, Some Social Support)")))

rd_table.si

contrast(m.si_reg, interaction = "trt.vs.ctrl") ### this gets you the interaction contrast. You see here the first column med_potrt.vs.ctrl is showing
### the differences between the (RD SI | PY Medical Use only vs no PO use) and (RD SI | talk_prob yes vs no), this is one interaction contrast
### and the differences between the (RD SI | PY Any Non-Medical Use vs no PO use) and (RD SI | talk_prob yes vs no), this is another interaction contrast

ic_table.si <- as_tibble(confint(contrast(m.si_reg, interaction = "trt.vs.ctrl"))) ### this gets you the confidence intervals for both interaction contrasts. You see there is evidence of additive interaction based 
### on the interaction contrast between (RD SI | PY Any Non-Medical Use vs no PO use) and (RD SI | talk_prob yes vs no) 0.14 (0.037 - 0.244)

ic_table.si <- ic_table.si %>%
  mutate(p = c("IC PY Medical PO: (p11 - p10) - (p01 - p00)", 
               "IC Any PY Non-Medical PO: (p11 - p10) - (p01 - p00)"), 
         contrast = factor(interaction(med_po_trt.vs.ctrl, talkprob_r2_trt.vs.ctrl), labels = c("IC PY Medical Use Only: (Yes - No) vs Social Support: (Yes - No)", 
                                                                                                "IC Any PY Non-Medical Use: (Yes - No) vs Social Support: (Yes - No)"))) %>%
  select(-c(SE, df, med_po_trt.vs.ctrl, talkprob_r2_trt.vs.ctrl)) %>%
  relocate(contrast, .before = estimate)


ic_table.si


###############################
#                             #
#            CHECKS           #
#                             #
###############################

### effect of PO on SI among those without social support
m.si.exp.nosup <- svyglm(suic_id ~ med_po, design = subset(design1, talkprob_r2 == "No one"), family = quasipoisson(link = "log"))

tidy(m.si.exp.nosup, exponentiate = T, conf.int = T)

### effect of PO on SI among those with social support
m.si.exp.yessup <- svyglm(suic_id ~ med_po, design = subset(design1, talkprob_r2 == "Someone"), family = quasipoisson(link = "log"))

tidy(m.si.exp.yessup, exponentiate = T, conf.int = T)

### effect of Social Support on SI among those without PO
m.si.efm.nopo <- svyglm(suic_id ~ talkprob_r2, design = subset(design1, med_po == "No PO Use"), family = quasipoisson(link = "log"))

tidy(m.si.efm.nopo, exponentiate = T, conf.int = T)

### effect of Social Support on SI among those with PO
m.si.efm.medpo <- svyglm(suic_id ~ talkprob_r2, design = subset(design1, med_po == "PY Medical Use Only"), family = quasipoisson(link = "log"))

tidy(m.si.efm.medpo, exponentiate = T, conf.int = T)

### effect of Social Support on SI among those with PO
m.si.efm.nomedpo <- svyglm(suic_id ~ talkprob_r2, design = subset(design1, med_po == "PY Any Non-Medical Use"), family = quasipoisson(link = "log"))

tidy(m.si.efm.nomedpo, exponentiate = T, conf.int = T)
