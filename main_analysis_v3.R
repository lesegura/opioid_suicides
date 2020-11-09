##############################################
#                                            #
#     Main Analysis for Suicidal Attempts    #
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
m.atp.exp <- svyglm(suic_atp ~ med_po, design = design1, family = quasipoisson(link = "log"), data = adolescents)

tidy(m.atp.exp, exponentiate = T, conf.int = T)

### Crude model with the effect modifier only
m.atp.efm <- svyglm(suic_atp ~ talkprob_r2, design = design1, family = quasipoisson(link = "log"), data = adolescents)

tidy(m.atp.efm, exponentiate = T, conf.int = T)

### adjusted model exposure no effect modifier
m.atp.exp.adj <- svyglm(suic_atp ~ med_po + sex_r + year_r + race_r + county + bnghvymon_r + illyr_r + anysedmf, design = design1, family = quasipoisson(link = "log"), data = adolescents)

mainfx.exp <- tidy(m.atp.exp.adj, exponentiate = T, conf.int = T)[-c(1, 4:18), -c(3:5)]

mainfx.exp ### Risk Ratios


### adjusted model effect modifier no exposure
m.atp.efm.adj <- svyglm(suic_atp ~ talkprob_r2 + sex_r + year_r + race_r + county + bnghvymon_r + illyr_r + anysedmf, design = design1, family = quasipoisson(link = "log"), data = adolescents)

mainfx.efm <- tidy(m.atp.efm.adj, exponentiate = T, conf.int = T)[-c(1, 3:18), -c(3:5)]

mainfx.efm ### Risk Ratios


### MAIN EFFECTS TABLE

main.fx.tab <- rbind(mainfx.exp, mainfx.efm)

main.fx.tab


##################################################
#                                                #
#     Looking for interaction additive scale     #
#                                                #
##################################################

### social support as an effect modifier
m.atp <- svyglm(suic_atp ~ med_po*talkprob_r2 + sex_r + year_r + race_r + county + bnghvymon_r + illyr_r + anysedmf, design = design1, family = quasipoisson(link = "log"), data = adolescents)

tidy(m.atp, conf.int = T, exponentiate = T)[-c(5:18), ]



m.atp_emm <- emmeans(m.atp, specs = ~ med_po : talkprob_r2, 
                    infer = c(T, T), level = .95) ### you need to tell emmeans that there is an interaction between med_po and talk_prob.
### Now it should display the log odds differences (emmean column) between both med_po and talk_prob 


summary(m.atp_emm, type = "response")

### IC for social support / SI

m.atp_reg <- regrid(m.atp_emm, transform = "response") ### this is the backtransformation step.

m.atp_reg ### if you call this object, it shows you the risk differences (RD) of SI between med_po and talk_prob

ptable.sa <- contrast(m.atp_reg, method = "identity", reverse = T, infer = c(TRUE, TRUE)) ### p00, p01, p10, p11

ptable.sa <- as_tibble(ptable.sa) %>%
  mutate(p = c("p00", "p10", "p10", "p01", "p11", "p11"))

ptable.sa

rd_table.sa <- as.tibble(contrast(m.atp_reg, method = "revpairwise", reverse = T, infer = c(TRUE, TRUE))) ### p11 - p10 and p01 - p00.

rd_table.sa

rd_table.sa <- rd_table.sa[rd_table.sa$contrast %in% c("No PO Use No one - No PO Use Someone", 
                                              "PY Medical Use Only No one - PY Medical Use Only Someone",
                                              "(PY Any Non-Medical Use No one) - (PY Any Non-Medical Use Someone)"), 
                     -c(3:4, 7:8)]

rd_table.sa <- rd_table.sa %>%
  mutate(p = c("p11 - p10", "p11 - p10", "p01 - p00"), 
         contrast = factor(contrast, 
                           levels = unique(contrast),
                           labels = c("RD(No PO Use, No Social Support - No PO Use, Some Social Support)", 
                                      "RD(PY Medical Use Only No one - PY Medical Use Only Someone)",
                                      "RD(PY Any Non-Medical Use, No Social Support) - (PY Any Non-Medical Use, Some Social Support)")))

rd_table.sa

contrast(m.atp_reg, interaction = "trt.vs.ctrl") ### this gets you the interaction contrast. You see here the first column med_potrt.vs.ctrl is showing
### the differences between the (RD SI | PY Medical Use only vs no PO use) and (RD SI | talk_prob yes vs no), this is one interaction contrast
### and the differences between the (RD SI | PY Any Non-Medical Use vs no PO use) and (RD SI | talk_prob yes vs no), this is another interaction contrast

ic_table.sa <- as_tibble(confint(contrast(m.atp_reg, interaction = "trt.vs.ctrl"))) ### this gets you the confidence intervals for both interaction contrasts. You see there is evidence of additive interaction based 
### on the interaction contrast between (RD SI | PY Any Non-Medical Use vs no PO use) and (RD SI | talk_prob yes vs no) 0.14 (0.037 - 0.244)

ic_table.sa <- ic_table.sa %>%
  mutate(p = c("IC PY Medical PO: (p11 - p10) - (p01 - p00)", 
               "IC Any PY Non-Medical PO: (p11 - p10) - (p01 - p00)"), 
         contrast = factor(interaction(med_po_trt.vs.ctrl, talkprob_r2_trt.vs.ctrl), 
                           labels = c("IC PY Medical Use Only: (Yes - No) vs Social Support: (Yes - No)", 
                                      "IC Any PY Non-Medical Use: (Yes - No) vs Social Support: (Yes - No)"))) %>%
  select(-c(SE, df, med_po_trt.vs.ctrl, talkprob_r2_trt.vs.ctrl)) %>%
  relocate(contrast, .before = estimate)

ic_table.sa



###############################
#                             #
#            CHECKS           #
#                             #
###############################

### effect of PO on SI among those without social support
m.atp.exp.nosup <- svyglm(suic_atp ~ med_po, design = subset(design1, talkprob_r2 == "No one"), family = quasipoisson(link = "log"))

tidy(m.atp.exp.nosup, exponentiate = T, conf.int = T)

### effect of PO on SI among those with social support
m.atp.exp.yessup <- svyglm(suic_atp ~ med_po, design = subset(design1, talkprob_r2 == "Someone"), family = quasipoisson(link = "log"))

tidy(m.atp.exp.yessup, exponentiate = T, conf.int = T)

### effect of Social Support on SI among those without PO
m.atp.efm.nopo <- svyglm(suic_atp ~ talkprob_r2, design = subset(design1, med_po == "No PO Use"), family = quasipoisson(link = "log"))

tidy(m.atp.efm.nopo, exponentiate = T, conf.int = T)

### effect of Social Support on SI among those with PO
m.atp.efm.medpo <- svyglm(suic_atp ~ talkprob_r2, design = subset(design1, med_po == "PY Medical Use Only"), family = quasipoisson(link = "log"))

tidy(m.atp.efm.medpo, exponentiate = T, conf.int = T)

### effect of Social Support on SI among those with PO
m.atp.efm.nomedpo <- svyglm(suic_atp ~ talkprob_r2, design = subset(design1, med_po == "PY Any Non-Medical Use"), family = quasipoisson(link = "log"))

tidy(m.atp.efm.nomedpo, exponentiate = T, conf.int = T)
