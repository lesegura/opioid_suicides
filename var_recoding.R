##############################################
#                                            #
#               Variable Recoding            #
#                                            #
##############################################
library(tidyverse)

load("adolescents.RData")

adolescents <- adolescents %>%
  mutate(yo_mdea9_r = ifelse(yo_mdea9 == 99 & catag6 == 1 | yo_mdea9 == 2, 0, 
                             ifelse(yo_mdea9 == 94 | yo_mdea9 == 97 | yo_mdea9 == 89 |
                                      yo_mdea9 == 98 | yo_mdea9 == 99 & catag6 > 1, NA, yo_mdea9)), 
         yo_mdea9_fct = factor(yo_mdea9_r, labels = c("No", "Yes")),
         yowrsthk_r = ifelse(yowrsthk == 2 | yowrsthk == 99 & catag6 == 1, 0, 
                             ifelse(yowrsthk == 94 | yowrsthk == 97 | yowrsthk == 89 |
                                      yowrsthk == 98 | yowrsthk == 99 & catag6 > 1, NA, yowrsthk)), 
         yowrspln_r = ifelse(yowrspln == 2 | yowrspln == 99 & catag6 == 1, 0, 
                             ifelse(yowrspln == 94 | yowrspln == 97 |
                                      yowrspln == 98 | yowrspln == 99 & catag6 > 1, NA, yowrspln)), 
         yowrdbtr_r = ifelse(yowrdbtr == 2 | yowrdbtr == 99 & catag6 == 1, 0, 
                             ifelse(yowrdbtr == 94 | yowrdbtr == 97 | yowrdbtr == 89 |
                                      yowrdbtr == 98 | yowrdbtr == 99 & catag6 > 1, NA, yowrdbtr)), 
         yowrdlot_r = ifelse(yowrdlot == 2 | yowrdlot == 99 & catag6 == 1, 0, 
                             ifelse(yowrdlot == 94 | yowrdlot == 97 | yowrdlot == 89 |
                                      yowrdlot == 98 | yowrdlot == 99 & catag6 > 1, NA, yowrdlot)), 
         suic_atp = ifelse(yowrsatp == 2 | yowrsatp == 99 & catag6 == 1, 0, 
                           ifelse(yowrsatp == 94 | yowrsatp == 97 |
                                    yowrsatp == 98 | yowrsatp == 99 & catag6 > 1, NA, yowrsatp)), ### Suicidal Attempt
         race = factor(ifelse(newrace2 == 4 | newrace2 == 5 | newrace2 == 6, 3, 
                              ifelse(newrace2 == 7, 4, newrace2)), 
                       labels = c("Whites", "Blacks", "Others", "Hispanics")), 
         sex = factor(ifelse(irsex == 2, 0, irsex), labels = c("Female", "Male")), 
         year_r = factor(year), 
         sex_r = relevel(sex, "Male"), 
         race_r = relevel(race, "Others"), 
         county = factor(coutyp4, labels = c("Large Metro", "Small Metro", "Nonmetro")),
         ymdeyr_r = factor(ifelse(ymdeyr == 2, 0, ymdeyr), labels = c("No", "Yes")), 
         bnghvymon_r = factor(ifelse(hvydrkmon == 1, 4, 
                                     ifelse(bngdrkmon == 1 & hvydrkmon == 0, 3, 
                                            ifelse(alcmon == 1 & bngdrkmon == 0, 2, 1))), 
                              labels = c("Did not Use in PM", "Past Month But Not Binge", "Binge But Not Heavy Use", "Heavy Alcohol Use")),
         cocyr_r = factor(cocyr, labels = c("No", "Yes")), 
         crkyr_r = factor(crkyr, labels = c("No", "Yes")), 
         lsdyr_r = factor(lsdyr, labels = c("No", "Yes")), 
         pcpyr_r = factor(pcpyr, labels = c("No", "Yes")), 
         hallucyr_r = factor(hallucyr, labels = c("No", "Yes")),
         ecstmoyr_r = factor(ecstmoyr, labels = c("No", "Yes")),
         damtfxyr_r = factor(damtfxyr, labels = c("No", "Yes")),
         ketminyr_r = factor(ketminyr, labels = c("No", "Yes")),
         salviayr_r = factor(salviayr, labels = c("No", "Yes")),
         inhalyr_r = factor(inhalyr, labels = c("No", "Yes")),
         methamyr_r = factor(methamyr, labels = c("No", "Yes")),
         trqanyyr_r = factor(trqanyyr, labels = c("No", "Yes")),
         stmanyyr_r = factor(stmanyyr, labels = c("No", "Yes")),
         sedanyyr_r = factor(sedanyyr, labels = c("No", "Yes")),
         illyr_r = factor(illyr, labels = c("No PY Illicit Drug Use", "PY Illicit Drug Use")),
         illmon_r = factor(illmon, labels = c("No PM Illicit Drug Use", "PM Illicit Drug Use")),
         pnranyyr_r = factor(pnranyyr, labels = c("No PO Use", "PY Any PO Use")), 
         pnrnmyr_r = factor(pnrnmyr, labels = c("No PO misuse", "PY Any PO Misuse")),
         med_po = factor(ifelse(pnranyyr == 0, 0, 
                                ifelse(pnranyyr == 1 & pnrnmyr == 0, 1, 
                                       ifelse(pnrnmyr == 1, 2, 999))), labels = c("No PO Use", 
                                                                                  "PY Medical Use Only", 
                                                                                  "PY Any Non-Medical Use")),
         anysedmf_r = factor(anysedmf, labels = c("No", "Yes")), 
         talkprob_r = factor(talkprob, labels = c("No one", "Someone")), 
         talkprob_r2 = fct_relevel(talkprob_r, "Someone"),
         wts4 = analwt_c / 4) 

### Creating Suicidality Variables

### Suicidal Ideation
adolescents <- adolescents %>% 
  mutate(suic_id_count = rowSums(select(., yowrsthk_r, yowrspln_r, yowrdbtr_r, yowrdlot_r), na.rm = TRUE),
         suic_id_miss = rowSums(is.na(select(., yowrsthk_r, yowrspln_r, yowrdbtr_r, yowrdlot_r))),
         suic_id = ifelse(suic_id_miss == 4, NA, 
                          ifelse(suic_id_count > 0, 1, suic_id_count)))

### Suicidal Ideation or Attempt
adolescents <- adolescents %>% 
  mutate(suic_id_atp_count = rowSums(select(., suic_atp, suic_id), na.rm = TRUE),
         suic_id_atp_miss = rowSums(is.na(select(., suic_atp, suic_id))), 
         suic_id_atp = ifelse(suic_id_atp_miss == 2, NA, 
                              ifelse(suic_id_atp_count == 2, 1, suic_id_atp_count)), 
         opinmyr_r = factor(opinmyr, labels = c("No Use", "PY Opioids Misuse (Heroin or PO)")), 
         po_freq = factor(ifelse(pnrnmflag == 1 & irpnrnminit == 93, 1, 
                                 ifelse(pnrnmyr == 1 & irpnrnminit == 1, 2, 
                                        ifelse(pnrnmyr == 1 & irpnrnminit == 2, 3, 0))), labels = c("No Use", 
                                                                                                    "Former PO Misuser", 
                                                                                                    "Recent-onset PO Misuser", 
                                                                                                    "Persistent  PO Misuse")))

adolescents <- adolescents %>% mutate(po_freq_r = factor(ifelse(po_freq =="No Use", NA, po_freq) - 1, labels = c("Former PO Nonmedical Use", 
                                                                                                     "Recent-onset PO Nonmedical Use", 
                                                                                                     "Persistent PO Nonmedical Use")))

table(adolescents$talkprob_r, adolescents$talkprob_r2)
levels(adolescents$talkprob_r)
levels(adolescents$talkprob_r2)
