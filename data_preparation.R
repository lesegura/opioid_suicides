library(survey)
library(srvyr)
library(tidyverse)


setwd("/Users/luissegura/Dropbox/NSDUH/R/Data/") ### set the directory where the datasets are

### load datasets directly from dropbox. NOTE: change dl=0 to dl=1
load(url("https://www.dropbox.com/s/rhki8s9so275ucv/NSDUH_2015.RData?dl=1"))
load(url("https://www.dropbox.com/s/d24bhm356bbnd1h/NSDUH_2016.RData?dl=1"))
load(url("https://www.dropbox.com/s/sragzrksgvj1jk3/NSDUH_2017.RData?dl=1"))
load(url("https://www.dropbox.com/s/3cqon8ki69p1cgd/NSDUH_2018.RData?dl=1"))

ls()

nsduh_15 <- as_tibble(PUF2015_021518)
nsduh_16 <- as_tibble(PUF2016_022818)
nsduh_17 <- as_tibble(PUF2017_100918)
nsduh_18 <- as_tibble(PUF2018_100819)

rm("PUF2015_021518", "PUF2016_022818", "PUF2017_100918", "PUF2018_100819")

names(nsduh_15) <- tolower(names(nsduh_15))
names(nsduh_16) <- tolower(names(nsduh_16))
names(nsduh_17) <- tolower(names(nsduh_17))
names(nsduh_18) <- tolower(names(nsduh_18))

nsduh_15$year <- 2015
nsduh_16$year <- 2016
nsduh_17$year <- 2017
nsduh_18$year <- 2018

nsduh_15$realid <- (nsduh_15$year * 1e10) + as.numeric(nsduh_15$questid2)
nsduh_16$realid <- (nsduh_16$year * 1e10) + as.numeric(nsduh_16$questid2)
nsduh_17$realid <- (nsduh_17$year * 1e10) + as.numeric(nsduh_17$questid2)
nsduh_18$realid <- (nsduh_18$year * 1e10) + as.numeric(nsduh_18$questid2)


myvars <- c("year", "questid2", "vestr", "verep", "analwt_c", "opinmyr", "opinmmon", "mrjylu",
            "fentpdapyu", "fentpdpymu", "pnrndaypm", "heraglst", "heryrtot",
            "herylu", "hermlu", "irpnrnminit", "irpnrnmage", "irpnrnmyfu", "mrjyr", "mrjmon", "heryr", 
            "herflag", "hermon", "pnranyflag", "pnranyyr", "pnrnmflag", "pnrnmyr", "pnrnmmon", "mrjyrbfr",
            "mjonlyflag", "mjonlyyr", "mjonlymon", "opinmyr", "opinmmon", "health2",
            "herpnryr", "illflag", "illyr", "illmon", "illemflag", "illemyr", "illemmon", 
            "bngdrkmon", "hvydrkmon", "alcmon", "illalcmon", "abodher", "udpypnr",
            "suicthnk", "suicplan", "mhsuithk", "adwrsthk", "adwrspln", "adwrsatp", "ad_mdea9", 
            "yowrdlot", "yowrdbtr", "yowrsthk", "yowrspln", "yowrsatp", "yo_mdea9", "pnrrsotrs2", 
            "smiyr_u", "amiyr_u", "smmiyr_u", "mmiyr_u", "lmiyr_u", "lmmiyru", "mi_cat_u", "smisudpy", 
            "amisudpy", "lmmisudpy", "snrlgsvc", "snrlgimp", "snrldcsn", "amdelt", "amdeyr", "ymdelt", 
            "ymdeyr", "ytxmdeyr", "yrxmdeyr", "ymdetxrx", "yhltmde", "yaltmde", "ymdehprx", "ymdehpo", "ymderxo2", 
            "ymdeharx", "ydocmde", "yomdmde", "ypsy1mde", "ypsy2mde", "ysocmde", "ycounmde", "yomhmde", "ynursmde", 
            "yrelmde", "yhbchmde", "yothmde", "anymhin2", "anymhout", "anysmh2", "anynsmh", "anymhed2", "anysedmf", 
            "yped", "yjail", "spinvst2", "spoutvst", "smhvst2", "smhdpr2", "smhfmly2", "smhsui2", "smhbrk2", "smhfea2",
            "smhschl2", "smhangr2", "smhfrnd2", "smhotpp2", "smheat2", "smhfite2", "smhmend2", "ymhosptx", "ymhnsptx", 
            "ysptxnmh", "ymhasptx", "newrace2", "irsex", "catage", "catag2", "catag3", "catag6", "catag7", "coutyp4", 
            "pden10", "cocyr", "crkyr", "lsdyr", "pcpyr", "hallucyr", "ecstmoyr", "damtfxyr", "oxycnanyyr",
            "ketminyr", "salviayr", "inhalyr", "methamyr", "trqanyyr", "stmanyyr", "sedanyyr", "methamyr",
            "sedanyyr", "trqnmyr", "dsthop12", "psyanyyr", "oxycnnmyr", "trqnmyr", "stmnmyr", "sednmyr", "psychyr")


for(i in myvars) {
  print(grep(i, names(nsduh_15), value = T))
}


nsduh <- bind_rows(nsduh_15[nsduh_15$catag6 < 3, myvars], 
                   nsduh_16[nsduh_16$catag6 < 3, myvars], 
                   nsduh_17[nsduh_17$catag6 < 3, myvars], 
                   nsduh_18[nsduh_18$catag6 < 3, myvars])


table(nsduh$year)

nsduh %>%
  group_by(catag6, ymdeyr) %>%
  count() %>%
  print(n = 200)


nsduh %>%
  group_by(catag6, yowrsthk, yowrspln, yowrdbtr, yowrdlot, yowrsatp) %>%
  count() %>%
  print(n = 200)



nsduh <- nsduh %>%
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
                                    yowrsatp == 98 | yowrsatp == 99 & catag6 > 1, NA, yowrsatp)), 
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
         wts4 = analwt_c / 4, 
         suic_id_count = rowSums(select(., yowrsthk_r, yowrspln_r, yowrdbtr_r, yowrdlot_r), na.rm = TRUE), 
         suic_id_miss = rowSums(is.na(select(., yowrsthk_r, yowrspln_r, yowrdbtr_r, yowrdlot_r))), 
         suic_id = ifelse(suic_id_miss == 4, NA, 
                          ifelse(suic_id_count > 0, 1, suic_id_count)), 
         suic_id_atp_count = rowSums(select(., suic_atp, suic_id), na.rm = TRUE),
         suic_id_atp_miss = rowSums(is.na(select(., suic_atp, suic_id))), 
         suic_id_atp = ifelse(suic_id_atp_miss == 2, NA, suic_id_atp_count), 
         opinmyr_r = factor(opinmyr, labels = c("No Use", "PY Opioids Misuse (Heroin or PO)")), 
         po_freq = factor(ifelse(pnrnmflag == 1 & irpnrnminit == 93, 1, 
                                 ifelse(pnrnmyr == 1 & irpnrnminit == 1, 2, 
                                        ifelse(pnrnmyr == 1 & irpnrnminit == 2, 3, 0))), labels = c("No Use", 
                                                                                                    "Former PO Misuser", 
                                                                                                    "Recent-onset PO Misuser", 
                                                                                                    "Persistent  PO Misuse")))

table(nsduh$irpnrnminit, nsduh$pnrnmflag, useNA = "always")

nsduh %>%
  group_by(catag6, yowrsthk_r, yowrspln_r, yowrdbtr_r, yowrdlot_r, suic_id, suic_atp, suic_id_atp_miss, suic_id_atp_count, suic_id_atp) %>%
  count() %>%
  print(n = 200)

nsduh %>% 
  group_by(pnrnmflag, pnrnmyr, irpnrnminit, po_freq) %>%
  count()


adolescents <- nsduh[nsduh$catag6 == 1, ]

adolescents <- adolescents %>%
  mutate(year_c = year - 2014)

rm("nsduh_15", "nsduh_16", "nsduh_17", "nsduh_18", "i", "myvars")