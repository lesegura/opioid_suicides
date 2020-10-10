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

### Setting up the survey desing
design1  <- svydesign(id = ~ verep, 
                      strata = ~ vestr,
                      weights = ~ analwt_c, ### note the weights for regression models
                      data = adolescents,
                      nest = TRUE)


### Checking trends for SI
trend.m <- svyglm(suic_id ~ year, design = design1, family = gaussian(), data = adolescents)
summary(trend.m)

design %>% 
  group_by(year) %>%
  summarise(n = survey_total(suic_id, na.rm = T), 
            pr = survey_mean(suic_id, na.rm = T)) %>%
  mutate(pr = pr * 100) %>% 
  ggplot(aes(x = year, y = pr)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1, se = F, color = "orange") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), size = 1, se = F, color = "red") +
  xlab("") +
  ylab("%") +
  labs(title = "")

### Checking trends for SA
trend.m <- svyglm(suic_atp ~ year, design = design1, family = gaussian(), data = adolescents)
summary(trend.m)

design %>% 
  group_by(year) %>%
  summarise(n = survey_total(suic_atp, na.rm = T), 
            pr = survey_mean(suic_atp, na.rm = T)) %>%
  mutate(pr = pr * 100) %>% 
  ggplot(aes(x = year, y = pr)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1, se = F, color = "orange") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), size = 1, se = F, color = "red") +
  xlab("") +
  ylab("%") +
  labs(title = "")


### Association between medical, nonmedical PO and SI
m1 <- svyglm(suic_id ~ med_po + year_r, design = design1, family = quasipoisson(link = "log"), data = adolescents)
tidy(m1, conf.int = T, exponentiate = T)

m1_emm <- emmeans(m1, "med_po", infer = c(T, T), level = .95)

summary(m1_emm, type = "response")


m2 <- svyglm(suic_id ~ med_po + year_r + sex_r + race_r + county, design = design1, family = quasipoisson(link = "log"), data = adolescents)
tidy(m2, conf.int = T, exponentiate = T)

m2_emm <- emmeans(m2, "med_po", infer = c(T, T), level = .95)

summary(m2_emm, type = "response")


m3 <- svyglm(suic_id ~ med_po + year_r + sex_r + race_r + county + bnghvymon_r, design = design1, family = quasipoisson(link = "log"), data = adolescents)
tidy(m3, conf.int = T, exponentiate = T)

m3_emm <- emmeans(m3, "med_po", infer = c(T, T), level = .95)

summary(m3_emm, type = "response")

m4 <- svyglm(suic_id ~ med_po + year_r + sex_r + race_r + county + bnghvymon_r + illyr_r, design = design1, family = quasipoisson(link = "log"), data = adolescents)
tidy(m4, conf.int = T, exponentiate = T)

m4_emm <- emmeans(m4, "med_po", infer = c(T, T), level = .95)

summary(m4_emm, type = "response")

m5 <- svyglm(suic_id ~ med_po*sex_r + year_r + race_r + county + bnghvymon_r + illyr_r + anysedmf, design = design1, family = quasipoisson(link = "log"), data = adolescents)
tidy(m5, conf.int = T, exponentiate = T)

m5.m <- svyglm(suic_id ~ med_po + year_r + race_r + county + bnghvymon_r + illyr_r + anysedmf, design = subset(design1, sex_r == "Male"), family = quasipoisson(link = "log"), data = adolescents)
tidy(m5.m, conf.int = T, exponentiate = T)

m5.f <- svyglm(suic_id ~ med_po + year_r + race_r + county + bnghvymon_r + illyr_r + anysedmf, design = subset(design1, sex_r == "Female"), family = quasipoisson(link = "log"), data = adolescents)
tidy(m5.f, conf.int = T, exponentiate = T)

### Association between medical, nonmedical PO and SA
m6 <- svyglm(suic_atp ~ med_po + year_r, design = design1, family = quasipoisson(link = "log"), data = adolescents)
tidy(m6, conf.int = T, exponentiate = T)

m7 <- svyglm(suic_atp ~ med_po + year_r + sex_r + race_r + county, design = design1, family = quasipoisson(link = "log"), data = adolescents)
tidy(m7, conf.int = T, exponentiate = T)

m8 <- svyglm(suic_atp ~ med_po + year_r + sex_r + race_r + county + bnghvymon_r, design = design1, family = quasipoisson(link = "log"), data = adolescents)
tidy(m8, conf.int = T, exponentiate = T)

m9 <- svyglm(suic_atp ~ med_po + year_r + sex_r + race_r + county + bnghvymon_r + illyr_r, design = design1, family = quasipoisson(link = "log"), data = adolescents)
tidy(m9, conf.int = T, exponentiate = T)

m10 <- svyglm(suic_atp ~ med_po*sex_r + year_r + race_r + county + bnghvymon_r + illyr_r + anysedmf, design = design1, family = quasipoisson(link = "log"), data = adolescents)
tidy(m10, conf.int = T, exponentiate = T)

m10.m <- svyglm(suic_atp ~ med_po + year_r + race_r + county + bnghvymon_r + illyr_r + anysedmf, design = subset(design1, sex_r == "Male"), family = quasipoisson(link = "log"), data = adolescents)
tidy(m10.m, conf.int = T, exponentiate = T)

m10.f <- svyglm(suic_atp ~ med_po + year_r + race_r + county + bnghvymon_r + illyr_r + anysedmf, design = subset(design1, sex_r == "Female"), family = quasipoisson(link = "log"), data = adolescents)
tidy(m10.f, conf.int = T, exponentiate = T)


m5_emm <- emmeans(m5, "med_po", by = c("sex_r"), infer = c(T, T), level = .95)

m5_sm <- summary(m5_emm, type = "response")

m5_sm$outcome <- "ideation"


m10_emm <- emmeans(m10, "med_po", by = c("sex_r"), infer = c(T, T), level = .95)

m10_sm <- summary(m10_emm, type = "response")

m10_sm$outcome <- "attempt"

estimates <- c("aPR = 1.00", "aPR = 1.27 \n(1.11; 1.44)", "aPR = 1.42 \n(1.14; 1.77)", 
               "aPR = 1.00", "aPR = 1.28 \n(1.20; 1.36)", "aPR = 1.27 \n(1.18; 1.36)", 
               "aPR = 1.00", "aPR = 1.18 \n(0.86; 1.62)", "aPR = 1.90 \n(1.24; 2.91)", 
               "aPR = 1.00", "aPR = 1.63 \n(1.45; 1.84)", "aPR = 1.78 \n(1.51; 2.10)")

m.plot <- rbind(m5_sm, m10_sm)

m.plot

m.plot %>%
  mutate_if(is.numeric, proportion) %>%
  mutate(outcome = factor(outcome, levels = c("ideation", "attempt"), labels = c("Suicidal Ideation", "Suicidal Attempt"))) %>%
  ggplot(aes(x = med_po, y = rate, colour = med_po, fill = med_po)) +
  geom_bar(position = "dodge", stat = "identity") + 
  geom_text(aes(label = paste(paste(round(rate, 2), "%", sep = ""), estimates, sep = "\n")), 
            position = position_dodge(width = 0.9), 
            hjust = -.3, 
            colour = "black", 
            size = 5.5) +
  coord_flip() +
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(), 
        panel.background=element_rect(fill="white", 
                                      colour="black"), 
        legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(family = "Avenir", size = 16, colour = "black"), 
        axis.text = element_text(colour = "black", size = 12), 
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.spacing.x = unit(0.5, "cm"), 
        legend.text = element_text(size = 16), 
        strip.background = element_rect(fill = "white"), 
        strip.text = element_text(colour = "black", size = 18)) +
  scale_y_continuous(breaks = seq(from = 0, to = 50, by = 10)) +
  expand_limits(y = c(0, 50)) +
  xlab("") +
  ylab("Prevalence (%)") +
  facet_grid(sex_r ~ outcome)

ggsave("figure_4.eps", width = 13 , height = 8, units = "in", dpi = 1800)

### Association between frequency of PO and SI
m1 <- svyglm(suic_id ~ po_freq + year_r, design = design1, family = quasipoisson(link = "log"), data = adolescents)
tidy(m1, conf.int = T, exponentiate = T)

m2 <- svyglm(suic_id ~ po_freq + year_r + sex_r + race_r + county, design = design1, family = quasipoisson(link = "log"), data = adolescents)
tidy(m2, conf.int = T, exponentiate = T)

m3 <- svyglm(suic_id ~ po_freq + year_r + sex_r + race_r + county + bnghvymon_r, design = design1, family = quasipoisson(link = "log"), data = adolescents)
tidy(m3, conf.int = T, exponentiate = T)

m4 <- svyglm(suic_id ~ po_freq + year_r + sex_r + race_r + county + bnghvymon_r + illyr_r, design = design1, family = quasipoisson(link = "log"), data = adolescents)
tidy(m4, conf.int = T, exponentiate = T)

m5 <- svyglm(suic_id ~ po_freq*sex_r + year_r + race_r + county + bnghvymon_r + illyr_r + anysedmf, design = design1, family = quasipoisson(link = "log"), data = adolescents)
tidy(m5, conf.int = T, exponentiate = T)

m5.m <- svyglm(suic_id ~ po_freq + year_r + race_r + county + bnghvymon_r + illyr_r + anysedmf, design = subset(design1, sex_r == "Male"), family = quasipoisson(link = "log"), data = adolescents)
tidy(m5.m, conf.int = T, exponentiate = T)

m5.f <- svyglm(suic_id ~ po_freq + year_r + race_r + county + bnghvymon_r + illyr_r + anysedmf, design = subset(design1, sex_r == "Female"), family = quasipoisson(link = "log"), data = adolescents)
tidy(m5.f, conf.int = T, exponentiate = T)

### Association between medical, nonmedical PO and SA
m6 <- svyglm(suic_atp ~ po_freq + year_r, design = design1, family = quasipoisson(link = "log"), data = adolescents)
tidy(m6, conf.int = T, exponentiate = T)

m7 <- svyglm(suic_atp ~ po_freq + year_r + sex_r + race_r + county, design = design1, family = quasipoisson(link = "log"), data = adolescents)
tidy(m7, conf.int = T, exponentiate = T)

m8 <- svyglm(suic_atp ~ po_freq + year_r + sex_r + race_r + county + bnghvymon_r, design = design1, family = quasipoisson(link = "log"), data = adolescents)
tidy(m8, conf.int = T, exponentiate = T)

m9 <- svyglm(suic_atp ~ po_freq + year_r + sex_r + race_r + county + bnghvymon_r + illyr_r, design = design1, family = quasipoisson(link = "log"), data = adolescents)
tidy(m9, conf.int = T, exponentiate = T)

m10 <- svyglm(suic_atp ~ po_freq*sex_r + year_r + race_r + county + bnghvymon_r + illyr_r + anysedmf, design = design1, family = quasipoisson(link = "log"), data = adolescents)
tidy(m10, conf.int = T, exponentiate = T)

m10.m <- svyglm(suic_atp ~ po_freq + year_r + race_r + county + bnghvymon_r + illyr_r + anysedmf, design = subset(design1, sex_r == "Male"), family = quasipoisson(link = "log"), data = adolescents)
tidy(m10.m, conf.int = T, exponentiate = T)

m10.f <- svyglm(suic_atp ~ po_freq + year_r + race_r + county + bnghvymon_r + illyr_r + anysedmf, design = subset(design1, sex_r == "Female"), family = quasipoisson(link = "log"), data = adolescents)
tidy(m10.f, conf.int = T, exponentiate = T)


m5_emm <- emmeans(m5, "po_freq", by = c("sex_r"), infer = c(T, T), level = .95)

m5_sm <- summary(m5_emm, type = "response")

m5_sm$outcome <- "ideation"


m10_emm <- emmeans(m10, "po_freq", by = c("sex_r"), infer = c(T, T), level = .95)

m10_sm <- summary(m10_emm, type = "response")

m10_sm$outcome <- "attempt"

estimates <- c("aPR = 1.00", "aPR = 2.31 \n(1.73; 3.07)", "aPR = 1.22 \n(0.88; 1.69)", "aPR = 1.59 \n(1.25; 2.03)", 
               "aPR = 1.00", "aPR = 1.37 \n(1.20; 1.55)", "aPR = 1.18 \n(1.06; 1.32)", "aPR = 1.23 \n(1.12; 1.35)", 
               "aPR = 1.00", "aPR = 2.38 \n(1.38; 4.10)", "aPR = 1.24 \n(0.70; 2.17)", "aPR = 2.53 \n(1.60; 3.98)", 
               "aPR = 1.00", "aPR = 1.60 \n(1.23; 2.08)", "aPR = 1.36 \n(1.05; 1.75)", "aPR = 1.76 \n(1.46; 2.13)")

m.plot <- rbind(m5_sm, m10_sm)

m5_sm
m10_sm

m.plot %>%
  mutate_if(is.numeric, proportion) %>%
  mutate(outcome = factor(outcome, levels = c("ideation", "attempt"), labels = c("Suicidal Ideation", "Suicidal Attempt"))) %>%
  ggplot(aes(x = po_freq, y = rate, colour = po_freq, fill = po_freq)) +
  geom_bar(position = "dodge", stat = "identity") + 
  geom_text(aes(label = paste(paste(round(rate, 2), "%", sep = ""), estimates, sep = "\n")), 
            position = position_dodge(width = 0.9), 
            hjust = -.3, 
            colour = "black", 
            size = 4.5) +
  coord_flip() +
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(), 
        panel.background=element_rect(fill="white", 
                                      colour="black"), 
        legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(family = "Avenir", size = 16, colour = "black"), 
        axis.text = element_text(colour = "black", size = 12), 
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.spacing.x = unit(0.5, "cm"), 
        legend.text = element_text(size = 16), 
        strip.background = element_rect(fill = "white"), 
        strip.text = element_text(colour = 'black', size = 18)) +
  scale_y_continuous(breaks = seq(from = 0, to = 50, by = 10)) +
  scale_x_discrete(labels = c("No Use" = "No Use",
                              "Former PO Misuser" = "Former PO \n Misuse",
                              "Recent-onset PO Misuser" = "Recent-onset \n PO Misuse",
                              "Persistent  PO Misuse" = "Persistent \n PO Misuse")) +
  expand_limits(y = c(0, 50)) +
  xlab("") +
  ylab("Prevalence (%)") +
  facet_grid(sex_r ~ outcome)


ggsave("figure_5.eps", width = 13 , height = 8, units = "in", dpi = 1800)

