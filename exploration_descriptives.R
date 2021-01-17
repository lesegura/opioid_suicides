################################################
#                                              #
#     Data Exploration and Descriptives        #
#                                              #
################################################

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

mycols <- c("#ad67c0", "#ff6a69", "#286fb8") 

### Prevalence of Medical/Nonmedical PO Use
design %>% 
  group_by(med_po) %>%
  summarize(proportion = survey_mean(vartype = "ci"), 
            N = unweighted(n())) %>%
  mutate_if(is.numeric, percent) %>%
  arrange(proportion) %>%
  mutate(ypos = cumsum(proportion) - 0.5 * proportion) %>%
  mutate(label = paste0(med_po, "\n", round(proportion, 2), "%")) %>%
  print(n = 100) %>%
  ggplot(aes(x = "", y = proportion, fill = med_po)) +
  geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start = 0) +
  geom_text(aes(y = ypos, label = label), color = "white", size = 12, family = "Tahoma") +
  theme_void() +
  theme(legend.position = "none") + 
  scale_fill_manual(values = mycols)

ggsave("po_use.eps", width = 13, height = 13, units = "in", dpi = 1800) 


### Prevalence of Social Support
design %>% 
  filter(is.na(talkprob_r2) == F) %>%
  group_by(talkprob_r2) %>%
  summarize(proportion = survey_mean(vartype = "ci"), 
            N = unweighted(n())) %>%
  mutate_if(is.numeric, percent) %>%
  arrange(proportion) %>%
  mutate(ypos = cumsum(proportion) - 0.5 * proportion) %>%
  mutate(label = paste0(talkprob_r2, "\n", round(proportion, 2), "%")) %>%
  print(n = 100) %>%
  ggplot(aes(x = "", y = proportion, fill = talkprob_r2)) +
  geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start = 0) +
  geom_text(aes(y = ypos, label = label), color = "white", size = 12, family = "Tahoma") +
  theme_void() +
  theme(legend.position = "none") + 
  scale_fill_manual(values = mycols)

ggsave("talkprob_use.eps", width = 13, height = 13, units = "in", dpi = 1800) 


### Prevalence of Suicidality
design %>%
  mutate(suic_id_atp_fct = factor(interaction(suic_id, suic_atp), 
                                  labels = c("No Suicidality", 
                                             "Suicidal Ideation", 
                                             "Suicidal Attempt"))) %>%
  group_by(suic_id_atp_fct) %>%
  summarize(proportion = survey_mean(vartype = "ci"),
            N = unweighted(n())) %>%
  drop_na() %>%
  mutate_if(is.numeric, percent) %>%
  arrange(proportion) %>%
  mutate(ypos = cumsum(proportion) - 0.5 * proportion) %>%
  mutate(label = paste0(suic_id_atp_fct, "\n", round(proportion, 2), "%")) %>%
  print(n = 100) %>%
  ggplot(aes(x = "", y = proportion, fill = suic_id_atp_fct)) +
  geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start = 0) +
  geom_text(aes(y = ypos, label = label), color = "white", size = 12, family = "Tahoma") +
  theme_void() +
  theme(legend.position = "none") + 
  scale_fill_manual(values = mycols)

ggsave("suicidality.eps", width = 13, height = 13, units = "in", dpi = 1800) 


list.tab <- list()

myvars <- c("med_po", "talkprob_r2", "suic_id_atp_fct")

for(i in seq_along(1:3)){
  list.tab[[i]]  <-  
  design %>% 
      group_by(design$variable[myvars[i]]) %>%
      summarise(proportion = survey_mean(vartype = "ci", na.rm = T))%>%
      mutate_if(is.numeric, percent) %>%
      drop_na() %>%
      rename(., var2 = myvars[i]) %>%
      print()
}

list.tab

hist.plot <- do.call(rbind, list.tab)

mylabs <- unique(hist.plot$var2)[-c(1, 4, 6)]

hist.plot %>%
  mutate(var1 = factor(c(rep(1, 3), rep(2, 2), rep(3, 3)), 
                       labels = c("Prescription Opioid Use", 
                                  "Social Support", 
                                  "Suicidality"))) %>%
  relocate(var1, .before = var2) %>%
  filter(!var2 %in% c("No PO Use", "Someone", "No Suicidality")) %>%
  droplevels() %>%
  mutate(labelpos = cumsum(proportion) - 0.5 * proportion) %>%
  ggplot(aes(x = var1, y = proportion, fill = var2, label = var2)) +
  geom_bar(position = "stack", stat = "identity", width = 0.6) +
  geom_text(position = "stack", vjust = +3, color = "white") +
  scale_x_discrete(name = "") + 
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, 2), name = "%") +
  theme_hc() +
  theme(text = element_text(family = "Avenir", size = 14), 
        ### separating the x axis title from the x axis
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)), 
        ### removing legend
        legend.position = "none", 
        axis.ticks.x = element_blank())

ggsave("descriptives.eps", width = 12, height = 8, units = "in", dpi = 1800)
  

### Prevalence of PY PO Frequency 
design %>% 
  group_by(suicid) %>%
  summarize(proportion = survey_mean(vartype = "ci"), 
            N = unweighted(n())) %>%
  mutate_if(is.numeric, proportion) %>%
  mutate(Cum_Prop = cumsum(proportion),
         Bottom = c(0, 94.9, 96.7, 98.1),
         N = N / 100, 
         label = paste0(po_freq, "\n", round(proportion, 2), "%"), 
         label_position = (Cum_Prop + Bottom) / 2) %>%
  print(n = 100) %>%
  ggplot(aes(ymax = Cum_Prop, ymin = Bottom, xmax = 4, xmin = 3, fill = po_freq)) +
  geom_rect() + 
  geom_text(x = 4.35, aes(y = label_position, label = label), size = 10, family = "Avenir") +
  scale_fill_brewer(palette = "Dark2", direction = - 1) +
  scale_color_brewer(palette = "Dark2", direction = - 1 ) +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) + 
  theme_void() +
  theme(legend.position = "none")

setwd("/Users/luissegura/Dropbox/Silvia/Teen Suicide and Opioids/Poster/CPDD2020") ### set the directory where the datasets are

ggsave("donut_2.eps", width = 13, height = 13, units = "in", dpi = 1800) 


### EXPLORING NSDUH YEARS 2015 - 2019

### Proportion of male/females
design %>%
  group_by(year_r, sex) %>%
  summarise(proportion = survey_mean(vartype = "ci"), 
            N = unweighted(n()))

### Prevalence over time of any PO use
design %>% 
  group_by(year_r, pnranyyr_r) %>%
  summarize(proportion = survey_mean(vartype = "ci"), 
            N = unweighted(n())) %>%
  print(n = 100)

### Prevalence over time of any PO misuse
design %>% 
  group_by(year_r, pnrnmyr_r) %>%
  summarize(proportion = survey_mean(vartype = "ci"), 
            N = unweighted(n())) %>%
  print(n = 100)

design %>% 
  group_by(year_r, talkprob_r) %>%
  summarize(proportion = survey_mean(vartype = "ci"), 
            N = unweighted(n())) %>%
  print(n = 100)

### Prevalence over time of PO Use

design %>% 
  group_by(year_r, med_po) %>%
  summarize(proportion = survey_mean(vartype = "ci"), 
            N = unweighted(n())) %>%
  print(n = 100)


### Prevalence over time of PY Heroin
design %>% 
  mutate(heryr_r = factor(heryr, labels = c("No Use", "PY Heroin Use"))) %>%
  group_by(year_r, heryr_r) %>%
  summarize(proportion = survey_mean(vartype = "ci"), 
            N = unweighted(n())) %>%
  print(n = 100)

### Prevalence over time of PY Opioids Use (PO + Heroin)
design %>% 
  group_by(year_r, po_freq) %>%
  summarize(proportion = survey_mean(vartype = "ci"), 
            N = unweighted(n())) %>%
  print(n = 100)

### Prevalence over time of PY PO frequency
design %>% 
  group_by(year_r, po_freq) %>%
  summarize(proportion = survey_mean(vartype = "ci"), 
            N = unweighted(n())) %>%
  print(n = 100)

### Prevalence over time of Social Support
design %>% 
  filter(!is.na(talkprob_r2)) %>%
  group_by(year_r, talkprob_r2) %>%
  summarize(proportion = survey_mean(vartype = "ci"), 
            N = unweighted(n())) %>%
  print(n = 100)

### Prevalence over time of Social Support by PO Use
design %>% 
  filter(!is.na(talkprob_r2)) %>%
  mutate(ssuport = ifelse(talkprob_r2 == "No one", 1, 0)) %>%
  group_by(year_r, med_po) %>%
  summarize(proportion = survey_mean(ssuport, vartype = "ci"), 
            N = unweighted(n())) %>%
  mutate_if(is.numeric, percent) %>%
  print(n = 100) %>%
  ggplot(aes(x = as.integer(year_r), y = proportion, ymin = proportion_low, ymax = proportion_upp, 
             color = med_po, fill = med_po)) +
  geom_line() +
  geom_ribbon(alpha = 0.2) +
  scale_x_continuous(labels = c("2015", " 2016", "2017", " 2018", "2019")) +
  ylab("%") + 
  xlab("") +
  scale_y_continuous(breaks = seq(1, 30, 4)) + 
  expand_limits(y = c(0, 30)) + 
  facet_wrap(~ med_po)
  

### Prevalence over time of Social Support by NO PO Use
design %>% 
  filter(!is.na(talkprob_r2) & med_po == "No PO Use") %>%
  group_by(year_r, talkprob_r2) %>%
  summarize(proportion = survey_mean(vartype = "ci"), 
            N = unweighted(n())) %>%
  print(n = 100)

### Prevalence over time of Social Support by PO Medical Use
design %>% 
  filter(!is.na(talkprob_r2) & med_po == "PY Medical Use Only") %>%
  group_by(year_r, talkprob_r2) %>%
  summarize(proportion = survey_mean(vartype = "ci"), 
            N = unweighted(n())) %>%
  print(n = 100)

### Prevalence over time of Social Support by PO Medical Use
design %>% 
  filter(!is.na(talkprob_r2) & med_po == "PY Any Non-Medical Use") %>%
  group_by(year_r, talkprob_r2) %>%
  summarize(proportion = survey_mean(vartype = "ci"), 
            N = unweighted(n())) %>%
  print(n = 100)

### Prevalence of PY Suicide Ideation
design %>% 
  # group_by(year_r) %>%
  summarize(proportion = survey_mean(suic_id, na.rm = T, 
                                     vartype = "ci", 
                                     proportion = T, 
                                     prop_method = "mean"), 
            N = unweighted(n())) %>%
  print(n = 100)


### Prevalence of PY Suicide Attempts
design %>% 
  # group_by(year_r) %>%
  summarize(proportion = survey_mean(suic_atp, na.rm = T, 
                                     vartype = "ci", 
                                     proportion = T, 
                                     prop_method = "mean"), 
            N = unweighted(n())) %>%
  print(n = 100)


### Prevalence over time of PY Suicide Ideation & Attempts
design %>% 
  group_by(year_r) %>%
  summarize(proportion = survey_mean(suic_id_atp, na.rm = T, 
                                     vartype = "ci", 
                                     proportion = T, 
                                     prop_method = "mean"), 
            N = unweighted(n())) %>%
  print(n = 100) 

### Proportion of social support (yes vs no) over time

design %>%
  group_by(year_r, talkprob_r) %>%
  summarise(proportion = survey_mean(vartype = "ci"), 
            N = unweighted(n()))

### GRAPH
g.df <- design %>%
  mutate(suic = "suic_atp") %>%
  group_by(suic, year) %>%
  summarize(pr = survey_mean(suic_atp, na.rm = T, 
                             vartype = "ci", 
                             proportion = T, 
                             prop_method = "mean"))

g.df

g.df2 <- design %>% 
  mutate(suic = "suic_id") %>%
  group_by(suic, year) %>%
  summarize(pr = survey_mean(suic_id, na.rm = T, 
                             vartype = "ci", 
                             proportion = T, 
                             prop_method = "mean"))

g.df <- rbind(g.df, g.df2)


g.df %>% 
  mutate(suic_r = factor(suic), 
         pr = pr * 100, 
         pr_low = pr_low * 100, 
         pr_upp = pr_upp * 100) %>%
  ggplot(aes(x = year, y = pr, ymin = pr_low, ymax = pr_upp, linetype = suic_r,
             fill = suic_r, shape = suic_r, color = suic_r)) +
  geom_smooth(method = "loess") +
  geom_point(stroke = 1) 



### Prevalence of SI/SA by Any PO Use
design %>%
  group_by(year_r, sex_r, med_po) %>%
  summarise(ideation = survey_mean(suic_id, na.rm = T), 
            attempt = survey_mean(suic_atp, na.rm = T)) %>%
  mutate_if(is.numeric, proportion) %>%
  gather(key = outcome, value = rate, ideation, attempt) %>%
  gather(key = var, value = ideation_se, attempt_se)  %>%
  mutate(outcome = factor(outcome, levels = c("ideation", "attempt"), 
                          labels = c("Suicidal Ideation", "Suicidal Attempt"))) %>%
  ggplot(aes(x = as.numeric(year_r), y = rate, ymin = rate - ideation_se, ymax = rate + ideation_se,
             linetype = med_po, 
             shape = med_po, color = med_po, fill = med_po)) + 
  geom_smooth(method = "loess") + 
  geom_point(size = 3.5) +
  geom_ribbon(alpha = 0.2, colour = NA, size = 2.8) +
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(), 
        panel.background=element_rect(fill="white", 
                                      colour="black"), 
        legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(family = "Helvetica", size = 16, colour = "black"), 
        axis.text = element_text(colour = "black"), 
        # axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.spacing.x = unit(0.5, "cm"), 
        legend.text = element_text(size = 18), 
        strip.background = element_rect(fill = "white"), 
        strip.text = element_text(colour = "black", size = 18)) + 
  ylab("Prevalence (%)") + 
  xlab("") +
  scale_y_continuous(breaks = seq(from = 0, to = 60, by = 10)) + 
  scale_x_continuous(breaks = c(1:4), 
                     labels = c("2015", "2016", "2017", "2018")) +
  expand_limits(y = c(0, 60)) + 
  facet_grid(sex_r~ outcome, scales = "free")


setwd("/Users/luissegura/Dropbox/Silvia/Teen Suicide and Opioids/Poster/CPDD2020") ### set the directory where the datasets are

ggsave("figure_1.pdf", width = 13 ,height = 9, units = "in", dpi = 1800) 


### Prevalence of SI/SA by Frequency of PO

design %>%
  group_by(year_r, sex_r, po_freq) %>%
  summarise(ideation = survey_mean(suic_id, na.rm = T), 
            attempt = survey_mean(suic_atp, na.rm = T)) %>%
  mutate_if(is.numeric, proportion) %>%
  gather(key = outcome, value = rate, ideation, attempt) %>%
  gather(key = var, value = ideation_se, attempt_se) %>%
  mutate(outcome = factor(outcome, levels = c("ideation", "attempt"), labels = c("Suicidal Ideation", "Suicidal Attempt"))) %>%
  print(n = Inf) %>%
  ggplot(aes(x = as.numeric(year_r), y = rate, ymin = rate - ideation_se, ymax = rate + ideation_se,
             linetype = po_freq, shape = po_freq, color = po_freq, fill = po_freq)) +  
  geom_smooth(method = "loess") + 
  geom_point(size = 3.5) +
  geom_ribbon(alpha = 0.2, colour = NA, size = 2.8) +
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(), 
        panel.background=element_rect(fill="white", 
                                      colour="black"), 
        legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(family = "Helvetica", size = 16, colour = "black"), 
        axis.text = element_text(colour = "black"), 
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.spacing.x = unit(0.5, "cm"), 
        legend.text = element_text(size = 18), 
        strip.background = element_rect(fill = "white"), 
        strip.text = element_text(colour = "black", size = 18)) + 
  ylab("Prevalence (%)") + 
  xlab("") +
  scale_y_continuous(breaks = seq(from = 0, to = 60, by = 10)) + 
  scale_x_continuous(breaks = c(1:4), 
                     labels = c("2015", "2016", "2017", "2018")) +
  scale_fill_discrete(labels = c("No Use", "Former PO Misuse", "Recent-onset PO Misuse", "Persistent PO Misuse")) +
  scale_color_discrete(labels = c("No Use", "Former PO Misuse", "Recent-onset PO Misuse", "Persistent PO Misuse")) +
  scale_shape_discrete(labels = c("No Use", "Former PO Misuse", "Recent-onset PO Misuse", "Persistent PO Misuse")) +
  scale_linetype_discrete(labels = c("No Use", "Former PO Misuse", "Recent-onset PO Misuse", "Persistent PO Misuse")) +
  expand_limits(y = c(0, 60)) + 
  facet_grid(sex_r ~ outcome, scales = "free")



ggsave("figure_2.pdf", width = 13 , height = 9, units = "in", dpi = 1800) 

### Prevalence of SI/SA by Opioid Use


g.df <- design %>%
  group_by(year, opinmyr_r) %>%
  summarise(pr_id = survey_mean(suic_id, na.rm = T, 
                                vartype = "ci", 
                                proportion = T, 
                                prop_method = "mean"), 
            pr_atp = survey_mean(suic_atp, na.rm = T, 
                                 vartype = "ci", 
                                 proportion = T, 
                                 prop_method = "mean")) %>%
  gather(., "suic", "pr", pr_id, pr_atp, pr_id_low, pr_atp_low, pr_id_upp, pr_atp_upp) %>%
  print(n = Inf)

g.df <- cbind(g.df[g.df$suic %in% c("pr_id", "pr_atp"), ], 
              g.df[g.df$suic %in% c("pr_id_low", "pr_atp_low"), -c(1:3)], 
              g.df[g.df$suic %in% c("pr_id_upp", "pr_atp_upp"), -c(1:3)])

names(g.df)[4:6] <- c("pr", "pr_low", "pr_upp")

g.df %>% 
  mutate(suic = factor(suic, levels = c("pr_id", "pr_atp"), labels = c("Suicidal Ideation", "Suicidal Attempt")), 
         pr = pr * 100, 
         pr_low = pr_low * 100, 
         pr_upp = pr_upp * 100) %>%
  ggplot(aes(x = year, y = pr, ymin = pr_low, ymax = pr_upp, linetype = opinmyr_r, 
             shape = opinmyr_r, color = "purple1", fill = opinmyr_r)) + 
  geom_smooth(method = "loess", color = "purple1") + 
  geom_point(color = "purple1") +
  geom_ribbon(alpha = 0.2, colour = NA, fill = "purple1") +
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(), 
        panel.background=element_rect(fill="white", 
                                      colour="black"), 
        legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(family = "Avenir", size = 16, colour = "black"), 
        axis.text = element_text(colour = "black"), 
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.spacing.x = unit(0.5, "cm"), 
        legend.text = element_text(size = 18)) + 
  ylab("Prevalence (%)") + 
  xlab("") +
  scale_y_continuous(breaks = seq(from = 0, to = 50, by = 5)) + 
  expand_limits(y = c(0, 50)) + 
  facet_grid(. ~ suic) 

ggsave("figure_3.jpeg", width=11 ,height=7, units= "in", dpi=1200) 


