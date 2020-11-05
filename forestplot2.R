library(tidyverse)
library(ggplot2)
library(ggthemes)
library(wesanderson)

######################################
#                                    #
#   Effects for Suicidal Ideation    #
#                                    #
######################################

### NOTE: First run the script main_analysis_v2.R
### first create the table we are going to plot from the backtransformed models using
### the risk differences tables for the interaction effects and remove the columns SE and df
### rename columns med_po, talkprob_r and rate
plot <- as.tibble(ptable)[, -c(3:4, 7:8)]

plot <- plot %>%
  mutate(contrast = factor(contrast, labels = c("No PO Use, Some Social Support", "PY Medical Use Only, Some Social Support", 
                      "PY Any Non-Medical Use, Some Social Support", "No PO Use, No Social Support", 
                      "PY Medical Use Only, No Social Support", "PY Any Non-Medical Use, No Social Support")))

### merge both tables
plot.final <- rbind(plot, rd_table, ic_table)

plot.final <- plot.final %>%
                  mutate(outcome = "Suicidal Ideation")

plot.final

######################################
#                                    #
#   Effects for Suicidal Attempts    #
#                                    #
######################################

### NOTE: First run the script main_analysis_v3.R
### first create the table we are going to plot from the backtransformed models using
### the risk differences tables for the interaction effects and remove the columns SE and df
### rename columns med_po, talkprob_r and rate
plot <- as.tibble(ptable)[, -c(3:4, 7:8)]

plot <- plot %>%
  mutate(contrast = factor(contrast, labels = c("No PO Use, Some Social Support", "PY Medical Use Only, Some Social Support", 
                                                "PY Any Non-Medical Use, Some Social Support", "No PO Use, No Social Support", 
                                                "PY Medical Use Only, No Social Support", "PY Any Non-Medical Use, No Social Support")))

### merge both tables
plot.final.2 <- rbind(plot, rd_table, ic_table)


plot.final.2 <- plot.final.2 %>%
                      mutate(outcome = "Suicidal Attempt")

plot.final.2

### merge both suicidal ideation and attemp

plot.final.r <- rbind(plot.final, plot.final.2)
  
plot.final.r

nonmed.names <- str_subset(plot.final.r$contrast, "Non-Medical")
med.names <- str_subset(plot.final.r$contrast, "PY Medical Use")
nonmed.names
med.names

plot.final.r

plot.final.medpo <- plot.final.r %>%
  filter(!contrast %in% nonmed.names) %>%
  droplevels() %>%
  ### create an indicator for color
  mutate(color = factor(rep(1:7, times = 2)), 
         outcome_r = factor(ifelse(outcome == "Suicidal Ideation", 0, 1), labels = unique(outcome))) %>%
  arrange(color) 
  
levels(plot.final.medpo$contrast) <- c("No Use, Some Support (p00)",
                                       "Medical Use Only,\n Some Support (p10)",
                                       "No Use, No Support (p01)",
                                       "Medical Use Only,\n No Support (p11)",
                                       "RD p01 - p00",
                                       "RD p11 - p10",
                                       "IC (p11 - p00) - (p01 - p00)")

plot.final.medpo

plot.final.nomedpo <- plot.final.r %>%
  filter(!contrast %in% med.names) %>%
  droplevels() %>%
  ### create an indicator for color
  mutate(color = factor(rep(1:7, times = 2)), 
         outcome_r = factor(ifelse(outcome == "Suicidal Ideation", 0, 1), labels = unique(outcome))) %>%
  arrange(color) 

levels(plot.final.nomedpo$contrast) <- c("No Use, Some Support (p00)",
                                       "Any Non-Medical Use Only,\n Some Support (p10)",
                                       "No Use, No Support (p01)",
                                       "Any Non-Medical Use Only,\n No Support (p11)",
                                       "RD p01 - p00",
                                       "RD p11 - p10",
                                       "IC (p11 - p00) - (p01 - p00)")

plot.final.nomedpo

### forestplot exposure PY Medical Use
plot.final.nomedpo %>%
  ### definiting the x, y axis of the plot and CIs
  ggplot(aes(y = contrast, x = estimate, xmin = asymp.LCL, xmax = asymp.UCL, color = color)) +
  ### size of points for effect estimates and shapes
  geom_point(size = 5, shape = "diamond") +
  ### error bars for CI and height of whisks
  geom_errorbarh(height = 0.3) + 
  ### defining how many and length of ticks in x axis
  scale_x_continuous(limits = c(-0.4, 0.8), breaks= seq(-0.4, 0.8, 0.1), name = "Risk")  +
  ### defining color palette
  scale_color_manual(values = c("#bc5090", "#726dc3","#ad67c0", "#de5ead", "#ff5c8f", "#ff6a69", "#ff8540",
                                "#bc5090", "#726dc3","#ad67c0", "#de5ead", "#ff5c8f", "#ff6a69", "#ff8540")) +
  ### drawing reference line for the null
geom_vline(xintercept = 0, color = "black", linetype = "solid", alpha = 0.5) +
  ### choosing a theme for the plot
  theme_calc() +
  ### choosing font, font size
  theme(text = element_text(family = "Avenir", size = 14), 
        ### separating the x axis title from the x axis
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        ### removing legend
        legend.position = "none", 
        axis.title.y = element_blank(), 
        strip.background = element_blank()) +
  facet_grid(cols = vars(outcome_r))


### saving the plot as .eps
ggsave("interaction_suicatp.jpg", width = 18, height = 8, units = "in", dpi = 1800)
ggsave("interaction_suicatp.eps", width = 18, height = 8, units = "in", dpi = 1800)

