library(tidyverse)
library(ggplot2)
library(ggthemes)
library(wesanderson)

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

### forestplot
plot.final %>%
  ### create an indicator for color 1 for baseline risk, 2 for effect of E not EM, 3 for EM not E, and 4 for effect of E and EM, and 5 for interaction contrasts
  # mutate(color = factor(c(1, rep(2, each = 1, len = 2), 3, rep(4, each = 1, len = 2), rep(5, each = 1, len = 2), rep(6, each = 1, len = 2)))) %>%
  ### definiting the x, y axis of the plot and CIs
  ggplot(aes(y = contrast, x = estimate, xmin = asymp.LCL, xmax = asymp.UCL, color = contrast)) +
  ### size of points for effect estimates and shapes
  geom_point(size = 4, shape = "diamond") +
  ### error bars for CI and height of whisks
  geom_errorbarh(height = 0.3) + 
  ### defining how many and length of ticks in x axis
  scale_x_continuous(limits = c(-0.4, 0.8), breaks= seq(-0.4, 0.8, 0.05), name = "Risk")  +
  ### defining color palette
  scale_color_manual(values = c("#999999", "#E69F00", "#E69F00", "#56B4E9", "#009E73", "#009E73",
                                "#0072B2", "#0072B2", "#D55E00", "#D55E00")) +
  ### renaming the y axis ticks
  # scale_y_discrete(name = "", 
  #                  breaks = interaction(plot.final$var1, plot.final$var2), 
  #                  labels = c("No PO Use, No Social Capita (p00)", 
  #                             "PY Medical Use Only, No Social Capita (p10)", 
  #                             "PY Any Non-Medical Use, No Social Capita (p10)", 
  #                             "No PO Use, Yes Social Capita (p01)", 
  #                             "PY Medical Use Only, Yes Social Capita (p11)", 
  #                             "PY Any Non-Medical Use, Yes Social Capita (p11)", 
  #                             "IC Medical Use Only: Yes vs No (p11 - p00) vs \nSocial Capita: Yes vs No (p01 - p00)     ", 
  #                             "IC Any Non-Medical Use: Yes vs No (p11 - p00) vs \nSocial Capita: Yes vs No (p01 - p00)     ")) +
  # ### drawing reference line for the null
  geom_vline(xintercept = 0, color = "black", linetype = "solid", alpha = 0.5) +
  ### choosing a theme for the plot
  theme_calc() +
  ### choosing font, font size
  theme(text = element_text(family = "Avenir", size = 14), 
        ### separating the x axis title from the x axis
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        ### removing legend
        legend.position = "none", 
        axis.title.y = element_blank())


### saving the plot as .eps
ggsave("forestplot_1.jpeg", width = 18, height = 8, units = "in", dpi = 1800)
