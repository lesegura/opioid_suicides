library(tidyverse)
library(ggplot2)
library(ggthemes)
library(wesanderson)

### first create the table we are going to plot from the backtransformed models using
### the risk differences tables for the interaction effects and remove the columns SE and df
### rename columns med_po, talkprob_r and rate
plot <- as.tibble(m11_reg)[, c("med_po", "talkprob_r", "rate", "asymp.LCL", "asymp.UCL")]

plot <- plot %>%
  rename(var1 = med_po,
         var2 = talkprob_r, 
         estimate = rate)

### we are going to use also the table for the IC removing the columns of SE and df
### also renaming some columns to join it to the table above.
plot2 <- as.tibble(confint(contrast(m11_reg, interaction = "trt.vs.ctrl"))) %>%
          select(!SE:df)

plot2 <- plot2 %>%
  rename(var1 = med_po_trt.vs.ctrl, 
         var2 = talkprob_r_trt.vs.ctrl)

### merge both tables
plot.final <- rbind(plot, plot2)

### forestplot
plot.final %>%
  ### create an indicator for color 1 for baseline risk, 2 for effect of E not EM, 3 for EM not E, and 4 for effect of E and EM, and 5 for interaction contrasts
  mutate(color = factor(c(1, rep(2, each = 1, len = 2), 3, rep(4, each = 1, len = 2), rep(5, each = 1, len = 2)))) %>%
  ### definiting the x, y axis of the plot and CIs
  ggplot(aes(y = interaction(var1, var2), x = estimate, xmin = asymp.LCL, xmax = asymp.UCL, color = color)) +
  ### size of points for effect estimates and shapes
  geom_point(size = 4, shape = "diamond") +
  ### error bars for CI and height of whisks
  geom_errorbarh(height = 0.3) + 
  ### defining how many and length of ticks in x axis
  scale_x_continuous(limits = c(-0.1, 0.8), breaks= seq(-0.1, 0.8, 0.05), name = "Risk Difference")  +
  ### defining color palette
  scale_color_manual(values = wes_palette("Darjeeling1", n = 5)) +
  ### renaming the y axis ticks
  scale_y_discrete(name = "", 
                   breaks = interaction(plot.final$var1, plot.final$var2), 
                   labels = c("No PO Use, No Social Capita (p00)", 
                              "PY Medical Use Only, No Social Capita (p10)", 
                              "PY Any Non-Medical Use, No Social Capita (p10)", 
                              "No PO Use, Yes Social Capita (p01)", 
                              "PY Medical Use Only, Yes Social Capita (p11)", 
                              "PY Any Non-Medical Use, Yes Social Capita (p11)", 
                              "IC Medical Use Only: Yes vs No (p11 - p00) vs \nSocial Capita: Yes vs No (p01 - p00)     ", 
                              "IC Any Non-Medical Use: Yes vs No (p11 - p00) vs \nSocial Capita: Yes vs No (p01 - p00)     ")) +
  ### drawing reference line for the null
  geom_vline(xintercept = 0, color = "black", linetype = "solid", alpha = 0.5) +
  ### choosing a theme for the plot
  theme_calc() +
  ### choosing font, font size
  theme(text = element_text(family = "Avenir", size = 14), 
        ### separating the x axis title from the x axis
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        ### removing legend
        legend.position = "none")

### saving the plot as .eps
ggsave("forestplot_1.eps", width = 13 , height = 8, units = "in", dpi = 1800)
