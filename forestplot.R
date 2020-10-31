library(tidyverse)
library(ggplot2)
library(ggthemes)
library(wesanderson)

plot <- as.tibble(m11_reg)[, c("med_po", "talkprob_r", "rate", "asymp.LCL", "asymp.UCL")]

plot <- plot %>%
  rename(var1 = med_po,
         var2 = talkprob_r, 
         estimate = rate)

plot2 <- as.tibble(confint(contrast(m11_reg, interaction = "trt.vs.ctrl"))) %>%
          select(!SE:df)

plot2 <- plot2 %>%
  rename(var1 = med_po_trt.vs.ctrl, 
         var2 = talkprob_r_trt.vs.ctrl)

plot.final <- rbind(plot, plot2)


plot.final %>%
  mutate(color = factor(c(rep(1:3, each = 1, len = 6), rep(4, each = 1, len = 2)))) %>%
  ggplot(aes(y = interaction(var1, var2), x = estimate, xmin = asymp.LCL, xmax = asymp.UCL, color = color)) +
  geom_point(size = 4, shape = "diamond") +
  geom_errorbarh(height = 0.3) + 
  scale_x_continuous(limits = c(-0.1, 0.8), breaks= seq(-0.1, 0.8, 0.05), name = "Risk Difference")  +
  scale_color_manual(values = wes_palette("Darjeeling1", n = 4)) +
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
  geom_vline(xintercept = 0, color = "black", linetype = "solid", alpha = 0.5) +
  theme_calc() +
  theme(text = element_text(family = "Avenir", size = 14), 
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        legend.position = "none")
 
ggsave()
