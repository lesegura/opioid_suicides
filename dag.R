library(ggdag)
theme_set(theme_dag())

suidicality.dag <- dagify(suicidality ~ PO_use + NO_social_support + demographics + other_substance_use + mental_health, 
                          PO_use ~ demographics + other_substance_use + mental_health, 
                          mental_health_treatment ~ mental_health, 
                          latent = "mental_health", 
                          exposure = c("PO_use", "NO_social_support"), 
                          outcome = "suicidality", 
                          labels = c("suicidality" = "Suicidality", 
                                     "PO_use" = "PO Use", 
                                     "demographics" = "Female,\n Race,\n Temporal Trends,\n County Type", 
                                     "other_substance_use" = "Other Substance Use", 
                                     "mental_health" = "Mental Disorders", 
                                     "NO_social_support" = "No Social Support", 
                                     "mental_health_treatment" = "Mental Disorders Treatment"))

ggdag(suidicality.dag, text = FALSE, use_labels = "label")

ggsave("suic_dag.jpeg", width = 18, height = 8, units = "in", dpi = 1800)

