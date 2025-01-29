library(tidyverse)
library(readxl)
library(viridis)
library(car)
library(ggpubr)
library(forcats)
library(emmeans)
library(MASS)

# read in data 
df <- read_excel("data/Consolidated_fly_food_data.xlsx")

# calculate mean and stdev of triplicates
df_avg <- df %>% 
  group_by(Experiment, Food, Setting, Food_Base) %>% 
  summarize(triplicate_mean = mean(Flies), triplicate_sd = sd(Flies), .groups="drop") %>% 
  mutate(Food = str_replace(Food, "Banana_and_Yeast", "Banana & yeast"),
         Food = str_replace(Food, "Homemade_banana_extract", "Banana extract"),
         Food = str_replace(Food, "Plain_cornmeal_food", "Standard cornmeal food"),
         Food = str_replace(Food, "Artificial_banana_extract", "Isoamyl acetate"),
         Food = str_replace(Food, "Ethyl_Isovalerate", "Ethyl isovalerate"),
         Food = str_replace(Food, "Marula_fruit_oil", "Marula fruit oil"),
         Food = str_replace(Food, "Plain_potato_food", "Standard potato food"))

# calculate fly #s as fraction of total flies in expt
df <- df %>%
  group_by(Experiment, Setting, Food_Base, Replicate) %>% 
  mutate(total_flies_in_expt = sum(Flies), 
         fraction_flies_in_expt = Flies / total_flies_in_expt) %>% 
  mutate(Food = str_replace(Food, "Banana_and_Yeast", "Banana & yeast"),
         Food = str_replace(Food, "Homemade_banana_extract", "Banana extract"),
         Food = str_replace(Food, "Plain_cornmeal_food", "Standard cornmeal food"),
         Food = str_replace(Food, "Artificial_banana_extract", "Isoamyl acetate"),
         Food = str_replace(Food, "Ethyl_Isovalerate", "Ethyl isovalerate"),
         Food = str_replace(Food, "Marula_fruit_oil", "Marula fruit oil"),
         Food = str_replace(Food, "Plain_potato_food", "Standard potato food"),
         Food = str_replace(Food, "Isoamyl_acetate", "Isoamyl acetate"),
         Food = str_replace(Food, "0.45_ethyl", "0.45% Ethyl isovalerate"),
         Food = str_replace(Food, "0.9_ethyl", "0.9% Ethyl isovalerate"),
         Food = str_replace(Food, "1.8_ethyl", "1.8% Ethyl isovalerate"),
         Food = str_replace(Food, "3.6_ethyl", "3.6% Ethyl isovalerate"),
         Food = str_replace(Food, "7.2_ethyl", "7.2% Ethyl isovalerate"),
         Food = factor(Food)) %>% 
  mutate(Food = fct_relevel(Food, c("Standard cornmeal food", "Standard potato food",
                                    "Banana & yeast", "Banana extract", "Isoamyl acetate",
                                    "Ethyl isovalerate", "Marula fruit oil"))) %>% 
  ungroup()

#stats
exp_1 <- df %>% 
  filter(Experiment == 1) %>% 
  dplyr::select(Replicate, Food, Flies) %>% 
  mutate(Food = factor(Food))

# The poisson model did not pass a goodness of fit test, moved to negative binomial
#exp_1_glm <- glm(Flies ~ Food, data = exp_1, family = "quasipoisson")
#summary(exp_1_glm)
#exp1_stats <- summary(pairs(emmeans(exp_1_glm, ~ Food)))$p.value

#with(exp_1_glm, cbind(res.deviance = deviance, df = df.residual,
#               p = pchisq(deviance, df.residual, lower.tail=FALSE)))

glm1_nb <- glm.nb(Flies ~ Food, data = exp_1, link = "log")
summary(glm1_nb)
with(glm1_nb, cbind(res.deviance = deviance, df = df.residual,
                      p = pchisq(deviance, df.residual, lower.tail=FALSE)))
exp1_stats <- summary(pairs(emmeans(glm1_nb, ~ Food)))$p.value

res1 <- tibble(group1 = c("Standard cornmeal food", "Standard cornmeal food", "Banana & yeast"),
               group2 = c("Banana & yeast", "Banana extract", "Banana extract"),
               p_val = case_when(exp1_stats <= 0.0001 ~ "****",
                                 between(exp1_stats, 0.0001, 0.001) ~ "***",
                                 between(exp1_stats, 0.001, 0.01) ~ "**",
                                 between(exp1_stats, 0.01, 0.05) ~ "*",
                                 exp1_stats > 0.05 ~ "ns"))

exp_2 <- df %>% 
  filter(Experiment == 2) %>% 
  dplyr::select(Replicate, Food, Flies) %>% 
  mutate(Food = factor(Food))

# The poisson model did not pass a goodness of fit test, moved to negative binomial
#exp_2_glm <- glm(Flies ~ Food, data = exp_2, family = "poisson")
#summary(exp_2_glm)
#exp2_stats <- summary(pairs(emmeans(exp_2_glm, ~ Food)))$p.value

#with(exp_2_glm, cbind(res.deviance = deviance, df = df.residual,
#                      p = pchisq(deviance, df.residual, lower.tail=FALSE)))

glm2_nb <- glm.nb(Flies ~ Food, data = exp_2, link = "log")
summary(glm2_nb)
with(glm2_nb, cbind(res.deviance = deviance, df = df.residual,
                    p = pchisq(deviance, df.residual, lower.tail=FALSE)))
exp2_stats <- summary(pairs(emmeans(glm2_nb, ~ Food)))$p.value

res2 <- tibble(group1 = c("Standard cornmeal food", "Standard cornmeal food", "Banana & yeast"),
               group2 = c("Banana & yeast", "Banana extract", "Banana extract"),
               p_val = case_when(exp2_stats <= 0.0001 ~ "****",
                                 between(exp2_stats, 0.0001, 0.001) ~ "***",
                                 between(exp2_stats, 0.001, 0.01) ~ "**",
                                 between(exp2_stats, 0.01, 0.05) ~ "*",
                                 exp2_stats > 0.05 ~ "ns"))

exp_3 <- df %>% 
  filter(Experiment == 3) %>% 
  dplyr::select(Replicate, Food, Flies) %>% 
  mutate(Food = factor(Food))

# The poisson model did not pass a goodness of fit test, moved to negative binomial
#exp_3_glm <- glm(Flies ~ Food, data = exp_3, family = "poisson")
#summary(exp_3_glm)
#exp3_stats <- summary(pairs(emmeans(exp_3_glm, ~ Food)))$p.value#

#with(exp_3_glm, cbind(res.deviance = deviance, df = df.residual,
#                      p = pchisq(deviance, df.residual, lower.tail=FALSE)))

glm3_nb <- glm.nb(Flies ~ Food, data = exp_3, link = "log")
summary(glm3_nb)
with(glm3_nb, cbind(res.deviance = deviance, df = df.residual,
                    p = pchisq(deviance, df.residual, lower.tail=FALSE)))
exp3_stats <- summary(pairs(emmeans(glm3_nb, ~ Food)))$p.value

res3 <- tibble(group1 = c("Standard cornmeal food", "Standard cornmeal food", "Standard cornmeal food", "Standard cornmeal food",
                          "Banana extract", "Banana extract", "Banana extract",
                          "Isoamyl acetate", "Isoamyl acetate",
                          "Ethyl isovalerate"),
               group2 = c("Banana extract", "Isoamyl acetate", "Ethyl isovalerate", "Marula fruit oil",
                          "Isoamyl acetate", "Ethyl isovalerate", "Marula fruit oil", 
                          "Ethyl isovalerate", "Marula fruit oil", 
                          "Marula fruit oil"),
               p = case_when(exp3_stats <= 0.0001 ~ "****",
                                 between(exp3_stats, 0.0001, 0.001) ~ "***",
                                 between(exp3_stats, 0.001, 0.01) ~ "**",
                                 between(exp3_stats, 0.01, 0.05) ~ "*",
                                 exp3_stats > 0.05 ~ "ns"))

exp_4 <- df %>% 
  filter(Experiment == 4) %>% 
  dplyr::select(Replicate, Food, Flies) %>% 
  mutate(Food = factor(Food))

#exp_4_glm <- glm(Flies ~ Food, data = exp_4, family = "poisson")
#summary(exp_4_glm)
#exp4_stats <- summary(pairs(emmeans(exp_4_glm, ~ Food)))$p.value

#with(exp_4_glm, cbind(res.deviance = deviance, df = df.residual,
#                      p = pchisq(deviance, df.residual, lower.tail=FALSE)))

glm4_nb <- glm.nb(Flies ~ Food, data = exp_4, link = "log")
summary(glm4_nb)
with(glm4_nb, cbind(res.deviance = deviance, df = df.residual,
                    p = pchisq(deviance, df.residual, lower.tail=FALSE)))
exp4_stats <- summary(pairs(emmeans(glm4_nb, ~ Food)))$p.value

res4 <- tibble(group1 = c("Standard cornmeal food", "Standard cornmeal food", "Standard cornmeal food", "Standard cornmeal food",
                          "Banana extract", "Banana extract", "Banana extract",
                          "Isoamyl acetate", "Isoamyl acetate",
                          "Ethyl isovalerate"),
               group2 = c("Banana extract", "Isoamyl acetate", "Ethyl isovalerate", "Marula fruit oil",
                          "Isoamyl acetate", "Ethyl isovalerate", "Marula fruit oil", 
                          "Ethyl isovalerate", "Marula fruit oil", 
                          "Marula fruit oil"),
               p = case_when(exp4_stats <= 0.0001 ~ "****",
                                 between(exp4_stats, 0.0001, 0.001) ~ "***",
                                 between(exp4_stats, 0.001, 0.01) ~ "**",
                                 between(exp4_stats, 0.01, 0.05) ~ "*",
                                 exp4_stats > 0.05 ~ "ns"))

exp_5 <- df %>% 
  filter(Experiment == 5) %>% 
  dplyr::select(Replicate, Food, Flies) %>% 
  mutate(Food = factor(Food),
         pseudo = Flies + 1)

#exp_5_glm <- glm(Flies ~ Food, data = exp_5, family = "poisson")
#summary(exp_5_glm)
#exp5_stats <- summary(pairs(emmeans(exp_5_glm, ~ Food)))$p.value

#with(exp_5_glm, cbind(res.deviance = deviance, df = df.residual,
#                      p = pchisq(deviance, df.residual, lower.tail=FALSE)))

glm5_nb <- glm.nb(pseudo ~ Food, data = exp_5, link = "log")
summary(glm5_nb)
with(glm5_nb, cbind(res.deviance = deviance, df = df.residual,
                    p = pchisq(deviance, df.residual, lower.tail=FALSE)))
exp5_stats <- summary(pairs(emmeans(glm5_nb, ~ Food)))$p.value

res5 <- tibble(group1 = c("Standard potato food", "Standard potato food", "Standard potato food", "Standard potato food",
                          "Banana extract", "Banana extract", "Banana extract",
                          "Isoamyl acetate", "Isoamyl acetate",
                          "Ethyl isovalerate"),
               group2 = c("Banana extract", "Isoamyl acetate", "Ethyl isovalerate", "Marula fruit oil",
                          "Isoamyl acetate", "Ethyl isovalerate", "Marula fruit oil", 
                          "Ethyl isovalerate", "Marula fruit oil", 
                          "Marula fruit oil"),
               p = case_when(exp5_stats <= 0.0001 ~ "****",
                                 between(exp5_stats, 0.0001, 0.001) ~ "***",
                                 between(exp5_stats, 0.001, 0.01) ~ "**",
                                 between(exp5_stats, 0.01, 0.05) ~ "*",
                                 exp5_stats > 0.05 ~ "ns"))

exp_6 <- df %>% 
  filter(Experiment == 6) %>% 
  dplyr::select(Replicate, Food, Flies) %>% 
  mutate(Food = factor(Food))

#exp_6_glm <- glm(Flies ~ Food, data = exp_6, family = "poisson")
#summary(exp_6_glm)
#exp6_stats <- summary(pairs(emmeans(exp_6_glm, ~ Food)))$p.value

#with(exp_6_glm, cbind(res.deviance = deviance, df = df.residual,
#                      p = pchisq(deviance, df.residual, lower.tail=FALSE)))

glm6_nb <- glm.nb(Flies ~ Food, data = exp_6, link = "log")
summary(glm6_nb)
with(glm6_nb, cbind(res.deviance = deviance, df = df.residual,
                    p = pchisq(deviance, df.residual, lower.tail=FALSE)))
exp6_stats <- summary(pairs(emmeans(glm6_nb, ~ Food)))$p.value


res6 <- tibble(group1 = c("Standard potato food", "Standard potato food", "Standard potato food", "Standard potato food",
                          "Banana extract", "Banana extract", "Banana extract",
                          "Isoamyl acetate", "Isoamyl acetate",
                          "Ethyl isovalerate"),
               group2 = c("Banana extract", "Isoamyl acetate", "Ethyl isovalerate", "Marula fruit oil",
                          "Isoamyl acetate", "Ethyl isovalerate", "Marula fruit oil", 
                          "Ethyl isovalerate", "Marula fruit oil", 
                          "Marula fruit oil"),
               p = case_when(exp6_stats <= 0.0001 ~ "****",
                                 between(exp6_stats, 0.0001, 0.001) ~ "***",
                                 between(exp6_stats, 0.001, 0.01) ~ "**",
                                 between(exp6_stats, 0.01, 0.05) ~ "*",
                                 exp6_stats > 0.05 ~ "ns"))

exp_7 <- df %>% 
  filter(Experiment == 7) %>% 
  dplyr::select(Replicate, Food, Flies) %>% 
  mutate(Food = factor(Food))

#exp_7_glm <- glm(Flies ~ Food, data = exp_7, family = "poisson")
#summary(exp_7_glm)
#exp7_stats <- summary(pairs(emmeans(exp_7_glm, ~ Food)))$p.value

#with(exp_7_glm, cbind(res.deviance = deviance, df = df.residual,
#                      p = pchisq(deviance, df.residual, lower.tail=FALSE)))

glm7_nb <- glm.nb(Flies ~ Food, data = exp_7, link = "log")
summary(glm7_nb)
with(glm7_nb, cbind(res.deviance = deviance, df = df.residual,
                    p = pchisq(deviance, df.residual, lower.tail=FALSE)))
exp7_stats <- summary(pairs(emmeans(glm7_nb, ~ Food)))$p.value

res7 <- tibble(group1 = c("Standard cornmeal food", "Standard cornmeal food", "Standard cornmeal food", "Standard cornmeal food",
                          "Standard cornmeal food", "Standard cornmeal food",
                          "Isoamyl acetate", "Isoamyl acetate", "Isoamyl acetate", "Isoamyl acetate",
                          "Isoamyl acetate",
                          "0.45% Ethyl isovalerate", "0.45% Ethyl isovalerate",
                          "0.45% Ethyl isovalerate", "0.45% Ethyl isovalerate",
                          "0.9% Ethyl isovalerate", "0.9% Ethyl isovalerate",
                          "0.9% Ethyl isovalerate",
                          "1.8% Ethyl isovalerate", "1.8% Ethyl isovalerate",
                          "3.6% Ethyl isovalerate"),
               group2 = c("Isoamyl acetate", "0.45% Ethyl isovalerate", "0.9% Ethyl isovalerate",
                          "1.8% Ethyl isovalerate", "3.6% Ethyl isovalerate", "7.2% Ethyl isovalerate",
                          "0.45% Ethyl isovalerate", "0.9% Ethyl isovalerate", "1.8% Ethyl isovalerate",
                          "3.6% Ethyl isovalerate", "7.2% Ethyl isovalerate",
                          "0.9% Ethyl isovalerate", "1.8% Ethyl isovalerate",
                          "3.6% Ethyl isovalerate", "7.2% Ethyl isovalerate",
                          "1.8% Ethyl isovalerate","3.6% Ethyl isovalerate",
                          "7.2% Ethyl isovalerate",
                          "3.6% Ethyl isovalerate","7.2% Ethyl isovalerate",
                          "7.2% Ethyl isovalerate"),
               p = case_when(exp7_stats <= 0.0001 ~ "****",
                                 between(exp7_stats, 0.0001, 0.001) ~ "***",
                                 between(exp7_stats, 0.001, 0.01) ~ "**",
                                 between(exp7_stats, 0.01, 0.05) ~ "*",
                                 exp7_stats > 0.05 ~ "ns"))

exp_8 <- df %>% 
  filter(Experiment == 8) %>% 
  dplyr::select(Replicate, Food, Flies) %>% 
  mutate(Food = factor(Food))

#exp_8_glm <- glm(Flies ~ Food, data = exp_8, family = "poisson")
#summary(exp_8_glm)
#exp8_stats <- summary(pairs(emmeans(exp_8_glm, ~ Food)))

#with(exp_8_glm, cbind(res.deviance = deviance, df = df.residual,
#                      p = pchisq(deviance, df.residual, lower.tail=FALSE)))

glm8_nb <- glm.nb(Flies ~ Food, data = exp_8, link = "log")
summary(glm8_nb)
with(glm8_nb, cbind(res.deviance = deviance, df = df.residual,
                    p = pchisq(deviance, df.residual, lower.tail=FALSE)))
exp8_stats <- summary(pairs(emmeans(glm8_nb, ~ Food)))$p.value

res8 <- tibble(group1 = c("Standard cornmeal food", "Standard cornmeal food", "Isoamyl acetate"),
               group2 = c("Isoamyl acetate", "1.8% Ethyl isovalerate", "1.8% Ethyl isovalerate"),
               p = case_when(exp8_stats <= 0.0001 ~ "****",
                                 between(exp8_stats, 0.0001, 0.001) ~ "***",
                                 between(exp8_stats, 0.001, 0.01) ~ "**",
                                 between(exp8_stats, 0.01, 0.05) ~ "*",
                                 exp8_stats > 0.05 ~ "ns"))

# first plot: HBE vs banana + yeast
HBE_ban_yeast_in <- ggplot(filter(df, Experiment == 1), 
                        aes(x = as.factor(Food), y = fraction_flies_in_expt)) + 
  geom_point(aes(fill=Food), shape=21, size=2, color="black") +
  geom_boxplot(aes(fill=Food), shape=21, size=0.25, color="black", alpha=0.5) +
  scale_fill_viridis(discrete = TRUE, breaks = c("Standard cornmeal food", 
                                                 "Banana extract",
                                                 "Banana & yeast")) +
  scale_x_discrete(limits = c("Standard cornmeal food", "Banana extract",
                              "Banana & yeast")) +
  stat_pvalue_manual(res1, y.position = 1.00, step.increase = 0.1, label = "p_val") +
  ylim(0, 1.25) +
  xlab("") + 
  ylab("Fraction of flies trapped")  +
  theme_minimal(base_size = 11) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA),
        strip.background = element_rect(colour = "black", fill = "white"),
        axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        axis.text = element_text(face = "bold", angle = 90)) 


HBE_ban_yeast_in
ggsave("Figures/HBE_ban_yeast_in.pdf", units="in", width=10, height=7.5)

HBE_ban_yeast_out <- ggplot(filter(df, Experiment == 2), 
                           aes(x = as.factor(Food), y = fraction_flies_in_expt)) + 
  geom_point(aes(fill=Food), shape=21, size=2, color="black") +
  geom_boxplot(aes(fill=Food), shape=21, size=0.25, color="black", alpha=0.5) +
  scale_fill_viridis(discrete = TRUE, breaks = c("Standard cornmeal food", 
                                                 "Banana extract",
                                                 "Banana & yeast")) +
  scale_x_discrete(limits = c("Standard cornmeal food", "Banana extract",
                              "Banana & yeast")) +
  stat_pvalue_manual(res2, y.position = 1.00, step.increase = 0.1, label = "p_val") +
  ylim(0, 1.25) +
  xlab("") + 
  ylab("Fraction of flies trapped")  +
  theme_minimal(base_size = 11) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA),
        strip.background = element_rect(colour = "black", fill = "white"),
        axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        axis.text = element_text(face = "bold", angle = 90)) 

HBE_ban_yeast_out
ggsave("Figures/HBE_ban_yeast_out.pdf", units="in", width=10, height=7.5)

# second plot: all vs. all on cornmeal indoors & outdoors
all_v_all_in <- ggplot(filter(df, Experiment == 3)) + 
  geom_point(aes(x=Food, y=fraction_flies_in_expt, fill=Food), shape=21, size=2, 
             color="black") +
  geom_boxplot(aes(x=Food, y=fraction_flies_in_expt, fill=Food), shape=21, 
               size=0.25, color="black", alpha=0.25) +
  scale_fill_viridis(discrete = TRUE, breaks = c("Standard cornmeal food", 
                                                 "Banana extract", 
                                                 "Isoamyl acetate", 
                                                 "Ethyl isovalerate",
                                                 "Marula fruit oil")) +
  stat_pvalue_manual(res3, y.position = 0.55, step.increase = 0.07, label = "p", hide.ns = TRUE) +
  scale_x_discrete(limits = c("Standard cornmeal food", "Banana extract", 
                              "Isoamyl acetate", "Ethyl isovalerate",
                              "Marula fruit oil")) +
  xlab("") + 
  ylab("Fraction of flies trapped")  +
  theme_minimal(base_size = 11) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA),
        strip.background = element_rect(colour = "black", fill = "white"),
        axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        axis.text = element_text(face = "bold", angle = 90)) 

all_v_all_in
ggsave("Figures/all_v_all_in.pdf", units="in", width=10, height=7.5)

all_v_all_out <- ggplot(filter(df, Experiment == 4)) + 
  geom_point(aes(x=Food, y=fraction_flies_in_expt, fill=Food), shape=21, size=2, 
             color="black") +
  geom_boxplot(aes(x=Food, y=fraction_flies_in_expt, fill=Food), shape=21, 
               size=0.25, color="black", alpha=0.25) +
  scale_fill_viridis(discrete = TRUE, breaks = c("Standard cornmeal food", 
                                                 "Banana extract", 
                                                 "Isoamyl acetate", 
                                                 "Ethyl isovalerate",
                                                 "Marula fruit oil")) +
  stat_pvalue_manual(res4, y.position = 0.55, step.increase = 0.07, label = "p", hide.ns = TRUE) +
  scale_x_discrete(limits = c("Standard cornmeal food", "Banana extract", 
                              "Isoamyl acetate", "Ethyl isovalerate",
                              "Marula fruit oil")) +
  xlab("") + 
  ylab("Fraction of flies trapped")  +
  theme_minimal(base_size = 11) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA),
        strip.background = element_rect(colour = "black", fill = "white"),
        axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        axis.text = element_text(face = "bold", angle = 90)) 

all_v_all_out
ggsave("Figures/all_v_all_out.pdf", units="in", width=10, height=7.5)

# thrid plot: all vs. all on potato indoors & outdoors
all_v_all_potato_in <- ggplot(filter(df, Experiment == 5)) + 
  geom_point(aes(x=Food, y=fraction_flies_in_expt, fill=Food), shape=21, size=2, 
             color="black") +
  geom_boxplot(aes(x=Food, y=fraction_flies_in_expt, fill=Food), shape=21, 
               size=0.25, color="black", alpha=0.25) +
  scale_fill_viridis(discrete = TRUE, breaks = c("Standard potato food", 
                                                 "Banana extract", 
                                                 "Isoamyl acetate", 
                                                 "Ethyl isovalerate",
                                                 "Marula fruit oil")) +
  stat_pvalue_manual(res5, y.position = 0.5, step.increase = 0.07, label = "p", hide.ns = TRUE) +
  scale_x_discrete(limits = c("Standard potato food", "Banana extract", 
                              "Isoamyl acetate", "Ethyl isovalerate",
                              "Marula fruit oil")) +
  xlab("") + 
  ylab("Fraction of flies trapped")  +
  theme_minimal(base_size = 11) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA),
        strip.background = element_rect(colour = "black", fill = "white"),
        axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        axis.text = element_text(face = "bold", angle = 90)) 

all_v_all_potato_in
ggsave("Figures/all_v_all_potato_in.pdf", units="in", width=10, height=7.5)

all_v_all_potato_out <- ggplot(filter(df, Experiment == 6)) + 
  geom_point(aes(x=Food, y=fraction_flies_in_expt, fill=Food), shape=21, size=2, 
             color="black") +
  geom_boxplot(aes(x=Food, y=fraction_flies_in_expt, fill=Food), shape=21, 
               size=0.25, color="black", alpha=0.25) +
  scale_fill_viridis(discrete = TRUE, breaks = c("Standard potato food", 
                                                 "Banana extract", 
                                                 "Isoamyl acetate", 
                                                 "Ethyl isovalerate",
                                                 "Marula fruit oil")) +
  stat_pvalue_manual(res6, y.position = 0.55, step.increase = 0.07, label = "p", hide.ns = TRUE) +
  scale_x_discrete(limits = c("Standard potato food", "Banana extract", 
                              "Isoamyl acetate", "Ethyl isovalerate",
                              "Marula fruit oil")) +
  xlab("") + 
  ylab("Fraction of flies trapped")  +
  theme_minimal(base_size = 11) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA),
        strip.background = element_rect(colour = "black", fill = "white"),
        axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        axis.text = element_text(face = "bold", angle = 90)) 

all_v_all_potato_out
ggsave("Figures/all_v_all_potato_out.pdf", units="in", width=10, height=7.5)

ethyl_dil <- ggplot(filter(df, Experiment == 7)) +
  geom_point(aes(x=Food, y=fraction_flies_in_expt, fill=Food), shape=21, size=2, 
             color="black") +
  geom_boxplot(aes(x=Food, y=fraction_flies_in_expt, fill=Food), shape=21, 
               size=0.25, color="black", alpha=0.25) +
  scale_fill_viridis(discrete = TRUE) +
  scale_x_discrete(limits = c("Standard cornmeal food", "Isoamyl acetate", 
                              "0.45% Ethyl isovalerate", "0.9% Ethyl isovalerate",
                              "1.8% Ethyl isovalerate", "3.6% Ethyl isovalerate", 
                              "7.2% Ethyl isovalerate")) +
  stat_pvalue_manual(res7, y.position = 0.55, step.increase = 0.07, label = "p", hide.ns = TRUE) +
  xlab("") + 
  ylab("Fraction of flies trapped")  +
  theme_minimal(base_size = 11) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA),
        strip.background = element_rect(colour = "black", fill = "white"),
        axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        axis.text = element_text(face = "bold", angle = 90)) 

ethyl_dil
ggsave("Figures/ethyl_dil.pdf", units="in", width=10, height=7.5)

ethyl_iso <- ggplot(filter(df, Experiment == 8)) +
  geom_point(aes(x=Food, y=fraction_flies_in_expt, fill=Food), shape=21, size=2, 
             color="black") +
  geom_boxplot(aes(x=Food, y=fraction_flies_in_expt, fill=Food), shape=21, 
               size=0.25, color="black", alpha=0.25) +
  scale_fill_viridis(discrete = TRUE) +
  scale_x_discrete(limits = c("Standard cornmeal food", "Isoamyl acetate",
                              "1.8% Ethyl isovalerate")) +
  stat_pvalue_manual(res8, y.position = 0.8, step.increase = 0.1, label = "p_val") +
  xlab("") + 
  ylab("Fraction of flies trapped")  +
  theme_minimal(base_size = 11) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA),
        strip.background = element_rect(colour = "black", fill = "white"),
        axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        axis.text = element_text(face = "bold", angle = 90)) 

ethyl_iso
