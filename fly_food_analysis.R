library(tidyverse)
library(readxl)
library(viridis)
library(forcats)
library(ggthemes)

# read in data 
df <- read_excel("data/Consolidated_fly_food_data.xlsx")

df <- df %>% 
  mutate(proportion = Flies/total) %>% 
  group_by(Experiment, Replicate) %>% 
  mutate(Food = str_replace(Food, "Banana_and_Yeast", "Banana & yeast"),
         Food = str_replace(Food, "Homemade_banana_extract", "Banana extract"),
         Food = str_replace(Food, "Plain_cornmeal_food", "Base cornmeal food"),
         Food = str_replace(Food, "Artificial_banana_extract", "Isoamyl acetate"),
         Food = str_replace(Food, "Ethyl_Isovalerate", "Ethyl isovalerate"),
         Food = str_replace(Food, "Marula_fruit_oil", "Marula fruit oil"),
         Food = str_replace(Food, "Plain_potato_food", "Base potato food"))

banana <- ggplot(filter(df, Experiment_description == "HBE_vs_banana_and_yeast"),
              aes(fill = Food, y = proportion, x = Replicate)) +
  geom_bar(position="fill", stat="identity")+
  scale_fill_viridis(discrete = TRUE) +
  facet_wrap(~Setting) +
  theme_minimal(base_size = 11) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA),
        strip.background = element_rect(colour = "black", fill = "white"),
        axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        axis.text = element_text(face = "bold")) +
  labs(x = "", y = "Proprotion of flies", 
       fill = "Food Type")
banana
ggsave("plots/banana_extract_plain.pdf", units = "in", width = 10, height = 8)

all_vs_all <- ggplot(filter(df, Experiment_description == "All_vs_all"),
       aes(fill = Food, y = proportion, x = Replicate)) +
  geom_bar(position="fill", stat="identity")+
  scale_fill_viridis(discrete = TRUE) +
  facet_wrap(~Setting) +
  theme_minimal(base_size = 11) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA),
        strip.background = element_rect(colour = "black", fill = "white"),
        axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        axis.text = element_text(face = "bold")) +
  labs(x = "", y = "Proprotion of flies in each bottle", 
       fill = "Food Type")
all_vs_all
ggsave("plots/all_vs_all.pdf", units = "in", width = 10, height = 8)

all_vs_all_potato <- ggplot(filter(df, Experiment_description == "All_vs_all_potato"),
       aes(fill = Food, y = proportion, x = Replicate)) +
  geom_bar(position="fill", stat="identity")+
  scale_fill_viridis(discrete = TRUE) +
  facet_wrap(~Setting) +
  theme_minimal(base_size = 11) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA),
        strip.background = element_rect(colour = "black", fill = "white"),
        axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        axis.text = element_text(face = "bold")) +
  labs(x = "", y = "Proprotion of flies in each bottle", 
       fill = "Food Type")
all_vs_all_potato
ggsave("plots/all_vs_all_potato.pdf", units = "in", width = 10, height = 8)


# Ethyl Isovalerate 
df2 <- read_excel("data/EthylIsovalerateDil_Tidy.xlsx")

df2 <- df2 %>% 
  mutate(Food = str_replace(Food, "Plain_cornmeal_food", "Standard Cornmeal Food"),
         Food = str_replace(Food, "Artificial_banana_extract", "Commercial Banana Extract"),
         Food = str_replace(Food, "Homemade_banana_extract", "Homemade Banana Extract"),
         Food = str_replace(Food, "2%_Ethyl_Isovalerate", "2% Ethyl Isovalerate"),
         Food = str_replace(Food, "1%_Ethyl_Isovalerate", "1% Ethyl Isovalerate"),
         Food = str_replace(Food, "0.5%_Ethyl_Isovalerate", "0.5% Ethyl Isovalerate"),
         Food = str_replace(Food, "0.25%_Ethyl_Isovalerate", "0.25% Ethyl Isovalerate"),
         Food = str_replace(Food, "0.125%_Ethyl_Isovalertate", "0.125% Ethyl Isovalerate")) 

ggplot(data=df2, aes(x=Food, y=Flies)) +
  geom_col(fill = c("goldenrod", "yellow3", "yellow2", "Steelblue4", "steelblue3", 
                    "steelblue2", "steelblue1", "steelblue")) +
  geom_text(aes(label=Flies), vjust=-0.4, color="black", size=3.5) + 
  theme_few(base_size = 11) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA),
        strip.background = element_rect(colour = "black", fill = "white"),
        axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        axis.text = element_text(face = "bold")) +
  scale_x_discrete(limits = c("Standard Cornmeal Food",	"Homemade Banana Extract",	
                              "Commercial Banana Extract",	"2% Ethyl Isovalerate",	
                              "1% Ethyl Isovalerate",	"0.5% Ethyl Isovalerate",	
                              "0.25% Ethyl Isovalerate",	"0.125% Ethyl Isovalerate")) +
  labs(x="", y="Proportion of Flies in Each Bottle")

ggsave("plots/EthylIsovalerateDil.pdf", units="in", width=10, height=7.5)






