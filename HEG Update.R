install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("modeldata")
library(tidyverse)
library(dplyr)
library(ggplot2)
library(modeldata)
library(ggpubr)
library(readr)
library(readxl)
HEG <- read_excel("Phenoloxidase Activity data/HEG.xlsx")
view(HEG)
str(HEG)
head(HEG)
summary(HEG)
glimpse(HEG)

ggplot(HEG, aes(x = Site,
                y = Phenoloxidase_Activity,
                fill = Site)) + geom_boxplot()+ 
  labs(x = "Site", y = "Phenoloxidase Activity (µmol/gsoil/h)") +
  theme_classic(base_family = "serif")

ggplot(HEG, aes(x = Soil_Type,
                y = Phenoloxidase_Activity,
                fill = Soil_Type)) + geom_boxplot(show.legend = FALSE)+ 
  labs(x = "Soil Type", y = "Phenoloxidase Activity (µmol/gsoil/h)") +
  theme_classic(base_family = "serif")


# Correlation of pH and Phenoloxidase Activity

x <- HEG$pH 
y <- HEG$Phenoloxidase_Activity 
correlation_result <- cor.test(x, y)
print(correlation_result)
   
r_squared <- correlation_result$estimate^2
print(r_squared)

plot <- ggplot(HEG, aes(x = pH, y = Phenoloxidase_Activity)) +
  geom_point(show.legend = FALSE, color= "green") +
  geom_smooth(method = "lm", color = "grey", se = FALSE) +
  labs(title = "Correlation between pH and Phenoloxidase Activity in HEG",
       x = "pH",
       y = "Phenoloxidase Activity (µmol/gsoil/h)") +
  theme_classic(base_family = "serif")

print(plot)


lm_fit <- lm(y ~ x)


summary_fit <- summary(lm_fit)
r_squared <- summary_fit$r.squared
p_value <- summary_fit$coefficients[2, 4]


plot + annotate("text", x = 6.5, y = 18, 
                label = paste0("R² = ", round(r_squared, 3), 
                               "\nP = ", signif(p_value, 3)),
                hjust = 1, size = 4, color = "black")



# Correlation of Organic carbon and Phenoloxidase Activity

x <- HEG$Organic_C
y <- HEG$Phenoloxidase_Activity 
correlation_result <- cor.test(x, y)
print(correlation_result)

r_squared <- correlation_result$estimate^2
print(r_squared)

plot <- ggplot(HEG, aes(x = Organic_C, y = Phenoloxidase_Activity)) +
  geom_point(show.legend = FALSE, color= "green") +
  geom_smooth(method = "lm", color = "grey", se = FALSE) +
  labs(title = "Correlation between Organic Carbon and Phenoloxidase Activity
                                      in HEG",
       x = "Organic Carbon(g/kgsoil)",
       y = "Phenoloxidase Activity (µmol/gsoil/h)") +
  theme_classic(base_family = "serif")

print(plot)

lm_fit <- lm(y ~ x)

summary_fit <- summary(lm_fit)
r_squared <- summary_fit$r.squared
p_value <- summary_fit$coefficients[2, 4]


plot + annotate("text", x = 75, y = 20, 
                label = paste0("R² = ", round(r_squared, 3), 
                               "\nP = ", signif(p_value, 3)),
                hjust = 1, size = 4, color = "black")


# Correlation of Total nitrogen and Phenoloxidase Activity

x <- HEG$Total_N
y <- HEG$Phenoloxidase_Activity 
correlation_result <- cor.test(x, y)
print(correlation_result)

r_squared <- correlation_result$estimate^2
print(r_squared)

plot <- ggplot(HEG, aes(x = Total_N, y = Phenoloxidase_Activity)) +
  geom_point(show.legend = FALSE, color= "green") +
  geom_smooth(method = "lm", color = "grey", se = FALSE) +
  labs(title = "Correlation between Total Nitrogen and Phenoloxidase Activity
                                    in HEG",
       x = "Total Nitrogen(g/kgsoil)",
       y = "Phenoloxidase Activity(µmol/gsoil/h)") +
  theme_classic(base_family = "serif")

print(plot)


lm_fit <- lm(y ~ x)


summary_fit <- summary(lm_fit)
r_squared <- summary_fit$r.squared
p_value <- summary_fit$coefficients[2, 4]


plot + annotate("text", x = 7.5, y = 20, 
                label = paste0("R² = ", round(r_squared, 3), 
                               "\nP = ", signif(p_value, 3)),
                hjust = 1, size = 4, color = "black")


# Correlation of Phenol and Phenoloxidase Activity

x <- HEG$Phenol_Content
y <- HEG$Phenoloxidase_Activity 
correlation_result <- cor.test(x, y)
print(correlation_result)

r_squared <- correlation_result$estimate^2
print(r_squared)

plot <- ggplot(HEG, aes(x = Phenol_Content, y = Phenoloxidase_Activity)) +
  geom_point(show.legend = FALSE, color= "green") +
  geom_smooth(method = "lm", color = "grey", se = FALSE) +
  labs(title = "Correlation between Phenol Content and Phenoloxidase Activity
                                      in HEG",
       x = "Phenol Content(µmol/gsoil)",
       y = "Phenoloxidase Activity(µmol/gsoil/h)") +
  theme_classic(base_family = "serif")

print(plot)


lm_fit <- lm(y ~ x)


summary_fit <- summary(lm_fit)
r_squared <- summary_fit$r.squared
p_value <- summary_fit$coefficients[2, 4]


plot + annotate("text", x = 26, y = 18, 
                label = paste0("R² = ", round(r_squared, 6), 
                               "\nP = ", signif(p_value, 3)),
                hjust = 1, size = 4, color = "black")



# Correlation of LUI index 2022 and Phenoloxidase Activity

x <- HEG$LUI_index_2022
y <- HEG$Phenoloxidase_Activity 
correlation_result <- cor.test(x, y)
print(correlation_result)

r_squared <- correlation_result$estimate^2
print(r_squared)

plot <- ggplot(HEG, aes(x = LUI_index_2022, y = Phenoloxidase_Activity)) +
  geom_point(show.legend = FALSE, color= "green") +
  geom_smooth(method = "lm", color = "grey", se = FALSE) +
  labs(title = "Correlation between LUI index 2022 and Phenoloxidase 
                             Activity in HEG",
       x = "LUI index 2022",
       y = "Phenoloxidase Activity(µmol/gsoil/h)") +
  theme_classic(base_family = "serif")

print(plot)


lm_fit <- lm(y ~ x)


summary_fit <- summary(lm_fit)
r_squared <- summary_fit$r.squared
p_value <- summary_fit$coefficients[2, 4]


plot + annotate("text", x = 2.5, y = 19, 
                label = paste0("R² = ", round(r_squared, 6), 
                               "\nP = ", signif(p_value, 3)),
                hjust = 1, size = 4, color = "black")

# Correlation of Gazing 2022 and Phenoloxidase Activity

x <- HEG$G_2022
y <- HEG$Phenoloxidase_Activity 
correlation_result <- cor.test(x, y)
print(correlation_result)

r_squared <- correlation_result$estimate^2
print(r_squared)

plot <- ggplot(HEG, aes(x = G_2022, y = Phenoloxidase_Activity)) +
  geom_point(show.legend = FALSE, color= "green") +
  geom_smooth(method = "lm", color = "grey", se = FALSE) +
  labs(title = "Correlation between Gazing index 2022 and Phenoloxidase 
                               Activity in HEG",
       x = "Gazing index 2022",
       y = "Phenoloxidase Activity(µmol/gsoil/h)") +
  theme_classic(base_family = "serif")

print(plot)


lm_fit <- lm(y ~ x)


summary_fit <- summary(lm_fit)
r_squared <- summary_fit$r.squared
p_value <- summary_fit$coefficients[2, 4]


plot + annotate("text", x = 7.5, y = 20, 
                label = paste0("R² = ", round(r_squared, 6), 
                               "\nP = ", signif(p_value, 3)),
                hjust = 1, size = 4, color = "black")

# Correlation of Mowing 2022 and Phenoloxidase Activity

x <- HEG$M_2022
y <- HEG$Phenoloxidase_Activity 
correlation_result <- cor.test(x, y)
print(correlation_result)

r_squared <- correlation_result$estimate^2
print(r_squared)

plot <- ggplot(HEG, aes(x = M_2022, y = Phenoloxidase_Activity)) +
  geom_point(show.legend = FALSE, color= "green") +
  geom_smooth(method = "lm", color = "grey", se = FALSE) +
  labs(title = "Correlation between Mowing index 2022 and Phenoloxidase
                              Activity in HEG",
       x = "Mowing index 2022",
       y = "Phenoloxidase Activity(µmol/gsoil/h)") +
  theme_classic(base_family = "serif")

print(plot)


lm_fit <- lm(y ~ x)


summary_fit <- summary(lm_fit)
r_squared <- summary_fit$r.squared
p_value <- summary_fit$coefficients[2, 4]


plot + annotate("text", x = 2, y = 18, 
                label = paste0("R² = ", round(r_squared, 6), 
                               "\nP = ", signif(p_value, 3)),
                hjust = 1, size = 4, color = "black")



# Correlation of Fertilizer 2022 and Phenoloxidase Activity

x <- HEG$F_2022
y <- HEG$Phenoloxidase_Activity 
correlation_result <- cor.test(x, y)
print(correlation_result)

r_squared <- correlation_result$estimate^2
print(r_squared)

plot <- ggplot(HEG, aes(x = F_2022, y = Phenoloxidase_Activity)) +
  geom_point(show.legend = FALSE, color= "green") +
  geom_smooth(method = "lm", color = "grey", se = FALSE) +
  labs(title = "Correlation between Fertilizer index 2022 and Phenoloxidase 
                              Activity in HEG",
       x = "Fertilizer index 2022",
       y = "Phenoloxidase Activity(µmol/gsoil/h)") +
  theme_classic(base_family = "serif")

print(plot)


lm_fit <- lm(y ~ x)


summary_fit <- summary(lm_fit)
r_squared <- summary_fit$r.squared
p_value <- summary_fit$coefficients[2, 4]


plot + annotate("text", x = 3, y = 18, 
                label = paste0("R² = ", round(r_squared, 6), 
                               "\nP = ", signif(p_value, 3)),
                hjust = 1, size = 4, color = "black")


#loading the appropriate libaries
library (datasets)
library(ggplot2)
library(multcompView)
library(dplyr)


##Soil Type##

#analysis of variance
anova_result <- aov (Phenoloxidase_Activity ~ Soil_Type , data = HEG)
summary(anova_result)

#Tukey's test
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

#compact letter display
cld <- multcompLetters4(anova_result,tukey_result)
print(cld)

#table with factors and 3rd quantile
Tk <- group_by(HEG,Soil_Type) %>%
  summarise(mean = mean(Phenoloxidase_Activity), quant = quantile(Phenoloxidase_Activity, probs= 0.75)) %>%
  arrange(desc(mean))

#extracting the compact letter display and adding to the Tk table 
cld <- as.data.frame.list(cld$Soil_Type)
Tk$cld <- cld$Letters
print(Tk)

#boxplot
soil_colors <- c("Cambisol" = "orange", "Albeluvisol" = "grey", 
                 "Gleysol" = "greenyellow", "Histosol" = "limegreen", 
                 "Leptosol" = "cyan", "Luvisol" = "gold",
                 "Stagnosol" = "purple", "Vertisol" = "magenta")

ggplot(HEG, aes(x = Soil_Type, y = Phenoloxidase_Activity, fill = Soil_Type)) +
  geom_boxplot(show.legend = FALSE) +
  scale_fill_manual(values = soil_colors) +
  labs(title = "Phenoloxidase Activity by Soil Type in HEG",
       x = "Soil Type",
       y = "Phenoloxidase Activity(µmol/gsoil/h)") +
  theme_classic(base_family = "serif") +
  geom_text(data= Tk, aes(label=cld, x= Soil_Type, y = quant),
            vjust = -1, hjust = -1, size = 3) 

