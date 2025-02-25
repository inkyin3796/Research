install.packages("tidyverse")
install.packages("plotly")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("modeldata")


library(tidyverse)
library(plotly)
library(dplyr)
library(ggplot2)
library(modeldata)
library(readr)
library(readxl)

SEG <- read_excel("Phenoloxidase Activity data/SEG.xlsx")
view(SEG)
str(SEG)
summary(SEG)
glimpse(SEG)

ggplot(HEG, aes(x = Site,
                y = Phenoloxidase_Activity,
                fill = Site)) + geom_boxplot()+ 
  labs(x = "Site", y = "Phenoloxidase Activity (µmol/gsoil/h)") +
  theme_classic(base_family = "serif")

ggplot(HEG, aes(x = Soil_Type,
                y = Phenoloxidase_Activity,
                fill = Soil_Type)) + geom_boxplot()+ 
  labs(x = "Soil Type", y = "Phenoloxidase Activity (µmol/gsoil/h)") +
  theme_classic(base_family = "serif")


# Correlation of pH and Phenoloxidase Activity

x <- HEG$pH 
y <- HEG$Phenoloxidase_Activity 
correlation_result <- cor.test(x, y)
print(correlation_result)

r_squared <- correlation_result$estimate^2
print(r_squared)

plot <- ggplot(HEG, aes(x = pH, y = Phenoloxidase_Activity, colour = Site)) +
  geom_point() +
  geom_smooth(method = "lm", color = "grey", se = FALSE) +
  labs(title = "Correlation between pH and Phenoloxidase Activity",
       x = "pH",
       y = "Phenoloxidase Activity (µmol/gsoil/h)") +
  theme_classic(base_family = "serif")

print(plot)


lm_fit <- lm(y ~ x)


summary_fit <- summary(lm_fit)
r_squared <- summary_fit$r.squared
p_value <- summary_fit$coefficients[2, 4]


plot + annotate("text", x = 5.5, y = 40, 
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

plot <- ggplot(HEG, aes(x = Organic_C, y = Phenoloxidase_Activity, colour = Site)) +
  geom_point() +
  geom_smooth(method = "lm", color = "grey", se = FALSE) +
  labs(title = "Correlation between Organic Carbon and Phenoloxidase Activity",
       x = "Organic Carbon(g/kgsoil)",
       y = "Phenoloxidase Activity (µmol/gsoil/h)") +
  theme_classic(base_family = "serif")

print(plot)

lm_fit <- lm(y ~ x)

summary_fit <- summary(lm_fit)
r_squared <- summary_fit$r.squared
p_value <- summary_fit$coefficients[2, 4]


plot + annotate("text", x = 200, y = 30, 
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

plot <- ggplot(HEG, aes(x = Total_N, y = Phenoloxidase_Activity, colour = Site)) +
  geom_point() +
  geom_smooth(method = "lm", color = "grey", se = FALSE) +
  labs(title = "Correlation between Total Nitrogen and Phenoloxidase Activity",
       x = "Total Nitrogen(g/kgsoil)",
       y = "Phenoloxidase Activity(µmol/gsoil/h)") +
  theme_classic(base_family = "serif")

print(plot)


lm_fit <- lm(y ~ x)


summary_fit <- summary(lm_fit)
r_squared <- summary_fit$r.squared
p_value <- summary_fit$coefficients[2, 4]


plot + annotate("text", x = 25, y = 30, 
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

plot <- ggplot(HEG, aes(x = Phenol_Content, y = Phenoloxidase_Activity, colour = Site)) +
  geom_point() +
  geom_smooth(method = "lm", color = "grey", se = FALSE) +
  labs(title = "Correlation between Phenol Content and Phenoloxidase Activity",
       x = "Phenol Content(µmol/gsoil)",
       y = "Phenoloxidase Activity(µmol/gsoil/h)") +
  theme_classic(base_family = "serif")

print(plot)


lm_fit <- lm(y ~ x)


summary_fit <- summary(lm_fit)
r_squared <- summary_fit$r.squared
p_value <- summary_fit$coefficients[2, 4]


plot + annotate("text", x = 100, y = 40, 
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

plot <- ggplot(HEG, aes(x = LUI_index_2022, y = Phenoloxidase_Activity, colour = Site)) +
  geom_point() +
  geom_smooth(method = "lm", color = "grey", se = FALSE) +
  labs(title = "Correlation between LUI index 2022 and Phenoloxidase Activity",
       x = "LUI index 2022",
       y = "Phenoloxidase Activity(µmol/gsoil/h)") +
  theme_classic(base_family = "serif")

print(plot)


lm_fit <- lm(y ~ x)


summary_fit <- summary(lm_fit)
r_squared <- summary_fit$r.squared
p_value <- summary_fit$coefficients[2, 4]


plot + annotate("text", x = 3, y = 40, 
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

plot <- ggplot(HEG, aes(x = G_2022, y = Phenoloxidase_Activity, colour = Site)) +
  geom_point() +
  geom_smooth(method = "lm", color = "grey", se = FALSE) +
  labs(title = "Correlation between Gazing index 2022 and Phenoloxidase Activity",
       x = "Gazing index 2022",
       y = "Phenoloxidase Activity(µmol/gsoil/h)") +
  theme_classic(base_family = "serif")

print(plot)


lm_fit <- lm(y ~ x)


summary_fit <- summary(lm_fit)
r_squared <- summary_fit$r.squared
p_value <- summary_fit$coefficients[2, 4]


plot + annotate("text", x = 3, y = 40, 
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

plot <- ggplot(HEG, aes(x = M_2022, y = Phenoloxidase_Activity, colour = Site)) +
  geom_point() +
  geom_smooth(method = "lm", color = "grey", se = FALSE) +
  labs(title = "Correlation between Mowing index 2022 and Phenoloxidase Activity",
       x = "Mowing index 2022",
       y = "Phenoloxidase Activity(µmol/gsoil/h)") +
  theme_classic(base_family = "serif")

print(plot)


lm_fit <- lm(y ~ x)


summary_fit <- summary(lm_fit)
r_squared <- summary_fit$r.squared
p_value <- summary_fit$coefficients[2, 4]


plot + annotate("text", x = 3, y = 40, 
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

plot <- ggplot(HEG, aes(x = F_2022, y = Phenoloxidase_Activity, colour = Site)) +
  geom_point() +
  geom_smooth(method = "lm", color = "grey", se = FALSE) +
  labs(title = "Correlation between Fertilizer index 2022 and Phenoloxidase Activity",
       x = "Fertilizer index 2022",
       y = "Phenoloxidase Activity(µmol/gsoil/h)") +
  theme_classic(base_family = "serif")

print(plot)


lm_fit <- lm(y ~ x)


summary_fit <- summary(lm_fit)
r_squared <- summary_fit$r.squared
p_value <- summary_fit$coefficients[2, 4]


plot + annotate("text", x = 3, y = 40, 
                label = paste0("R² = ", round(r_squared, 6), 
                               "\nP = ", signif(p_value, 3)),
                hjust = 1, size = 4, color = "black")
