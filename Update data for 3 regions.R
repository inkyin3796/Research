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
data <- read_excel("Phenoloxidase Activity data/Final_data.xlsx")
view(data)
head(data)
str(data)

ggplot(data, aes(x = Site,
                 y = Phenoloxidase_Activity,
                 fill = Site)) + geom_boxplot()+ 
  labs(x = "Site", y = "Phenoloxidase Activity (µmol/gsoil/h)") +
  theme_classic(base_family = "serif")

ggplot(data, aes(x = Soil_Type,
                 y = Phenoloxidase_Activity,
                 fill = Soil_Type)) + geom_boxplot()+ 
  labs(x = "Soil Type", y = "Phenoloxidase Activity (µmol/gsoil/h)") +
  theme_classic(base_family = "serif")


# Correlation of pH and Phenoloxidase Activity

x <- data$pH 
y <- data$Phenoloxidase_Activity 
correlation_result <- cor.test(x, y)
print(correlation_result)

r_squared <- correlation_result$estimate^2
print(r_squared)

plot <- ggplot(data, aes(x = pH, y = Phenoloxidase_Activity, colour = Site)) +
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

x <- data$Organic_C
y <- data$Phenoloxidase_Activity 
correlation_result <- cor.test(x, y)
print(correlation_result)

r_squared <- correlation_result$estimate^2
print(r_squared)

plot <- ggplot(data, aes(x = Organic_C, y = Phenoloxidase_Activity, colour = Site)) +
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

x <- data$Total_N
y <- data$Phenoloxidase_Activity 
correlation_result <- cor.test(x, y)
print(correlation_result)

r_squared <- correlation_result$estimate^2
print(r_squared)

plot <- ggplot(data, aes(x = Total_N, y = Phenoloxidase_Activity, colour = Site)) +
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

x <- data$Phenol_Content
y <- data$Phenoloxidase_Activity 
correlation_result <- cor.test(x, y)
print(correlation_result)

r_squared <- correlation_result$estimate^2
print(r_squared)

plot <- ggplot(data, aes(x = Phenol_Content, y = Phenoloxidase_Activity, colour = Site)) +
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

x <- data$LUI_index_2022
y <- data$Phenoloxidase_Activity 
correlation_result <- cor.test(x, y)
print(correlation_result)

r_squared <- correlation_result$estimate^2
print(r_squared)

plot <- ggplot(data, aes(x = LUI_index_2022, y = Phenoloxidase_Activity, colour = Site)) +
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

x <- data$G_2022
y <- data$Phenoloxidase_Activity 
correlation_result <- cor.test(x, y)
print(correlation_result)

r_squared <- correlation_result$estimate^2
print(r_squared)

plot <- ggplot(data, aes(x = G_2022, y = Phenoloxidase_Activity, colour = Site)) +
  geom_point() +
  geom_smooth(method = "lm", color = "grey", se = FALSE) +
  labs(title = "Correlation between Gazing index 2022 and Phenoloxidase 
                                    Activity",
       x = "Gazing index 2022",
       y = "Phenoloxidase Activity(µmol/gsoil/h)") +
  theme_classic(base_family = "serif")

print(plot)


lm_fit <- lm(y ~ x)


summary_fit <- summary(lm_fit)
r_squared <- summary_fit$r.squared
p_value <- summary_fit$coefficients[2, 4]


plot + annotate("text", x = 6, y = 40, 
                label = paste0("R² = ", round(r_squared, 6), 
                               "\nP = ", signif(p_value, 3)),
                hjust = 1, size = 4, color = "black")

# Correlation of Mowing 2022 and Phenoloxidase Activity

x <- data$M_2022
y <- data$Phenoloxidase_Activity 
correlation_result <- cor.test(x, y)
print(correlation_result)

r_squared <- correlation_result$estimate^2
print(r_squared)

plot <- ggplot(data, aes(x = M_2022, y = Phenoloxidase_Activity, colour = Site)) +
  geom_point() +
  geom_smooth(method = "lm", color = "grey", se = FALSE) +
  labs(title = "Correlation between Mowing index 2022 and Phenoloxidase
                                    Activity",
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

x <- data$F_2022
y <- data$Phenoloxidase_Activity 
correlation_result <- cor.test(x, y)
print(correlation_result)

r_squared <- correlation_result$estimate^2
print(r_squared)

plot <- ggplot(data, aes(x = F_2022, y = Phenoloxidase_Activity, colour = Site)) +
  geom_point() +
  geom_smooth(method = "lm", color = "grey", se = FALSE) +
  labs(title = "Correlation between Fertilizer index 2022 and Phenoloxidase 
                                       Activity",
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



library(patchwork)
install.packages("gapminder")
library(gapminder)
library(forcats)


#Soil Type
anova_result <- aov (Phenoloxidase_Activity ~ Soil_Type , data = data)
summary(anova_result)
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

ggplot(data, aes(x = Soil_Type, y = Phenoloxidase_Activity, fill = Soil_Type)) +
  geom_boxplot() +
  labs(title = "Phenoloxidase Activity by Soil Type",
       x = "Soil Type",
       y = "Phenoloxidase Activity(µmol/gsoil/h)") +
  theme_classic(base_family = "serif")


# Extract Tukey HSD results for SoilType and convert to data frame
tukey_df <- as.data.frame(tukey_result$Soil_Type)

# View the data frame
print(tukey_df)
write.csv(tukey_df, "TukeyHSD_Results.csv")


# Save the boxplot
ggsave("Phenoloxidase_Boxplot.png")


#Site

anova_result1 <- aov (Phenoloxidase_Activity ~ Site , data = data)
summary(anova_result1)
tukey_result1 <- TukeyHSD(anova_result1)
print(tukey_result1)

ggplot(data, aes(x = Site, y = Phenoloxidase_Activity, fill = Site)) +
  geom_boxplot() +
  labs(title = "Phenoloxidase Activity by Site",
       x = "Site",
       y = "Phenoloxidase Activity(µmol/gsoil/h)") +
  theme_classic(base_family = "serif")


# Extract Tukey HSD results for SoilType and convert to data frame
tukey_df1 <- as.data.frame(tukey_result1$Site)

# View the data frame
print(tukey_df1)
write.csv(tukey_df1, "TukeyHSD_Results1.csv")


# Save the boxplot
ggsave("Phenoloxidaseandsite_Boxplot.png")



#loading the appropriate libaries
library (datasets)
library(ggplot2)
library(multcompView)
library(dplyr)

#loading and checking the data
str(data)

##Soil Type##

#analysis of variance
anova_result <- aov (Phenoloxidase_Activity ~ Soil_Type , data = data)
summary(anova_result)

#Tukey's test
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

#compact letter display
cld <- multcompLetters4(anova_result,tukey_result)
print(cld)

#table with factors and 3rd quantile
Tk <- group_by(data,Soil_Type) %>%
  summarise(mean = mean(Phenoloxidase_Activity), quant = quantile(Phenoloxidase_Activity, probs= 0.75)) %>%
  arrange(desc(mean))

#extracting the compact letter display and adding to the Tk table 
cld <- as.data.frame.list(cld$Soil_Type)
Tk$cld <- cld$Letters
print(Tk)

#boxplot

ggplot(data, aes(x = Soil_Type, y = Phenoloxidase_Activity, fill = Soil_Type)) +
  geom_boxplot(show.legend = FALSE) +
  labs(title = "Phenoloxidase Activity by Soil Type",
       x = "Soil Type",
       y = "Phenoloxidase Activity(µmol/gsoil/h)") +
  theme_classic(base_family = "serif") +
  geom_text(data= Tk, aes(label=cld, x= Soil_Type, y = quant),
            vjust = -1, hjust = -1, size = 3) 

ggplot(data, aes(x = Soil_Type, y = Phenoloxidase_Activity)) +
  geom_boxplot(aes(fill = factor(..middle..)), show.legend = FALSE) +
  labs(title = "Phenoloxidase Activity by Soil Type",
       x = "Soil Type",
       y = "Phenoloxidase Activity(µmol/gsoil/h)") +
  theme_classic(base_family = "serif") +
  geom_text(data= Tk, aes(label=cld, x= Soil_Type, y = quant),
            vjust = -1, hjust = -1, size = 3) +
  scale_fill_brewer(palette = "Blues")


#saving the final figure
ggsave("boxplot.png", width = 4, height = 3, dpi = 1000)


#Site#

#analysis of variance
anova_result1 <- aov (Phenoloxidase_Activity ~ Site , data = data)
summary(anova_result1)

#Tukey's test
tukey_result1 <- TukeyHSD(anova_result1)
print(tukey_result1)

#compact letter display
cld1 <- multcompLetters4(anova_result1,tukey_result1)
print(cld1)

#table with factors and 3rd quantile
Tk1 <- group_by(data,Site) %>%
  summarise(mean = mean(Phenoloxidase_Activity), quant = quantile(Phenoloxidase_Activity, probs= 0.75)) %>%
  arrange(desc(mean))

#extracting the compact letter display and adding to the Tk table 
cld1 <- as.data.frame.list(cld1$Site)
Tk1$cld1 <- cld1$Letters
print(Tk1)

#boxplot

ggplot(data, aes(x = Site, y = Phenoloxidase_Activity, fill = Site)) +
  geom_boxplot(show.legend = FALSE) +
  labs(title = "Phenoloxidase Activity by Site",
       x = "Site",
       y = "Phenoloxidase Activity(µmol/gsoil/h)") +
  theme_classic(base_family = "serif") +
  geom_text(data= Tk1, aes(label=cld1, x= Site, y = quant),
            vjust = -1, hjust = -1, size = 3) 




