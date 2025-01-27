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

data <- read_excel("Phenoloxidase Activity data/Final_data.xlsx")
view(data)
head(data)
str(data)
summary(data)
glimpse(data)

ggplot(data, aes(x = Site,
                 y = Phenoloxidase_Activity,
                 fill = Site)) + geom_boxplot()
plot_ly(
  data = data,
  y = ~Phenoloxidase_Activity,
  x = ~Site,
  type = "box",
  color = ~Site,
  showlegend = FALSE
)

plot_ly(
  data = data,
  y = ~Phenoloxidase_Activity,
  x = ~G_2022,
  type = "scatter",
  color = ~Site,
  showlegend = TRUE
)

 
plot_ly(
  data = data,
  y = ~Phenoloxidase_Activity,
  x = ~M_2022,
  type = "scatter",
  color = ~Site,
  showlegend = TRUE
)

plot_ly(
  data = data,
  y = ~Phenoloxidase_Activity,
  x = ~F_2022,
  type = "scatter",
  color = ~Site,
  showlegend = TRUE
)

plot_ly(
  data = data,
  y = ~Total_N,
  x = ~F_2022,
  type = "scatter",
  color = ~Site,
  showlegend = TRUE
)

plot_ly(
  data = data,
  y = ~Phenoloxidase_Activity,
  x = ~pH,
  type = "scatter",
  color = ~Site,
  showlegend = TRUE
)


plot_ly(
  data = data,
  y = ~Phenoloxidase_Activity,
  x = ~Organic_C,
  type = "scatter",
  color = ~Site,
  showlegend = TRUE
)


plot_ly(
  data = data,
  y = ~Phenoloxidase_Activity,
  x = ~Total_C,
  type = "scatter",
  color = ~Site,
  showlegend = TRUE
)



plot_ly(
  data = data,
  y = ~Phenoloxidase_Activity,
  x = ~Total_N,
  type = "scatter",
  color = ~Site,
  showlegend = TRUE
)

plot_ly(
  data = data,
  y = ~Phenoloxidase_Activity,
  x = ~LUI_index_2022,
  type = "scatter",
  color = ~Site,
  showlegend = TRUE
)

plot_ly(
  data = data,
  y = ~Phenoloxidase_Activity,
  x = ~`LUI_index_(2018-2022)`,
  type = "scatter",
  color = ~Site,
  showlegend = TRUE
)

plot_ly(
  data = data,
  y = ~Phenoloxidase_Activity,
  x = ~`Soil Type`,
  type = "box",
  color = ~`Soil Type`,
  showlegend = FALSE
)



plot_ly(
  data = data,
  y = ~Phenoloxidase_Activity,
  x = ~Phenol_Content,
  type = "scatter",
  color = ~Site,
  showlegend = TRUE
)

library(ggplot2)
library(ggpubr)
library(readxl)

# Correlation of pH and Phenoloxidase Activity

ggplot(data, aes(x = pH, y = Phenoloxidase_Activity)) + geom_point(size = 1) + 
  geom_smooth(method = "lm", se=FALSE, color ="grey", 
              formula = y~x ) +
  stat_cor(label.x = 5, label.y = 40, size =4)+ 
  stat_regline_equation(label.x = 5, label.y = 38, size = 4) + 
  labs(x = "pH", y = "Phenoloxidase Activity") +
  theme_classic(base_family = "serif")

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
       y = "Phenoloxidase Activity") +
  theme_minimal()

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

ggplot(data, aes(x = Organic_C, y = Phenoloxidase_Activity)) + geom_point(size = 1) + 
  geom_smooth(method = "lm", se=FALSE, color ="grey", 
              formula = y~x ) +
  stat_cor(label.x = 300, label.y = 25, size =4)+ 
  stat_regline_equation(label.x = 300, label.y = 23, size = 4) + 
  labs(x = "Organic Carbon", y = "Phenoloxidase Activity") +
  theme_classic(base_family = "serif")

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
       x = "Organic Carbon",
       y = "Phenoloxidase Activity") +
  theme_minimal()

print(plot)


lm_fit <- lm(y ~ x)


summary_fit <- summary(lm_fit)
r_squared <- summary_fit$r.squared
p_value <- summary_fit$coefficients[2, 4]


plot + annotate("text", x = 200, y = 30, 
                label = paste0("R² = ", round(r_squared, 3), 
                               "\nP = ", signif(p_value, 3)),
                hjust = 1, size = 4, color = "black")


# Correlation of Total carbon and Phenoloxidase Activity

ggplot(data, aes(x = Total_C, y = Phenoloxidase_Activity)) + geom_point(size = 1) + 
  geom_smooth(method = "lm", se=FALSE, color ="grey", 
              formula = y~x ) +
  stat_cor(label.x = 300, label.y = 30, size =4)+ 
  stat_regline_equation(label.x = 300, label.y = 28, size = 4) + 
  labs(x = "Total Carbon", y = "Phenoloxidase Activity") +
  theme_classic(base_family = "serif")

x <- data$Total_C
y <- data$Phenoloxidase_Activity 
correlation_result <- cor.test(x, y)
print(correlation_result)

r_squared <- correlation_result$estimate^2
print(r_squared)

plot <- ggplot(data, aes(x = Total_C, y = Phenoloxidase_Activity, colour = Site)) +
  geom_point() +
  geom_smooth(method = "lm", color = "grey", se = FALSE) +
  labs(title = "Correlation between Total Carbon and Phenoloxidase Activity",
       x = "Total Carbon",
       y = "Phenoloxidase Activity") +
  theme_minimal()

print(plot)


lm_fit <- lm(y ~ x)


summary_fit <- summary(lm_fit)
r_squared <- summary_fit$r.squared
p_value <- summary_fit$coefficients[2, 4]


plot + annotate("text", x = 300, y = 25, 
                label = paste0("R² = ", round(r_squared, 6), 
                               "\nP = ", signif(p_value, 3)),
                hjust = 1, size = 4, color = "black")



# Correlation of Total nitrogen and Phenoloxidase Activity

ggplot(data, aes(x = Total_N, y = Phenoloxidase_Activity)) + geom_point(size = 1) + 
  geom_smooth(method = "lm", se=FALSE, color ="grey", 
              formula = y~x ) +
  stat_cor(label.x = 20, label.y = 25, size =4)+ 
  stat_regline_equation(label.x = 20, label.y = 23, size = 4) + 
  labs(x = "Total Nitrogen", y = "Phenoloxidase Activity") +
  theme_classic(base_family = "serif")

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
       x = "Total Nitrogen",
       y = "Phenoloxidase Activity") +
  theme_minimal()

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

ggplot(data, aes(x = Phenol_Content, y = Phenoloxidase_Activity)) + geom_point(size = 1) + 
  geom_smooth(method = "lm", se=FALSE, color ="grey", 
              formula = y~x ) +
  stat_cor(label.x = 100, label.y = 40, size =4)+ 
  stat_regline_equation(label.x = 100, label.y = 38, size = 4) + 
  labs(x = "Phenol content", y = "Phenoloxidase Activity") +
  theme_classic(base_family = "serif")

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
       x = "Phenol Content",
       y = "Phenoloxidase Activity") +
  theme_minimal()

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

ggplot(data, aes(x = LUI_index_2022, y = Phenoloxidase_Activity)) + geom_point(size = 1) + 
  geom_smooth(method = "lm", se=FALSE, color ="grey", 
              formula = y~x ) +
  stat_cor(label.x = 2, label.y = 40, size =4)+ 
  stat_regline_equation(label.x = 2, label.y = 35, size = 4) + 
  labs(x = "LUI index 2022", y = "Phenoloxidase Activity") +
  theme_classic(base_family = "serif")

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
       y = "Phenoloxidase Activity") +
  theme_minimal()

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

ggplot(data, aes(x = G_2022, y = Phenoloxidase_Activity)) + geom_point(size = 1) + 
  geom_smooth(method = "lm", se=FALSE, color ="grey", 
              formula = y~x ) +
  stat_cor(label.x = 5, label.y = 40, size =4)+ 
  stat_regline_equation(label.x = 5, label.y = 35, size = 4) + 
  labs(x = "Gazing 2022", y = "Phenoloxidase Activity") +
  theme_classic(base_family = "serif")

x <- data$G_2022
y <- data$Phenoloxidase_Activity 
correlation_result <- cor.test(x, y)
print(correlation_result)

r_squared <- correlation_result$estimate^2
print(r_squared)

plot <- ggplot(data, aes(x = G_2022, y = Phenoloxidase_Activity, colour = Site)) +
  geom_point() +
  geom_smooth(method = "lm", color = "grey", se = FALSE) +
  labs(title = "Correlation between Gazing index 2022 and Phenoloxidase Activity",
       x = "Gazing index 2022",
       y = "Phenoloxidase Activity") +
  theme_minimal()

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

ggplot(data, aes(x = M_2022, y = Phenoloxidase_Activity)) + geom_point(size = 1) + 
  geom_smooth(method = "lm", se=FALSE, color ="grey", 
              formula = y~x ) +
  stat_cor(label.x = 1, label.y = 40, size =4)+ 
  stat_regline_equation(label.x = 1, label.y = 35, size = 4) + 
  labs(x = "Mowing 2022", y = "Phenoloxidase Activity") +
  theme_classic(base_family = "serif")

x <- data$M_2022
y <- data$Phenoloxidase_Activity 
correlation_result <- cor.test(x, y)
print(correlation_result)

r_squared <- correlation_result$estimate^2
print(r_squared)

plot <- ggplot(data, aes(x = M_2022, y = Phenoloxidase_Activity, colour = Site)) +
  geom_point() +
  geom_smooth(method = "lm", color = "grey", se = FALSE) +
  labs(title = "Correlation between Mowing index 2022 and Phenoloxidase Activity",
       x = "Mowing index 2022",
       y = "Phenoloxidase Activity") +
  theme_minimal()

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

ggplot(data, aes(x = F_2022, y = Phenoloxidase_Activity)) + geom_point(size = 1) + 
  geom_smooth(method = "lm", se=FALSE, color ="grey", 
              formula = y~x ) +
  stat_cor(label.x = 5, label.y = 40, size =4)+ 
  stat_regline_equation(label.x = 5, label.y = 35, size = 4) + 
  labs(x = "Fertilizer 2022", y = "Phenoloxidase Activity") +
  theme_classic(base_family = "serif")

x <- data$F_2022
y <- data$Phenoloxidase_Activity 
correlation_result <- cor.test(x, y)
print(correlation_result)

r_squared <- correlation_result$estimate^2
print(r_squared)

plot <- ggplot(data, aes(x = F_2022, y = Phenoloxidase_Activity, colour = Site)) +
  geom_point() +
  geom_smooth(method = "lm", color = "grey", se = FALSE) +
  labs(title = "Correlation between Fertilizer index 2022 and Phenoloxidase Activity",
       x = "Fertilizer index 2022",
       y = "Phenoloxidase Activity") +
  theme_minimal()

print(plot)


lm_fit <- lm(y ~ x)


summary_fit <- summary(lm_fit)
r_squared <- summary_fit$r.squared
p_value <- summary_fit$coefficients[2, 4]


plot + annotate("text", x = 3, y = 40, 
                label = paste0("R² = ", round(r_squared, 6), 
                               "\nP = ", signif(p_value, 3)),
                hjust = 1, size = 4, color = "black")



# Correlation of LUI index (2018-2022) and Phenoloxidase Activity

ggplot(data, aes(x = `LUI_index_(2018-2022)`, y = Phenoloxidase_Activity)) + geom_point(size = 1) + 
  geom_smooth(method = "lm", se=FALSE, color ="grey", 
              formula = y~x ) +
  stat_cor(label.x = 2, label.y = 40, size =4)+ 
  stat_regline_equation(label.x = 2, label.y = 38, size = 4) + 
  labs(x = "LUI index (2018-2022)", y = "Phenoloxidase Activity") +
  theme_classic(base_family = "serif")

x <- data$`LUI_index_(2018-2022)`
y <- data$Phenoloxidase_Activity 
correlation_result <- cor.test(x, y)
print(correlation_result)

r_squared <- correlation_result$estimate^2
print(r_squared)

plot <- ggplot(data, aes(x = `LUI_index_(2018-2022)`, y = Phenoloxidase_Activity, colour = Site)) +
  geom_point() +
  geom_smooth(method = "lm", color = "grey", se = FALSE) +
  labs(title = "Correlation between LUI index (2018-2022) and Phenoloxidase Activity",
       x = "LUI index (2018-2022)",
       y = "Phenoloxidase Activity") +
  theme_minimal()

print(plot)


lm_fit <- lm(y ~ x)


summary_fit <- summary(lm_fit)
r_squared <- summary_fit$r.squared
p_value <- summary_fit$coefficients[2, 4]


plot + annotate("text", x = 3, y = 40, 
                label = paste0("R² = ", round(r_squared, 6), 
                               "\nP = ", signif(p_value, 3)),
                hjust = 1, size = 4, color = "black")

my_data <- data %>%
  select(Soil_Type, Phenoloxidase_Activity) %>%
  drop_na()

mod1 <- aov(Phenoloxidase_Activity ~ Soil_Type, data = my_data)

summary(mod1)



#using pipes
data %>%
  select(Soil_Type, Phenoloxidase_Activity) %>% 
  drop_na() %>%
  aov(Phenoloxidase_Activity ~ Soil_Type, data = .) %>%
  summary()
library(patchwork)
install.packages("gapminder")
library(gapminder)
library(forcats)
anova_result <- aov (Phenoloxidase_Activity ~ Soil_Type , data = data)
summary(anova_result)
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

ggplot(data, aes(x = Soil_Type, y = Phenoloxidase_Activity, fill = Soil_Type)) +
  geom_boxplot() +
  labs(title = "Phenoloxidase Activity by Soil Type",
       x = "Soil Type",
       y = "Phenoloxidase Activity") +
  theme_minimal()


# Extract Tukey HSD results for SoilType and convert to data frame
tukey_df <- as.data.frame(tukey_result$Soil_Type)

# View the data frame
print(tukey_df)
write.csv(tukey_df, "TukeyHSD_Results.csv")


# Save the boxplot
ggsave("Phenoloxidase_Boxplot.png")


anova_result1 <- aov (Phenoloxidase_Activity ~ Site , data = data)
summary(anova_result1)
tukey_result1 <- TukeyHSD(anova_result1)
print(tukey_result1)

ggplot(data, aes(x = Site, y = Phenoloxidase_Activity, fill = Site)) +
  geom_boxplot() +
  labs(title = "Phenoloxidase Activity by Site",
       x = "Site",
       y = "Phenoloxidase Activity") +
  theme_minimal()


# Extract Tukey HSD results for SoilType and convert to data frame
tukey_df1 <- as.data.frame(tukey_result1$Site)

# View the data frame
print(tukey_df1)
write.csv(tukey_df1, "TukeyHSD_Results1.csv")


# Save the boxplot
ggsave("Phenoloxidaseandsite_Boxplot.png")

# T test

# Subset data for two soil types, e.g., Soil1 and Soil2
data_subset <- subset(data, Soil_Type %in% c("Soil1", "Soil2"))

# Perform an independent two-sample t-test
t_test_result <- t.test(PhenoloxidaseActivity ~ SoilType, data = data_subset, var.equal = TRUE)

# View the results
print(t_test_result)

pairwise_t_test_result <- pairwise.t.test(data$PhenoloxidaseActivity, data$SoilType, p.adjust.method = "bonferroni")

# View the pairwise t-test results
print(pairwise_t_test_result)


