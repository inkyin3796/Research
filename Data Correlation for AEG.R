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

AEG <- read_excel("Phenoloxidase Activity data/AEG.xlsx")
view(AEG)
str(AEG)
summary(AEG)
glimpse(AEG)

ggplot(AEG, aes(x = Site,
                 y = Phenoloxidase_Activity,
                fill = Site)) + geom_boxplot()

plot_ly(
  data = AEG,
  y = ~Phenoloxidase_Activity,
  x = ~Site,
  type = "box",
  color = ~Site,
  showlegend = TRUE
)

plot_ly(
  data = AEG,
  y = ~Phenoloxidase_Activity,
  x = ~G_2022,
  type = "scatter",
  color = ~Site,
  showlegend = TRUE
)


plot_ly(
  data = AEG,
  y = ~Phenoloxidase_Activity,
  x = ~M_2022,
  type = "scatter",
  color = ~Site,
  showlegend = TRUE
)

plot_ly(
  data = AEG,
  y = ~Phenoloxidase_Activity,
  x = ~F_2022,
  type = "scatter",
  color = ~Site,
  showlegend = TRUE
)

plot_ly(
  data = AEG,
  y = ~Total_N,
  x = ~F_2022,
  type = "scatter",
  color = ~Site,
  showlegend = TRUE
)

plot_ly(
  data  = AEG,
  y = ~Phenoloxidase_Activity,
  x = ~pH,
  type = "scatter",
  color = ~Site,
  showlegend = TRUE
)


plot_ly(
  data  = AEG,
  y = ~Phenoloxidase_Activity,
  x = ~Organic_C,
  type = "scatter",
  color = ~Site,
  showlegend = TRUE
)


plot_ly(
  data  = AEG,
  y = ~Phenoloxidase_Activity,
  x = ~Total_C,
  type = "scatter",
  color = ~Site,
  showlegend = TRUE
)



plot_ly(
  data  = AEG,
  y = ~Phenoloxidase_Activity,
  x = ~Total_N,
  type = "scatter",
  color = ~Site,
  showlegend = TRUE
)

plot_ly(
  data  = AEG,
  y = ~Phenoloxidase_Activity,
  x = ~LUI_index_2022,
  type = "scatter",
  color = ~Site,
  showlegend = TRUE
)

plot_ly(
  data  = AEG,
  y = ~Phenoloxidase_Activity,
  x = ~`LUI_index_(2018-2022)`,
  type = "scatter",
  color = ~Site,
  showlegend = TRUE
)

plot_ly(
  data  = AEG,
  y = ~Phenoloxidase_Activity,
  x = ~`Soil Type`,
  type = "box",
  color = ~`Soil Type`,
  showlegend = TRUE
)

plot_ly(
  data  = AEG,
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

ggplot(AEG, aes(x = pH, y = Phenoloxidase_Activity)) + geom_point(size = 1) + 
  geom_smooth(method = "lm", se=FALSE, color ="grey", 
              formula = y~x ) +
  stat_cor(label.x = 5, label.y = 45, size =4)+ 
  stat_regline_equation(label.x = 5, label.y = 40, size = 4) + 
  labs(x = "pH", y = "Phenoloxidase Activity") +
  theme_classic(base_family = "serif")

a <- AEG$pH 
b <- AEG$Phenoloxidase_Activity 
correlation_result <- cor.test(a, b)
print(correlation_result)

r_squared <- correlation_result$estimate^2
print(r_squared)

plot <- ggplot(AEG, aes(x = pH, y = Phenoloxidase_Activity)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Correlation between pH and Phenoloxidase Activity in AEG",
       x = "pH",
       y = "Phenoloxidase Activity") +
  theme_minimal()

print(plot)


lm_fit <- lm(b ~ a)


summary_fit <- summary(lm_fit)
r_squared <- summary_fit$r.squared
p_value <- summary_fit$coefficients[2, 4]


plot + annotate("text", x = max(x), y = max(y), 
                label = paste0("R² = ", round(r_squared, 3), 
                               "\nP = ", signif(p_value, 3)),
                hjust = 1, size = 5, color = "black")



# Correlation of Organic carbon and Phenoloxidase Activity

ggplot(AEG, aes(x = Organic_C, y = Phenoloxidase_Activity)) + geom_point(size = 1) + 
  geom_smooth(method = "lm", se=FALSE, color ="grey", 
              formula = y~x ) +
  stat_cor(label.x = 50, label.y = 40, size =4)+ 
  stat_regline_equation(label.x = 50, label.y = 38, size = 4) + 
  labs(x = "Organic Carbon", y = "Phenoloxidase Activity") +
  theme_classic(base_family = "serif")

x <- AEG$Organic_C
y <- AEG$Phenoloxidase_Activity 
correlation_result <- cor.test(x, y)
print(correlation_result)

r_squared <- correlation_result$estimate^2
print(r_squared)

plot <- ggplot(AEG, aes(x = Organic_C, y = Phenoloxidase_Activity)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Correlation between Organic Carbon and Phenoloxidase Activity in AEG",
       x = "Organic Carbon",
       y = "Phenoloxidase Activity") +
  theme_minimal()

print(plot)


lm_fit <- lm(y ~ x)


summary_fit <- summary(lm_fit)
r_squared <- summary_fit$r.squared
p_value <- summary_fit$coefficients[2, 4]


plot + annotate("text", x = max(x), y = max(y), 
                label = paste0("R² = ", round(r_squared, 3), 
                               "\nP = ", signif(p_value, 3)),
                hjust = 1, size = 5, color = "black")


# Correlation of Total carbon and Phenoloxidase Activity

ggplot(AEG, aes(x = Total_C, y = Phenoloxidase_Activity)) + geom_point(size = 1) + 
  geom_smooth(method = "lm", se=FALSE, color ="grey", 
              formula = y~x ) +
  stat_cor(label.x = 50, label.y = 45, size =4)+ 
  stat_regline_equation(label.x = 50, label.y = 40, size = 4) + 
  labs(x = "Total Carbon", y = "Phenoloxidase Activity") +
  theme_classic(base_family = "serif")

x <- AEG$Total_C
y <- AEG$Phenoloxidase_Activity 
correlation_result <- cor.test(x, y)
print(correlation_result)

r_squared <- correlation_result$estimate^2
print(r_squared)

plot <- ggplot(AEG, aes(x = Total_C, y = Phenoloxidase_Activity)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Correlation between Total Carbon and Phenoloxidase Activity in AEG",
       x = "Total Carbon",
       y = "Phenoloxidase Activity") +
  theme_minimal()

print(plot)


lm_fit <- lm(y ~ x)


summary_fit <- summary(lm_fit)
r_squared <- summary_fit$r.squared
p_value <- summary_fit$coefficients[2, 4]


plot + annotate("text", x = max(x), y = max(y), 
                label = paste0("R² = ", round(r_squared, 3), 
                               "\nP = ", signif(p_value, 3)),
                hjust = 1, size = 5, color = "black")




# Correlation of Total nitrogen and Phenoloxidase Activity

ggplot(AEG, aes(x = Total_N, y = Phenoloxidase_Activity)) + geom_point(size = 1) + 
  geom_smooth(method = "lm", se=FALSE, color ="grey", 
              formula = y~x ) +
  stat_cor(label.x = 5, label.y = 45, size =4)+ 
  stat_regline_equation(label.x = 5, label.y = 40, size = 4) + 
  labs(x = "Total Nitrogen", y = "Phenoloxidase Activity") +
  theme_classic(base_family = "serif")

x <- AEG$Total_N
y <- AEG$Phenoloxidase_Activity 
correlation_result <- cor.test(x, y)
print(correlation_result)

r_squared <- correlation_result$estimate^2
print(r_squared)

plot <- ggplot(AEG, aes(x = Total_N, y = Phenoloxidase_Activity)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Correlation between Total Nitrogen and Phenoloxidase Activity in AEG",
       x = "Total Nitrogen",
       y = "Phenoloxidase Activity") +
  theme_minimal()

print(plot)


lm_fit <- lm(y ~ x)


summary_fit <- summary(lm_fit)
r_squared <- summary_fit$r.squared
p_value <- summary_fit$coefficients[2, 4]


plot + annotate("text", x = max(x), y = max(y), 
                label = paste0("R² = ", round(r_squared, 3), 
                               "\nP = ", signif(p_value, 3)),
                hjust = 1, size = 5, color = "black")




# Correlation of Phenol and Phenoloxidase Activity

ggplot(AEG, aes(x = Phenol_Content, y = Phenoloxidase_Activity)) + geom_point(size = 1) + 
  geom_smooth(method = "lm", se=FALSE, color ="grey", 
              formula = y~x ) +
  stat_cor(label.x = 5, label.y = 45, size =4)+ 
  stat_regline_equation(label.x = 5, label.y = 40, size = 4) + 
  labs(x = "Phenol content", y = "Phenoloxidase Activity") +
  theme_classic(base_family = "serif")

x <- AEG$Phenol_Content
y <- AEG$Phenoloxidase_Activity 
correlation_result <- cor.test(x, y)
print(correlation_result)

r_squared <- correlation_result$estimate^2
print(r_squared)

plot <- ggplot(AEG, aes(x = Phenol_Content, y = Phenoloxidase_Activity)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Correlation between Phenol Content and Phenoloxidase Activity in AEG",
       x = "Phenol Content",
       y = "Phenoloxidase Activity") +
  theme_minimal()

print(plot)


lm_fit <- lm(y ~ x)


summary_fit <- summary(lm_fit)
r_squared <- summary_fit$r.squared
p_value <- summary_fit$coefficients[2, 4]


plot + annotate("text", x = max(x), y = max(y), 
                label = paste0("R² = ", round(r_squared, 3), 
                               "\nP = ", signif(p_value, 3)),
                hjust = 1, size = 5, color = "black")



# Correlation of LUI index 2022 and Phenoloxidase Activity

ggplot(AEG, aes(x = LUI_index_2022, y = Phenoloxidase_Activity)) + geom_point(size = 1) + 
  geom_smooth(method = "lm", se=FALSE, color ="grey", 
              formula = y~x ) +
  stat_cor(label.x = 1, label.y = 45, size =4)+ 
  stat_regline_equation(label.x = 1, label.y = 40, size = 4) + 
  labs(x = "LUI index 2022", y = "Phenoloxidase Activity") +
  theme_classic(base_family = "serif")

x <- AEG$LUI_index_2022
y <- AEG$Phenoloxidase_Activity 
correlation_result <- cor.test(x, y)
print(correlation_result)

r_squared <- correlation_result$estimate^2
print(r_squared)

plot <- ggplot(AEG, aes(x = LUI_index_2022, y = Phenoloxidase_Activity)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Correlation between LUI index 2022 and Phenoloxidase Activity in AEG",
       x = "LUI index 2022",
       y = "Phenoloxidase Activity") +
  theme_minimal()

print(plot)


lm_fit <- lm(y ~ x)


summary_fit <- summary(lm_fit)
r_squared <- summary_fit$r.squared
p_value <- summary_fit$coefficients[2, 4]


plot + annotate("text", x = max(x), y = max(y), 
                label = paste0("R² = ", round(r_squared, 6), 
                               "\nP = ", signif(p_value, 3)),
                hjust = 1, size = 4, color = "black")

# Correlation of Gazing 2022 and Phenoloxidase Activity

ggplot(AEG, aes(x = G_2022, y = Phenoloxidase_Activity)) + geom_point(size = 1) + 
  geom_smooth(method = "lm", se=FALSE, color ="grey", 
              formula = y~x ) +
  stat_cor(label.x = 5, label.y = 40, size =4)+ 
  stat_regline_equation(label.x = 5, label.y = 35, size = 4) + 
  labs(x = "Gazing 2022", y = "Phenoloxidase Activity") +
  theme_classic(base_family = "serif")

x <- AEG$G_2022
y <- AEG$Phenoloxidase_Activity 
correlation_result <- cor.test(x, y)
print(correlation_result)

r_squared <- correlation_result$estimate^2
print(r_squared)

plot <- ggplot(AEG, aes(x = G_2022, y = Phenoloxidase_Activity, colour = Site)) +
  geom_point() +
  geom_smooth(method = "lm", color = "grey", se = FALSE) +
  labs(title = "Correlation between Gazing index 2022 and Phenoloxidase Activity in AEG",
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

ggplot(AEG, aes(x = M_2022, y = Phenoloxidase_Activity)) + geom_point(size = 1) + 
  geom_smooth(method = "lm", se=FALSE, color ="grey", 
              formula = y~x ) +
  stat_cor(label.x = 1, label.y = 40, size =4)+ 
  stat_regline_equation(label.x = 1, label.y = 35, size = 4) + 
  labs(x = "Mowing 2022", y = "Phenoloxidase Activity") +
  theme_classic(base_family = "serif")

x <- AEG$M_2022
y <- AEG$Phenoloxidase_Activity 
correlation_result <- cor.test(x, y)
print(correlation_result)

r_squared <- correlation_result$estimate^2
print(r_squared)

plot <- ggplot(AEG, aes(x = M_2022, y = Phenoloxidase_Activity, colour = Site)) +
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

ggplot(AEG, aes(x = F_2022, y = Phenoloxidase_Activity)) + geom_point(size = 1) + 
  geom_smooth(method = "lm", se=FALSE, color ="grey", 
              formula = y~x ) +
  stat_cor(label.x = 2, label.y = 40, size =4)+ 
  stat_regline_equation(label.x = 2, label.y = 35, size = 4) + 
  labs(x = "Fertilizer 2022", y = "Phenoloxidase Activity") +
  theme_classic(base_family = "serif")

x <- AEG$F_2022
y <- AEG$Phenoloxidase_Activity 
correlation_result <- cor.test(x, y)
print(correlation_result)

r_squared <- correlation_result$estimate^2
print(r_squared)

plot <- ggplot(AEG, aes(x = F_2022, y = Phenoloxidase_Activity, colour = Site)) +
  geom_point() +
  geom_smooth(method = "lm", color = "grey", se = FALSE) +
  labs(title = "Correlation between Fertilizer index 2022 and Phenoloxidase Activity",
       x = "Fertilizer index 2022",
       y = "Phenoloxidase Activity (micromol/gsoil/hour)") +
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

ggplot(AEG, aes(x = `LUI_index_(2018-2022)`, y = Phenoloxidase_Activity)) + geom_point(size = 1) + 
  geom_smooth(method = "lm", se=FALSE, color ="grey", 
              formula = y~x ) +
  stat_cor(label.x = 1, label.y = 45, size =4)+ 
  stat_regline_equation(label.x = 1, label.y = 40, size = 4) + 
  labs(x = "LUI index (2018-2022)", y = "Phenoloxidase Activity") +
  theme_classic(base_family = "serif")

x <- AEG$`LUI_index_(2018-2022)`
y <- AEG$Phenoloxidase_Activity 
correlation_result <- cor.test(x, y)
print(correlation_result)

r_squared <- correlation_result$estimate^2
print(r_squared)

plot <- ggplot(AEG, aes(x = `LUI_index_(2018-2022)`, y = Phenoloxidase_Activity)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Correlation between LUI index (2018-2022) and Phenoloxidase Activity 
       in AEG",
       x = "LUI index (2018-2022)",
       y = "Phenoloxidase Activity") +
  theme_minimal()

print(plot)


lm_fit <- lm(y ~ x)


summary_fit <- summary(lm_fit)
r_squared <- summary_fit$r.squared
p_value <- summary_fit$coefficients[2, 4]


plot + annotate("text", x = max(x), y = max(y), 
                label = paste0("R² = ", round(r_squared, 6), 
                               "\nP = ", signif(p_value, 3)),
                hjust = 1, size = 4, color = "black")



