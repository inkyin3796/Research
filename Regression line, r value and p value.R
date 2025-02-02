#Plot linear regression with adding equation and linear equation
library(ggplot2)
library(ggpubr)
library(readxl)
SEG <- read_excel("Phenoloxidase Activity data/SEG.xlsx")
view(SEG)

# Check column names and structure
str(SEG)

ggplot(SEG, aes(x = pH, y = Phenoloxidase_Activity)) + geom_point(size = 1) + 
  geom_smooth(method = "lm", se=FALSE, color ="black", 
              formula = y~x ) +
stat_cor(label.x = 5, label.y = 18, size =4)+ 
  stat_regline_equation(label.x = 5, label.y = 19, size = 4) + 
  labs(x = "pH", y = "Phenoloxidase Activity") +
  theme_classic(base_family = "serif")

ggplot(SEG, aes(x = pH, y = Phenoloxidase_Activity)) + geom_point(size = 1) + 
  geom_smooth(method = "lm", se=FALSE, color ="black", 
              formula = y~x ) +
  stat_cor(label.x = 5, label.y = 18, size =4)+ 
  stat_regline_equation(label.x = 5, label.y = 19, size = 4) + 
  labs(x = "pH", y = "Phenoloxidase Activity") +
  theme_classic(base_family = "serif") + 
  scale_x_continuous(limits = c(5,8), breaks = scales::breaks_width(1)) +
  scale_y_continuous(limits = c(5,25), breaks = scales::breaks_width(5))


# Extract the columns of interest
x <- SEG$pH  # Replace 'column1' with your column name
y <- SEG$Phenoloxidase_Activity  # Replace 'column2' with your column name
correlation_result <- cor.test(x, y)
print(correlation_result)

r_squared <- correlation_result$estimate^2
print(r_squared)

# Create the scatter plot with a regression line
plot <- ggplot(SEG, aes(x = pH, y = Phenoloxidase_Activity)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Correlation between pH and Phenoloxidase Activity",
       x = "pH",
       y = "Phenoloxidase Activity") +
  theme_minimal()

print(plot)

# Fit the linear model
lm_fit <- lm(y ~ x)

# Extract R-squared and p-value
summary_fit <- summary(lm_fit)
r_squared <- summary_fit$r.squared
p_value <- summary_fit$coefficients[2, 4]

# Add annotations to the plot
plot + annotate("text", x = max(x), y = max(y), 
                label = paste0("R² = ", round(r_squared, 3), 
                               "\nP = ", signif(p_value, 3)),
                hjust = 1, size = 5, color = "black")




# Perform ANOVA
anova_result <- aov (Phenoloxidase_Activity ~ Soil_Type , data = data)

# Get the p-value from the ANOVA summary
anova_summary <- summary(anova_result)
p_value <- anova_summary[[1]][["Pr(>F)"]][1]
library(ggplot2)

# Basic boxplot
plot <- ggplot(data, aes(x = Soil_Type, y = Phenoloxidase_Activity, fill = Soil_Type)) +
  geom_boxplot() +
  labs(title = "Phenoloxidase Activity by Soil Type",
       x = "Soil Type",
       y = "Phenoloxidase Activity (micromol/gsoil/hour)") +
  theme_minimal()

plot
# Add ANOVA result to the plot
plot <- plot +
  annotate("text", x = 2, y = max(data$Phenoloxidase_Activity) + 2, 
           label = paste("ANOVA p-value =", signif(p_value, digits = 3)),
           size = 5, color = "grey")

plot

# Install and load ggpubr
install.packages("ggpubr")
library(ggpubr)

# Perform pairwise comparisons with Tukey's HSD
tukey_result <- TukeyHSD(anova_result)

# Add pairwise comparisons to the plot
plot <- ggplot(data, aes(x = "Soil_Type", y = "Phenoloxidase_Activity", fill = "Soil_Type")) +
  stat_compare_means(method = "anova", label = "p.signif") + # Add ANOVA result
  stat_compare_means(method = "tukey", label = "p.format", 
                     comparisons = list(c("Cambisol", "Albeluvisol"), 
                                        c("Gleysol", "Albeluvisol"), 
                                        c("Histosol", "Albeluvisol"),
                                        c("Leptosol", "Albeluvisol"), 
                                        c("Luvisol", "Albeluvisol"), 
                                        c("Stagnosol", "Albeluvisol"),
                                        c("Vertisol", "Albeluvisol"), 
                                        c("Gleysol", "Cambisol"), 
                                        c("Histosol", "Cambisol"),
                                        c("Leptosol", "Cambisol"), 
                                        c("Luvisol", "Cambisol"), 
                                        c("Stagnosol", "Cambisol"),
                                        c("Vertisol", "Cambisol"), 
                                        c("Histosol", "Gleysol"), 
                                        c("Leptosol", "Gleysol"),
                                        c("Luvisol", "Gleysol"), 
                                        c("Stagnosol", "Gleysol"), 
                                        c("Vertisol", "Gleysol"),
                                        c("Leptosol", "Histosol"), 
                                        c("Luvisol", "Histosol"), 
                                        c("Stagnosol", "Histosol"),
                                        c("Vertisol", "Histosol"), 
                                        c("Luvisol", "Leptosol"), 
                                        c("Stagnosol", "Leptosol"),
                                        c("Vertisol", "Leptosol"), 
                                        c("Stagnosol", "Luvisol"), 
                                        c("Vertisol", "Luvisol"),
                                        c("Vertisol", "Stagnosol")))

plot

# Perform ANOVA
anova_result1 <- aov (Phenoloxidase_Activity ~ Site , data = data)

# Get the p-value from the ANOVA summary
anova_summary <- summary(anova_result)
p_value <- anova_summary[[1]][["Pr(>F)"]][1]
library(ggplot2)

# Basic boxplot
plot <- ggplot(data, aes(x = Site, y = Phenoloxidase_Activity, fill = Site)) +
  geom_boxplot() +
  labs(title = "Phenoloxidase Activity by Soil Type",
       x = "Soil Type",
       y = "Phenoloxidase Activity (micromol/gsoil/hour)") +
  theme_minimal()

plot
# Add ANOVA result to the plot
plot <- plot +
  annotate("text", x = 2, y = max(data$Phenoloxidase_Activity) + 2, 
           label = paste("ANOVA p-value =", signif(p_value, digits = 3)),
           size = 5, color = "grey")

plot


# Perform pairwise comparisons with Tukey's HSD
tukey_result <- TukeyHSD(anova_result)

# Add pairwise comparisons to the plot
plot <- ggboxplot(data, x = "Site", y = "Phenoloxidase_Activity", fill = "Site") +
  stat_compare_means(method = "anova", label = "p.signif") + # Add ANOVA result
  stat_compare_means(method = "tukey", label = "p.format", 
                     comparisons = list(c("AEG", "HEG"), 
                                        c("AEG", "SEG"), 
                                        c("HEG", "SEG")))

plot
library(ggpubr)

# Boxplot with ANOVA p-value
plot <- ggboxplot(data, x = "Site", y = "Phenoloxidase_Activity", fill = "Site") +
  stat_compare_means(method = "anova", label = "p.format")  # Add ANOVA p-value

print(plot)

# Perform ANOVA
anova_result <- aov(Phenoloxidase_Activity ~ Site, data = data)

# Perform Tukey's HSD
tukey_result <- TukeyHSD(anova_result)

# Convert Tukey results to a data frame for better handling
tukey_df <- as.data.frame(tukey_result$Site)
print(tukey_df)

# Define pairwise comparisons
comparisons <- list(c("HEG", "AEG"), c("SEG", "AEG"), c("SEG", "HEG"))

# Add Tukey's p-values as labels
plot <- ggboxplot(data, x = "Site", y = "Phenoloxidase_Activity", fill = "Site") +
  stat_compare_means(method = "anova", label = "p.format") +  # ANOVA p-value
  stat_pvalue_manual(tukey_df, label = "p adj", 
                     comparisons = comparisons)  # Tukey's p-values

print(plot)

# Check the summary data and the letters
print(summary_data)

geom_text(aes(label = letters, y = mean_activity + se_activity + 0.5), size = 5)

plot <- plot +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(summary_data$mean_activity + summary_data$se_activity) + 1))

# Remove rows with NA letters
summary_data <- summary_data %>% filter(!is.na(letters))

# Update the plot
plot <- ggplot(summary_data, aes(x = Site, y = mean_activity, fill = Site)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = mean_activity - se_activity, ymax = mean_activity + se_activity), 
                width = 0.2) +
  geom_text(aes(label = letters, y = mean_activity + se_activity + 0.5), size = 5) +
  labs(
    title = "Phenoloxidase Activity by Site",
    x = "Site",
    y = "Mean Phenoloxidase Activity (micromol/gsoil/hour)"
  ) +
  theme_minimal()

print(plot)



library(ggplot2)
library(dplyr)
library(multcompView)

# ANOVA
anova_result <- aov(Phenoloxidase_Activity ~ Site, data = data)

# Tukey's HSD and group letters
tukey_result <- TukeyHSD(anova_result)

# Extract Tukey's results into a data frame
tukey_df <- as.data.frame(tukey_result$Site)
tukey_df$comparison <- rownames(tukey_df)  # Add rownames as a new column

# View the resulting data frame
print(tukey_df)

letters <- multcompLetters(tukey_matrix)

# Summary data
summary_data <- data %>%
  group_by(Site) %>%
  summarise(
    mean_activity = mean(Phenoloxidase_Activity),
    se_activity = sd(Phenoloxidase_Activity) / sqrt(n())
  )
summary_data$letters <- letters$Letters[summary_data$Site]

# Fix missing values
summary_data <- summary_data %>% filter(!is.na(letters))

# Plot
plot <- ggplot(summary_data, aes(x = Site, y = mean_activity, fill = Site)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = mean_activity - se_activity, ymax = mean_activity + se_activity), 
                width = 0.2) +
  geom_text(aes(label = letters, y = mean_activity + se_activity + 0.5), size = 5) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(summary_data$mean_activity + summary_data$se_activity) + 1)) +
  labs(
    title = "Phenoloxidase Activity by Soil Type",
    x = "Soil Type",
    y = "Mean Phenoloxidase Activity"
  ) +
  theme_minimal()

print(plot)



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

