colors()
library(ggplot2)

# Define fixed colors for each soil type
soil_colors <- c("Cambisol" = "orange", "Albeluvisol" = "grey", 
                  "Gleysol" = "greenyellow", "Histosol" = "limegreen", 
                  "Leptosol" = "cyan", "Luvisol" = "gold",
                  "Stagnosol" = "purple", "Vertisol" = "magenta")

# Create the box plot
ggplot(data, aes(x = Soil_Type, y = Phenoloxidase_Activity, fill = Soil_Type)) +
  geom_boxplot(show.legend = FALSE) +
  scale_fill_manual(values = soil_colors) +  # Apply the fixed colors
  labs(
    title = "Phenoloxidase Activity by Soil Type",
    x = "Soil Type",
    y = "Phenoloxidase Activity"
  ) + 
  theme_minimal()+
  theme_classic(base_family = "serif")+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),  # Bold title
    axis.title.x = element_text(face = "bold"),  # Bold x-axis label
    axis.title.y = element_text(face = "bold"),   # Bold y-axis label
    axis.text = element_text(face = "bold", size = 9)  # Bold axis numbers (both x and y)
  )


library(ggplot2)

# Define a fixed color palette
soil_colors <- c("Cambisol" = "orange", "Albeluvisol" = "grey", 
                 "Gleysol" = "greenyellow", "Histosol" = "limegreen", 
                 "Leptosol" = "cyan", "Luvisol" = "gold",
                 "Stagnosol" = "purple", "Vertisol" = "magenta")

# Save the color mapping globally
update_geom_defaults("boxplot", list(fill = soil_colors))




# Correlation of Organic carbon and Phenoloxidase Activity

x <- data$Organic_C
y <- data$Phenoloxidase_Activity 
correlation_result <- cor.test(x, y)
print(correlation_result)

r_squared <- correlation_result$estimate^2
print(r_squared)
soil_colors <- c("Cambisol" = "orange", "Albeluvisol" = "grey", 
                 "Gleysol" = "greenyellow", "Histosol" = "limegreen", 
                 "Leptosol" = "cyan", "Luvisol" = "gold",
                 "Stagnosol" = "purple", "Vertisol" = "magenta")

plot <- ggplot(data, aes(x = Organic_C, y = Phenoloxidase_Activity, colour = Soil_Type)) +
  geom_point() +
  scale_color_manual(values = soil_colors) +
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



library(ggplot2)

# Scatter plot with multiple regression lines
ggplot(data, aes(x = Organic_C, y = Phenoloxidase_Activity, color = Soil_Type)) +
  geom_point(size = 2) +  # Scatter points
  geom_smooth(method = "lm", se = FALSE) +  
  scale_color_manual(values = soil_colors)+
  theme_minimal() +
  labs(title = "Regression Lines for Different Soil Types",
       x = "Soil Organic Carbon",
       y = "Phenoloxidase Activity")

#Alternative : Facet Wrap for Separate Plots
# If you want separate plots for each soil type

ggplot(data, aes(x = Organic_C, y = Phenoloxidase_Activity)) +
  geom_point(size = 2, aes(color = Soil_Type)) +
  geom_smooth(method = "lm", se = FALSE, aes(color = Soil_Type)) +
  facet_wrap(~Soil_Type) +  # Creates separate plots for each soil type
  theme_minimal() +
  labs(title = "Regression Lines for Different Soil Types",
       x = "Soil Organic Carbon",
       y = "Phenoloxidase Activity")


# Add P values and R-squared for each soil type

library(ggplot2)
install.packages("ggpmics")
library(ggpmisc)


# Define regression equation format
formula <- y ~ x

# Scatter plot with regression lines and statistics
ggplot(data, aes(x = Organic_C, y = Phenoloxidase_Activity, color = Soil_Type)) +
  geom_point(size = 2) +  # Scatter points
  geom_smooth(method = "lm", se = FALSE) +  # Regression lines
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., ..p.value.label.., sep = "~~~")), 
    formula = formula, 
    parse = TRUE, 
    label.x.npc = "right",  # Position text on the right side
    label.y.npc = "top"  # Position text at the top
  ) +
  scale_color_manual(values = soil_colors) +  # Fixed colors
  theme_minimal() +
  labs(title = "Regression Lines with P-Values and R²",
       x = "Soil Organic Carbon",
       y = "Phenoloxidase Activity")



library(ggplot2)
library(ggpmisc)


# Define regression formula
formula <- y ~ x

# Scatter plot with regression lines and statistics
ggplot(data, aes(x = Organic_C, y = Phenoloxidase_Activity, color = Soil_Type)) +
  geom_point(size = 2) +  # Scatter points
  geom_smooth(method = "lm", se = FALSE) +  # Regression lines
  stat_fit_glance(method = "lm",
                  method.args = list(formula = formula),
                  aes(label = paste("R² =", signif(..r.squared.., 3), 
                                    "p =", signif(..p.value.., 3))),
                  label.x = "right",  # Position: "left", "center", "right"
                  label.y = "top",  # Position: "bottom", "middle", "top"
                  size = 5,  # Adjust text size
                  color = "black") +  # Text color
  scale_color_manual(values = soil_colors) +  # Fixed colors
  theme_minimal() +
  labs(title = "Regression Lines with P-Values and R²",
       x = "Soil Organic Carbon",
       y = "Phenoloxidase Activity") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))  # Center title
