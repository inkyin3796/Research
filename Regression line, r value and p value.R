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


