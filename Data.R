install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("car")
library(car)
install.packages("ggpubr")
library(ggpubr)

# Install required packages
install.packages("readxl")
install.packages("dplyr")
install.packages("writexl", repos = "https://cloud.r-project.org/")


# Load libraries
library(readxl)
library(dplyr)
library(writexl)

# Step 1: Read Excel file
data <- read_excel("Phenoloxidase Activity data/Data_for_ R.xlsx")
View(data)

# Step 2: Remove outliers (2 SD from the mean)
filtered_data <- data %>%
  mutate(Mean = mean(`micromol*g soil*hour`, na.rm = TRUE), 
         SD = sd(`micromol*g soil*hour`, na.rm = TRUE)) %>%
  filter(`micromol*g soil*hour` >= Mean - 2 * SD & `micromol*g soil*hour` <= Mean + 2 * SD)

# Step 3: Save the filtered data back to Excel
write_xlsx(filtered_data, "filtered_data.xlsx")

# Done!
cat("Filtered data saved to 'filtered_data.xlsx'")

# Install and load the package
install.packages("outliers")
library(outliers)

# Example data
data <- c(18.69, 17.68, 17.93, 21.46, 25.25, 24.49)


# Test for normality
shapiro.test(data)

# Perform Grubbs' Test
result_AEG1 <- grubbs.test(data)

# Print results
print(result_AEG1)

# Remove the detected outlier if p-value < 0.05
if (result_AEG1$p.value < 0.05) {
  data <- data[data != max(data)]
  cat("Outlier removed. Updated data:", data, "\n")
}
