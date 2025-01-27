# Install and load the package
install.packages("outliers")
library(outliers)

# AEG 1 
AEG1 <- c(18.69, 17.68, 17.93, 21.46, 25.25, 24.49)


# Test for normality
shapiro.test(AEG1)

# Perform Grubbs' Test
result_AEG1 <- grubbs.test(AEG1)

# Print results
print(result_AEG1)

# Remove the detected outlier if p-value < 0.05
if (result_AEG1$p.value < 0.05) {
  AEG1 <- AEG1[AEG1 != max(AEG1)]
  cat("Outlier removed. Updated data:", AEG1, "\n")
}

# AEG 2 
AEG2 <- c(69.86, 62.45, 54.78, 53.30, 47.98, 50.08)

# Test for normality
shapiro.test(AEG2)

# Perform Grubbs' Test
result_AEG2 <- grubbs.test(AEG2)

# Print results
print(result_AEG2)



# AEG 3 
AEG3 <- c(9.48, 17.53, 17.30, 13.98, 17.30, 19.43)


# Test for normality
shapiro.test(AEG3)

# Perform Grubbs' Test
result_AEG3 <- grubbs.test(AEG3)

# Print results
print(result_AEG3)


# AEG 4
AEG4 <- c(11.71, 17.68, 9.96, 9.46, 19.67)


# Test for normality
shapiro.test(AEG4)

# Perform Grubbs' Test
result_AEG4 <- grubbs.test(AEG4)

# Print results
print(result_AEG4)


# AEG 5
AEG5 <- c(35.79, 33.42, 33.42, 26.81, 37.91, 28.40)

# Test for normality
shapiro.test(AEG5)

# Perform Grubbs' Test
result_AEG5 <- grubbs.test(AEG5)

# Print results
print(result_AEG5)


# AEG 6 
AEG6 <- c(28.99, 19.83, 22.30, 27.01, 35.93, 25.77)


# Test for normality
shapiro.test(AEG6)

# Perform Grubbs' Test
result_AEG6 <- grubbs.test(AEG6)

# Print results
print(result_AEG6)


# AGE 7 
AEG7 <- c(23.82, 20.96, 29.06, 26.44, 23.82, 7.62)


# Test for normality
shapiro.test(AEG7)

# Perform Grubbs' Test
result_AEG7 <- grubbs.test(AEG7)

# Print results
print(result_AEG7)


# AGE 8 
AEG8 <- c(16.91, 19.73, 18.20, 20.50, 21.78, 19.22)


# Test for normality
shapiro.test(AEG8)

# Perform Grubbs' Test
result_AEG8 <- grubbs.test(AEG8)

# Print results
print(result_AEG8)


# AGE 9
AEG9 <- c(28.16, 33.69, 40.99, 35.20, 39.48, 30.68)


# Test for normality
shapiro.test(AEG9)

# Perform Grubbs' Test
result_AEG9 <- grubbs.test(AEG9)

# Print results
print(result_AEG9)


# AGE 10 
AEG10 <- c(20.48, 28.67, 23.55, 25.34, 27.65, 23.55)


# Test for normality
shapiro.test(AEG10)

# Perform Grubbs' Test
result_AEG10 <- grubbs.test(AEG10)

# Print results
print(result_AEG10)


# AGE 11 
AEG11 <- c(13.89, 17.25, 20.84, 16.05, 14.61, 14.13)



# Test for normality
shapiro.test(AEG11)

# Perform Grubbs' Test
result_AEG11 <- grubbs.test(AEG11)

# Print results
print(result_AEG11)


# AGE 12 
AEG12 <- c(29.97, 25.94, 23.17, 19.90, 17.88, 21.16)


# Test for normality
shapiro.test(AEG12)

# Perform Grubbs' Test
result_AEG12 <- grubbs.test(AEG12)

# Print results
print(result_AEG12)



# AGE 13 
AEG13 <- c(14.30, 24.01, 16.09, 17.88, 16.09, 12.26)


# Test for normality
shapiro.test(AEG13)

# Perform Grubbs' Test
result_AEG13 <- grubbs.test(AEG13)

# Print results
print(result_AEG13)


# AGE 14
AEG14 <- c(22.80, 25.74, 23.29, 28.68, 18.38, 27.70)


# Test for normality
shapiro.test(AEG14)

# Perform Grubbs' Test
result_AEG14 <- grubbs.test(AEG14)

# Print results
print(result_AEG14)


# AGE 15 
AEG15 <- c(12.50, 17.89, 18.38, 15.44, 16.91, 11.03)


# Test for normality
shapiro.test(AEG15)

# Perform Grubbs' Test
result_AEG15 <- grubbs.test(AEG15)

# Print results
print(result_AEG15)


# AGE 16 
AEG16 <- c(19.31, 17.24, 14.94, 10.11, 12.87, 14.94)


# Test for normality
shapiro.test(AEG16)

# Perform Grubbs' Test
result_AEG16 <- grubbs.test(AEG16)

# Print results
print(result_AEG16)



# AGE 17 
AEG17 <- c(24.93, 32.05, 28.05, 22.70, 19.14, 21.37)


# Test for normality
shapiro.test(AEG17)

# Perform Grubbs' Test
result_AEG17 <- grubbs.test(AEG17)

# Print results
print(result_AEG17)


# AGE 18 
AEG18 <- c(12.41, 15.51, 14.40, 17.06, 13.96, 19.05)


# Test for normality
shapiro.test(AEG18)

# Perform Grubbs' Test
result_AEG18 <- grubbs.test(AEG18)

# Print results
print(result_AEG18)

# AGE 19
AEG19 <- c(14.11, 26.48, 18.23, 16.71, 23.22, 11.72)


# Test for normality
shapiro.test(AEG19)

# Perform Grubbs' Test
result_AEG19 <- grubbs.test(AEG19)

# Print results
print(result_AEG19)


# AGE 20 
AEG20 <- c(24.89, 45.67, 50.29, 47.72, 40.28, 41.05)


# Test for normality
shapiro.test(AEG20)

# Perform Grubbs' Test
result_AEG20 <- grubbs.test(AEG20)

# Print results
print(result_AEG20)


# AGE 21 
AEG21 <- c(11.85, 14.44, 16.60, 15.09, 26.29, 13.15)


# Test for normality
shapiro.test(AEG21)

# Perform Grubbs' Test
result_AEG21 <- grubbs.test(AEG21)

# Print results
print(result_AEG21)



# AGE 22
AEG22 <- c(32.19, 14.23, 30.00, 21.68, 33.50, 24.96)


# Test for normality
shapiro.test(AEG22)

# Perform Grubbs' Test
result_AEG22 <- grubbs.test(AEG22)

# Print results
print(result_AEG22)



# AGE 23 
AEG23 <- c(18.29, 21.22, 17.81, 17.32, 22.44, 11.22)


# Test for normality
shapiro.test(AEG23)

# Perform Grubbs' Test
result_AEG23 <- grubbs.test(AEG23)

# Print results
print(result_AEG23)


# AGE 24 
AEG24 <- c(13.86, 9.09, 9.77, 10.68, 10.68, 10.22)


# Test for normality
shapiro.test(AEG24)

# Perform Grubbs' Test
result_AEG24 <- grubbs.test(AEG24)

# Print results
print(result_AEG24)



# AGE 25 
AEG25 <- c(27.19, 25.54, 29.56, 27.66, 25.30, 21.99)


# Test for normality
shapiro.test(AEG25)

# Perform Grubbs' Test
result_AEG25 <- grubbs.test(AEG25)

# Print results
print(result_AEG25)


# AGE 26 
AEG26 <- c(19.60, 21.09, 20.10, 23.08, 21.34, 20.35)


# Test for normality
shapiro.test(AEG26)

# Perform Grubbs' Test
result_AEG26 <- grubbs.test(AEG26)

# Print results
print(result_AEG26)


# AGE 27 
AEG27 <- c(10.21, 12.21, 12.46, 12.70, 11.21, 9.96)


# Test for normality
shapiro.test(AEG27)

# Perform Grubbs' Test
result_AEG27 <- grubbs.test(AEG27)

# Print results
print(result_AEG27)


# AGE 28 
AEG28 <- c(15.35, 11.34, 13.79, 16.24, 12.68, 15.57)


# Test for normality
shapiro.test(AEG28)

# Perform Grubbs' Test
result_AEG28 <- grubbs.test(AEG28)

# Print results
print(result_AEG28)

# AGE 29 
AEG29 <- c(15.03, 15.25, 14.81, 15.47, 15.92, 12.16)


# Test for normality
shapiro.test(AEG29)

# Perform Grubbs' Test
result_AEG29 <- grubbs.test(AEG29)

# Print results
print(result_AEG29)


# AGE 30
AEG30 <- c(24.20, 23.51, 22.37, 21.68, 28.99, 23.28)


# Test for normality
shapiro.test(AEG30)

# Perform Grubbs' Test
result_AEG30 <- grubbs.test(AEG30)

# Print results
print(result_AEG30)



# AGE 31
AEG31 <- c(26.52, 20.65, 34.50, 26.05, 27.22, 21.83)


# Test for normality
shapiro.test(AEG31)

# Perform Grubbs' Test
result_AEG31 <- grubbs.test(AEG31)

# Print results
print(result_AEG31)


# AGE 32
AEG32 <- c(16.74, 14.65, 13.95, 20.46, 14.88, 14.18)


# Test for normality
shapiro.test(AEG32)

# Perform Grubbs' Test
result_AEG32 <- grubbs.test(AEG32)

# Print results
print(result_AEG32)


# AGE 33
AEG33 <- c(16.81, 15.93, 17.48, 22.68, 33.19, 21.25, 21.97, 20.30)


# Test for normality
shapiro.test(AEG33)

# Perform Grubbs' Test
result_AEG33 <- grubbs.test(AEG33)

# Print results
print(result_AEG33)




# AGE 34
AEG34 <- c(22.00, 16.55, 13.61, 22.52, 26.43)


# Test for normality
shapiro.test(AEG34)

# Perform Grubbs' Test
result_AEG34 <- grubbs.test(AEG34)

# Print results
print(result_AEG34)

# Remove the detected outlier if p-value < 0.05
if (result_AEG2$p.value < 0.05) {
  AEG2 <- AEG2[AEG2 != max(AEG2)]
  cat("Outlier removed. Updated data:", AEG2, "\n")
}


# AGE 35
AEG35 <- c(11.47, 13.50, 13.72, 18.94, 12.38)


# Test for normality
shapiro.test(AEG35)

# Perform Grubbs' Test
result_AEG35 <- grubbs.test(AEG35)

# Print results
print(result_AEG35)



# AGE 36
AEG36 <- c(10.17, 11.10, 16.19, 18.72, 14.23)


# Test for normality
shapiro.test(AEG36)

# Perform Grubbs' Test
result_AEG36 <- grubbs.test(AEG36)

# Print results
print(result_AEG36)


# AGE 37
AEG37 <- c(22.02, 16.10, 21.31, 21.73, 22.49)


# Test for normality
shapiro.test(AEG37)

# Perform Grubbs' Test
result_AEG37 <- grubbs.test(AEG37)

# Print results
print(result_AEG37)


# AGE 38
AEG38 <- c(16.51, 14.54, 13.06, 11.63, 16.28, 13.69, 12.40, 12.14, 11.11)


# Test for normality
shapiro.test(AEG38)

# Perform Grubbs' Test
result_AEG38 <- grubbs.test(AEG38)

# Print results
print(result_AEG38)



# AGE 39
AEG39 <- c(20.90, 19.97, 22.52, 12.53)


# Test for normality
shapiro.test(AEG39)

# Perform Grubbs' Test
result_AEG39 <- grubbs.test(AEG39)

# Print results
print(result_AEG39)

# AGE 40
AEG40 <- c(42.30, 34.27, 27.74, 17.83, 41.12, 22.29, 28.49, 26.26)


# Test for normality
shapiro.test(AEG40)

# Perform Grubbs' Test
result_AEG40 <- grubbs.test(AEG40)

# Print results
print(result_AEG40)


# AGE 41
AEG41 <- c(8.75, 13.37, 11.67, 16.05, 9.00, 14.34)


# Test for normality
shapiro.test(AEG41)

# Perform Grubbs' Test
result_AEG41 <- grubbs.test(AEG41)

# Print results
print(result_AEG41)


# AGE 42
AEG42 <- c(23.54, 33.06, 29.30, 24.54, 38.32, 27.55)


# Test for normality
shapiro.test(AEG42)

# Perform Grubbs' Test
result_AEG42 <- grubbs.test(AEG42)

# Print results
print(result_AEG42)


# AGE 43
AEG43 <- c(26.19, 16.87, 27.45, 17.88, 18.89, 23.67)


# Test for normality
shapiro.test(AEG43)

# Perform Grubbs' Test
result_AEG43 <- grubbs.test(AEG43)

# Print results
print(result_AEG43)

# AGE 44
AEG44 <- c(32.66, 27.09, 23.95, 22.98, 22.50, 17.42)


# Test for normality
shapiro.test(AEG44)

# Perform Grubbs' Test
result_AEG44 <- grubbs.test(AEG44)

# Print results
print(result_AEG44)


# AGE 45
AEG45 <- c(13.04, 12.79, 16.55, 15.55, 10.78,19.06)


# Test for normality
shapiro.test(AEG45)

# Perform Grubbs' Test
result_AEG45 <- grubbs.test(AEG45)

# Print results
print(result_AEG45)

# AGE 46
AEG46 <- c(34.57, 26.25, 30.41, 23.65, 14.29, 18.45)


# Test for normality
shapiro.test(AEG46)

# Perform Grubbs' Test
result_AEG46 <- grubbs.test(AEG46)

# Print results
print(result_AEG46)


# AGE 47
AEG47 <- c(7.80, 5.28, 6.43, 4.82, 6.43, 8.03)


# Test for normality
shapiro.test(AEG47)

# Perform Grubbs' Test
result_AEG47 <- grubbs.test(AEG47)

# Print results
print(result_AEG47)


# AGE 48
AEG48 <- c(0.99, 0.60, 0.79, 0.79, 0.79)


# Test for normality
shapiro.test(AEG48)

# Perform Grubbs' Test
result_AEG48 <- grubbs.test(AEG48)

# Print results
print(result_AEG48)


# AGE 49
AEG49 <- c(21.49, 27.98, 26.42, 26.64, 32.01, 21.27)


# Test for normality
shapiro.test(AEG49)

# Perform Grubbs' Test
result_AEG49 <- grubbs.test(AEG49)

# Print results
print(result_AEG49)



# AGE 50
AEG50 <- c(18.89, 15.29, 17.99, 15.74, 21.36, 14.62)


# Test for normality
shapiro.test(AEG50)

# Perform Grubbs' Test
result_AEG50 <- grubbs.test(AEG50)

# Print results
print(result_AEG50)

