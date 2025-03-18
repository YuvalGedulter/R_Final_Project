install.packages("readxl")
library(dplyr)
library(ggplot2)
library(readxl)

data <- read_excel("C:/Users/97250/Desktop/Tamarix/Data/Salt-Tamarix data.xlsx")
data <- data |>
  mutate(Treatment_factor = factor(Treatment,
                                   levels = c("-", "Pip", "Si", "Pip+Si", "NaCl", "Pip+NaCl", "Si+NaCl", "Pip+Si+NaCl"),
                                   labels = c("Control", "Pip", "Si", "Pip+Si", "NaCl", "NaCl+Pip", "NaCl+Si", "NaCl+Pip+Si")))
contrasts(data$Treatment_factor) <- contr.treatment(8)
contrasts(data$Treatment_factor)


# השפעת הטיפולים השונים על המוליכות החשמלית (EC)
ggplot(data, aes(x = Treatment_factor, y = Normal_EC)) +
  geom_violin(binwidth = 1, alpha = 0.5, color = "#76D7C4") + #, color = "#76D7C4"
  geom_jitter(width = 0.1, alpha = 0.6, color = "black") +
  geom_boxplot(width = 0.7, fill = "white", alpha = 0.5, outlier.shape = NA) +
  #scale_fill_manual(values = c("Male" = "#DAF7A6", "Female" = "#CCCCFF")) + # בחירת צבעים ידנית לכל קטגוריה
  theme_classic() + # סגנון הגרף
  xlab("Treatment") +
  ylab("EC")

# השפעת האוכלוסייה על המוליכות החשמלית
ggplot(data, aes(x = Population, y = Normal_EC, color = Treatment_factor)) +
  geom_boxplot(width = 0.4, alpha = 0.5, outlier.shape = NA, position = position_dodge(0.75)) + 
  #geom_violin(aes(fill = Treatment_factor), alpha = 0.3, color = NA) + # מוסיף ויולין רך
  theme_minimal() +
  xlab("Population") +
  ylab("EC")

# השפעת הטיפולים השונים על הביומסה
ggplot(data, aes(x = Treatment_factor, y = Biomass)) +
  geom_col(fill = "#DAF7A6") +
  theme_bw() +
  xlab("Treatment") +
  ylab("Biomass (g)")

# השפעת האוכלוסייה על הביומסה
ggplot(data, aes(x = Population, y = Biomass, fill = Population)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  xlab("Population") +
  ylab("Biomass (g)")

# השפעת הטיפולים על הגובה
data$Height <- as.numeric(data$Height)  # המרה למספרים
ggplot(data, aes(x = Treatment_factor, y = Height)) +
  stat_summary(fun = mean, geom = "col", fill = "#F1C40F") +  
  theme_bw() +
  xlab("Treatment") +
  ylab("Average Height (cm)")

# השפעת האוכלוסייה על הגובה
data$Height <- as.numeric(data$Height)  # המרה למספרים
ggplot(data, aes(x = Population, y = Height)) +
  stat_summary(fun = mean, geom = "col", fill = "#CCCCFF") +  
  theme_bw() +
  xlab("Treatment") +
  ylab("Average Height (cm)")

# השפעת הטופולים והאוכלוסיה על מספר הפיצולים
data$Splits <- as.numeric(data$Splits)  # המרה למספרים
ggplot(data, aes(x = Treatment_factor, y = Splits, color = Population)) +
  geom_boxplot() +
  geom_jitter(aes(color = Population, shape = Population), alpha = 0.4, position = position_jitter(0.15))  + 
  theme_minimal() +
  xlab("Treatmenr") +
  ylab("Number of splits")

# בדיקת קורלציה בין הגובה למספר הפיצולים
height <- data$Height
splits <- data$Splits
correlation <- cor(height, splits, method = "pearson", use = "complete.obs")
print(correlation)

# בדיקת קורלציה בין הגובה לשטח הפנים
surface <- data$`plant_surface(cm^2)`
correlation1 <- cor(height, surface, method = "pearson", use = "complete.obs")
print(correlation1)