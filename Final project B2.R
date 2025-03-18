source("Final project A2.R")


data <- data |>
  filter(!is.na(Height))

data <- data |>
  mutate(Normal_EC_binary = ifelse(data$Normal_EC > 6000, 1, 0))

# טבלה מסכמת ליצירת גרפים של ממוצעים עם שגיאת תקן
sum_data <- data |>
  group_by(Treatment_factor) |>
  summarise(
    mean_height = mean(Height, na.rm = TRUE),
    sd_height = sd(Height, na.rm = TRUE),
    se_height = sd_height / sqrt(n()),
    ymin = mean_height - se_height,
    ymax = mean_height + se_height,
    mean_splits = mean(Splits, na.rm = TRUE),
    sd_splits = sd(Splits, na.rm = TRUE),
    se_splits = sd_splits / sqrt(n()),
    ymin1 = mean_splits - se_splits,
    ymax1 = mean_splits + se_splits,
    mean_biomass = mean(Biomass, na.rm = TRUE),
    sd_biomass = sd(Biomass, na.rm = TRUE),
    se_biomass = sd_biomass / sqrt(n()),
    ymin_biomass = mean_biomass - se_biomass,
    ymax_biomass = mean_biomass + se_biomass,
    mean_EC = mean(Normal_EC, na.rm = TRUE),
    sd_EC = sd(Normal_EC, na.rm = TRUE),
    se_EC = sd_EC / sqrt(n()),
    ymin_EC = mean_EC - se_EC,
    ymax_EC = mean_EC + se_EC,
    mean_surface = mean(`plant_surface(cm^2)`, na.rm = TRUE),
    sd_surface = sd(`plant_surface(cm^2)`, na.rm = TRUE),
    se_surface = sd_surface / sqrt(n()),
    ymin_surface = mean_surface - se_surface,
    ymax_surface = mean_surface + se_surface
  )
View(sum_data)