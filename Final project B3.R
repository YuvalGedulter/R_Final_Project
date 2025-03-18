source("Final project B2.R")


# פונקציה שמייצרת גרפים לממוצעים עם שגיאת תקן (שאגב, הוספו לפוסטר )
plot_bar <- function(sum_data, x_var, y_var, error_var) {
  
  ggplot(sum_data, aes_string(x = x_var, y = y_var)) +
    geom_bar(stat = "identity", fill = "#52be80", color = "black") +
    geom_errorbar(aes_string(ymin = paste0(y_var, " - ", error_var), 
                             ymax = paste0(y_var, " + ", error_var)), 
                  width = 0.2, color = "black") +
    geom_text(aes_string(label = y_var), vjust = -0.5, size = 2) +
    labs(x = x_var, y = y_var) + 
    theme_minimal()
}

plot_bar(sum_data, "Treatment_factor", "mean_EC", "se_EC")

plot_bar(sum_data, "Treatment_factor", "mean_biomass", "se_biomass")

plot_bar(sum_data, "Treatment_factor", "mean_height", "se_height")

plot_bar(sum_data, "Treatment_factor", "mean_splits", "se_splits")

plot_bar(sum_data, "Treatment_factor", "mean_surface", "se_surface")