library(tidyverse)

# S1
# Q1.1
df_soi_w <- read_csv("soi_data_wide.csv")

cols_names <- names(df_soi_w)

cols_names <- cols_names[- 1]

df_soi <- pivot_longer(df_soi_w, cols = cols_names, names_to = "Month", values_to = "Soi")
head(df_soi, 3)

# Q1.2
month_levels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
ggplot(data = df_soi, aes(x = Year, y = Soi)) +
  geom_point(aes(colour = factor(Month, levels = month_levels))) +
  theme_bw() +
  labs(colour = "Month",
       title = "Soi data of year") +
  theme(plot.title=element_text( hjust=0.5, face='bold'))

# Q1.3
summer_months = c("Dec", "Jan", "Feb")
df_soi <- df_soi %>%
  mutate(Season = NA_character_) %>%
  mutate(Season = if_else(Month %in% summer_months, "Summer", Season))
