month_levels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
ggplot(data = df_soi, aes(x = Year, y = Soi)) +
  geom_point(aes(colour = factor(Month, levels = month_levels))) +
  theme_bw() +
  labs(colour = "Month",
       title = "Soi data of year") +
  theme(plot.title=element_text( hjust=0.5, face='bold'))

df_soi <- mutate(df_soi, Month = factor(Month, levels = month_levels))

ggplot(data = df_soi, aes(x = Month, y = Soi)) +
  geom_boxplot(aes(fill = factor(Month, levels = month_levels))) +
  theme_bw() +
  labs(fill = "Month",
       title = "Soi data for each month from 1876 to 2021") +
  theme(plot.title=element_text( hjust=0.5, face='bold'))

ggplot(data = df_soi, aes(x = Year, y = Soi)) +
  geom_point(aes(colour = factor(Month, levels = month_levels))) +
  theme_bw() +
  labs(colour = "Month",
       title = "Soi data of year") +
  facet_wrap( ~ Month) +
  theme(plot.title=element_text( hjust=0.5, face='bold'))


df_soi %>% 
  filter(Month == "Jan") %>%
  ggplot(aes(x = Year, y = Soi)) +
  geom_point(colour = "blue") +
  geom_line()

df_soi %>% 
  filter(Month == "Jan") %>%
  ggplot(aes(x = Year, y = Soi)) +
  geom_area(colour = "blue") +
  geom_line()


df <- data.frame(x = 1:10, y = c(2,3,5,5,7,9,10,11,13,14))

df_lm <- lm(data = df, y ~ x)

summary(df_lm)

ggplot(df, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm")


stations <- unique(total_seasonal_rainfall$name)

for(station in stations) {
  for(season in seasons) {
    df <- total_seasonal_rainfall %>% 
      filter(name == station & Season == season)
    
    df_lm <- lm(data = df, total_seas_prcp ~ SeasonalSOI)
    r_squared <- round(glance(df_lm)$r.squared, 4)
    result_df[nrow(result_df) + 1, ] = c(station, season, r_squared)
  }
}

result_df <- data.frame(name = character(), 
                        season = factor(levels = seasons),
                        r = numeric(),
                        stringsAsFactors = F)

brisbane_spring2 <- total_seasonal_rainfall %>% 
  filter(name == "BRISBANE REGIONAL OFFICE" & Season == "Summer") 

brisbane_spring2_lm <- lm(data = brisbane_spring2, total_seas_prcp ~ SeasonalSOI)

summary(brisbane_spring2_lm)

df <- total_seasonal_rainfall %>% 
  filter(name == "PERTH REGIONAL OFFICE" & Season == "Spring")

df_lm <- lm(data = df, total_seas_prcp ~ SeasonalSOI)
r_squared <- round(glance(df_lm)$r.squared, 4)
result_df[nrow(result_df) + 1, ] = c(station, season, r_squared)

total_seasonal_rainfall2 <- read_csv("total_seasonal_rainfall.csv") %>%
  mutate(total_seas_prcp = total_seas_prcp/10) %>%
  group_by(Year, Season) %>%
  summarise(prcp = mean(total_seas_prcp, na.rm = T)) %>%
  left_join(seasonal_soi_values)

ggplot(total_seasonal_rainfall2, aes(x = SeasonalSOI, y = prcp)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~Season)


df <- total_seasonal_rainfall2 %>% 
  filter(Season == "Summer")

df_lm <- lm(data = df, prcp ~ SeasonalSOI)
summary(df_lm)
