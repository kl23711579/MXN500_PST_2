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
spring_months = c("Sep", "Oct", "Nov")
summer_months = c("Dec", "Jan", "Feb")
autumn_months = c("Mar", "Apr", "May")
winter_months = c("Jun", "Jul", "Aug")
seasons = c("Spring", "Summer", "Autumn", "Winter")
df_soi <- df_soi %>%
  mutate(Season = NA_character_) %>%
  mutate(Season = if_else(Month %in% summer_months, "Summer", Season)) %>%
  mutate(Season = if_else(Month %in% spring_months, "Spring", Season)) %>%
  mutate(Season = if_else(Month %in% autumn_months, "Autumn", Season)) %>%
  mutate(Season = if_else(Month %in% winter_months, "Winter", Season)) %>%
  mutate(Season = factor(Season, levels = seasons))

#Q1.4
seasonal_soi_values <- df_soi %>% 
  group_by(Year, Season) %>% 
  summarise(SeasonalSOI = mean(Soi, na.rm = T))

filter(seasonal_soi_values, Year == 2020)

#Q1.5
seasonal_soi_values <- seasonal_soi_values %>%
  mutate(Phase = "Neutral") %>% 
  mutate(Phase = if_else(SeasonalSOI > 8, "LaNina", Phase)) %>%
  mutate(Phase = if_else(SeasonalSOI < -8, "ElNino", Phase))

filter(seasonal_soi_values, Year == 2011)

#Q1.6
seasons = c("Spring", "Summer", "Autumn", "Winter")
phases = c("LaNina", "Neutral", "ElNino")
seasonal_soi_values <- seasonal_soi_values %>%
  mutate(Season = factor(Season, levels = seasons)) %>%
  mutate(Phase = factor(Phase, levels = phases))

levels(seasonal_soi_values$Season)
levels(seasonal_soi_values$Phase)

#Q2.1
total_seasonal_rainfall <- read_csv("total_seasonal_rainfall.csv") %>%
  mutate(total_seas_prcp = total_seas_prcp/10) %>%
  left_join(seasonal_soi_values)

# For the Brisbane Station in Spring, 
# a  linear model was specified to model how the total seasonal precipitation, ... y ,
# is related to the mean seasonal SOI value, .... x 
# The parameter ... beta1 describes the rate of change in the total seasonal precipitation with an increase in mean seasonal SOI value. 
# The parameter . . . beta0 represents the total seasonal precipitation when the mean seasonal SOI value is 0.


#Q2.2
brisbane_spring <- total_seasonal_rainfall %>% 
  filter(name == "BRISBANE REGIONAL OFFICE" & Season == "Spring") 
  
brisbane_spring_lm <- lm(data = brisbane_spring, total_seas_prcp ~ SeasonalSOI)

summary(brisbane_spring_lm)

ggplot(brisbane_spring, aes(x = SeasonalSOI, y = total_seas_prcp)) +
  geom_point() +
  geom_smooth(method = "lm")

#Q2.3

#Q2.4
library(broom)
tidy(brisbane_spring_lm, conf.int = T, conf.level = 0.95)

#Q2.5
brisbane_spring_fort <- fortify(brisbane_spring_lm)

ggplot(brisbane_spring_fort, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_smooth() +
  theme_bw()

ggplot(data = brisbane_spring_fort, aes(sample = .stdresid))+
  stat_qq()+
  geom_abline(intercept = 0, slope = 1)+
  coord_equal()+
  labs(x = "q values from standard normal",
       y = "q values from standardised residuals")+
  theme_bw()

#Q2.7
ggplot(total_seasonal_rainfall, aes(x = SeasonalSOI, y = total_seas_prcp)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(rows = vars(name), cols = vars(Season))



#Q3.1
brisbane_spring_lm2 <- lm(data = brisbane_spring, total_seas_prcp ~ poly(SeasonalSOI, 2, raw = T))

summary(brisbane_spring_lm2)

#Q3.2

#Q3.3
new_soi_data = data.frame(SeasonalSOI = seq(-25,25,1))
pred_value <- predict(brisbane_spring_lm2, newdata = new_soi_data, interval = "pred", level=0.95)

plot_data <- cbind(new_soi_data, pred_value)

plot_data %>%
  ggplot(aes(x= SeasonalSOI,y = fit ))+
  geom_point()+
  geom_ribbon(aes(ymin= lwr, ymax = upr), fill= "lightskyblue", alpha = 0.25) +
  theme_bw()

#Q3.4
anova(brisbane_spring_lm, brisbane_spring_lm2)

total_seasonal_rainfall %>%
  filter(Year == 2020)












