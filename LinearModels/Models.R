### Regression applied to baseball statistics
library(tidyverse)
library(Lahman)

p <- Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(HR_per_game = HR/G, R_per_game = R/G) %>%
  ggplot(aes(HR_per_game, R_per_game)) +
  geom_point(alpha = 0.5)
p

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(z_HR = round((HR - mean(HR))/sd(HR)),
         R_per_game = R/G) %>%
  filter(z_HR %in% -2:3) %>%
  ggplot() +
  stat_qq(aes(sample=R_per_game)) +
  facet_wrap(~z_HR)

summary_stats <- Teams %>%
  filter(yearID %in% 1961:2001 ) %>%
  mutate(HR_per_game = HR/G, R_per_game = R/G) %>%
  summarize(avg_HR = mean(HR_per_game),
            s_HR = sd(HR_per_game),
            avg_R = mean(R_per_game),
            s_R = sd(R_per_game),
            r = cor(HR_per_game, R_per_game))
summary_stats

reg_line <- summary_stats %>% summarize(slope = r * s_R / s_HR, intercept = avg_R - slope * avg_HR)    

p +  geom_abline(intercept = reg_line$intercept, slope = reg_line$slope)

p + geom_smooth(method = "lm")

exam_1 <- Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(HR_per_game = HR/G, R_per_game = R/G, BB_per_game = BB / G) %>% 
  select(HR_per_game, R_per_game, BB_per_game)
  
fit = lm(exam_1$R_per_game ~  exam_1$HR_per_game + exam_1$BB_per_game, data = exam_1)
summary(fit)

library(HistData)
data("GaltonFamilies")

set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm")

model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as_tibble(predictions) %>% bind_cols(father = galton_heights$father)
data
ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galton_heights, aes(x = father, y = son))


model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model)
data <- as_tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_point(data = galton_heights, aes(x = father, y = son))


