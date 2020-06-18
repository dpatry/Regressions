
# Create dataset
library(tidyverse)
library(HistData)
data("GaltonFamilies")

### Case study
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)



# number of fathers with height 72 or 72.5 inches
sum(galton_heights$father == 72)
sum(galton_heights$father == 72.5)

# predicted height of a son with a 72 inch tall father
conditional_avg <- galton_heights %>%
  filter(round(father) == 72) %>%
  summarize(avg = mean(son)) %>%
  pull(avg)
conditional_avg

# stratify fathers' heights to make a boxplot of son heights
galton_heights %>% mutate(father_strata = factor(round(father))) %>%
  ggplot(aes(father_strata, son)) +
  geom_boxplot() +
  geom_point()

# center of each boxplot
galton_heights %>%
  mutate(father = round(father)) %>%
  group_by(father) %>%
  summarize(son_conditional_avg = mean(son)) %>%
  ggplot(aes(father, son_conditional_avg)) +
  geom_point()

# calculate values to plot regression line on original data
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)

m <- r * s_y/s_x
b <- mu_y - m*mu_x

# add regression line to plot
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = b, slope = m)

galton_heights %>%
  ggplot(aes(scale(father), scale(son))) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = r)

### Regression improves precision
B <- 1000
N <- 50
set.seed(1983)
conditional_avg <- replicate(B, {
  dat <- sample_n(galton_heights, N)
  dat %>% filter(round(father) == 72) %>%
    summarize(avg = mean(son)) %>%
    pull(avg)
})
regression_prediction <- replicate(B, {
  dat <- sample_n(galton_heights, N)
  mu_x <- mean(dat$father)
  mu_y <- mean(dat$son)
  s_x <- sd(dat$father)
  s_y <- sd(dat$son)
  r <- cor(dat$father, dat$son)
  mu_y + r*(72 - mu_x)/s_x*s_y
})

mean(conditional_avg, na.rm = TRUE)
#> [1] 70.8
mean(regression_prediction)
#> [1] 70.5

sd(conditional_avg, na.rm = TRUE)
#> [1] 0.947
sd(regression_prediction)
#> [1] 0.521


### Bivariate Normal Distribution
galton_heights %>%
  mutate(z_father = round((father - mean(father)) / sd(father))) %>%
  filter(z_father %in% -2:2) %>%
  ggplot() +  
  stat_qq(aes(sample = son)) +
  facet_wrap( ~ z_father)

### Variance Explained see RegressionEquation.PNG



### Warning: there are two regression lines
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m_1 <- r * s_y / s_x
b_1 <- mu_y - m_1*mu_x

m_2 <- r * s_x / s_y # 38.8 + 0.44 X.
b_2 <- b_2 <- mu_x - m_2*mu_y # 45.2 + 0.34 Y.

galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = b_1, slope = m_1, col = "blue") +
  geom_abline(intercept = -b_2/m_2, slope = 1/m_2, col = "red")

### Excercises Stratification
# set.seed(1989) #if you are using R 3.5 or earlier
set.seed(1989, sample.kind = "Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies %>%     
  filter( gender == "female" ) %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select( mother, childHeight) %>%     
  rename(daughter = childHeight)
female_heights

m_m <- mean(female_heights$mother)
sd_m <-sd(female_heights$mother)

m_d <- mean(female_heights$daughter)
sd_d <- sd(female_heights$daughter)

r <- cor(female_heights$mother, female_heights$daughter)
slope <- r * sd_d / sd_m
Intcp <- m_d - (slope * m_m)

R <- r^2

Conditional_avgD <- female_heights %>% 
  filter(round(mother) == 60 ) %>% 
  summarize(avg = mean(daughter)) %>% 
  pull(avg)
  
Cond_exp <- Intcp + slope * 60
  
fit_mother <- lm(mother ~ daughter, female_heights)
summary(fit_mother)  


model <- lm(mother ~ daughter, data = female_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as_tibble(predictions) %>% bind_cols(daughters = female_heights$daughter)








