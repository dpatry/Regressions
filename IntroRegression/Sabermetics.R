
library(tidyverse)
library(Lahman)
# View(Teams)

### Code: Scatterplot of the relationship between HRs and wins
# HR/G vs R/G
Teams %>% filter(yearID %in% 1961:2001) %>% 
  mutate(HR_per_game = HR / G, R_per_game = R / G) %>% 
  ggplot(aes(HR_per_game, R_per_game)) +
  geom_point(alpha = 0.5)

### Code: Scatterplot of the relationship between stolen bases and wins
# SB/G vs R/G
Teams %>% filter(yearID %in% 1961:2001) %>% 
  mutate(SB_per_game = SB / G, R_per_game = R / G) %>% 
  ggplot(aes(SB_per_game, R_per_game)) +
  geom_point(alpha = 0.5)

### Code: Scatterplot of the relationship between bases on balls and runs
# BB/G vs R/G
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>%
  ggplot(aes(BB_per_game, R_per_game)) +
  geom_point(alpha = 0.5)

#HR/G vs BB/G
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(HR_per_game = HR/G, BB_per_game = BB/G) %>%
  ggplot(aes(HR_per_game, BB_per_game)) +
  geom_point(alpha = 0.5)


# First, notice that the HR and Run data appear to be bivariate
# normal. We save the plot into the object p as we will use it again later.
p <- Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(HR_per_game = HR/G, R_per_game = R/G) %>%
  ggplot(aes(HR_per_game, R_per_game)) +
  geom_point(alpha = 0.5)
p

# qqplot
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(z_HR = round((HR - mean(HR))/sd(HR)),
         R_per_game = R/G) %>%
  filter(z_HR %in% -2:3) %>%
  ggplot() +
  stat_qq(aes(sample=R_per_game)) +
  facet_wrap(~z_HR)







