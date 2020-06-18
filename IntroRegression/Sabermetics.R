library(tidyverse)
library(Lahman)
View(Teams)


# HR/G vs R/G
Teams %>% filter(yearID %in% 1961:2001) %>% 
  mutate(HR_per_game = HR / G, R_per_game = R / G) %>% 
  ggplot(aes(HR_per_game, R_per_game)) +
  geom_point(alpha = 0.5)

# SB/G vs R/G
Teams %>% filter(yearID %in% 1961:2001) %>% 
  mutate(SB_per_game = SB / G, R_per_game = R / G) %>% 
  ggplot(aes(SB_per_game, R_per_game)) +
  geom_point(alpha = 0.5)

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

### Exam
library(Lahman)

bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB,
         singles = (H - X2B - X3B - HR) / pa,
         bb = BB / pa) %>%
  filter(pa >= 100) %>%
 select(playerID, singles, bb, pa)


bat_03 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB,
         singles = (H - X2B - X3B - HR) / pa,
         bb = BB / pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID) %>%
  summarise(average_singles = mean(singles),
            average_bb = mean(bb),
            n()) 
  # filter(average_singles > .2)
  # filter(average_bb > .2)
bat_03

join_df <- inner_join(bat_03, bat_02, by = "playerID")
join_df   

cor(join_df$singles, join_df$average_singles)
cor(join_df$average_bb, join_df$bb)

ggplot(join_df, aes(average_singles, singles)) +
  geom_point()

ggplot(join_df, aes(average_bb, bb)) +
  geom_point()

fit <- lm(singles ~ average_singles, join_df)
summary(fit)

fit <- lm(bb ~ average_bb, join_df)
summary(fit)



