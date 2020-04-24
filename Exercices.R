### Exercices
# Q uestion 4: You want to know whether teams with more at-bats per 
# game have more runs per game.
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

# Question 6
Teams %>% filter(yearID %in% 1961:2001) %>% 
  mutate(Rpg = R/G, Abg = AB/G) %>% 
  ggplot(aes(Abg, Rpg)) +
  geom_point(alpha = 0.5)
