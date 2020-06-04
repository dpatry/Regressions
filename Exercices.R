### Exercices
# Q uestion 4: You want to know whether teams with more at-bats per 
# game have more runs per game.


Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

# Question 6
Teams %>% filter(yearID %in% 1961:2001) %>% 
  mutate(Rpg = R / G, Abg = AB / G) %>% 
  ggplot(aes(Abg, Rpg)) +
  geom_point(alpha = 0.5)

# Question 7
Teams %>% filter(yearID %in% 1961:2001) %>% 
  mutate(Wins_game = W / G, Error = E / G) %>% 
  ggplot(aes(Error, Wins_game)) +
  geom_point(alpha = 0.5)

# Question 8
Teams %>% filter(yearID %in% 1961:2001 ) %>% 
  mutate(X3b_g = X3B / G, X2b_g = X2B / G) %>% 
  ggplot(aes(X2b_g, X3b_g)) +
  geom_point(alpha = .05)
# Question 9
Teams %>% filter((yearID %in% 1961:2001)) %>% 
  mutate(runs_g = R / G, Ab_g = AB / G ) %>% 
  summarise(cor(runs_g, Ab_g))

# Question 10
Teams %>% filter(yearID %in% 1961:2001) %>% 
  mutate(Wins_game = W / G, Error = E / G) %>% 
  summarise(cor(Wins_game, Error))

# Questtion 11
Teams %>% filter(yearID %in% 1961:2001 ) %>% 
  mutate(X3b_g = X3B / G, X2b_g = X2B / G) %>% 
  summarise(cor(X3b_g, X2b_g))















