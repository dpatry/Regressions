### do function
### Broom package
library(tidyverse)
library(Lahman)
library(broom)
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1),
         BB = BB/G,
         R = R/G) %>%
  select(HR, BB, R) %>%
  filter(HR >= 0.4 & HR<=1.2)

dat %>%
  group_by(HR) %>%
  summarize(slope = cor(BB,R)*sd(R)/sd(BB))

dat %>%
  group_by(HR) %>%
  lm(R ~ BB, data = .) %>%
  .$coef

### not so good since <lm> object
dat %>%
  group_by(HR) %>%
  do(fit = lm(R ~ BB, data = .))

### Result in a error
dat %>%
  group_by(HR) %>%
  do(lm(R ~ BB, data = .))

### Good
get_slope <- function(data){
  fit <- lm(R ~ BB, data = data)
  data.frame(slope = fit$coefficients[2],
             se = summary(fit)$coefficient[2,2])
}

dat %>%
  group_by(HR) %>%
  do(get_slope(.))

### not good since <data frame>
dat %>%
  group_by(HR) %>%
  do(slope = get_slope(.))

### good
get_lse <- function(data){
  fit <- lm(R ~ BB, data = data)
  data.frame(term = names(fit$coefficients),
             slope = fit$coefficients,
             se = summary(fit)$coefficient[,2])
}
dat %>%
  group_by(HR) %>%
  do(get_lse(.))

dat %>%
  group_by(HR) %>%
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high) %>%
  ggplot(aes(HR, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()

glance(fit)

####### Broom package #########################
library(broom)
fit <- lm(R ~ BB, data = dat)
tidy(fit, conf.int = TRUE)

dat %>%
  group_by(HR) %>%
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high)

glance(fit)

##### Exam #####
# Q-1
get_slope <- function(data) {
  fit <- lm(R ~ BB, data = data)
  sum.fit <- summary(fit)
  
  data.frame(slope = sum.fit$coefficients[2, "Estimate"], 
             se = sum.fit$coefficients[2, "Std. Error"],
             pvalue = sum.fit$coefficients[2, "Pr(>|t|)"])
}
dat %>% 
  group_by(HR) %>% 
  do(get_slope(.))

# Q-2
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = HR/G,
         R = R/G) %>%
  select(lgID, HR, BB, R) 

dat %>% 
  group_by(lgID) %>% 
  do(tidy(lm(R ~ HR, data = .), conf.int = T)) %>% 
  filter(term == "HR") 

dat%>%
  group_by(HR) %>%
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high) %>%
  ggplot(aes(HR, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()
glance(as.tibble(dat)


# Q-3
library(tidyverse)
library(HistData)
data("GaltonFamilies")
# set.seed(1) # if you are using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton_heights <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>% 
  ungroup()
  
  cors <- galton_heights %>%
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child")) %>%
  group_by(pair) %>%
  summarize(cor = cor(parentHeight, childHeight))


  cors_count <- galton_heights %>%
    gather(parent, parentHeight, father:mother) %>%
    mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
    unite(pair, c("parent", "child")) %>%
    group_by(pair) %>%
    summarize(n = n())

GaltonFamilies_count <-GaltonFamilies %>% 
  group_by(gender) %>% 
  summarise(n = n())

 


### Q8-9-10a-10b
library(tidyverse)
library(HistData)
library(broom)
data("GaltonFamilies")
# set.seed(1) # if you are using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child")) %>% 
  group_by(pair) 
  
  #filter(pair == "mother_daughter")
 
 chisq <- chisq.test(galton)
 chisq
   
#father_son
ct_f_s <-  cor.test(galton$parentHeight, galton$childHeight , 
                               alternative = "two.sided", method = "pearson", conf.level = .95)
ct_f_s$p.value
#father_daughter
ct_f_d <-  cor.test(galton$parentHeight, galton$childHeight , 
                    alternative = "two.sided", method = "pearson", conf.level = .95)
ct_f_d$p.value
#mother_son
ct_m_s <-  cor.test(galton$parentHeight, galton$childHeight , 
                    alternative = "two.sided", method = "pearson", conf.level = .95)
ct_m_s$p.value
#mother_daughter
ct_m_d <-  cor.test(galton$parentHeight, galton$childHeight , 
                    alternative = "two.sided", method = "pearson", conf.level = .95)
ct_m_d$p.value


galton %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>% 
filter(term == "parentHeight")  %>% 
select(pair, estimate, conf.low, conf.high) %>% 
  ggplot(aes(
    pair,
    y = estimate,
    ymin = conf.low,
    ymax = conf.high
  )) +
  geom_errorbar() +
  geom_point()

       












