library(tidyverse)
library(vroom)

dat <- vroom("http://had.co.nz/stat645/week-05/birthweight.txt")
head(dat)
colnames(dat) <- str_replace(colnames(dat), " ", "_")

dat %>%
  filter(Smoking %in% c("never", "now")) -> smoke_dat

smoke_dat %>%
  filter(Race == "white") %>%
glm(Birth_Weight ~ Gestation  + Smoking  + Age + Parity, data=.) %>%
  summary()

t.test()

smoke_dat %>%
  group_by(Smoking) %>%
  count()

smoke_dat %>%
  drop_na(Smoking, Birth_Weight) %>% 
  ggplot(aes(Smoking, Birth_Weight, color = Smoking)) +
  geom_boxplot() 

  smoke_dat %>%
    filter(Race == "white") %>%
  drop_na(Smoking, Gestation, Birth_Weight) %>%
  ggplot(aes(Gestation, Birth_Weight, color = Smoking)) +
  geom_point() +
  geom_smooth(method = "lm")
