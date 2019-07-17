
# https://flowingdata.com/2016/02/09/why-people-visit-the-emergency-room/
options(tibble.width =Inf)
library(neiss)
library(tidyverse)
library(funModeling)

?neiss::injuries

head(injuries)
df_status(injuries) %>%
  as_tibble() %>%
  arrange(-q_na) %>% 
  head(40)

injury_data <- injuries %>%
  left_join(., products, by = c("prod1" = "code")) %>% 
  rename(product1 = title) %>%
  left_join(., products, by = c("prod2" = "code")) %>%
  rename(product2 = title)

injury_data <- injury_data %>%
  select(date=trmt_date,
         age:body_part, -race_other,
         diagnosis = diag,
         diagnosis_other = diag_other,
         disposition, location,
         fire_dept = fmv,
         n_injuries =weight,
         description=narrative)
  

# save small data for building the tutorial
injury_data %>%
  sample_frac(0.1) %>% write_rds("tutorial/demo_data.rds")


# location
top20injuries <- injury_data %>% 
  filter(location == "School") %>% 
  group_by(body_part) %>% 
  summarise(number_of_injuries=sum(n_injuries)) %>% 
  top_n(20)
  
  
  
  ggplot(aes(body_part, number_of_injuries)) +
  geom_col() +
  coord_flip()



# “cig”, “vape”, “vapor”, “ENDS” or “electronic nicotine device”
products %>%
  filter(str_detect(title, "cigarette") )

injuries %>%
  filter(str_detect(narrative, "CIG"))

injuries %>%
  filter(str_detect(narrative, "CIG")) %>%
  group_by(diag, ) %>%
  summarise(n = sum(weight)) -> diag_count

ord <- diag_count %>% arrange(n) %>% pull(diag)  

diag_count %>%
ggplot(aes(diag, n)) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limits = ord)
  
  
  

