library(readxl)
library(Hmisc)
library(funModeling)
library(tidyverse)
library(NHANES)
head(NHANES)

NHANES %>%
  filter(ID == 51624) %>% 
  mutate(PregnantNow = c(NA, NA, "yes")) %>% distinct()

# Get number of observations (rows) and variables (columns)
# and type data each column contains
glimpse(NHANES)

# Get number of missing (NA) or zero (0) data points
df_status(NHANES) %>%
  as_tibble() %>%
  arrange(-q_na) %>% 
  head(40)

df_status(NHANES) %>%
  as_tibble() %>%
  filter(p_na < 40) %>%
  arrange(-p_na) %>%
  pull(variable) -> comp_vars



NHANES %>%
  select(comp_vars) %>%
  drop_na() -> NHANEScompl
  
NHANEScompl %>%
  ggplot(aes(Race1, fill=HHIncome)) +
  geom_bar() +
  coord_flip()

NHANEScompl %>%
  group_by(BMI_WHO, Diabetes) %>%
  count %>%
  ggplot(aes(BMI_WHO, n)) +
  geom_col() +
  facet_grid(.~Diabetes)


# Get frequencies for categorical variables
# This also generates plots for each
freq(NHANEScompl) 

# Profile numeric variables
# Get summary statistics
profiling_num(NHANEScompl)


NHANEScompl %>%
  ggplot(aes(x= HHIncomeMid, y=Poverty)) +
  geom_point()
  

NHANEScompl %>%
  ggplot(aes(x= BMI, y=DaysPhysHlthBad)) +
  geom_point() +
  geom_smooth(method = "lm")

NHANEScompl %>%
  ggplot(aes(x= BMI_WHO, y=Poverty)) +
  geom_boxplot() 

NHANEScompl %>%
  ggplot(aes(x= Depressed, y=Poverty)) +
  geom_violin() 

NHANEScompl %>%
  ggplot(aes(x= Education, y=Poverty)) +
  geom_violin() 

NHANEScompl %>%
  ggplot(aes(x=Poverty, y=DirectChol))+
  geom_point()

NHANEScompl %>%
  ggplot(aes(x=LittleInterest, y=Poverty))+
  geom_boxplot()

NHANEScompl %>%
  ggplot(aes(x=HealthGen, y=Poverty))+
  geom_boxplot() +
  facet_grid(.~ Gender)

NHANEScompl %>%
  ggplot(aes(x=Age, y=log2(TotChol)))+
  geom_point() +geom_smooth(method = "lm") 

NHANEScompl %>%
  ggplot(aes(x=Diabetes, y=Poverty))+
  geom_boxplot()


NHANEScompl %>%
  ggplot(aes(x=HealthGen, y=Pulse))+
  geom_violin()


glm( ~ )
