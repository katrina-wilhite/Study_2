# Load the dataset
load("Z:/7-Data/M&B/LSAC dataset/Study_2/Study_2/Multi_Trajecotry_Analysis_Domain_Specific_Movement_Behaviours.RData")

Multi_Trajecotry_Analysis_Domain_Specific_Movement_Behaviours %>%
  summarise(across(starts_with(c("LPA","MVPA","SB","sleep")), .fns=~max(.x)))

Multi_Trajecotry_Analysis_Domain_Specific_Movement_Behaviours <- 
  Multi_Trajecotry_Analysis_Domain_Specific_Movement_Behaviours %>% 
  as_tibble() %>% 
  mutate(across(starts_with(c("LPA","MVPA","SB","sleep")), scale, .names = "z{.col}"))

Multi_Trajecotry_Analysis_Domain_Specific_Movement_Behaviours %>%
  summarise(across(starts_with("z"), .fns=~max(.x)))

Multi_Trajecotry_Analysis_Domain_Specific_Movement_Behaviours %>%
  summarise(across(starts_with("z"), .fns=~min(.x)))

Multi_Trajecotry_Analysis_Domain_Specific_Movement_Behaviours %>%
  select(starts_with("z")) %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(x=name, y=value)) + 
  geom_boxplot(aes(fill=name)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# Save the dataset
foreign::write.dta(Multi_Trajecotry_Analysis_Domain_Specific_Movement_Behaviours, 
                   "Z:/7-Data/M&B/LSAC dataset/Study_2/Study_2/Multi_Trajecotry_Analysis_Domain_Specific_Movement_Behaviours_Scaled.dta")

