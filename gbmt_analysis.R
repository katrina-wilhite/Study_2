library(gbmt)
library(tidyverse)

df <- haven::read_dta("Z:\\7-Data\\M&B\\LSAC dataset\\Study_2\\Study_2\\Multi_Trajecotry_Analysis_Domain_Specific_Movement_Behaviours_with_Groups.dta")

df_long <- df %>% 
  distinct(hicid, .keep_all = TRUE) %>% 
  
  pivot_longer(cols = ends_with(c("10","12","14")),
                    names_to = c(".value","time"),
                    names_pattern = "(.*?)_(\\d\\d)") %>% 
  
  mutate(across(where(is.numeric), ~if_else(.x==0, 0.01,.x)),
         time = as.integer(time)) %>% 
  
  as.data.frame()

variables = c("LPA_at","MVPA_at", "SB_at", "sleep_at")

df_long_reduced <- df_long %>% select(variables, time, hicid)

threegroup_sq <- gbmt(x.names=variables, unit="hicid", time="time", d=1, ng=4, data=df_long_reduced)

threegroup_sq$assign.list
threegroup_sq$fitted


plot(threegroup_sq, group=1)
plot(threegroup_sq, group=2)
plot(threegroup_sq, group=3)


length(threegroup_sq$assign.list$`1`)
length(threegroup_sq$assign.list$`2`)
length(threegroup_sq$assign.list$`3`)
df %>% group_by(nd_threegroup_Group) %>% tally()





df_long %>% as_tibble() %>% select(variables, time, hicid) %>% drop_na()

df_long %>% as_tibble() %>% filter(hicid=="11103481")



data(agrisus2)
varNames <- c("TFP_2005_CAP","NetCapital_GVA","Manager_ratio",
              "FactorIncome_paid_2010","EntrIncome_unpaid_2010",
              "Income_rur","Unempl_rur","Poverty_rur",
              "RenewProd","Organic_p","GHG_UAA")
m4_3 <- gbmt(x.names=varNames, unit="Country", time="Year", d=3, ng=4 ,data=agrisus2)
m4_3$assign.list


agrisus2 %>% as_tibble()


df <- haven::read_dta("Z:\\LSAC dataset\\Study_2\\Study_2\\No_outliers_Multi_Trajectory_Analysis_Domain_Specific_Movement_Behaviours.dta")



df_long <- df %>% 
  distinct(hicid, .keep_all = TRUE) %>% 
  
  pivot_longer(cols = ends_with(c("10","12","14")),
               names_to = c(".value","time"),
               names_pattern = "(.*?)_(\\d\\d)") %>% 
  
  mutate(across(where(is.numeric), ~if_else(.x==0, 0.01,.x)),
         time = as.integer(time)) %>% 
  
  as.data.frame()

variables = c("LPA_at","MVPA_at", "SB_at", "sleep_at")

df_long_reduced <- df_long %>% select(variables, time, hicid)

fourgroup_sq <- gbmt(x.names=variables, unit="hicid", time="time", d=1, ng=4, data=df_long_reduced)

fourgroup_sq$assign.list
fourgroup_sq$fitted


plot(fourgroup_sq, group=1)
plot(fourgroup_sq, group=2)
plot(fourgroup_sq, group=3)
plot(fourgroup_sq, group=4)


length(fourgroup_sq$assign.list$`1`)
length(fourgroup_sq$assign.list$`2`)
length(fourgroup_sq$assign.list$`3`)
length(fourgroup_sq$assign.list$`4`)
df %>% group_by(nd_fourgroup_Group) %>% tally()

fourgroup_r <- gbmt(x.names=variables, unit="hicid", time="time", d=1, ng=4, data=df_long_reduced, nstart = 100)

ng <- 1:6
modList <- vector("list", length=length(ng))
names(modList) <- ng
for(i in 1:length(ng)) {
  set.seed(123)  ## seed for reproducibility
  modList[[i]] <- gbmt(x.names=variables, unit="hicid", time="time", d=1, ng=ng[i],
                       data=df_long_reduced, nstart=100)
}
do.call(rbind, lapply(modList, function(x){x$ic}))

threegroup_sq <- gbmt(x.names=variables, unit="hicid", time="time", d=1, ng=3, data=df_long_reduced)

threegroup_sq$assign.list
threegroup_sq$fitted


plot(threegroup_sq, group=1)
plot(threegroup_sq, group=2)
plot(threegroup_sq, group=3)


length(threegroup_sq$assign.list$`1`)
length(threegroup_sq$assign.list$`2`)
length(threegroup_sq$assign.list$`3`)
df %>% group_by(nd_threegroup_Group) %>% tally()

#6 group model is the best fit 

#6 Groups Results
df_long <- df %>% 
  distinct(hicid, .keep_all = TRUE) %>% 
  
  pivot_longer(cols = ends_with(c("10","12","14")),
               names_to = c(".value","time"),
               names_pattern = "(.*?)_(\\d\\d)") %>% 
  
  mutate(across(where(is.numeric), ~if_else(.x==0, 0.01,.x)),
         time = as.integer(time)) %>% 
  
  as.data.frame()

variables = c("LPA_at","MVPA_at", "SB_at", "sleep_at")

df_long_reduced <- df_long %>% select(variables, time, hicid)

sixgroup_sq <- gbmt(x.names=variables, unit="hicid", time="time", d=1, ng=6, data=df_long_reduced)

sixgroup_sq$assign.list
sixgroup_sq$fitted


plot(sixgroup_sq, group=1)
plot(sixgroup_sq, group=2)
plot(sixgroup_sq, group=3)
plot(sixgroup_sq, group=4)
plot(sixgroup_sq, group=5)
plot(sixgroup_sq, group=6)


length(sixgroup_sq$assign.list$`1`)
length(sixgroup_sq$assign.list$`2`)
length(sixgroup_sq$assign.list$`3`)
length(sixgroup_sq$assign.list$`4`)
length(sixgroup_sq$assign.list$`5`)
length(sixgroup_sq$assign.list$`6`)
df %>% group_by(nd_sixgroup_Group) %>% tally()

#5 Groups results

fivegroup_sq <- gbmt(x.names=variables, unit="hicid", time="time", d=1, ng=5, data=df_long_reduced)

fivegroup_sq$assign.list
fivegroup_sq$fitted


plot(fivegroup_sq, group=1)
plot(fivegroup_sq, group=2)
plot(fivegroup_sq, group=3)
plot(fivegroup_sq, group=4)
plot(fivegroup_sq, group=5)


length(fivegroup_sq$assign.list$`1`)
length(fivegroup_sq$assign.list$`2`)
length(fivegroup_sq$assign.list$`3`)
length(fivegroup_sq$assign.list$`4`)
length(fivegroup_sq$assign.list$`5`)
length(fivegroup_sq$assign.list$`6`)

