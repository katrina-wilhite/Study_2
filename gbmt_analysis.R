install.packages("foreach")
install.packages("doParallel")
library(gbmt)
library(tidyverse)
library(foreach)
library(doParallel)
library(data.table)
library(reshape2)
library(ggplot2)
library(fmsb)
library(dplyr)
library(devtools)
install.packages("GGally")
library("GGally")

df <- haven::read_dta("Z:\\LSAC dataset\\Study_2\\Study_2\\No_outliers_Multi_Trajectory_Analysis_Domain_Specific_Movement_Behaviours.dta")

df_weekday <- df[df$day_of_week_at_10 ==1,]

df_long <- df_weekday %>%
  distinct(hicid, .keep_all = TRUE) %>%
  pivot_longer(
    cols = ends_with(c("10", "12", "14")),
    names_to = c(".value", "time"),
    names_pattern = "(.*?)_(\\d\\d)"
  ) %>%
  mutate(across(where(is.numeric), ~ if_else(.x == 0, 0.01, .x)),
         time = as.integer(time)) %>%
  as.data.frame()

variables = c("LPA_at","MVPA_at", "SB_at", "sleep_at")

df_long_reduced <- df_long %>% 
  select(variables, time, hicid)


# Setup the backend cluster
cl <- makeCluster(detectCores()-1) # you can reduce this if needed
registerDoParallel(cl)

# Define the function to process
get_metrics <- function(df, variables){
  foreach(ng=rep(2:6,2), d=c(rep(1,5), rep(2,5))) %dopar% {
    set.seed(42)
    res <- gbmt::gbmt(x.names=variables, unit="hicid", time="time", 
                d=d, ng=ng, 
                data=df, 
                nstart = 10)
    res
  }
  
}

# Run the function
results <- get_metrics(df_long_reduced, variables)

# Put the results in a table
do.call(rbind, lapply(results, function(x){
  y <- x$ic
  y["ng"] <- x$call$ng
  y["d"] <- x$call$d
  y
}))

#Best fit is model 5 (bic, caic, ssbic) or 10 (aic, hqic), depending on test (6 group for both models); model 10 had the lowest score overall
# You do not need to rerun the model, just call the best fitting one
# E.g., if model 3 had the best fit:
results[[10]]$fitted
results[[10]]$assign.list 


plot(results[[10]], group=1)
plot(results[[10]], group=2)
plot(results[[10]], group=3)
plot(results[[10]], group=4)
plot(results[[10]], group=5)
plot(results[[10]], group=6)


length(results[[10]]$assign.list$`1`)
length(results[[10]]$assign.list$`2`)
length(results[[10]]$assign.list$`3`)
length(results[[10]]$assign.list$`4`)
length(results[[10]]$assign.list$`5`)
length(results[[10]]$assign.list$`6`)

#Add trajectory assignments to original dataframe 
results[[10]]$assign.list$`1`-> traj_1
results[[10]]$assign.list$`2`-> traj_2
results[[10]]$assign.list$`3`-> traj_3
results[[10]]$assign.list$`4`-> traj_4
results[[10]]$assign.list$`5`-> traj_5
results[[10]]$assign.list$`6`-> traj_6

as.data.frame(traj_1)-> traj_1_df
as.data.frame(traj_2)-> traj_2_df
as.data.frame(traj_3)-> traj_3_df
as.data.frame(traj_4)-> traj_4_df
as.data.frame(traj_5)-> traj_5_df
as.data.frame(traj_6)-> traj_6_df

traj_1_df <-
  traj_1_df %>% 
  mutate(trajectory = 1)
traj_2_df <-
  traj_2_df %>% 
  mutate(trajectory = 2)
traj_3_df <-
  traj_3_df %>% 
  mutate(trajectory = 3)
traj_4_df <-
  traj_4_df %>% 
  mutate(trajectory = 4)
traj_5_df <-
  traj_5_df %>% 
  mutate(trajectory = 5)
traj_6_df <-
  traj_6_df %>% 
  mutate(trajectory = 6)

rename(traj_1_df, hicid = traj_1) -> traj_1_df
rename(traj_2_df, hicid = traj_2) -> traj_2_df
rename(traj_3_df, hicid = traj_3) -> traj_3_df
rename(traj_4_df, hicid = traj_4) -> traj_4_df
rename(traj_5_df, hicid = traj_5) -> traj_5_df
rename(traj_6_df, hicid = traj_6) -> traj_6_df

do.call("rbind", list(traj_1_df, traj_2_df, traj_3_df, traj_4_df, traj_5_df, traj_6_df)) -> weekday_trajectory_assignments
arrange(weekday_trajectory_assignments, hicid) -> weekday_trajectory_assignments
df_weekday$trajecotry_assignments <- weekday_trajectory_assignments$trajectory
df_weekday[,"trajecotry_assignments"]


trajectory_1 <- df_weekday[(df_weekday$trajecotry_assignments == 1),]
trajectory_2 <- df_weekday[(df_weekday$trajecotry_assignments == 2),]
trajectory_3 <- df_weekday[(df_weekday$trajecotry_assignments == 3),]
trajectory_4 <- df_weekday[(df_weekday$trajecotry_assignments == 4),]
trajectory_5 <- df_weekday[(df_weekday$trajecotry_assignments == 5),]
trajectory_6 <- df_weekday[(df_weekday$trajecotry_assignments == 6),]

#Plot LPA per trajectory
round(mean(trajectory_1$LPA_at_10), digits = 0) -> traj_1_10_LPA
round(mean(trajectory_1$LPA_at_12), digits = 0) -> traj_1_12_LPA 
round(mean(trajectory_1$LPA_at_14), digits = 0) -> traj_1_14_LPA

round(mean(trajectory_2$LPA_at_10), digits = 0) -> traj_2_10_LPA
round(mean(trajectory_2$LPA_at_12), digits = 0) -> traj_2_12_LPA
round(mean(trajectory_2$LPA_at_14), digits = 0) -> traj_2_14_LPA

round(mean(trajectory_3$LPA_at_10), digits = 0) -> traj_3_10_LPA
round(mean(trajectory_3$LPA_at_12), digits = 0) -> traj_3_12_LPA 
round(mean(trajectory_3$LPA_at_14), digits = 0) -> traj_3_14_LPA

round(mean(trajectory_4$LPA_at_10), digits = 0) -> traj_4_10_LPA
round(mean(trajectory_4$LPA_at_12), digits = 0) -> traj_4_12_LPA 
round(mean(trajectory_4$LPA_at_14), digits = 0) -> traj_4_14_LPA

round(mean(trajectory_5$LPA_at_10), digits = 0) -> traj_5_10_LPA
round(mean(trajectory_5$LPA_at_12), digits = 0) -> traj_5_12_LPA 
round(mean(trajectory_5$LPA_at_14), digits = 0) -> traj_5_14_LPA

round(mean(trajectory_6$LPA_at_10), digits = 0) -> traj_6_10_LPA
round(mean(trajectory_6$LPA_at_12), digits = 0) -> traj_6_12_LPA 
round(mean(trajectory_6$LPA_at_14), digits = 0) -> traj_6_14_LPA

age <- rep(c("10", "12", "14"), times = 6)
trajectory <- rep(c(1:6), each = 3)
minutes_LPA <- c(traj_1_10_LPA, traj_1_12_LPA, traj_1_14_LPA, traj_2_10_LPA, traj_2_12_LPA, traj_2_14_LPA, traj_3_10_LPA, traj_3_12_LPA, traj_3_14_LPA, traj_4_10_LPA, traj_4_12_LPA, traj_4_14_LPA, traj_5_10_LPA, traj_5_12_LPA, traj_5_14_LPA, traj_6_10_LPA, traj_6_12_LPA, traj_6_14_LPA)
LPA_plot <- data.frame(age, trajectory, minutes_LPA)
LPA_plot[] <- lapply(LPA_plot, as.numeric)
p_LPA <- ggplot(data = LPA_plot, aes(x = age , y = minutes_LPA, group = trajectory, colour = as.factor(trajectory))) + geom_line() + scale_color_discrete(name = "Trajectory") + ggtitle("Light-Intensity Physical Activity") + theme(plot.title = element_text(hjust = 0.5)) + labs(y = "minutes") 

#Plot MVPA per trajectory
round(mean(trajectory_1$MVPA_at_10), digits = 0) -> traj_1_10_MVPA
round(mean(trajectory_1$MVPA_at_12), digits = 0) -> traj_1_12_MVPA 
round(mean(trajectory_1$MVPA_at_14), digits = 0) -> traj_1_14_MVPA

round(mean(trajectory_2$MVPA_at_10), digits = 0) -> traj_2_10_MVPA
round(mean(trajectory_2$MVPA_at_12), digits = 0) -> traj_2_12_MVPA
round(mean(trajectory_2$MVPA_at_14), digits = 0) -> traj_2_14_MVPA

round(mean(trajectory_3$MVPA_at_10), digits = 0) -> traj_3_10_MVPA
round(mean(trajectory_3$MVPA_at_12), digits = 0) -> traj_3_12_MVPA 
round(mean(trajectory_3$MVPA_at_14), digits = 0) -> traj_3_14_MVPA

round(mean(trajectory_4$MVPA_at_10), digits = 0) -> traj_4_10_MVPA
round(mean(trajectory_4$MVPA_at_12), digits = 0) -> traj_4_12_MVPA 
round(mean(trajectory_4$MVPA_at_14), digits = 0) -> traj_4_14_MVPA

round(mean(trajectory_5$MVPA_at_10), digits = 0) -> traj_5_10_MVPA
round(mean(trajectory_5$MVPA_at_12), digits = 0) -> traj_5_12_MVPA 
round(mean(trajectory_5$MVPA_at_14), digits = 0) -> traj_5_14_MVPA

round(mean(trajectory_6$MVPA_at_10), digits = 0) -> traj_6_10_MVPA
round(mean(trajectory_6$MVPA_at_12), digits = 0) -> traj_6_12_MVPA 
round(mean(trajectory_6$MVPA_at_14), digits = 0) -> traj_6_14_MVPA

age <- rep(c("10", "12", "14"), times = 6)
trajectory <- rep(c(1:6), each = 3)
minutes_MVPA <- c(traj_1_10_MVPA, traj_1_12_MVPA, traj_1_14_MVPA, traj_2_10_MVPA, traj_2_12_MVPA, traj_2_14_MVPA, traj_3_10_MVPA, traj_3_12_MVPA, traj_3_14_MVPA, traj_4_10_MVPA, traj_4_12_MVPA, traj_4_14_MVPA, traj_5_10_MVPA, traj_5_12_MVPA, traj_5_14_MVPA, traj_6_10_MVPA, traj_6_12_MVPA, traj_6_14_MVPA)
MVPA_plot <- data.frame(age, trajectory, minutes_MVPA)
MVPA_plot[] <- lapply(MVPA_plot, as.numeric)
p_MVPA <- ggplot(data = MVPA_plot, aes(x = age , y = minutes_MVPA, group = trajectory, colour = as.factor(trajectory))) + geom_line() + scale_color_discrete(name = "Trajectory") + ggtitle("Moderate- to Vigorous- Intensity Physical Activity") + theme(plot.title = element_text(hjust = 0.5)) + labs(y = "minutes") 

#Plot sleep per trajectory
round(mean(trajectory_1$sleep_at_10), digits = 0) -> traj_1_10_sleep
round(mean(trajectory_1$sleep_at_12), digits = 0) -> traj_1_12_sleep 
round(mean(trajectory_1$sleep_at_14), digits = 0) -> traj_1_14_sleep

round(mean(trajectory_2$sleep_at_10), digits = 0) -> traj_2_10_sleep
round(mean(trajectory_2$sleep_at_12), digits = 0) -> traj_2_12_sleep
round(mean(trajectory_2$sleep_at_14), digits = 0) -> traj_2_14_sleep

round(mean(trajectory_3$sleep_at_10), digits = 0) -> traj_3_10_sleep
round(mean(trajectory_3$sleep_at_12), digits = 0) -> traj_3_12_sleep 
round(mean(trajectory_3$sleep_at_14), digits = 0) -> traj_3_14_sleep

round(mean(trajectory_4$sleep_at_10), digits = 0) -> traj_4_10_sleep
round(mean(trajectory_4$sleep_at_12), digits = 0) -> traj_4_12_sleep 
round(mean(trajectory_4$sleep_at_14), digits = 0) -> traj_4_14_sleep

round(mean(trajectory_5$sleep_at_10), digits = 0) -> traj_5_10_sleep
round(mean(trajectory_5$sleep_at_12), digits = 0) -> traj_5_12_sleep 
round(mean(trajectory_5$sleep_at_14), digits = 0) -> traj_5_14_sleep

round(mean(trajectory_6$sleep_at_10), digits = 0) -> traj_6_10_sleep
round(mean(trajectory_6$sleep_at_12), digits = 0) -> traj_6_12_sleep 
round(mean(trajectory_6$sleep_at_14), digits = 0) -> traj_6_14_sleep

age <- rep(c("10", "12", "14"), times = 6)
trajectory <- rep(c(1:6), each = 3)
minutes_sleep <- c(traj_1_10_sleep, traj_1_12_sleep, traj_1_14_sleep, traj_2_10_sleep, traj_2_12_sleep, traj_2_14_sleep, traj_3_10_sleep, traj_3_12_sleep, traj_3_14_sleep, traj_4_10_sleep, traj_4_12_sleep, traj_4_14_sleep, traj_5_10_sleep, traj_5_12_sleep, traj_5_14_sleep, traj_6_10_sleep, traj_6_12_sleep, traj_6_14_sleep)
sleep_plot <- data.frame(age, trajectory, minutes_sleep)
sleep_plot[] <- lapply(sleep_plot, as.numeric)
p_sleep <- ggplot(data = sleep_plot, aes(x = age , y = minutes_sleep, group = trajectory, colour = as.factor(trajectory))) + geom_line() + scale_color_discrete(name = "Trajectory") + ggtitle("Sleep") + theme(plot.title = element_text(hjust = 0.5)) + labs(y = "minutes") 

#Plot SB per trajectory
round(mean(trajectory_1$SB_at_10), digits = 0) -> traj_1_10_SB
round(mean(trajectory_1$SB_at_12), digits = 0) -> traj_1_12_SB 
round(mean(trajectory_1$SB_at_14), digits = 0) -> traj_1_14_SB

round(mean(trajectory_2$SB_at_10), digits = 0) -> traj_2_10_SB
round(mean(trajectory_2$SB_at_12), digits = 0) -> traj_2_12_SB
round(mean(trajectory_2$SB_at_14), digits = 0) -> traj_2_14_SB

round(mean(trajectory_3$SB_at_10), digits = 0) -> traj_3_10_SB
round(mean(trajectory_3$SB_at_12), digits = 0) -> traj_3_12_SB 
round(mean(trajectory_3$SB_at_14), digits = 0) -> traj_3_14_SB

round(mean(trajectory_4$SB_at_10), digits = 0) -> traj_4_10_SB
round(mean(trajectory_4$SB_at_12), digits = 0) -> traj_4_12_SB 
round(mean(trajectory_4$SB_at_14), digits = 0) -> traj_4_14_SB

round(mean(trajectory_5$SB_at_10), digits = 0) -> traj_5_10_SB
round(mean(trajectory_5$SB_at_12), digits = 0) -> traj_5_12_SB 
round(mean(trajectory_5$SB_at_14), digits = 0) -> traj_5_14_SB

round(mean(trajectory_6$SB_at_10), digits = 0) -> traj_6_10_SB
round(mean(trajectory_6$SB_at_12), digits = 0) -> traj_6_12_SB 
round(mean(trajectory_6$SB_at_14), digits = 0) -> traj_6_14_SB

age <- rep(c("10", "12", "14"), times = 6)
trajectory <- rep(c(1:6), each = 3)
minutes_SB <- c(traj_1_10_SB, traj_1_12_SB, traj_1_14_SB, traj_2_10_SB, traj_2_12_SB, traj_2_14_SB, traj_3_10_SB, traj_3_12_SB, traj_3_14_SB, traj_4_10_SB, traj_4_12_SB, traj_4_14_SB, traj_5_10_SB, traj_5_12_SB, traj_5_14_SB, traj_6_10_SB, traj_6_12_SB, traj_6_14_SB)
SB_plot <- data.frame(age, trajectory, minutes_SB)
SB_plot[] <- lapply(SB_plot, as.numeric)
p_SB <- ggplot(data = SB_plot, aes(x = age , y = minutes_SB, group = trajectory, colour = as.factor(trajectory))) + geom_line() + scale_color_discrete(name = "Trajectory") + ggtitle("Sedentary Behaviour") + theme(plot.title = element_text(hjust = 0.5)) + labs(y = "minutes") 

#Make Radar Chart of General MB based on trajectories 
#First, make new mini datframes for each MB by timepoint
LPA_plot[LPA_plot$age == 10,] %>% 
  select(-age) %>% 
  mutate(MB = "LPA") %>% 
  rename("minutes" = "minutes_LPA") -> LPA_for_radar_10

LPA_plot[LPA_plot$age == 12,] %>% 
  select(-age) %>% 
  mutate(MB = "LPA") %>% 
  rename("minutes" = "minutes_LPA") -> LPA_for_radar_12

LPA_plot[LPA_plot$age == 14,] %>% 
  select(-age) %>% 
  mutate(MB = "LPA") %>% 
  rename("minutes" = "minutes_LPA") -> LPA_for_radar_14

MVPA_plot[MVPA_plot$age == 10,] %>% 
  select(-age) %>% 
  mutate(MB = "MVPA") %>% 
  rename("minutes" = "minutes_MVPA") -> MVPA_for_radar_10

MVPA_plot[MVPA_plot$age == 12,] %>% 
  select(-age) %>% 
  mutate(MB = "MVPA") %>% 
  rename("minutes" = "minutes_MVPA") -> MVPA_for_radar_12

MVPA_plot[MVPA_plot$age == 14,] %>% 
  select(-age) %>% 
  mutate(MB = "MVPA") %>% 
  rename("minutes" = "minutes_MVPA") -> MVPA_for_radar_14

sleep_plot[sleep_plot$age == 10,] %>% 
  select(-age) %>% 
  mutate(MB = "sleep") %>% 
  rename("minutes" = "minutes_sleep") -> sleep_for_radar_10

sleep_plot[sleep_plot$age == 12,] %>% 
  select(-age) %>% 
  mutate(MB = "sleep") %>% 
  rename("minutes" = "minutes_sleep") -> sleep_for_radar_12

sleep_plot[sleep_plot$age == 14,] %>% 
  select(-age) %>% 
  mutate(MB = "sleep") %>% 
  rename("minutes" = "minutes_sleep") -> sleep_for_radar_14

SB_plot[SB_plot$age == 10,] %>% 
  select(-age) %>% 
  mutate(MB = "SB") %>% 
  rename("minutes" = "minutes_SB") -> SB_for_radar_10

SB_plot[SB_plot$age == 12,] %>% 
  select(-age) %>% 
  mutate(MB = "SB") %>% 
  rename("minutes" = "minutes_SB") -> SB_for_radar_12

SB_plot[SB_plot$age == 14,] %>% 
  select(-age) %>% 
  mutate(MB = "SB") %>% 
  rename("minutes" = "minutes_SB") -> SB_for_radar_14

#Combine each MB by timepoint 
do.call("rbind", list(LPA_for_radar_10, MVPA_for_radar_10, sleep_for_radar_10, SB_for_radar_10)) -> General_MB_radar_10
do.call("rbind", list(LPA_for_radar_12, MVPA_for_radar_12, sleep_for_radar_12, SB_for_radar_12)) -> General_MB_radar_12
do.call("rbind", list(LPA_for_radar_14, MVPA_for_radar_14, sleep_for_radar_14, SB_for_radar_14)) -> General_MB_radar_14
#Convert dataframes to wide format 
wide_General_MB_radar_10 <- spread(General_MB_radar_10, MB, minutes)  
wide_General_MB_radar_10 %>% 
  mutate_each_(list(~scale(.) %>% as.vector),
               vars = c(2:5)) -> scaled_general_MB_for_radar_10
scaled_general_MB_for_radar_10 <- rbind(rep(2,4) , rep(-2,4) , scaled_general_MB_for_radar_10)
scaled_general_MB_for_radar_10$trajectory = paste0("Trajectory", scaled_general_MB_for_radar_10$trajectory) 

trajcolour <- c("red","blue","green","yellow","purple","black")
radarchart(scaled_general_MB_for_radar_10[2:5], pcol = trajcolour, cglty = 1, cglcol = "grey", plty = 1, title = 'General Movement Behaviours at 10 per Trajectory', axistype = 2)
legend(x= -1.85, y = -.07, legend = (scaled_general_MB_for_radar_10$trajectory[3:8]), bty = "n", pch=20, text.col = "grey", cex=1.2, pt.cex=3, col = trajcolour)

#Make a radar chart with a reference group; I choose trajectory 3 as the reference group because they have the most "ideal" behaviors
wide_General_MB_radar_10 / wide_General_MB_radar_10[rep(3, nrow(wide_General_MB_radar_10)),] -> reference_radar_10
reference_radar_10 <- rbind(rep(1.5,4) , rep(0,4) , reference_radar_10)
reference_radar_10$trajectory <- reference_radar_10$trajectory*3
reference_radar_10$trajectory = paste0("Trajectory ", reference_radar_10$trajectory)
radarchart(reference_radar_10[2:5], pcol = trajcolour, cglty = 1, cglcol = "grey", plty = 1, title = 'General Movement Behaviours at 10 (Reference Group)', axistype = 2)
legend(x= -1.85, y = -.07, legend = (reference_radar_10$trajectory[3:8]), bty = "n", pch=20, text.col = "grey", cex=1.2, pt.cex=3, col = trajcolour)

#Repeat radar charts for age 12 
wide_General_MB_radar_12 <- spread(General_MB_radar_12, MB, minutes)  
wide_General_MB_radar_12 %>% 
  mutate_each_(list(~scale(.) %>% as.vector),
               vars = c(2:5)) -> scaled_general_MB_for_radar_12
scaled_general_MB_for_radar_12 <- rbind(rep(2,4) , rep(-2,4) , scaled_general_MB_for_radar_12)
scaled_general_MB_for_radar_12$trajectory = paste0("Trajectory", scaled_general_MB_for_radar_12$trajectory) 

trajcolour <- c("red","blue","green","yellow","purple","black")
radarchart(scaled_general_MB_for_radar_12[2:5], pcol = trajcolour, cglty = 1, cglcol = "grey", plty = 1, title = 'General Movement Behaviours at 12 per Trajectory', axistype = 2)
legend(x= -1.85, y = -.07, legend = (scaled_general_MB_for_radar_12$trajectory[3:8]), bty = "n", pch=20, text.col = "grey", cex=1.2, pt.cex=3, col = trajcolour)

#Make a radar chart with a reference group; I choose trajectory 3 as the reference group because they have the most "ideal" behaviors
wide_General_MB_radar_12 / wide_General_MB_radar_12[rep(3, nrow(wide_General_MB_radar_12)),] -> reference_radar_12
reference_radar_12 <- rbind(rep(1.5,4) , rep(0,4) , reference_radar_12)
reference_radar_12$trajectory <- reference_radar_12$trajectory*3
reference_radar_12$trajectory = paste0("Trajectory ", reference_radar_12$trajectory)
radarchart(reference_radar_12[2:5], pcol = trajcolour, cglty = 1, cglcol = "grey", plty = 1, title = 'General Movement Behaviours at 12 (Reference group)')
legend(x= -1.85, y = -.07, legend = (reference_radar_12$trajectory[3:8]), bty = "n", pch=20, text.col = "grey", cex=1.2, pt.cex=3, col = trajcolour)

#Repeat radar charts for age 14 
wide_General_MB_radar_14 <- spread(General_MB_radar_14, MB, minutes)  
wide_General_MB_radar_14 %>% 
  mutate_each_(list(~scale(.) %>% as.vector),
               vars = c(2:5)) -> scaled_general_MB_for_radar_14
scaled_general_MB_for_radar_14 <- rbind(rep(2,4) , rep(-2,4) , scaled_general_MB_for_radar_14)
scaled_general_MB_for_radar_14$trajectory = paste0("Trajectory", scaled_general_MB_for_radar_14$trajectory) 

trajcolour <- c("red","blue","green","yellow","purple","black")
radarchart(scaled_general_MB_for_radar_14[2:5], pcol = trajcolour, cglty = 1, cglcol = "grey", plty = 1, title = 'General Movement Behaviours at 14 per Trajectory', axistype = 2)
legend(x= -1.85, y = -.07, legend = (scaled_general_MB_for_radar_14$trajectory[3:8]), bty = "n", pch=20, text.col = "grey", cex=1.2, pt.cex=3, col = trajcolour)

#Make a radar chart with a reference group; I choose trajectory 3 as the reference group because they have the most "ideal" behaviors
wide_General_MB_radar_14 / wide_General_MB_radar_14[rep(3, nrow(wide_General_MB_radar_14)),] -> reference_radar_14
reference_radar_14 <- rbind(rep(1.5,4) , rep(0,4) , reference_radar_14)
reference_radar_14$trajectory <- reference_radar_14$trajectory*3
reference_radar_14$trajectory = paste0("Trajectory ", reference_radar_14$trajectory)
radarchart(reference_radar_14[2:5], pcol = trajcolour, cglty = 1, cglcol = "grey", plty = 1, title = 'General Movement Behaviours at 14 (Reference Group)', axistype = 2)
legend(x= -1.85, y = -.07, legend = (reference_radar_14$trajectory[3:8]), bty = "n", pch=20, text.col = "grey", cex=1.2, pt.cex=3, col = trajcolour)

#The radar charts are very difficult to read with so many groups; I have also read that you shouldn't plot more than 3 series on a single radar chart. So I decided  to try a proportional stacked bar graph and a parallel coordinate plots
wide_General_MB_radar_10$trajectory = paste0("Trajectory ", wide_General_MB_radar_10$trajectory)
install.packages("gcookbook")
library("gcookbook")
head(cabbage_exp)
ggplot(cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) +
  geom_col(position = "fill")

ggplot(General_MB_radar_10, aes(x = trajectory, y = minutes, fill = MB)) + geom_col(position = "fill") + ggtitle("General Movement Behaviours at Age 10") + theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0,1,.0833), labels = c(0,2,4,6,8,10,12,14,16,18,20,22,24), name = "hours") +
  scale_x_continuous(breaks = seq(1,6,1))
#Repeat for Age 12 
ggplot(General_MB_radar_12, aes(x = trajectory, y = minutes, fill = MB)) + geom_col(position = "fill") + ggtitle("General Movement Behaviours at Age 12") + theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0,1,.0833), labels = c(0,2,4,6,8,10,12,14,16,18,20,22,24), name = "hours") +
  scale_x_continuous(breaks = seq(1,6,1))
#Repeat for Age 14 
ggplot(General_MB_radar_14, aes(x = trajectory, y = minutes, fill = MB)) + geom_col(position = "fill") + ggtitle("General Movement Behaviours at Age 14") + theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0,1,.0833), labels = c(0,2,4,6,8,10,12,14,16,18,20,22,24), name = "hours") +
  scale_x_continuous(breaks = seq(1,6,1))
#Another option is a parallel coordinate plot - this one is my favorite 
install.packages("plotly")
library("plotly")

as.factor(wide_General_MB_radar_10$trajectory) -> wide_General_MB_radar_10$trajectory
wide_General_MB_radar_10$trajectory_id <- c(1:6)

General_MB_10_parallel_plot <- wide_General_MB_radar_10 %>% plot_ly(type = 'parcoords',
                     line = list(color = ~trajectory_id,
                                 colorscale = 'Rainbow'),                       
                      dimensions = list(
                        list(range = c(70,210),
                             label = 'LPA', values = ~LPA),
                        list(range = c(20,85),
                             label = 'MVPA', values = ~MVPA),
                        list(range = c(550,740),
                             label = 'SB', values = ~SB),
                        list(range = c(520,610),
                             label = 'Sleep', values = ~sleep)
                      )
)  %>% layout(title = "General Movement Behaviours at Age 10")

as.factor(wide_General_MB_radar_12$trajectory) -> wide_General_MB_radar_12$trajectory
wide_General_MB_radar_12$trajectory_id <- c(1:6)

General_MB_at_12 <- wide_General_MB_radar_12 %>% plot_ly(type = 'parcoords',
                                                         line = list(color = ~trajectory_id,
                                                                     colorscale = 'Rainbow'),                       
                                                         dimensions = list(
                                                           list(range = c(70,210),
                                                                label = 'LPA', values = ~LPA),
                                                           list(range = c(20,85),
                                                                label = 'MVPA', values = ~MVPA),
                                                           list(range = c(550,740),
                                                                label = 'SB', values = ~SB),
                                                           list(range = c(520,610),
                                                                label = 'Sleep', values = ~sleep)
                                                         )
)  %>% layout(title = "General Movement Behaviours at Age 12")

as.factor(wide_General_MB_radar_14$trajectory) -> wide_General_MB_radar_14$trajectory
wide_General_MB_radar_14$trajectory_id <- c(1:6)

General_MB_at_14 <- wide_General_MB_radar_14 %>% plot_ly(type = 'parcoords',
                                                         line = list(color = ~trajectory_id,
                                                                     colorscale = 'Rainbow'),                       
                                                         dimensions = list(
                                                           list(range = c(70,210),
                                                                label = 'LPA', values = ~LPA),
                                                           list(range = c(20,85),
                                                                label = 'MVPA', values = ~MVPA),
                                                           list(range = c(550,740),
                                                                label = 'SB', values = ~SB),
                                                           list(range = c(520,610),
                                                                label = 'Sleep', values = ~sleep)
                                                         )
)  %>% layout(title = "General Movement Behaviours at Age 14")