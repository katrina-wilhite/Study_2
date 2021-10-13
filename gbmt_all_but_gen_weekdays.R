#install.packages("foreach")
#install.packages("doParallel")
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
library(haven)


df <- haven::read_dta("Z:\\LSAC dataset\\Study_2\\Study_2\\No_outliers_Multi_Trajectory_Analysis_Domain_Specific_Movement_Behaviours.dta")

df_gen_weekend <- df[df$day_of_week_at_10 ==2,]
save(df_gen_weekend, file ='df_gen_weekend.Rdata')

df_long <- df_gen_weekend %>%
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
results_gen_weekend <- get_metrics(df_long_reduced, variables)

#Save results 
save(results_gen_weekend, file ='results_gen_weekend.Rdata')

# Put the results in a table
do.call(rbind, lapply(results_gen_weekend, function(x){
  y <- x$ic
  y["ng"] <- x$call$ng
  y["d"] <- x$call$d
  y
}))


#Make Universal CSV files for models 1-5 for FCAP analysis 
make_csvs <- function(i){
  grps <- results_gen_weekend[[i]]$call$ng
  grpcols <- grps + 2
  first_line <- paste(results_gen_weekend[[i]]$ic[["aic"]],results_gen_weekend[[i]]$ic[["bic"]],paste0(results_gen_weekend[[i]]$logLik,"\n"), sep = ",")
  out_table <- merge(results_gen_weekend[[i]]$posterior, results_gen_weekend[[i]]$assign, by="row.names")[,2:grpcols]
  filename <- paste0("./FCAP/FCAP_gen_weekends/FCAP CSV Files_models1_5/","universal_",grps,".csv")
  cat(first_line, file=filename)
  write.table(out_table, file=filename,  append=TRUE,sep=",", row.names = FALSE, col.names = FALSE)
}

for (n in 1:5) {
  make_csvs(n)
  
}

#Make Universal CSV files for models 6-10 for FCAP analysis 
make_csvs <- function(i){
  grps <- results_gen_weekend[[i]]$call$ng
  grpcols <- grps + 2
  first_line <- paste(results_gen_weekend[[i]]$ic[["aic"]],results_gen_weekend[[i]]$ic[["bic"]],paste0(results_gen_weekend[[i]]$logLik,"\n"), sep = ",")
  out_table <- merge(results_gen_weekend[[i]]$posterior, results_gen_weekend[[i]]$assign, by="row.names")[,2:grpcols]
  filename <- paste0("./FCAP/FCAP_gen_weekends/FCAP CSV Files_models6_10/","universal_",grps,".csv")
  cat(first_line, file=filename)
  write.table(out_table, file=filename,  append=TRUE,sep=",", row.names = FALSE, col.names = FALSE)
}

for (n in 6:10) {
  make_csvs(n)
  
}





#DOMAIN-SPECIFIC WEEKDAYS  
df_domsp_weekday <- df[df$day_of_week_at_10 ==1,]
save(df_domsp_weekday, file ='df_domsp_weekday.Rdata')

df_long <- df_domsp_weekday %>%
  distinct(hicid, .keep_all = TRUE) %>%
  pivot_longer(
    cols = ends_with(c("10", "12", "14")),
    names_to = c(".value", "time"),
    names_pattern = "(.*?)_(\\d\\d)"
  ) %>%
  mutate(across(where(is.numeric), ~ if_else(.x == 0, 0.01, .x)),
         time = as.integer(time)) %>%
  as.data.frame()

variables = c("active_transport_at","daytime_naps_at", "education_SB_at", "leisure_time_SB_at", "passive_transport_at", "screen_time_at", "self_care_SB_at", "social_based_SB_at", "structured_MVPA_at", "unstructured_LPA_at", "unstructured_MVPA_at", "household_LPA_at", "nighttime_sleep_at")

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
results_domsp_weekday <- get_metrics(df_long_reduced, variables)

#Save results 
save(results_domsp_weekday, file ='results_domsp_weekday.Rdata')

# Put the results in a table
do.call(rbind, lapply(results_domsp_weekday, function(x){
  y <- x$ic
  y["ng"] <- x$call$ng
  y["d"] <- x$call$d
  y
}))


#Make Universal CSV files for models 1-5 for FCAP analysis 
make_csvs <- function(i){
  grps <- results_domsp_weekday[[i]]$call$ng
  grpcols <- grps + 2
  first_line <- paste(results_domsp_weekday[[i]]$ic[["aic"]],results_domsp_weekday[[i]]$ic[["bic"]],paste0(results_domsp_weekday[[i]]$logLik,"\n"), sep = ",")
  out_table <- merge(results_domsp_weekday[[i]]$posterior, results_domsp_weekday[[i]]$assign, by="row.names")[,2:grpcols]
  filename <- paste0("./FCAP/FCAP_domsp_weekdays/FCAP CSV Files_models1_5/","universal_",grps,".csv")
  cat(first_line, file=filename)
  write.table(out_table, file=filename,  append=TRUE,sep=",", row.names = FALSE, col.names = FALSE)
}

for (n in 1:5) {
  make_csvs(n)
  
}

#Make Universal CSV files for models 6-10 for FCAP analysis 
make_csvs <- function(i){
  grps <- results_domsp_weekday[[i]]$call$ng
  grpcols <- grps + 2
  first_line <- paste(results_domsp_weekday[[i]]$ic[["aic"]],results_domsp_weekday[[i]]$ic[["bic"]],paste0(results_domsp_weekday[[i]]$logLik,"\n"), sep = ",")
  out_table <- merge(results_domsp_weekday[[i]]$posterior, results_domsp_weekday[[i]]$assign, by="row.names")[,2:grpcols]
  filename <- paste0("./FCAP/FCAP_domsp_weekdays/FCAP CSV Files_models6_10/","universal_",grps,".csv")
  cat(first_line, file=filename)
  write.table(out_table, file=filename,  append=TRUE,sep=",", row.names = FALSE, col.names = FALSE)
}

for (n in 6:10) {
  make_csvs(n)
  
}




#DOMAIN-SPECIFIC WEEKENDS 
df_domsp_weekend <- df[df$day_of_week_at_10 ==2,]
save(df_domsp_weekend, file ='df_domsp_weekend.Rdata')

df_long <- df_domsp_weekend %>%
  distinct(hicid, .keep_all = TRUE) %>%
  pivot_longer(
    cols = ends_with(c("10", "12", "14")),
    names_to = c(".value", "time"),
    names_pattern = "(.*?)_(\\d\\d)"
  ) %>%
  mutate(across(where(is.numeric), ~ if_else(.x == 0, 0.01, .x)),
         time = as.integer(time)) %>%
  as.data.frame()

variables = c("active_transport_at","daytime_naps_at", "education_SB_at", "leisure_time_SB_at", "passive_transport_at", "screen_time_at", "self_care_SB_at", "social_based_SB_at", "structured_MVPA_at", "unstructured_LPA_at", "unstructured_MVPA_at", "household_LPA_at", "nighttime_sleep_at")

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
results_domsp_weekend <- get_metrics(df_long_reduced, variables)

#Save results 
save(results_domsp_weekend, file ='results_domsp_weekend.Rdata')

# Put the results in a table
do.call(rbind, lapply(results_domsp_weekend, function(x){
  y <- x$ic
  y["ng"] <- x$call$ng
  y["d"] <- x$call$d
  y
}))


#Make Universal CSV files for models 1-5 for FCAP analysis 
make_csvs <- function(i){
  grps <- results_domsp_weekend[[i]]$call$ng
  grpcols <- grps + 2
  first_line <- paste(results_domsp_weekend[[i]]$ic[["aic"]],results_domsp_weekend[[i]]$ic[["bic"]],paste0(results_domsp_weekend[[i]]$logLik,"\n"), sep = ",")
  out_table <- merge(results_domsp_weekend[[i]]$posterior, results_domsp_weekend[[i]]$assign, by="row.names")[,2:grpcols]
  filename <- paste0("./FCAP/FCAP_domsp_weekends/FCAP CSV Files_models1_5/","universal_",grps,".csv")
  cat(first_line, file=filename)
  write.table(out_table, file=filename,  append=TRUE,sep=",", row.names = FALSE, col.names = FALSE)
}

for (n in 1:5) {
  make_csvs(n)
  
}

#Make Universal CSV files for models 6-10 for FCAP analysis 
make_csvs <- function(i){
  grps <- results_domsp_weekend[[i]]$call$ng
  grpcols <- grps + 2
  first_line <- paste(results_domsp_weekend[[i]]$ic[["aic"]],results_domsp_weekend[[i]]$ic[["bic"]],paste0(results_domsp_weekend[[i]]$logLik,"\n"), sep = ",")
  out_table <- merge(results_domsp_weekend[[i]]$posterior, results_domsp_weekend[[i]]$assign, by="row.names")[,2:grpcols]
  filename <- paste0("./FCAP/FCAP_domsp_weekends/FCAP CSV Files_models6_10/","universal_",grps,".csv")
  cat(first_line, file=filename)
  write.table(out_table, file=filename,  append=TRUE,sep=",", row.names = FALSE, col.names = FALSE)
}

for (n in 6:10) {
  make_csvs(n)
  
}




#GENERAL WEEKDAYS SENSITIVITY ANALYSIS 
df <- haven::read_dta("Z:\\LSAC dataset\\Study_2\\Study_2\\Multi_Trajectory_Analysis_Domain_Specific_Movement_Behaviours.dta")

df_sens_gen_weekday <- df[df$day_of_week_at_10 ==1,]
save(df_sens_gen_weekday, file ='df_sens_gen_weekday.Rdata')

df_long <- df_sens_gen_weekday %>%
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
results_sens_gen_weekday <- get_metrics(df_long_reduced, variables)

#Save results 
save(results_sens_gen_weekday, file ='results_sens_gen_weekday.Rdata')

# Put the results in a table
do.call(rbind, lapply(results_sens_gen_weekday, function(x){
  y <- x$ic
  y["ng"] <- x$call$ng
  y["d"] <- x$call$d
  y
}))


#Make Universal CSV files for models 1-5 for FCAP analysis 
make_csvs <- function(i){
  grps <- results_sens_gen_weekday[[i]]$call$ng
  grpcols <- grps + 2
  first_line <- paste(results_sens_gen_weekday[[i]]$ic[["aic"]],results_sens_gen_weekday[[i]]$ic[["bic"]],paste0(results_sens_gen_weekday[[i]]$logLik,"\n"), sep = ",")
  out_table <- merge(results_sens_gen_weekday[[i]]$posterior, results_sens_gen_weekday[[i]]$assign, by="row.names")[,2:grpcols]
  filename <- paste0("./FCAP/Sensitivity Analyses/FCAP_gen_weekdays_sensitivity/FCAP CSV Files_models1_5/","universal_",grps,".csv")
  cat(first_line, file=filename)
  write.table(out_table, file=filename,  append=TRUE,sep=",", row.names = FALSE, col.names = FALSE)
}

for (n in 1:5) {
  make_csvs(n)
  
}

#Make Universal CSV files for models 6-10 for FCAP analysis 
make_csvs <- function(i){
  grps <- results_sens_gen_weekday[[i]]$call$ng
  grpcols <- grps + 2
  first_line <- paste(results_sens_gen_weekday[[i]]$ic[["aic"]],results_sens_gen_weekday[[i]]$ic[["bic"]],paste0(results_sens_gen_weekday[[i]]$logLik,"\n"), sep = ",")
  out_table <- merge(results_sens_gen_weekday[[i]]$posterior, results_sens_gen_weekday[[i]]$assign, by="row.names")[,2:grpcols]
  filename <- paste0("./FCAP/Sensitivity Analyses/FCAP_gen_weekdays_sensitivity/FCAP CSV Files_models6_10/","universal_",grps,".csv")
  cat(first_line, file=filename)
  write.table(out_table, file=filename,  append=TRUE,sep=",", row.names = FALSE, col.names = FALSE)
}

for (n in 6:10) {
  make_csvs(n)
  
}




#GENERAL WEEKends SENSITIVITY ANALYSIS 
df_sens_gen_weekend <- df[df$day_of_week_at_10 ==2,]
save(df_sens_gen_weekend, file ='df_sens_gen_weekend.Rdata')

df_long <- df_sens_gen_weekend %>%
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
results_sens_gen_weekend <- get_metrics(df_long_reduced, variables)

#Save results 
save(results_sens_gen_weekend, file ='results_sens_gen_weekend.Rdata')

# Put the results in a table
do.call(rbind, lapply(results_sens_gen_weekend, function(x){
  y <- x$ic
  y["ng"] <- x$call$ng
  y["d"] <- x$call$d
  y
}))


#Make Universal CSV files for models 1-5 for FCAP analysis 
make_csvs <- function(i){
  grps <- results_sens_gen_weekend[[i]]$call$ng
  grpcols <- grps + 2
  first_line <- paste(results_sens_gen_weekend[[i]]$ic[["aic"]],results_sens_gen_weekend[[i]]$ic[["bic"]],paste0(results_sens_gen_weekend[[i]]$logLik,"\n"), sep = ",")
  out_table <- merge(results_sens_gen_weekend[[i]]$posterior, results_sens_gen_weekend[[i]]$assign, by="row.names")[,2:grpcols]
  filename <- paste0("./FCAP/Sensitivity Analyses/FCAP_gen_weekends_sensitivity/FCAP CSV Files_models1_5/","universal_",grps,".csv")
  cat(first_line, file=filename)
  write.table(out_table, file=filename,  append=TRUE,sep=",", row.names = FALSE, col.names = FALSE)
}

for (n in 1:5) {
  make_csvs(n)
  
}

#Make Universal CSV files for models 6-10 for FCAP analysis 
make_csvs <- function(i){
  grps <- results_sens_gen_weekend[[i]]$call$ng
  grpcols <- grps + 2
  first_line <- paste(results_sens_gen_weekend[[i]]$ic[["aic"]],results_sens_gen_weekend[[i]]$ic[["bic"]],paste0(results_sens_gen_weekend[[i]]$logLik,"\n"), sep = ",")
  out_table <- merge(results_sens_gen_weekend[[i]]$posterior, results_sens_gen_weekend[[i]]$assign, by="row.names")[,2:grpcols]
  filename <- paste0("./FCAP/Sensitivity Analyses/FCAP_gen_weekends_sensitivity/FCAP CSV Files_models6_10/","universal_",grps,".csv")
  cat(first_line, file=filename)
  write.table(out_table, file=filename,  append=TRUE,sep=",", row.names = FALSE, col.names = FALSE)
}

for (n in 6:10) {
  make_csvs(n)
  
}




#DOMAIN-SPECIFIC WEEKDAYS SENSITIVITY 
df_sens_domsp_weekday <- df[df$day_of_week_at_10 ==1,]
save(df_sens_domsp_weekday, file ='df_sens_domsp_weekday.Rdata')

df_long <- df_sens_domsp_weekday %>%
  distinct(hicid, .keep_all = TRUE) %>%
  pivot_longer(
    cols = ends_with(c("10", "12", "14")),
    names_to = c(".value", "time"),
    names_pattern = "(.*?)_(\\d\\d)"
  ) %>%
  mutate(across(where(is.numeric), ~ if_else(.x == 0, 0.01, .x)),
         time = as.integer(time)) %>%
  as.data.frame()

variables = c("active_transport_at","daytime_naps_at", "education_SB_at", "leisure_time_SB_at", "passive_transport_at", "screen_time_at", "self_care_SB_at", "social_based_SB_at", "structured_MVPA_at", "unstructured_LPA_at", "unstructured_MVPA_at", "household_LPA_at", "nighttime_sleep_at")

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
results_sens_domsp_weekday <- get_metrics(df_long_reduced, variables)

#Save results 
save(results_sens_domsp_weekday, file ='results_sens_domsp_weekday.Rdata')

# Put the results in a table
do.call(rbind, lapply(results_sens_domsp_weekday, function(x){
  y <- x$ic
  y["ng"] <- x$call$ng
  y["d"] <- x$call$d
  y
}))


#Make Universal CSV files for models 1-5 for FCAP analysis 
make_csvs <- function(i){
  grps <- results_sens_domsp_weekday[[i]]$call$ng
  grpcols <- grps + 2
  first_line <- paste(results_sens_domsp_weekday[[i]]$ic[["aic"]],results_sens_domsp_weekday[[i]]$ic[["bic"]],paste0(results_sens_domsp_weekday[[i]]$logLik,"\n"), sep = ",")
  out_table <- merge(results_sens_domsp_weekday[[i]]$posterior, results_sens_domsp_weekday[[i]]$assign, by="row.names")[,2:grpcols]
  filename <- paste0("./FCAP/FCAP_domsp_weekdays_sensitivity/FCAP CSV Files_models1_5/","universal_",grps,".csv")
  cat(first_line, file=filename)
  write.table(out_table, file=filename,  append=TRUE,sep=",", row.names = FALSE, col.names = FALSE)
}

for (n in 1:5) {
  make_csvs(n)
  
}

#Make Universal CSV files for models 6-10 for FCAP analysis 
make_csvs <- function(i){
  grps <- results_sens_domsp_weekday[[i]]$call$ng
  grpcols <- grps + 2
  first_line <- paste(results_sens_domsp_weekday[[i]]$ic[["aic"]],results_sens_domsp_weekday[[i]]$ic[["bic"]],paste0(results_sens_domsp_weekday[[i]]$logLik,"\n"), sep = ",")
  out_table <- merge(results_sens_domsp_weekday[[i]]$posterior, results_sens_domsp_weekday[[i]]$assign, by="row.names")[,2:grpcols]
  filename <- paste0("./FCAP/FCAP_domsp_weekdays_sensitivity/FCAP CSV Files_models6_10/","universal_",grps,".csv")
  cat(first_line, file=filename)
  write.table(out_table, file=filename,  append=TRUE,sep=",", row.names = FALSE, col.names = FALSE)
}

for (n in 6:10) {
  make_csvs(n)
  
}




#DOMAIN-SPECIFIC WEEKends SENSITIVITY 
df_sens_domsp_weekend <- df[df$day_of_week_at_10 ==2,]
save(df_sens_domsp_weekend, file ='df_sens_domsp_weekend.Rdata')

df_long <- df_sens_domsp_weekend %>%
  distinct(hicid, .keep_all = TRUE) %>%
  pivot_longer(
    cols = ends_with(c("10", "12", "14")),
    names_to = c(".value", "time"),
    names_pattern = "(.*?)_(\\d\\d)"
  ) %>%
  mutate(across(where(is.numeric), ~ if_else(.x == 0, 0.01, .x)),
         time = as.integer(time)) %>%
  as.data.frame()

variables = c("active_transport_at","daytime_naps_at", "education_SB_at", "leisure_time_SB_at", "passive_transport_at", "screen_time_at", "self_care_SB_at", "social_based_SB_at", "structured_MVPA_at", "unstructured_LPA_at", "unstructured_MVPA_at", "household_LPA_at", "nighttime_sleep_at")

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
results_sens_domsp_weekend <- get_metrics(df_long_reduced, variables)

#Save results 
save(results_sens_domsp_weekend, file ='results_sens_domsp_weekend.Rdata')

# Put the results in a table
do.call(rbind, lapply(results_sens_domsp_weekend, function(x){
  y <- x$ic
  y["ng"] <- x$call$ng
  y["d"] <- x$call$d
  y
}))


#Make Universal CSV files for models 1-5 for FCAP analysis 
make_csvs <- function(i){
  grps <- results_sens_domsp_weekend[[i]]$call$ng
  grpcols <- grps + 2
  first_line <- paste(results_sens_domsp_weekend[[i]]$ic[["aic"]],results_sens_domsp_weekend[[i]]$ic[["bic"]],paste0(results_sens_domsp_weekend[[i]]$logLik,"\n"), sep = ",")
  out_table <- merge(results_sens_domsp_weekend[[i]]$posterior, results_sens_domsp_weekend[[i]]$assign, by="row.names")[,2:grpcols]
  filename <- paste0("./FCAP/FCAP_domsp_weekends_sensitivity/FCAP CSV Files_models1_5/","universal_",grps,".csv")
  cat(first_line, file=filename)
  write.table(out_table, file=filename,  append=TRUE,sep=",", row.names = FALSE, col.names = FALSE)
}

for (n in 1:5) {
  make_csvs(n)
  
}

#Make Universal CSV files for models 6-10 for FCAP analysis 
make_csvs <- function(i){
  grps <- results_sens_domsp_weekend[[i]]$call$ng
  grpcols <- grps + 2
  first_line <- paste(results_sens_domsp_weekend[[i]]$ic[["aic"]],results_sens_domsp_weekend[[i]]$ic[["bic"]],paste0(results_sens_domsp_weekend[[i]]$logLik,"\n"), sep = ",")
  out_table <- merge(results_sens_domsp_weekend[[i]]$posterior, results_sens_domsp_weekend[[i]]$assign, by="row.names")[,2:grpcols]
  filename <- paste0("./FCAP/FCAP_domsp_weekends_sensitivity/FCAP CSV Files_models6_10/","universal_",grps,".csv")
  cat(first_line, file=filename)
  write.table(out_table, file=filename,  append=TRUE,sep=",", row.names = FALSE, col.names = FALSE)
}

for (n in 6:10) {
  make_csvs(n)
  
}