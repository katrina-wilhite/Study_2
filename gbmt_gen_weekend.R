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
results_df_gen_weekend <- get_metrics(df_long_reduced, variables)

#Save results 
foreign::write_dta(results_df_gen_weekend, "df_gen_weekend.RData")
save(results_df_gen_weekend, file ='Z:/LSAC dataset/Study_2/Study_2/df_gen_weekend.Rdata')

# Put the results in a table
do.call(rbind, lapply(results, function(x){
  y <- x$ic
  y["ng"] <- x$call$ng
  y["d"] <- x$call$d
  y
}))


#Make Universal CSV files for models 1-5 for FCAP analysis 
make_csvs <- function(i){
  grps <- results[[i]]$call$ng
  grpcols <- grps + 2
  first_line <- paste(results[[i]]$ic[["aic"]],results[[i]]$ic[["bic"]],paste0(results[[i]]$logLik,"\n"), sep = ",")
  out_table <- merge(results[[i]]$posterior, results[[i]]$assign, by="row.names")[,2:grpcols]
  filename <- paste0("universal_",grps,".csv")
  cat(first_line, file=filename)
  write.table(out_table, file=filename,  append=TRUE,sep=",", row.names = FALSE, col.names = FALSE)
}

for (n in 1:5) {
  make_csvs(n)
  
}

#Make Universal CSV files for models 6-10 for FCAP analysis 
make_csvs <- function(i){
  grps <- results[[i]]$call$ng
  grpcols <- grps + 2
  first_line <- paste(results[[i]]$ic[["aic"]],results[[i]]$ic[["bic"]],paste0(results[[i]]$logLik,"\n"), sep = ",")
  out_table <- merge(results[[i]]$posterior, results[[i]]$assign, by="row.names")[,2:grpcols]
  filename <- paste0("universal_",grps,".csv")
  cat(first_line, file=filename)
  write.table(out_table, file=filename,  append=TRUE,sep=",", row.names = FALSE, col.names = FALSE)
}

for (n in 6:10) {
  make_csvs(n)
  
}