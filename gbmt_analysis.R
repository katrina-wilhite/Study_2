library(gbmt)
library(tidyverse)
library(foreach)
library(doParallel)

df <- haven::read_dta("Z:\\7-Data\\M&B\\LSAC dataset\\Study_2\\Study_2\\Multi_Trajecotry_Analysis_Domain_Specific_Movement_Behaviours_with_Groups.dta")

df_long <- df %>%
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
                data=df)
    res
  }
  
}

# Run the function
results <- get_metrics(df_long_reduced, variables)

# Put the results in a table
do.call(rbind, lapply(results, function(x){
  y <- x$ic
  y["ng"] <- x$call$ng
  y["d"] <- x$call$ng
  y
}))

# You do not need to rerun the model, just call the best fitting one
# E.g., if model 3 had the best fit:
results[[3]]$fitted
