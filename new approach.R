##?
library(arm)
library(tidyverse)
library(magrittr)
library(data.table)
library(vroom)

all_edges <- fread("all_edges_real.csv", 
                   colClasses = list(character = 1:2),
                  sep = ",")
{
  data <- read_csv("cleaned up data.csv", col_types = "cc")
  names <- unique(
    union(data$node_1, data$node_2)
  )}
all_vars <- tibble(names)
rm(names, data)

perc_conn <- mean(all_edges$connection)
sum_betas <- logit(perc_conn)

all_vars$beta_est <- sum_betas / 2

# so the first draft estimates of the betas are just what
# they would need to be to have the right expected number of connections

all_edges$pred <- 0

for(i in 1:length(all_edges$pred)){
  
}
  
  
all_edges$preds <- invlogit(
  all_vars$beta_est[[all_vars$names == all_edges$node_1]] + 
    all_vars$beta_est[[all_vars$names == all_edges$node_2]])
