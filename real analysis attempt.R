
# pretend

library(arm)
library(reshape2)
library(tidyverse)
library(magrittr)
library(data.table)

### figuring out how to get all the possible edges counted
# a <- 1:10
# all_edges2 <- crossing(first = a, second = a)
# all_edges2 <- all_edges2 %>% filter(first < second)
###


sample <- read_csv("public_figure_edges.csv", col_types = "cc")
sample$connection <- 1

part_sample <- sample %>% filter(connection == 1) %>% select(-connection)

names <- unique(
  union(part_sample$node_1, part_sample$node_2)
)

n <- length(names)
num_rows <- n * (n - 1) / 2

## make the full connections dataframe
all_edges <- crossing(node_1 = names, node_2 = names)
all_edges %<>% filter(as.numeric(node_1) < as.numeric(node_2))
##
