# loading data
library(tidyverse)
library(magrittr)

public_figures <- read_csv("public_figure_edges.csv")

# high <- map2(public_figures$node_1, public_figures$node_2, max) %>% flatten_dbl()
# low <- map2(public_figures$node_1, public_figures$node_2, min) %>% flatten_dbl()

public_figures %<>% filter(node_1 != node_2)
