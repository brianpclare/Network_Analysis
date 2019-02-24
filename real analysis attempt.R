
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

# make only chunk_length rows in each dataframe
chunk_length = 100

### make the full connections dataframe
# all_edges <- crossing(node_1 = names, node_2 = names)
# all_edges %<>% filter(as.numeric(node_1) < as.numeric(node_2))
###

index <- 0
while(index < 300) {
  # wide <- tibble(connection = rep(0, num_rows))
  wide <- tibble(connection = rep(0, chunk_length))
  
  for (i in 1:n) {
    # wide %<>% mutate(!!names[[i]] := 0)
    wide[[names[i]]] <- 0
  }
  
  # for (i in 1:num_rows) {
  #   wide[[sample$node_1[i]]][[i]] <- 1
  #   wide[[sample$node_2[i]]][[i]] <- 1
  #   wide$connection[i] <- sample$connection[i]
  # }
  
  for (i in 1:chunk_length) {
    wide[[sample$node_1[index + i]]][[i]] <- 1
    wide[[sample$node_2[index + i]]][[i]] <- 1
    wide$connection[i] <- sample$connection[index + i]
  }
  
  fwrite(wide, "wide_file.csv", append = TRUE)
  print(index)
  
  index <- index + chunk_length
  rm(wide)
}

##### move this stuff over to a new GLM script

# not looking for wide, new dataframe now. we'll worry about this later
test_model <- glm(data = wide, connection ~ 0 + ., family = binomial(link = "logit"))
summary(test_model)

preds <- predict(test_model, wide) %>% invlogit()
preds <- ifelse(preds > 0.5, "Pred1", "Pred0")

table <- table(wide$connection, preds)
wide$preds <- preds

degrees <- tibble(node = names, degree = 0, beta = test_model$coefficients)


for (i in 1:n) {
  node <- degrees$node[i]
  
  tbl <- sample %>% filter(node_1 == node | node_2 == node)
  
  degrees$degree[i] <- sum(tbl$connection)
}

degrees$logit <- logit(degrees$degree / (n - 1))
plot(data = degrees, beta ~ logit)
