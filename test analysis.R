# small test of the process
library(tidyverse)
library(magrittr)
library(arm)

test <- read_csv("test network data.txt")
invert1 <- test$node_1
invert2 <- test$node_2
invert <- tibble(invert2, invert1, test$connection)
colnames(invert) <- c("node_1", "node_2", "connection")

doubled <- bind_rows(test, invert)

model <- glm(data = test, connection ~ node_1 + node_2, 
             family = binomial(link = "logit"))
summary(model)

model2 <- glm(data = doubled, connection ~ 0 + node_1 + 0 + node_2, 
             family = binomial(link = "logit"))
summary(model2)

by_node_1 <- doubled %>% group_by(node_1) %>% summarize(percent = mean(connection))
by_nodes <- doubled %>% group_by(node_1, node_2) %>% summarize(percent = mean(connection))

preds <- invlogit(predict(model2, doubled))
preds <- ifelse(preds > 0.5, 1, 0)

doubled$preds <- preds
table <- table(preds, doubled$connection)
