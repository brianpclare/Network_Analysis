# test 2

library(arm)
library(tidyverse)
library(magrittr)
library(biglm)
library(data.table)


sample <- read_csv("test network data.txt")
part_sample <- sample %>% filter(connection == 1) %>% select(-connection)

names <- unique(
  union(part_sample$node_1, part_sample$node_2)
)

n <- length(names)
num_rows <- n * (n - 1) / 2

wide <- tibble(connection = rep(0, num_rows))

for (i in 1:n) {
  wide[[names[i]]] <- 0
}

for (i in 1:num_rows) {
  wide[[sample$node_1[i]]][[i]] <- 1
  wide[[sample$node_2[i]]][[i]] <- 1
  wide$connection[i] <- sample$connection[i]
}

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

####

fwrite(wide, "test_wide.csv")

glm_read <- function(chunksize, ...){
  fread("test_wide.csv", nrows = chunksize)
}

# big_model <- bigglm(data = glm_read(), 
#                 formula = connection ~ 0 + a + b + c + d + e + f + g + h + i + j + k,
#                 family = binomial(link = "logit"), chunksize = 20)
# summary(big_model)

var_names <- str_c(names, collapse = " + ")
var_names <- str_c("0", var_names, sep = " + ")
bigglm_formula <- as.formula(str_c("connection", var_names, sep = " ~ "))

big_model2 <- bigglm(data = glm_read(chunksize = 50), 
                    formula = bigglm_formula, 
                    family = binomial(link = "logit"))
summary(big_model2)
