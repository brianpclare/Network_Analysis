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

## initializing variables
Z <- as.matrix(wide %>% select(-connection))
mean_p <- mean(wide$connection)

betas <- tibble(names)
# betas$one <- 0

# alternatively:
betas$one <- logit(mean_p) / 2


pi_vec <- rep(mean_p, num_rows)
make_preds <- function(){
  x <- rep(0, num_rows)
  for(i in 1:num_rows){
    node1 <- sample$node_1[[i]]
    node2 <- sample$node_2[[i]]
    
    b1 <- betas[betas$names == node1, length(betas)]
    b2 <- betas[betas$names == node2, length(betas)]
    
    x[i] <- invlogit(b1 + b2)
  }
  
  return(unlist(x))
}

sample$preds <- make_preds()

Y <- wide$connection
W <- (pi_vec) * (1 - pi_vec) * diag(num_rows)

E <- Y - pi_vec

X <- Z %*% as.matrix(betas[, length(betas)]) +
  solve(W) %*% E


do_an_iteration <- function(c_name){
  betas <<- betas %>% add_column(!!c_name := 
                                  betas[, length(betas)] + 
                                  solve(t(Z) %*% W %*% Z) %*% 
                                  (t(Z) %*% W %*% X) )
  
  sample$preds <<- make_preds()
  pi_vec <<- sample$preds %>% unlist()
  W <<- (pi_vec) * (1 - pi_vec) * diag(num_rows)
  E <<- Y - pi_vec
  X <<- Z %*% as.matrix(betas[, length(betas)]) +
    solve(W) %*% E
}

names <- 1:20 %>% as.character()

for(i in names){
  do_an_iteration(i)
}

###############

comp_model <- glm(data = wide, connection ~ 0 + ., 
                  family = binomial(link = "logit"))
summary(comp_model)
preds2 <- predict(comp_model, wide %>% select(-connection),
                  type = "response")

comparison <- tibble(glm = preds2, mine = sample$preds)
beta_comp <- tibble(glm = comp_model$coefficients, 
                    mine = betas[, length(betas)])
