library(arm)
library(tidyverse)
library(magrittr)
library(data.table)


sample <- read_csv("test network data.txt")
part_sample <- sample %>% filter(connection == 1) %>% select(-connection)

names <- union(part_sample$node_1, part_sample$node_2) %>% unique() %>% 
  sort()

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
betas$one <- 0
pi_vec <- rep(0.5, num_rows)

# alternatively:
# betas$one <- logit(mean_p) / 2
# pi_vec <- rep(mean_p, num_rows)

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
W <- diag((pi_vec) * (1 - pi_vec))

E <- Y - pi_vec

X <- Z %*% betas$one + (1 / ((pi_vec) * (1 - pi_vec))) * E


# do_an_iteration <- function(c_name){
#   betas = betas %>% add_column(!!c_name := 
#                                    betas[, length(betas)] + 
#                                    solve(t(Z) %*% W %*% Z) %*% 
#                                    (t(Z) %*% W %*% X) )
#   
#   sample$preds = make_preds()
#   pi_vec = sample$preds
#   W =  diag((pi_vec) * (1 - pi_vec))
#   E = Y - pi_vec
#   X = Z %*% as.matrix(betas[, length(betas)]) +
#     solve(W) %*% E
# }
# 
# names <- 1:20 %>% as.character()
# 
# for(i in names){
#   do_an_iteration(i)
# }

###############
betas$two <- betas$one + solve(crossprod(Z, W) %*% Z) %*% 
                               (crossprod(Z, W) %*% X)

sample$preds <- make_preds()
pi_vec <- sample$preds
W <-  diag((pi_vec) * (1 - pi_vec))
E <- Y - pi_vec
X <- Z %*% betas$two + (1 / ((pi_vec) * (1 - pi_vec))) * E


###############

test <- glm(Y ~ 0 + Z, family = binomial(link = "logit"),
            start = rep(0, n), control = list(maxit = 1))

test$coefficients
pred_com <- tibble(glm = predict(test, type = "response"),
                   me = pi_vec)

################

comp_model <- glm(data = wide, connection ~ 0 + ., 
                  family = binomial(link = "logit"))
summary(comp_model)

test <- glm(Y ~ 0 + Z, family = binomial(link = "logit"))
preds2 <- predict(comp_model, wide %>% select(-connection),
                  type = "response")

comparison <- tibble(true = sample$connection,
                     glm = preds2, mine = sample$preds)
mse_me <- sum((comparison$mine - comparison$true)^2)
mse_glm <- sum((comparison$glm - comparison$true)^2)


beta_comp <- tibble(glm = comp_model$coefficients, 
                    mine = betas[, length(betas)])

