library(arm)
library(tidyverse)
library(magrittr)
library(data.table)

# chunk_size <- 5782
chunk_size <- 500

data <- fread("all_edges_real.csv", 
              colClasses = c("character", "character", "numeric"))

names <- union(data$node_1, data$node_2) %>%
  unique()

n <- length(names)
num_rows <- n * (n - 1) / 2

# rm(data)

betas <- matrix(0, nrow = n, ncol = 1)
rownames(betas) <- names
# betas[, 1] <- 0
pi_vec <- rep(0.5, chunk_size)

make_preds <- function(sample, chunk_size){
  x <- rep(0, chunk_size)
  for(i in 1:chunk_size){
    bl <- ncol(betas)
    x[i] <- 1/(1 + exp(-(betas[rownames(betas) == .subset2(sample, 1)[i], bl] +
                           betas[rownames(betas) == .subset2(sample, 2)[i], bl])))
  }
  return(unlist(x))
}

chunk_calc <- function(index, chunk_size, sample){
  Z <- matrix(0, nrow = chunk_size, ncol = n)
  colnames(Z) <- names
  Y <- rep(0, chunk_size)
  
  for (i in 1:chunk_size) {
    Z[[i, .subset2(sample, 1)[i]]] <- 1
    Z[[i, .subset2(sample, 2)[i]]] <- 1
    Y[i] <- .subset2(sample, 3)[i]
  }
  pi_vec <- make_preds(sample, chunk_size)
  W <- (pi_vec) * (1 - pi_vec)
  
  ZYPI <- crossprod(Z, (Y - pi_vec))
  ZTWZ <- crossprod(Z, (W*Z))
  
  return(list(ZTWZ, ZYPI))
}







do_an_iteration <- function(c_name, chunk_size){
  print(Sys.time())
  index <- 0
  ztwz <- matrix(0, nrow = n, ncol = n)
  zypi <- rep(0, n)
  
  while(TRUE){
    df <- data[(index + 1):(index + chunk_size), ]
    new_stuff <- chunk_calc(index, nrow(df), df)
    ztwz <- ztwz + new_stuff[[1]]
    zypi <- zypi + new_stuff[[2]]
    index <- index + chunk_size
    print(index)
    if(index >= num_rows){
    # if(index >= 1){
      break
    }
  }
  
  bl <- ncol(betas)
  
  C <- chol(ztwz)
  
  betas <<- betas %>% cbind(betas[, bl] +
                              backsolve(C, backsolve(C, zypi,
                                                     transpose = TRUE)) )
  print(Sys.time())
}


do_an_iteration("two", chunk_size)
# do_an_iteration("three", chunk_size)
# do_an_iteration("four", chunk_size)
# 
# cns <- c("five", "six", "seven", "eight", "nine", "ten")
# 
# for(j in cns){
#   do_an_iteration(j, chunk_size)
# }

data$preds <- make_preds(data, chunk_size = 93096)