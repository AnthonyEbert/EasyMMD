

library(parallel)
library(dplyr)
library(ggplot2)
library(tidyr)

cl <- makeCluster(detectCores()-1)

ni <- 400
n_x <- 1500
n_y <- 1500

clusterEvalQ(cl, library(EasyMMD))

clusterExport(cl, c("n_x", "n_y"))

MMD_output <- parLapply(cl, c(1:ni), function(i, ...){
  x <- rnorm(n_x)
  y <- rnorm(n_y, 5)

  MMD_linear_output <- MMD_l(y, x)

  x <- rnorm(n_x)
  y <- rnorm(n_y, 5)

  MMD_biased <- MMD(y, x, bias = TRUE)

  x <- rnorm(n_x)
  y <- rnorm(n_y, 5)

  MMD_unbiased <- MMD(y, x, bias = FALSE)

  x <- rnorm(n_x)
  y <- rnorm(n_y, 5)

  MMD_linear_multi <- median(MMD_l_multi(y, x, 10, 1))

  output <- c(MMD_linear_output, MMD_biased, MMD_unbiased, MMD_linear_multi)

  return(output)
})


MMD_df <- data.frame(matrix(unlist(MMD_output), ncol = 4, byrow = TRUE))
names(MMD_df) <- c("MMD_linear", "MMD_biased", "MMD_unbiased", "MMD_linear_multi")

MMD_df2 <- MMD_df %>% gather()

ggplot(MMD_df2) + aes(x = value, col = key) + geom_density()







library(parallel)
library(dplyr)
library(ggplot2)
library(tidyr)

cl <- makeCluster(detectCores()-1)

ni <- 500

clusterEvalQ(cl, library(EasyMMD))

MMD_output <- parLapply(cl, c(1:ni), function(i, ...){
  x <- rnorm(150)
  y <- rnorm(150, 5)

  #MMD_full_output <- MMD(x,y)
  MMD_linear_output <- MMD_l(x,y)
  MMD_subsample <- MMD(sample(x, 35), sample(y, 35))

  output <- c(MMD_linear_output, MMD_subsample)

  return(output)
})


MMD_df <- data.frame(matrix(unlist(MMD_output), ncol = 2, byrow = TRUE))
names(MMD_df) <- c("MMD_l", "MMD_subsample")

MMD_df2 <- MMD_df %>% gather()

ggplot(MMD_df2) + aes(x = value, col = key) + geom_density()
