
library(parallel)
library(dplyr)
library(ggplot2)
library(tidyr)

cl <- makeCluster(detectCores()-1)

ni <- 500

clusterEvalQ(cl, library(EasyMMD))

MMD_output <- parLapply(cl, c(1:ni), function(i, ...){
  x <- rnorm(15000)
  y <- rnorm(15000, 5)

  #MMD_full_output <- MMD(x,y)
  MMD_linear_output <- MMD_l(x,y)
  MMD_subsample <- MMD(sample(x, 4000), sample(y, 4000))

  output <- c(MMD_linear_output, MMD_subsample)

  return(output)
})


MMD_df <- data.frame(matrix(unlist(MMD_output), ncol = 2, byrow = TRUE))
names(MMD_df) <- c("MMD_l", "MMD_subsample")

MMD_df2 <- MMD_df %>% gather()

ggplot(MMD_df2) + aes(x = value, col = key) + geom_density()


