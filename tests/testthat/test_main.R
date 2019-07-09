
library(testthat)
library(EasyMMD)

x <- c(rnorm(2000), rnorm(2000, 5))
y <- c(rnorm(2500, 5), rnorm(2500, 20))



print("C++ version speed. Approx ~ 0.84 on the Debian machine")
print(MMD(x,y, var = 1/2))
print(system.time(MMD(x,y, var = 1/2)))

print("C++ version speed with threshold = 6. Approx ~ 0.84 on the Debian machine")
print(MMD(x,y, var = 1/2, threshold = 2))
print(system.time(MMD(x,y, var = 1/2, threshold = 6)))


set.seed(1)
x <- rnorm(1e3)
y <- rnorm(1e3, 5)
mmd_0 <- MMD(y, x, sigma = 1/sqrt(2))
mmd_1 <- MMD(y, x, var = 1/2)
mmd_2 <- MMD(y, x, var = 1/2, threshold = 6)
mmd_3 <- MMD(y, x, var = 1/2, approx_exp = 1)
mmd_4 <- MMD(y, x, var = 1/2, bias = TRUE)
mmd_5 <- MMD(matrix(y, ncol = 1), matrix(x, ncol = 1), var = matrix(1/2), bias = TRUE)
mmd_6 <- MMD_l(y, x, var = 1/2)

set.seed(2)
mmd_7 <- MMD_l_multi(y, x, var = 1/2, k = 10)
mmd_8 <- MMD(matrix(y, ncol = 1), matrix(x, ncol = 1), var = matrix(1/2), threshold = 6)

x <- mvtnorm::rmvnorm(100, mean = c(0,1))
y <- mvtnorm::rmvnorm(100, mean = c(0,1))

x <- x[sample(100, 200, replace = TRUE),]
y <- y[sample(100, 200, replace = TRUE),]

x_df <- aggregate(data.frame(count = x[,1]), list(x1 = x[,1], x2 = x[,2]), length)
y_df <- aggregate(data.frame(count = y[,1]), list(y1 = y[,1], y2 = y[,2]), length)

mmd_9 <- MMD(y, x, var = diag(2))
mmd_10 <- MMD(cbind(y_df$y1, y_df$y2), cbind(x_df$x1, x_df$x2), w_y = y_df$count, w_x = x_df$count, var = diag(2))

x <- c(1,3,5)
y <- c(2,4,6)
y_kmmd <- kmmd(y)

mmd_11 <- MMD(y,x)
mmd_12 <- MMD(y,x, y_kmmd = y_kmmd)

testthat::expect_equal(mmd_0, mmd_1)
testthat::expect_equal(mmd_1, 0.86041675469766)
testthat::expect_equal(mmd_1, mmd_2)
testthat::expect_equal(mmd_3, 0.86022038987584)
testthat::expect_equal(mmd_4, 0.86154870766257)
testthat::expect_equal(mmd_4, mmd_5)
testthat::expect_equal(mmd_6, 0.87339614038578)
testthat::expect_equal(mmd_7, 0.86904668263110)
testthat::expect_equal(mmd_8, mmd_1)
testthat::expect_equal(mmd_9, mmd_10)
testthat::expect_equal(mmd_11, -0.5006591)
testthat::expect_equal(mmd_12, -0.5006591)
