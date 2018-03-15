
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
x <- rnorm(1e4)
y <- rnorm(1e4, 5)
mmd_1 <- MMD(y, x, var = 1/2)
mmd_2 <- MMD(y, x, var = 1/2, threshold = 6)
mmd_3 <- MMD(y, x, var = 1/2, approx_exp = 1)
mmd_4 <- MMD(y, x, var = 1/2, bias = TRUE)
mmd_5 <- MMD(matrix(y, ncol = 1), matrix(x, ncol = 1), var = matrix(1/2), bias = TRUE)
mmd_6 <- MMD_l(y, x, var = 1/2)

set.seed(2)
mmd_7 <- MMD_l_multi(y, x, var = 1/2, k = 10)

testthat::expect_equal(mmd_1, 0.88824062832437)
testthat::expect_equal(mmd_1, mmd_2)
testthat::expect_equal(mmd_3, 0.88804234756543)
testthat::expect_equal(mmd_4, 0.88835124947997)
testthat::expect_equal(mmd_4, mmd_5)
testthat::expect_equal(mmd_6, 0.87759954311812)
testthat::expect_equal(mmd_7, 0.88606371828789)
