
library(testthat)
library(EasyMMD)

x <- c(rnorm(2000), rnorm(2000, 5))
y <- c(rnorm(2500, 5), rnorm(2500, 20))



print("C++ version speed. Approx ~ 0.84 on the Debian machine")
print(MMD(x,y))
print(system.time(MMD(x,y)))

print("C++ version speed with threshold = 6. Approx ~ 0.84 on the Debian machine")
print(MMD(x,y, threshold = 6))
print(system.time(MMD(x,y, threshold = 6)))
