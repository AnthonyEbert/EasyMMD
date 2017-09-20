
library(EasyMMD)

x <- rnorm(4000)
y <- rnorm(4500, 5)

test_that("MMD calc", {
  expect_equal(MMD(x,y), EasyMMD:::MMD_oldR(x,y))
})

print(MMD(x,y))

print("C++ version speed. Approx ~ 0.84 on the Debian machine")
print(system.time(MMD(x,y)))

