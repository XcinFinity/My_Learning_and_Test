1+1
1+2
1+3


nn = rnorm(100)
plot(nn)

library(tidyverse)
rnorm(100) %>% p1{}tidyverse


add <- function(x, y) {
    x +y
}

print(add (1, 2))
print(add(1.0e10, 2.0e10))
print(paste("one", NULL))
print(paste(NA, "two"))
print(paste("multi-line", "multi-line"))

h <- c(1,2,3,4,5,6)
M <- c("A","B","C","D","E","F")
barplot(h, names.arg = M, xlab = "X", ylab = "Y", col = "#00cec9", main = "Chart", border = "#fdcb6e")
