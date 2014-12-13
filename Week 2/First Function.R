add2 <- function(x, y) {
  x+y
}

above10 <- function(vector) {
  vector[vector > 10]
}

above <- function(vector, n = 10) {
  vector[vector > n]
}

colMean <- function(data, removeNA = T) {
  num_col <- ncol(data)
  means <- numeric(num_col)
  for(i in 1:num_col) {
    means[i] <- mean(data[,i], na.rm = removeNA)
  }
  means
}
