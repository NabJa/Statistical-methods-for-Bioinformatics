
#Generate rdm data with different groups Y
D <- matrix(rnorm(1000, 0, 1), ncol = 100, nrow = 10)
Y <- rbinom(10, 1, .5)
D[Y==1] <- D[Y==1] + rnorm(1, 0, .1)


bootstrap <- function(data, y, nrps){
  pvals <- c()
  for(i in 1:nrps){
    subsamp <- sample(nrow(data), round(nrow(data)*0.7), replace = F)
    subdata <- data[subsamp, ]
    suby <- y[subsamp]
    p <- t.test(subdata ~ suby)$p.value
    pvals <- c(pvals, p)
  }
  plot(density(pvals))
  abline(v = mean(pvals), lty = 2, col = "red")
  abline(v = median(pvals), lty = 3, col = "blue")
  return(c(median = median(pvals), mean = mean(pvals), min = min(pvals)))
}

tt <- t.test(D ~ Y)
bt <- bootstrap(D, Y, 30)
tt$p.value
bt
