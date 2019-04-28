library(ggplot2)

sim.economy <- function(alpha, replicates){
a <- sample(rep(c(-alpha:alpha), replicates))
b <- cumsum(a)
}

plot.economy.simulations <- function(alpha, plots){
  a <- rep(alpha, plots)
  sims <- lapply(a, sim.economy, replicates = 100)
  par(mfrow = c(plots/2, plots/2))
  sapply(sims, plot, type = "l", ylab = "Count")
}

plot.economy.simulations(5, 4)


rekursive.plot <- function(alpha, times){
  if(alpha < times){
    rekursive.plot(alpha + 1, times)
    print("hi")
  }
  alpha  
}

rekursive.plot(1, 5)
