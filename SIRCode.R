library(deSolve)
## Create an SIR function
sir <- function(time, y, parameters) {   #time, state , parameters
  
  with(as.list(c(y, parameters)), {
    
    dS <- -beta * S * I
    dI <-  beta * S * I - gamma * I
    dR <-                 gamma * I
    dN <- dS + dI + dR
    
    return(list(c(dS, dI, dR, dN)))
  })
}

### Set parameters
## Proportion in each compartment: Susceptible 0.999999, Infected 0.000001, Recovered 0
#init       <- c(S = 1-1e-6, I = 1e-6, R = 0.0)
init <- c(S = 0.99, I = 0.01, R = 0, N = 1)
## beta: infection parameter; gamma: recovery parameter
#parameters <- c(beta = 1.4247, gamma = 0.14286)b=.0952381 g = 0.04761905
parameters <- c(beta = 1, gamma = 0.1) #gamma=0.1
## Time frame
times      <- seq(0, 300, by = 1)

## Solve using ode (General Solver for Ordinary Differential Equations)
out <- ode(y = init, times = times, func = sir, parms = parameters)
## change to data frame
out <- as.data.frame(out)
## Delete time variable
out$time <- NULL
## Show data
head(out, 10)

## Plot
matplot(x = times, y = out, type = "l",
        xlab = "Time", ylab = "Proportion", main = "SIR Model",
        lwd = 1, lty = 1, bty = "l", col = 1:4)

## Add legend
legend(240, 0.7, c("Susceptible", "Infected", "Recovered", "Total"), pch = 1, col = 1:4, bty = "n")