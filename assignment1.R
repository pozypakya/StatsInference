################################################################################################################################################

rm(list=ls(all=TRUE))
library(ggplot2)
library(knitr)

#######################################################################################################################################################
## In this project you will investigate the exponential distribution in R and compare it with the 
## Central Limit Theorem. The exponential distribution can be simulated in R with rexp(n, lambda) 
## where lambda is the rate parameter. The mean of exponential distribution is 1/lambda and the 
## standard deviation is also 1/lambda. Set lambda = 0.2 for all of the simulations. You will investigate 
## the distribution of averages of 40 exponentials. Note that you will need to do a thousand simulations.

## Density, distribution function, quantile function and random generation for the exponential distribution 
## with rate rate (i.e., mean 1/rate) ... rexp(n, rate = 1)
##############################################################################################################################################

          lambda = 0.2
          n = 40 
          sim = 1000

          set.seed(349)

#############################################################################################################################################

          exponen_simulation <- function(n, lambda)
            {
                      mean(rexp(n,lambda))
            }

##########################################################################################################
       
        simData <- data.frame(ncol=2,nrow=1000)
        names(simData) <- c("Index","Mean")

              for (i in 1:sim)
                {
                          simData[i,1] <- i
                          simData[i,2] <- exponen_simulation(n,lambda)
                }

#########################################################################################################
## mean of n = 1000

        sample_mean <- mean(simData$Mean)
        sample_mean

## Theoretical exponential mean of exponential distribution
        
        theoritical_mean <- 1/lambda
        theoritical_mean

#########################################################################################################
##          

          hist(simData$Mean, 
                 breaks=100, 
                 prob=TRUE, 
                 main="Exponential Distribution n = 1000", 
                 xlab="Spread")
                        abline(v = theoritical_mean, 
                                col= 3,
                                lwd = 2)
                        abline(v = sample_mean, 
                               col = 2,
                               lwd = 2)
                
                legend('topright', c("Sample Mean", "Theoretical Mean"), 
                              lty=c(1,1), 
                              bty = "n",
                              col = c(col=3, col=2))

############################################################################################################################################  

              sample_var <- var(simData$Mean)
              theor_var <- ((1/lambda)^2)/40

##############################################################################################################################################

          hist(simData$Mean, 
                        breaks = 100, 
                        prob = TRUE, 
                        main ="Exponential Distribution n = 1000", 
                        xlab ="Spread")
                        xfit <- seq(min(simData$Mean), max(simData$Mean), length = 100)
                        yfit <- dnorm(xfit, mean = 1/lambda, sd = (1/lambda/sqrt(40)))
                        lines(xfit, yfit, pch=22, col = 3, lwd=2)
                        legend('topright', c("Theoretical Curve"), lty=1,lwd=2, col=3)

###############################################################################################################################################

          hist(simData$Mean, 
                        breaks = 100, 
                        prob = TRUE, 
                        main = "Distribution of Simulated Exponential Distribution", xlab="")
                        lines(density(simData$Mean))
                        abline(v = 1/lambda, col = 3)
                        xfit <- seq(min(simData$Mean), max(simData$Mean), length = 100)
                        yfit <- dnorm(xfit, mean = 1/lambda, sd = (1/lambda/sqrt(40)))
                        lines(xfit, yfit, pch=22, col="red", lty=2)
                        legend('topright', c("Simulated Values", "Theoretical Values"), lty=c(1,2), col=c("black", "red"))

###################################################################################################################################################

            qqnorm(simData$Mean, 
                            main="Normal Q-Q Plot")
                            qqline(simData$Mean, 
                            col="3")

####################################################################################################################################################

setwd("D:/Google Drive/Coursera/Assignment 6.1/codes/StatsInference")
knit2html("assignment1.rmd", "assignment1.html")
opts_knit$set(base.dir = 'figure')

#####################################################################################################################################################