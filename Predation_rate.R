## Data set example
##
#Population | Site | Nest_ID    | Age_oldest | Date   | Time | Nestling_ID   | Weight_g | Tarsus_length | Wing_length | YoungerOne_ifKnow |
#__________________________________________________________________________________________________________________________________________
# Tatama    | 0    | TA19_RVA14 | 16         | 5/3/14 | NA   | TA19_RVA14_1A | 8.77     |  17.6         | 40.5        | 0                 |
#
# Code adapted from Sofear et al. 2014; Partitioning the sources of demographic variation reveals density-dependent nest predation in an island bird population
# in Ecology and Evolution
#
## Packages we need
library(nlme)
library(deSolve)

# Open our directory
setwd("C:/Users/jhanc/Box/Investigacion/Investigaciones/Side projects/Masius/Growth rate")
NTgrowth <- read.csv("Mass_Growth_Rate.csv") 
head(NTgrowth)
NTgrowth$Age <- NTgrowth$Age_Oldest #If we don't know the age of the youngest nestling we use this to replace

# Create two new dataframes containing data from the different populations, since we just have one we can skip this step
  # P1_growth = subset(NTgrowth, NTgrowth$site == 1) 
  # P2_growth = subset(NTgrowth, NTgrowth$site == 0)

###############################################################
# Analysis of growth trajectories in a single population 
# Goal is to fit: weight = A/(1+exp(-K*(t-I))). Note that in the manuscript and below I have
# incorporated the negative sign on K into the parentheses, rather than using this standard format 
# A = Asym = asymptotic wing (grams), K = growth rate constant, I = xmid = inflection point (days), t = Age (days)

# Define a logistic function.
OnePop = function(Age, Asym, xmid, K)
              { Asym/(1 + exp((xmid - Age)*K))
                }
OnePop

# Calculate the derivatives of the function
DerivOnePop = deriv(body(OnePop)[[2]], 
                    namevec = c("Asym", "xmid", "K"), 
                    function.arg = OnePop)
DerivOnePop

# Starting values; these can be estimates from nls (i.e. a model without random effects), or from previous studies
startOnePop = c(Asym = 39.5, xmid = 5, K = .32)

OnePop_NoRE_gnls = gnls(wing ~ Asym/(1 + exp((xmid - Age)*K)), 
                        data = NTgrowth, start = startOnePop) 

summary(OnePop_NoRE_gnls)

fm4DNase1 <- nls(wing ~ Asym/(1 + exp((xmid - Age)/K)),
                 data = NTgrowth,
                 start = list(Asym = 39.5, xmid = 5, K = .32))

summary(fm4DNase1)

#simulating some population growth from the logistic equation and estimating the parameters using nls
log_growth <- function(Age, Asym, xmid) {
                       with(as.list(c(Asym, xmid)), {
                       dN <- R * N * (1 - N / K)
                       return(list(c(dN)))
                                })
                            }

#the parameters for the logisitc growth
  pars  <- c(R = 0.2, K = 1000)
#the initial numbers
  N_ini  <- c(N = 1)
#the time step to evaluate the ODE
  times <- seq(0, 50, by = 1)
#the ODE
  out   <- ode(N_ini, times, log_growth, pars)
#add some random variation to it
  N_obs <- out[, 2] + rnorm(51, 0, 50)
#numbers cannot go lower than 1
  N_obs <- ifelse(N_obs < 1, 1, N_obs)

#plot
plot(times, N_obs)

