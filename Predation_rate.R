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
OnePop <- function(Age, Asym, xmid, K)
              { Asym/(1 + exp((xmid - Age)*K))
                }
OnePop

# Calculate the derivatives of the function
DerivOnePop <- deriv(body(OnePop)[[2]], 
                    namevec = c("Asym", "xmid", "K"), 
                    function.arg = OnePop)
DerivOnePop

# Starting values; these can be estimates from nls (i.e. a model without random effects), or from previous studies
startOnePop <- c(Asym = 39.5, xmid = 5, K = .32)

OnePop_NoRE_gnls <- gnls(wing ~ Asym/(1 + exp((xmid - Age)*K)), 
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


#######################################################################
## Running different nlme models to test which one best fit the data ##
#######################################################################
# Model with no random effect
gbww.wing_gnls <- gnls(wing ~ DerivOnePop(Age, Asym, xmid, K), data = NTgrowth, start = startOnePop)
    summary(gbww.wing_gnls)

# Model with asym, xmid, and K as random effects
gbww.wing_A.Ti.K_Nest_ID <- nlme(wing ~ DerivOnePop(Age, Asym, xmid, K), fixed = Asym + xmid + K ~ 1, 
                                 random = Asym + xmid + K ~ 1 | Nest_ID, data = NTgrowth, start =startOnePop)
    summary(gbww.wing_A.Ti.K_Nest_ID)

# Model with asym and xmid as random effects
gbww.wing_A.Ti_Nest_ID <- nlme(wing ~ DerivOnePop(Age, Asym, xmid, K), fixed = Asym + xmid + K ~ 1, 
                               random = Asym + xmid ~ 1 | Nest_ID, data = NTgrowth, start =startOnePop)
    summary(gbww.wing_A.Ti_Nest_ID)

# Model with Asym and K as random effects
gbww.wing_A.K_Nest_ID <- nlme(wing ~ DerivOnePop(Age, Asym, xmid, K), fixed = Asym + xmid + K ~ 1, 
                              random = Asym + K ~ 1 | Nest_ID, data = NTgrowth, start =startOnePop)
    summary(gbww.wing_A.K_Nest_ID)

# Model with xmid and K as random effects
gbww.wing_Ti.K_Nest_ID <- nlme(wing ~ DerivOnePop(Age, Asym, xmid, K), fixed = Asym + xmid + K ~ 1, 
                               random = xmid + K ~ 1 | Nest_ID, data = NTgrowth, start =startOnePop)
    summary(gbww.wing_Ti.K_Nest_ID)

# Model with Asym as random effect
gbww.wing_A_Nest_ID <- nlme(wing ~ DerivOnePop(Age, Asym, xmid, K), fixed = Asym + xmid + K ~ 1, 
                           random = Asym ~ 1 | Nest_ID, data = NTgrowth, start =startOnePop)
    summary(gbww.wing_A_Nest_ID)

# Model with xmid as random effect
gbww.wing_Ti_Nest_ID <- nlme(wing ~ DerivOnePop(Age, Asym, xmid, K), fixed = Asym + xmid + K ~ 1, 
                            random = xmid ~ 1 | Nest_ID, data = NTgrowth, start =startOnePop)
    summary(gbww.wing_Ti_Nest_ID)

# Model with K as random effect
gbww.wing_K_Nest_ID <- nlme(wing ~ DerivOnePop(Age, Asym, xmid, K), fixed = Asym + xmid + K ~ 1, 
                           random = K ~ 1 | Nest_ID, data = NTgrowth, start =startOnePop)
    summary(gbww.wing_K_Nest_ID)


## Let's choose which is the best model
## We used an ANOVA to test best fit model via AIC, but you can use any other test

kk.wing <- anova(gbww.wing_gnls, gbww.wing_A.Ti.K_Nest_ID,
              gbww.wing_A.Ti_Nest_ID, gbww.wing_A.K_Nest_ID, 
              gbww.wing_Ti.K_Nest_ID, gbww.wing_A_Nest_ID, 
              gbww.wing_Ti_Nest_ID)

kk.wing	## select from here the model with lowest AIC
write.csv(kk.wing, "AICtable_wing.csv") # Save the kk.wing results from the ANOVA


### AIC top model: gbww.wing_A.Ti.K_Nest_ID
summary(gbww.wing_A.Ti.K_Nest_ID)
intervals(gbww.wing_A.Ti.K_Nest_ID, level = 0.95, which = "fixed")


#############################
## Validation of Top Model ##
#############################

# "." references the fitted object
# Let's plot residuals vs. fitted values
plot(gbww.wing_A.Ti.K_Nest_ID) 

# Let plot the observed vs. fitted values
plot(gbww.wing_A.Ti.K_Nest_ID, wing ~ fitted(.))
plot(gbww.wing_A.Ti.K_Nest_ID, wing ~ fitted(.) | Nest_ID) # separately by nest

# Let's do the normal plots of residuals
qqnorm(gbww.wing_A.Ti.K_Nest_ID, ~ resid(., type = "p"))

# Check assumption that random effects are normally distributed
ranef(gbww.wing_A.Ti.K_Nest_ID, level = "Nest_ID")
qqnorm(gbww.wing_A.Ti.K_Nest_ID, ~ ranef(., level = "Nest_ID"))

# residuals by nest
plot(gbww.wing_A.Ti.K_Nest_ID, Nest_ID ~ resid(., type = "p"), abline = 0)

