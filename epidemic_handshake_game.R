#=============================================================================#
# Epidemic! The Handshake Game
#=============================================================================#

# Here you will have the chance to run the basic susceptible-infected-recovered
# model that we first discussed in the handshake game. We will also show how to 
# plot these results so you can visualize them. If you're up for an additional 
# challenge, you will also be able to adapt code to try out different infection 
# scenarios. 

# Lastly, we will go through how to run the susceptible-infected-recovered model
# with quarantine and vaccinated added into the model. 

#=============================================================================#
# Load packages
#=============================================================================#

## Differential equations library
require(deSolve)


#=============================================================================#
# Basic SIR Model
#=============================================================================#

## Create an SIR function
sir <- function(time, state, params) {
  with(as.list(c(state, params)), {
    dS <- -beta * S * I
    dI <-  beta * S * I - gamma * I
    dR <-                 gamma * I
    
    return(list(c(dS, dI, dR)))
  })
}


### Set parameters
## Proportion in each compartment: Susceptible 0.999999, Infected 0.000001, Recovered 0
init <- c(S = 1 - 1e-6, I = 1e-6, R = 0.0)


## beta: infection parameter; gamma: recovery parameter
params <- c(beta = 1.5, gamma = 1 / 8)


## Time frame
times <- seq(0, 70, by = 1)

## Solve using ode (General Solver for Ordinary Differential Equations)
out <- ode(
  y = init,
  times = times,
  func = sir,
  parms = params
)


# SOME PLOTTING
matplot(
  out[, 2:4],
  typ = 'l',
  lwd = 2,
  xlab = 'Time',
  ylab = 'Proportion'
)
legend('topright',
       c('S', 'I', 'R'),
       col = 1:3,
       lty = 2:3)


#=============================================================================#
# YOU TRY!
# Plug=in different values for beta (infection paramter) and gamma (recovery)
# parameter and run the model again to see how your results differ from before!

# QUESTIONS TO THINK ABOUT
# What would it mean if the infection parameter increased to 2? 
# What about decreased to 0.5? 

# What would it mean to have a slower (i.e., 1/20) recovery rate? 
# What about a faster (i.e., 1/2) recovery rate? 

#=============================================================================#


## beta: infection parameter; gamma: recovery parameter
## EDIT THE BETA AND GAMMA VALUES
params <- c(beta = 1.5, gamma = 1 / 8)


## Solve using ode (General Solver for Ordinary Differential Equations)
out <- ode(
  y = init,
  times = times,
  func = sir,
  parms = params
)


# Plot the trajectory of the outbreak
matplot(
  out[, 2:4],
  typ = 'l',
  lwd = 2,
  xlab = 'Time',
  ylab = 'Proportion'
)
legend('topright',
       c('S', 'I', 'R'),
       col = 1:3,
       lty = 2:3)


#=============================================================================#
# SIR Model with Vaccination and Quarantine
#=============================================================================#

## Create an SIR function with vaccination and quarantine
sir_v_q <- function(time, state, params) {
  with(as.list(c(state, params)), {
    dS <- -beta * S * I             - vaccination * S - beta_q * S * I
    dI <-  beta * S * I - gamma * I                   + beta_q * S * I
    dR <-                 gamma * I + vaccination * S
    
    return(list(c(dS, dI, dR)))
  })
}


### Set parameters
## Proportion in each compartment: Susceptible 0.999999, Infected 0.000001, Recovered 0
init_v_q <- c(S = 1 - 1e-6, I = 1e-6, R = 0.0)


## beta: infection parameter; gamma: recovery parameter
params_v_q <- c(beta = 1.5,
            beta_q = .5,
            gamma = 1 / 8,
            vaccination = 0.1)


## Time frame
times_v_q <- seq(0, 70, by = 1)

## Solve using ode (General Solver for Ordinary Differential Equations)
out_v_q <- ode(
  y = init_v_q,
  times = times_v_q,
  func = sir_v_q,
  parms = params_v_q
)


# SOME PLOTTING
matplot(
  out_v_q[, 2:4],
  typ = 'l',
  lwd = 2,
  xlab = 'Time',
  ylab = 'Proportion'
)
legend('topright',
       c('S', 'I', 'R'),
       col = 1:3,
       lty = 2:3)


#=============================================================================#
# ADDITIONAL CHALLENGE!
# Add a "waning_immunity" parameter, which will move people from the recovered
# box BACK to the susceptible box. You will need to include the "waning_immunity" 
# parameter into the params sir function and into the params argument. 

# This would be called a suscpetible-infected-recovered-suscpetible (SIRS) model.

# This would model a process like the common cold. 
#=============================================================================#


## Create an SIRS function
sirs <- function(time, state, params) {
  with(as.list(c(state, params)), {
    dS <- -beta * S * I
    dI <-  beta * S * I - gamma * I
    dR <-                 gamma * I
    
    return(list(c(dS, dI, dR)))
  })
}


### Set parameters
## Proportion in each compartment: Susceptible 0.999999, Infected 0.000001, Recovered 0
init_sirs <- c(S = 1 - 1e-6, I = 1e-6, R = 0.0)


## beta: infection parameter; gamma: recovery parameter
params_sirs <- c(beta = 1.5, gamma = 1 / 8)


## Time frame
times_sirs <- seq(0, 70, by = 1)

## Solve using ode (General Solver for Ordinary Differential Equations)
out_sirs <- ode(
  y = init_sirs,
  times = times_sirs,
  func = sirs,
  parms = params_sirs
)


# SOME PLOTTING
matplot(
  out_sirs[, 2:4],
  typ = 'l',
  lwd = 2,
  xlab = 'Time',
  ylab = 'Proportion'
)
legend('topright',
       c('S', 'I', 'R'),
       col = 1:3,
       lty = 2:3)



