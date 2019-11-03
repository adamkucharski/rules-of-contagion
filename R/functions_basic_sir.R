# Output simulated deterministic trajectories

Run_simulation<-function(dt, theta, theta_init,time.vals){
  
  # Simulation model ODE
  simulate_deterministic <- function(theta, init.state, times) {
    SIR_ode <- function(time, state, theta) {
      
      ## states
      S <- state[["s_init"]]
      I <- state[["i1_init"]]
      R <- state[["r_init"]]
      C <-state[["c_init"]]

      N1 <- state[["n1_init"]]
      
      ## extract parameters
      alpha <- theta[["alpha"]]
      beta <- theta[["beta"]]
      gamma <- theta[["gamma"]]
      repR <- theta[["rep"]]
      
      dN1 <- 0 
      dS <- -beta*S*I/N1
      dI <- beta*S*I/N1 - gamma*I
      dR <- gamma*I 
      dC <- repR*beta*S*I/N1

      return(list(c(dN1,dS,dI,dR,dC)))
    }
    
    # put incidence at 0 in init.state
    traj <- as.data.frame(ode(init.state, times, SIR_ode, theta, method = "ode45"))
    
    # avoid negative values in incidence at hospital
    traj[traj<0] <- 0
    return(traj)
  }
  
  init1=c(n1_init=theta_init[["n_init"]],
          s_init=theta_init[["s_init"]],
          i1_init=theta_init[["i1_init"]],
          r_init=theta_init[["r_init"]],
          c_init=0)
  
  # Output simulation data
  output = simulate_deterministic(theta,init1,seq(0,max(time.vals),0.1))
  S_traj = output[match(time.vals,output$time),"s_init"]
  I_traj = output[match(time.vals,output$time),"i1_init"]
  R_traj = output[match(time.vals,output$time),"r_init"]
  
  cases1 = output[match(time.vals,output$time),"c_init"]
  casecount = cases1-c(0,cases1[1:(length(time.vals)-1)])
  
  return(list(C1_trace=casecount,S_trace=S_traj,I_trace=I_traj,R_trace = R_traj ))
  
}

