#run in /home/bjvca/data/projects/OneCG/MIPP/study_design/
possible.ns <- seq(from=1000, to=4000, by=50)     # The sample sizes we'll be considering
powers <- rep(NA, length(possible.ns))           # Empty object to collect simulation estimates
alpha <- 0.05                                    # Standard significance level
sims <- 500                                      # Number of simulations to conduct for each N
wd <- getwd()
dta <- read.csv(paste(wd,"data/baseline_farmers.csv",sep="/"))
outcome <- dta$Check2.check.maize.q25a == "Yes"
#### Outer loop to vary the number of subjects ####
for (j in 1:length(possible.ns)){
  N <- possible.ns[j]                              # Pick the jth value for N
  
  significant.experiments <- rep(NA, sims)         # Empty object to count significant experiments
  
  #### Inner loop to conduct experiments "sims" times over for each N ####
  for (i in 1:sims){
    pr0 <- mean(sample(outcome, size=possible.ns[j],replace=TRUE))  
    tau <- 0.06                                       # Hypothesize treatment effect
                                # treatment potential outcome
    Z.sim <- rbinom(n=possible.ns[j], size=1, prob=.5)          # Do a random assignment
    Y.sim <- rbinom(n=possible.ns[j], size=1, prob=pr0+tau)*Z.sim + rbinom(n=possible.ns[j], size=1, prob=pr0)*(1-Z.sim)               # Reveal outcomes according to assignment
    
    
    

    fit.sim <- lm(Y.sim ~ Z.sim)                   # Do analysis (Simple regression)
    p.value <- summary(fit.sim)$coefficients[2,4]  # Extract p-values
    significant.experiments[i] <- (p.value <= alpha) # Determine significance according to p <= 0.05
  }
  
  powers[j] <- mean(significant.experiments)       # store average success rate (power) for each N
}
plot(possible.ns, powers, ylim=c(0,1),type="l")
