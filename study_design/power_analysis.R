#run in /home/bjvca/data/projects/OneCG/MIPP/study_design/
library(simstudy)

# specify between site variation



path <- getwd()

library(plotly)

library(lmtest)
library(car)
library(clubSandwich)
set.seed(1492023) #set seed to date
(setVar <- iccRE(ICC = 0.15, dist = "binary"))
#run in /home/bjvca/data/projects/OneCG/MIPP/study_design/
N_villages <- seq(from=10, to=80, by=5)     # The sample sizes we'll be considering
n_farmers_per_village <- seq(from=10, to=30, by=5)
power <- matrix(NA, length(N_villages),length(n_farmers_per_village))           # Empty object to collect simulation estimates
    
alpha <- 0.05                                    # Standard significance level
sims <- 1000                                    # Number of simulations to conduct for each N

#### Outer loop to vary the number of subjects ####
for (j in 1:length(N_villages)){
  for (k in 1:length(n_farmers_per_village)){
    N <- N_villages[j]                              # Pick the jth value for N
    N1 <- n_farmers_per_village[k]
    
    significant.experiments <- rep(NA, sims)         # Empty object to count significant experiments

    
    #### Inner loop to conduct experiments "sims" times over for each N ####
    for (i in 1:sims){
   
      d <- defData(varname = "a", formula = 0, variance = 0.580565, id = "grp")
      d <- defData(d, varname = "size", formula = N1, dist = "nonrandom")
      
      a <- defDataAdd(varname = "y1", formula = "a", 
                      variance = 4, dist = "normal")
      
      
      dT <- genData(N, d)
      
      dta <- genCluster(dtClust = dT, cLevelVar = "grp", 
                        numIndsVar = "size", level1ID = "id")
      dta <- addColumns(a, dta)
    #  dta$y1 <- dta$y1+.75
      dta$y2 <- dta$y1+.75
      
      dta$y1 <- dta$y1 > 0
      dta$y2 <- dta$y2 > 0
      
      #put half of the groups in a treatment group
      dta$treat <- 0
      dta$treat[dta$grp %in% sample(names(table(dta$grp)), size = N/2)] <- 1
      
      dta$y <- dta$y2*dta$treat + dta$y1*(1-dta$treat)
      
      fit.sim <- lm(y~treat,data=dta)
      summary(fit.sim)
      vcov_cluster <- vcovCR(fit.sim,cluster=dta$grp,type="CR3")
      coef_test( fit.sim , vcov_cluster)$p_Satt
      
      p.value <- summary(fit.sim)$coefficients[2,4]  # Extract p-values
      p.value<-  coef_test( fit.sim , vcov_cluster)$p_Satt[2]  # Extract p-values
     
      significant.experiments[i] <- p.value < alpha 
      # print(c(p.value, p.value_i, p.value_i2))
    }
    
    power[j,k] <- mean(significant.experiments)
    
  }
  # store average success rate (power) for each N
}

fig <- plot_ly(x=~N_villages,y=~n_farmers_per_village,z = ~power)

fig <- fig %>% add_surface(
  
  contours = list(
    
    z = list(
      
      show=TRUE,
      
      usecolormap=TRUE,
      
      highlightcolor="#ff0000",
      
      project=list(z=TRUE)
      
    )
    
  )
  
)

fig <- fig %>% layout(
  
  scene = list(
    
    camera=list(
      
      eye = list(x=1.87, y=0.88, z=-0.64)
      
    )
    
  )
  
)