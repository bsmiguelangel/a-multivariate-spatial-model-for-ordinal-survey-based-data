#### Required packages ####

## install.packages("pacman")

pacman::p_load(foreign, readxl, faraway, spdep, sp, ggplot2, 
               RColorBrewer, graphics, ggpubr, leaflet, nimble, 
               ggmcmc, extraDistr, parallel, MCMCvis, gridExtra, 
               corrplot, ggcorrplot, readr, lattice, install = FALSE)

#### Data loading ####

rm(list = ls())
HSRV2022 <- read.spss(file.path("data", "ESCV2022_UV_Matem.sav"), 
                      use.value.labels = TRUE, to.data.frame = TRUE)
# Sample size
NResp <- nrow(HSRV2022)

#### Mental health conditions: GHQ-12 items ####

# P8_1 Concentrate
levels(HSRV2022$P8_1)
table(HSRV2022$P8_1)
HSRV2022$P8_1[HSRV2022$P8_1 == "NS/NC"] <- NA
HSRV2022$P8_1 <- factor(HSRV2022$P8_1, levels = c("Más que lo habitual", "Igual que lo habitual", 
                                                  "Menos que lo habitual", "Mucho menos que lo habitual"))
levels(HSRV2022$P8_1) <- c("More", "Same", "Less", "Much less")
table(HSRV2022$P8_1)
P8_1 <- as.numeric(HSRV2022$P8_1)

# P8_2 Lose sleep over worries 
levels(HSRV2022$P8_2)
table(HSRV2022$P8_2)
HSRV2022$P8_2[HSRV2022$P8_2 == "NS/NC"] <- NA
HSRV2022$P8_2 <- factor(HSRV2022$P8_2, levels = c("Más que lo habitual", "Igual que lo habitual", 
                                                  "Menos que lo habitual", "Mucho menos que lo habitual"))
levels(HSRV2022$P8_2) <- c("Not at all", "No more", "Rather more", "Much more than usual")
table(HSRV2022$P8_2)
P8_2 <- as.numeric(HSRV2022$P8_2)

# P8_3 Play a useful role
levels(HSRV2022$P8_3)
table(HSRV2022$P8_3)
HSRV2022$P8_3[HSRV2022$P8_3 == "NS/NC"] <- NA
HSRV2022$P8_3 <- factor(HSRV2022$P8_3, levels = c("Más que lo habitual", "Igual que lo habitual", 
                                                  "Menos que lo habitual", "Mucho menos que lo habitual"))
levels(HSRV2022$P8_3) <- c("More", "Same", "Less", "Much less")
table(HSRV2022$P8_3)
P8_3 <- as.numeric(HSRV2022$P8_3)

# P8_4 Make decisions
levels(HSRV2022$P8_4)
table(HSRV2022$P8_4)
HSRV2022$P8_4[HSRV2022$P8_4 == "NS/NC"] <- NA
HSRV2022$P8_4 <- factor(HSRV2022$P8_4, levels = c("Más que lo habitual", "Igual que lo habitual", 
                                                  "Menos que lo habitual", "Mucho menos que lo habitual"))
levels(HSRV2022$P8_4) <- c("More", "Same", "Less", "Much less")
table(HSRV2022$P8_4)
P8_4 <- as.numeric(HSRV2022$P8_4)

# P8_5 Constantly under strain
levels(HSRV2022$P8_5)
table(HSRV2022$P8_5)
HSRV2022$P8_5[HSRV2022$P8_5 == "NS/NC"] <- NA
HSRV2022$P8_5 <- factor(HSRV2022$P8_5, levels = c("Más que lo habitual", "Igual que lo habitual", 
                                                  "Menos que lo habitual", "Mucho menos que lo habitual"))
levels(HSRV2022$P8_5) <- c("Not at all", "No more", "Rather more", "Much more than usual")
table(HSRV2022$P8_5)
P8_5 <- as.numeric(HSRV2022$P8_5)

# P8_6 Unable to overcome difficulties 
levels(HSRV2022$P8_6)
table(HSRV2022$P8_6)
HSRV2022$P8_6[HSRV2022$P8_6 == "NS/NC"] <- NA
HSRV2022$P8_6 <- factor(HSRV2022$P8_6, levels = c("Más que lo habitual", "Igual que lo habitual", 
                                                  "Menos que lo habitual", "Mucho menos que lo habitual"))
levels(HSRV2022$P8_6) <- c("Not at all", "No more", "Rather more", "Much more than usual")
table(HSRV2022$P8_6)
P8_6 <- as.numeric(HSRV2022$P8_6)

# P8_7 Enjoy activities 
levels(HSRV2022$P8_7)
table(HSRV2022$P8_7)
HSRV2022$P8_7[HSRV2022$P8_7 == "NS/NC"] <- NA
HSRV2022$P8_7 <- factor(HSRV2022$P8_7, levels = c("Más que lo habitual", "Igual que lo habitual", 
                                                  "Menos que lo habitual", "Mucho menos que lo habitual"))
levels(HSRV2022$P8_7) <- c("More", "Same", "Less", "Much less")
table(HSRV2022$P8_7)
P8_7 <- as.numeric(HSRV2022$P8_7)

# P8_8 Face up to problems 
levels(HSRV2022$P8_8)
table(HSRV2022$P8_8)
HSRV2022$P8_8[HSRV2022$P8_8 == "NS/NC"] <- NA
HSRV2022$P8_8 <- factor(HSRV2022$P8_8, levels = c("Más que lo habitual", "Igual que lo habitual", 
                                                  "Menos que lo habitual", "Mucho menos que lo habitual"))
levels(HSRV2022$P8_8) <- c("More", "Same", "Less", "Much less")
table(HSRV2022$P8_8)
P8_8 <- as.numeric(HSRV2022$P8_8)

# P8_9 Feel depressed
levels(HSRV2022$P8_9)
table(HSRV2022$P8_9)
HSRV2022$P8_9[HSRV2022$P8_9 == "NS/NC"] <- NA
HSRV2022$P8_9 <- factor(HSRV2022$P8_9, levels = c("Más que lo habitual", "Igual que lo habitual", 
                                                  "Menos que lo habitual", "Mucho menos que lo habitual"))
levels(HSRV2022$P8_9) <- c("Not at all", "No more", "Rather more", "Much more than usual")
table(HSRV2022$P8_9)
P8_9 <- as.numeric(HSRV2022$P8_9)

# P8_10 Lose confidence
levels(HSRV2022$P8_10)
table(HSRV2022$P8_10)
HSRV2022$P8_10[HSRV2022$P8_10 == "NS/NC"] <- NA
HSRV2022$P8_10 <- factor(HSRV2022$P8_10, levels = c("Más que lo habitual", "Igual que lo habitual", 
                                                    "Menos que lo habitual", "Mucho menos que lo habitual"))
levels(HSRV2022$P8_10) <- c("Not at all", "No more", "Rather more", "Much more than usual")
table(HSRV2022$P8_10)
P8_10 <- as.numeric(HSRV2022$P8_10)

# P8_11 Feel worthless
levels(HSRV2022$P8_11)
table(HSRV2022$P8_11)
HSRV2022$P8_11[HSRV2022$P8_11 == "NS/NC"] <- NA
HSRV2022$P8_11 <- factor(HSRV2022$P8_11, levels = c("Más que lo habitual", "Igual que lo habitual", 
                                                    "Menos que lo habitual", "Mucho menos que lo habitual"))
levels(HSRV2022$P8_11) <- c("Not at all", "No more", "Rather more", "Much more than usual")
table(HSRV2022$P8_11)
P8_11 <- as.numeric(HSRV2022$P8_11)

# P8_12 Feel reasonably happy
levels(HSRV2022$P8_12)
table(HSRV2022$P8_12)
HSRV2022$P8_12[HSRV2022$P8_12 == "NS/NC"] <- NA
HSRV2022$P8_12 <- factor(HSRV2022$P8_12, levels = c("Más que lo habitual", "Igual que lo habitual", 
                                                    "Menos que lo habitual", "Mucho menos que lo habitual"))
levels(HSRV2022$P8_12) <- c("More", "Same", "Less", "Much less")
table(HSRV2022$P8_12)
P8_12 <- as.numeric(HSRV2022$P8_12)

y <- data.frame(P8_1, P8_2, P8_3, P8_4, P8_5, P8_6,
                P8_7, P8_8, P8_9, P8_10, P8_11, P8_12)

# Number of response variables
NVars <- ncol(y)
# Number of levels
NCats <- unique(apply(y, 2, function(x) {length(table(x))}))

ones <- rep(1, NCats)

rm(list = c("P8_1", "P8_2", "P8_3", "P8_4", "P8_5", "P8_6", 
            "P8_7", "P8_8", "P8_9", "P8_10", "P8_11", "P8_12"))

#### Linear predictor and some objects ####

# Covariate sex: 1 = Male; 2 = Female
sexC <- HSRV2022$sexo
levels(sexC) <- c("Male", "Female")
sex <- as.numeric(sexC)

# Covariate age group: 1 = [15,25); 2 = [25,35); 3 = [35,45); 4 = [45,55); 5 = [55,65); 
# 6 = [65,70); 7 = [70,75); 8 = [75,...)
ageC <- cut(HSRV2022$Edad, breaks = c(15, 25, 35, 45, 55, 65, 70, 75, 103), 
            include.lowest = TRUE, right = FALSE)
levels(ageC)[length(table(ageC))] <- "[75,...)"
age <- as.numeric(ageC)

# Number of respondents by sex and age group
table(sexC, ageC)

# Number of levels of each (categorical) covariate
NSex <- length(table(sex))
NAges <- length(table(age))

### Preparation of maps ###

# Cartography of the Region of Valencia
load(file.path("data", "CartoCV.Rdata"))
# Cartography is sorted by municipality code
order(carto_muni$INE_MUN)-1:542
# Neighborhood structure by contiguity
cv.nb <- poly2nb(carto_muni)

# Some extra neighborhoods are added for Rincón de Ademuz comarca
cv.nb[[277]] <- as.integer(sort(c(cv.nb[[277]], 312, 317, 517, 523, 508)))
cv.nb[[363]] <- as.integer(sort(c(cv.nb[[363]], 312, 317, 517, 523, 508)))
cv.nb[[364]] <- as.integer(sort(c(cv.nb[[364]], 312, 317, 517, 523, 508)))
cv.nb[[477]] <- as.integer(sort(c(cv.nb[[477]], 312, 317, 517, 523, 508)))

cv.nb[[312]] <- as.integer(sort(c(cv.nb[[312]], 277, 363, 364, 477)))
cv.nb[[317]] <- as.integer(sort(c(cv.nb[[317]], 277, 363, 364, 477)))
cv.nb[[517]] <- as.integer(sort(c(cv.nb[[517]], 277, 363, 364, 477)))
cv.nb[[523]] <- as.integer(sort(c(cv.nb[[523]], 277, 363, 364, 477)))
cv.nb[[508]] <- as.integer(sort(c(cv.nb[[508]], 277, 363, 364, 477)))

# Municipality codes
INE_MUN <- as.numeric(as.character(carto_muni@data$INE_MUN))

# # Checking
# kk1 <- levels(HSRV2022$LOCALIDAD)
# kk2 <- carto_muni@data$NOMBRE_MUNI
# kk <- data.frame(kk1, kk2)

# Municipality of each respondent
muni <- as.numeric(HSRV2022$LOCALIDAD)
# Number of (distinct) municipalities (542)
NMuni <- length(INE_MUN); rm(INE_MUN)

# Number of neighbors of each municipality
nadj <- card(cv.nb)
# Neighbors of each municipality
map <- unlist(cv.nb)
# Sum of all the neighbor numbers of all municipalities
nadj.tot <- length(map)
# Cumulative sums of the number of neighbors of each municipality
index <- c(0, cumsum(nadj))

#### Coding the Leroux CAR distribution in NIMBLE ####

# Diagonal matrix with the number of neighbors of each area
D <- diag(nadj)
# Adjacency matrix
W <- nb2mat(cv.nb, style = "B", zero.policy = TRUE)
# Eigenvalues of D-W
Lambda <- eigen(D - W)$values
# Identity matrix
I <- diag(rep(1, NMuni))

# All the neighborhoods j ~ i where i < j
from.to <- cbind(rep(1:NMuni, times = nadj), map); colnames(from.to) <- c("from", "to")
from.to <- from.to[which(from.to[, 1] < from.to[, 2]), ]
NDist <- nrow(from.to)

dcar_leroux <- nimbleFunction(
  name = 'dcar_leroux',
  run = function(x = double(1),        # Spatial random effect (vector)
                 rho = double(0),      # Amount of spatial dependence (scalar)
                 sd.theta = double(0), # Standard deviation (scalar)
                 Lambda = double(1),   # Eigenvalues of matrix D - W
                 from.to = double(2),  # Matrix of distinct pairs of neighbors from.to[, 1] < from.to[, 2]
                 log = integer(0, default = 0)) {
    returnType(double(0))
    
    # Number of small areas
    NMuni <- dim(x)[1]
    # Number of distinct pairs of neighbors
    NDist <- dim(from.to)[1]
    # Required vectors
    x.from <- nimNumeric(NDist)
    x.to <- nimNumeric(NDist)
    for (Dist in 1:NDist) {
      x.from[Dist] <- x[from.to[Dist, 1]]
      x.to[Dist] <- x[from.to[Dist, 2]]
    }
    
    # Log-density
    logDens <- sum(dnorm(x[1:NMuni], mean = 0, sd = sd.theta * pow(1 - rho, -1/2), log = TRUE)) -
      NMuni/2 * log(1 - rho) + 1/2 * sum(log(rho * (Lambda[1:NMuni] - 1) + 1)) - 
      1/2 * pow(sd.theta, -2) * rho * sum(pow(x.from[1:NDist] - x.to[1:NDist], 2))
    if(log) return(logDens)
    else return(exp(logDens))
  }
)

#### Model-Indep ####

n.chains <- 5
this_cluster <- makeCluster(n.chains)

### Model code ###

modelCode <- nimbleCode(
  {
    for(Var in 1:NVars) {
      # Likelihood
      for (Resp in 1:NResp) {
        y[Resp, Var] ~ dcat(prlevels[Resp, Var, 1:NCats])
        
        # Definition of the probabilities of each category as a function of the
        # cumulative probabilities
        prlevels[Resp, Var, 1] <- p.gamma[Resp, Var, 1]
        for (Cat in 2:(NCats-1)) {
          prlevels[Resp, Var, Cat] <- p.gamma[Resp, Var, Cat] - p.gamma[Resp, Var, Cat-1]
        }
        prlevels[Resp, Var, NCats] <- 1 - p.gamma[Resp, Var, NCats-1]
        
        # Linear predictor
        for (Cat in 1:(NCats-1)) {
          logit(p.gamma[Resp, Var, Cat]) <- kappa[sex[Resp], age[Resp], Cat, Var] + 
            sd.theta[Var] * theta[muni[Resp], Var]
        }
      }
    }
    
    # Prior distributions
    
    # kappa[1:NSex, 1:NAges, 1:(NCats-1), 1:NVars] cut points
    # Monotonic transformation
    for (Var in 1:NVars) {
      for (SexGroup in 1:NSex) {
        for (AgeGroup in 1:NAges) {
          for (Cat in 1:(NCats-1)) {
            kappa[SexGroup, AgeGroup, Cat, Var] <- logit(sum(delta[SexGroup, AgeGroup, Var, 1:Cat]))
          }
          # delta[1:NSex, 1:NAges, 1:NVars, 1:NCats] Dirichlet prior
          delta[SexGroup, AgeGroup, Var, 1:NCats] ~ ddirch(ones[1:NCats])
        }
      }
    }
    
    # theta[1:NMuni, 1:NVars] spatial random effects
    for (Var in 1:NVars) {
      # theta[1:NMuni, 1:NVars] spatial random effects
      # LCAR distribution
      theta[1:NMuni, Var] ~ dcar_leroux(rho = rho[Var],
                                        sd.theta = 1,
                                        Lambda = Lambda[1:NMuni],
                                        from.to = from.to[1:NDist, 1:2])
    }
    
    # Hyperparameters of the spatial random effects
    for (Var in 1:NVars) {
      rho[Var] ~ dunif(0, 1)
      sd.theta[Var] ~ dhalfflat()
    }
    
    # Stochastic restrictions
    # Required vectors
    for (Var in 1:NVars) {
      for (Resp in 1:NResp) {
        theta.Resp[Resp, Var] <- theta[muni[Resp], Var]
      }
      
      # Zero-mean constraint for theta.Resp
      zero.theta.resp[Var] ~ dnorm(mean.thetas.resp[Var], 10000)
      mean.thetas.resp[Var] <- mean(theta.Resp[1:NResp, Var])
    }
    
  }
)

### Data to be loaded ###

modelData <- list(y = as.matrix(y), zero.theta.resp = rep(0, NVars))

modelConstants <- list(NResp = NResp, NCats = NCats, NVars = NVars, sex = sex, 
                       age = age, muni = muni, NSex = NSex, NAges = NAges, 
                       NMuni = NMuni, ones = ones, NDist = NDist, Lambda = Lambda, 
                       from.to = from.to)

### Parameters to be saved ###

modelParameters <- c("kappa", "theta", "sd.theta", "rho", 
                     "delta")

# Create a function with all the needed code
run_MCMC_allcode <- function(X, code, constants, data, monitors) {
  
  pacman::p_load(nimble, extraDistr, install = FALSE)
  
  dcar_leroux <- nimbleFunction(
    name = 'dcar_leroux',
    run = function(x = double(1),        # Spatial random effect (vector)
                   rho = double(0),      # Amount of spatial dependence (scalar)
                   sd.theta = double(0), # Standard deviation (scalar)
                   Lambda = double(1),   # Eigenvalues of matrix D - W
                   from.to = double(2),  # Matrix of distinct pairs of neighbors from.to[, 1] < from.to[, 2]
                   log = integer(0, default = 0)) {
      returnType(double(0))
      
      # Number of small areas
      NMuni <- dim(x)[1]
      # Number of distinct pairs of neighbors
      NDist <- dim(from.to)[1]
      # Required vectors
      x.from <- nimNumeric(NDist)
      x.to <- nimNumeric(NDist)
      for (Dist in 1:NDist) {
        x.from[Dist] <- x[from.to[Dist, 1]]
        x.to[Dist] <- x[from.to[Dist, 2]]
      }
      
      logDens <- sum(dnorm(x[1:NMuni], mean = 0, sd = sd.theta * pow(1 - rho, -1/2), log = TRUE)) -
        NMuni/2 * log(1 - rho) + 1/2 * sum(log(rho * (Lambda[1:NMuni] - 1) + 1)) - 
        1/2 * pow(sd.theta, -2) * rho * sum(pow(x.from[1:NDist] - x.to[1:NDist], 2))
      if(log) return(logDens)
      else return(exp(logDens))
    }
  )
  
  rcar_leroux <- nimbleFunction(
    name = 'rcar_leroux',
    run = function(n = integer(0),
                   rho = double(0),
                   sd.theta = double(0),
                   Lambda = double(1),
                   from.to = double(2)) {
      returnType(double(1))
      
      nimStop("user-defined distribution dcar_leroux provided without random generation function.")
      x <- nimNumeric(542)
      return(x)
    }
  )
  
  assign('dcar_leroux', dcar_leroux, envir = .GlobalEnv)
  assign('rcar_leroux', rcar_leroux, envir = .GlobalEnv)
  
  NSex <- constants$NSex
  NAges <- constants$NAges
  NCats <- constants$NCats
  ones <- constants$ones
  NResp <- constants$NResp
  NMuni <- constants$NMuni
  NVars <- constants$NVars
  
  # Let’s create the Nimble model, creates the nodes (inits should be passed now)
  model <- nimbleModel(code = code, 
                       constants = constants,
                       data = data, 
                       inits = list(delta = array(rdirichlet(NSex * NAges * NVars, ones),
                                                  dim = c(NSex, NAges, NVars, NCats)),
                                    rho = runif(NVars),
                                    theta = matrix(rnorm(NMuni * NVars, sd = 0.1), nrow = NMuni, ncol = NVars),
                                    sd.theta = runif(NVars)), 
                       calculate = FALSE)
  
  # Compile the model, which means generating C++ code, compiling that code, and loading it back into R
  Cmodel <- compileNimble(model)
  
  # model$getParents(model$getNodeNames(dataOnly = TRUE), stochOnly = TRUE)
  
  # Configuration
  modelMCMCconfiguration <- configureMCMC(model, useConjugacy = FALSE,
                                          enableWAIC = TRUE)
  
  # Remove desire samplers
  modelMCMCconfiguration$removeSamplers(c("theta", "rho", "sd.theta"))
  
  # Add slice/RW-MH theta[1:NMuni, 1:NVars] samplers
  thetas <- matrix(nrow = NMuni, ncol = NVars)
  for (Var in 1:NVars) {
    for (Muni in 1:NMuni) {
      thetas[Muni, Var] <- paste0("theta[",Muni,",",Var,"]")
    }
  }
  
  smuni <- sort(unique(constants$muni))
  for (Var in 1:NVars) {
    for (Muni in 1:NMuni) {
      ifelse(Muni %in% smuni,
             modelMCMCconfiguration$addSampler(target = thetas[Muni, Var], type = "RW"),
             modelMCMCconfiguration$addSampler(target = thetas[Muni, Var], type = "RW"))
    }
  }
  
  # Add slice rho sampler
  rhos <- character(NVars)
  for (Var in 1:NVars) {
    rhos[Var] <- paste0("rho[",Var,"]")
  }
  
  for (Var in 1:NVars) {
    modelMCMCconfiguration$addSampler(target = rhos[Var], type = "slice")
  }
  
  # Add slice sd.M.Muni sampler
  sd.thetas <- character(NVars)
  for (Var in 1:NVars) {
    sd.thetas[Var] <- paste0("sd.theta[",Var,"]")
  }
  
  for (Var in 1:NVars) {
    modelMCMCconfiguration$addSampler(target = sd.thetas[Var], type = "slice")
  }
  
  # Add new monitors
  modelMCMCconfiguration$monitors <- c()
  modelMCMCconfiguration$addMonitors(monitors)
  # Build MCMC object
  modelMCMC <- buildMCMC(modelMCMCconfiguration)
  # Need to reset the nimbleFunctions in order to add the new MCMC
  CmodelMCMC <- compileNimble(modelMCMC, project = model,
                              resetFunctions = TRUE)
  # Results
  results <- runMCMC(CmodelMCMC, niter = 8000, nburnin = 2000, thin = 30, setSeed = X)
  
  return(results)
}

system.time(salnimble <- parLapply(cl = this_cluster, X = 1:n.chains, 
                                   fun = run_MCMC_allcode, 
                                   code = modelCode,
                                   constants = modelConstants,
                                   data = modelData,
                                   monitors = modelParameters))

# It's good practice to close the cluster when you're done with it.
stopCluster(this_cluster)

# 1.84h on a server with: niter = 8000, nburnin = 2000, thin = 30
# saveRDS(salnimble, file = file.path("results", "multi-2022-nimble-MH-indep-8k-2k-30-WAIC.rds"))

#### Model-Corr ####

n.chains <- 5
this_cluster <- makeCluster(n.chains)

### Model code ###

modelCode <- nimbleCode(
  {
    for(Var in 1:NVars) {
      # Likelihood
      for (Resp in 1:NResp) {
        y[Resp, Var] ~ dcat(prlevels[Resp, Var, 1:NCats])
        
        # Definition of the probabilities of each category as a function of the
        # cumulative probabilities
        prlevels[Resp, Var, 1] <- p.gamma[Resp, Var, 1]
        for (Cat in 2:(NCats-1)) {
          prlevels[Resp, Var, Cat] <- p.gamma[Resp, Var, Cat] - p.gamma[Resp, Var, Cat-1]
        }
        prlevels[Resp, Var, NCats] <- 1 - p.gamma[Resp, Var, NCats-1]
        
        # Linear predictor
        for (Cat in 1:(NCats-1)) {
          logit(p.gamma[Resp, Var, Cat]) <- kappa[sex[Resp], age[Resp], Cat, Var] + 
            theta[muni[Resp], Var]
        }
      }
    }
    
    # Prior distributions
    
    # kappa[1:NSex, 1:NAges, 1:(NCats-1), 1:NVars] cut points
    # Monotonic transformation
    for (Var in 1:NVars) {
      for (SexGroup in 1:NSex) {
        for (AgeGroup in 1:NAges) {
          for (Cat in 1:(NCats-1)) {
            kappa[SexGroup, AgeGroup, Cat, Var] <- logit(sum(delta[SexGroup, AgeGroup, Var, 1:Cat]))
          }
          # delta[1:NSex, 1:NAges, 1:NVars, 1:NCats] Dirichlet prior
          delta[SexGroup, AgeGroup, Var, 1:NCats] ~ ddirch(ones[1:NCats])
        }
      }
    }
    
    # theta[1:NMuni, 1:NVars] spatial random effects
    for (Var in 1:NVars) {
      for (Muni in 1:NMuni) {
        theta[Muni, Var] <- inprod(sub.Muni[Muni, ], M.Muni[, Var])
      }
      # sub.Muni[1:NMuni, 1:NVars] underlying spatial REs
      # LCAR distribution
      sub.Muni[1:NMuni, Var] ~ dcar_leroux(rho = rho[Var],
                                           sd.theta = 1,
                                           Lambda = Lambda[1:NMuni],
                                           from.to = from.to[1:NDist, 1:2])
    }
    
    # Hyperparameter of the spatial random effects
    for (Var in 1:NVars) {
      rho[Var] ~ dunif(0, 1)
    }
    
    # M.Resp[1:NVars, 1:NVars] and M.Muni[1:NVars, 1:NVars] M-matrices
    for (Var1 in 1:NVars) {
      for (Var2 in 1:NVars) {
        M.Muni[Var1, Var2] ~ dnorm(0, tau.M.Muni)
      }
    }
    
    # Prior for precisions of M.Muni
    tau.M.Muni <- pow(sd.M.Muni, -2)
    sd.M.Muni ~ dhalfflat()
    
    # Stochastic restrictions
    # Required vectors
    for (Var in 1:NVars) {
      for (Resp in 1:NResp) {
        sub.Muni.Resp[Resp, Var] <- sub.Muni[muni[Resp], Var]
      }
      
      # Zero-mean constraint for sub.Muni.Resp
      zero.sub.Muni.resp[Var] ~ dnorm(mean.sub.Munis.resp[Var], 10000)
      mean.sub.Munis.resp[Var] <- mean(sub.Muni.Resp[1:NResp, Var])
    }
    
  }
)

### Data to be loaded ###

modelData <- list(y = as.matrix(y), zero.sub.Muni.resp = rep(0, NVars))

modelConstants <- list(NResp = NResp, NCats = NCats, NVars = NVars, sex = sex, 
                       age = age, muni = muni, NSex = NSex, NAges = NAges, 
                       NMuni = NMuni, ones = ones, NDist = NDist, Lambda = Lambda, 
                       from.to = from.to)

### Parameters to be saved ###

modelParameters <- c("kappa", "theta", "M.Muni", "sd.M.Muni", "rho", 
                     "delta", "sub.Muni")

# Create a function with all the needed code
run_MCMC_allcode <- function(X, code, constants, data, monitors) {
  
  pacman::p_load(nimble, extraDistr, install = FALSE)
  
  dcar_leroux <- nimbleFunction(
    name = 'dcar_leroux',
    run = function(x = double(1),        # Spatial random effect (vector)
                   rho = double(0),      # Amount of spatial dependence (scalar)
                   sd.theta = double(0), # Standard deviation (scalar)
                   Lambda = double(1),   # Eigenvalues of matrix D - W
                   from.to = double(2),  # Matrix of distinct pairs of neighbors from.to[, 1] < from.to[, 2]
                   log = integer(0, default = 0)) {
      returnType(double(0))
      
      # Number of small areas
      NMuni <- dim(x)[1]
      # Number of distinct pairs of neighbors
      NDist <- dim(from.to)[1]
      # Required vectors
      x.from <- nimNumeric(NDist)
      x.to <- nimNumeric(NDist)
      for (Dist in 1:NDist) {
        x.from[Dist] <- x[from.to[Dist, 1]]
        x.to[Dist] <- x[from.to[Dist, 2]]
      }
      
      logDens <- sum(dnorm(x[1:NMuni], mean = 0, sd = sd.theta * pow(1 - rho, -1/2), log = TRUE)) -
        NMuni/2 * log(1 - rho) + 1/2 * sum(log(rho * (Lambda[1:NMuni] - 1) + 1)) - 
        1/2 * pow(sd.theta, -2) * rho * sum(pow(x.from[1:NDist] - x.to[1:NDist], 2))
      if(log) return(logDens)
      else return(exp(logDens))
    }
  )
  
  rcar_leroux <- nimbleFunction(
    name = 'rcar_leroux',
    run = function(n = integer(0),
                   rho = double(0),
                   sd.theta = double(0),
                   Lambda = double(1),
                   from.to = double(2)) {
      returnType(double(1))
      
      nimStop("user-defined distribution dcar_leroux provided without random generation function.")
      x <- nimNumeric(542)
      return(x)
    }
  )
  
  assign('dcar_leroux', dcar_leroux, envir = .GlobalEnv)
  assign('rcar_leroux', rcar_leroux, envir = .GlobalEnv)
  
  NSex <- constants$NSex
  NAges <- constants$NAges
  NCats <- constants$NCats
  ones <- constants$ones
  NResp <- constants$NResp
  NMuni <- constants$NMuni
  NVars <- constants$NVars
  
  # Let’s create the Nimble model, creates the nodes (inits should be passed now)
  model <- nimbleModel(code = code, 
                       constants = constants,
                       data = data, 
                       inits = list(delta = array(rdirichlet(NSex * NAges * NVars, ones),
                                                  dim = c(NSex, NAges, NVars, NCats)),
                                    rho = runif(NVars),
                                    sub.Muni = matrix(rnorm(NMuni * NVars, sd = 0.01), nrow = NMuni, ncol = NVars),
                                    M.Muni = matrix(rnorm(NVars * NVars, sd = 0.5), ncol = NVars, nrow = NVars),
                                    sd.M.Muni = runif(1, min = 0.2, max = 0.8)), 
                       calculate = FALSE)
  
  # Compile the model, which means generating C++ code, compiling that code, and loading it back into R
  Cmodel <- compileNimble(model)
  
  # model$getParents(model$getNodeNames(dataOnly = TRUE), stochOnly = TRUE)
  
  # Configuration
  modelMCMCconfiguration <- configureMCMC(model, useConjugacy = FALSE,
                                          enableWAIC = TRUE)
  
  # Remove desire samplers
  modelMCMCconfiguration$removeSamplers(c("sub.Muni", "rho", "sd.M.Muni"))
  
  # Add slice/RW-MH sub.Muni[1:NMuni, 1:NVars] samplers
  sub.Munis <- matrix(nrow = NMuni, ncol = NVars)
  for (Var in 1:NVars) {
    for (Muni in 1:NMuni) {
      sub.Munis[Muni, Var] <- paste0("sub.Muni[",Muni,",",Var,"]")
    }
  }
  
  smuni <- sort(unique(constants$muni))
  for (Var in 1:NVars) {
    for (Muni in 1:NMuni) {
      ifelse(Muni %in% smuni,
             modelMCMCconfiguration$addSampler(target = sub.Munis[Muni, Var], type = "RW"),
             modelMCMCconfiguration$addSampler(target = sub.Munis[Muni, Var], type = "RW"))
    }
  }
  
  # Add slice rho sampler
  rhos <- character(NVars)
  for (Var in 1:NVars) {
    rhos[Var] <- paste0("rho[",Var,"]")
  }
  
  for (Var in 1:NVars) {
    modelMCMCconfiguration$addSampler(target = rhos[Var], type = "slice")
  }
  
  # Add slice sd.M.Muni sampler
  modelMCMCconfiguration$addSampler(target = "sd.M.Muni", type = "slice")
  
  # Add new monitors
  modelMCMCconfiguration$monitors <- c()
  modelMCMCconfiguration$addMonitors(monitors)
  # Build MCMC object
  modelMCMC <- buildMCMC(modelMCMCconfiguration)
  # Need to reset the nimbleFunctions in order to add the new MCMC
  CmodelMCMC <- compileNimble(modelMCMC, project = model,
                              resetFunctions = TRUE)
  # Results
  results <- runMCMC(CmodelMCMC, niter = 8000, nburnin = 2000, thin = 30, setSeed = X)
  
  return(results)
}

system.time(salnimble <- parLapply(cl = this_cluster, X = 1:n.chains, 
                                   fun = run_MCMC_allcode, 
                                   code = modelCode,
                                   constants = modelConstants,
                                   data = modelData,
                                   monitors = modelParameters))

# It's good practice to close the cluster when you're done with it.
stopCluster(this_cluster)

# 6.18h on a server with: niter = 8000, nburnin = 2000, thin = 30
# saveRDS(salnimble, file = file.path("results", "multi-2022-nimble-MH-corr-8k-2k-30-WAIC.rds"))

#### Model-Corr&IRE ####

n.chains <- 5
this_cluster <- makeCluster(n.chains)

### Model code ###

modelCode <- nimbleCode(
  {
    for(Var in 1:NVars) {
      # Likelihood
      for (Resp in 1:NResp) {
        y[Resp, Var] ~ dcat(prlevels[Resp, Var, 1:NCats])
        
        # Definition of the probabilities of each category as a function of the
        # cumulative probabilities
        prlevels[Resp, Var, 1] <- p.gamma[Resp, Var, 1]
        for (Cat in 2:(NCats-1)) {
          prlevels[Resp, Var, Cat] <- p.gamma[Resp, Var, Cat] - p.gamma[Resp, Var, Cat-1]
        }
        prlevels[Resp, Var, NCats] <- 1 - p.gamma[Resp, Var, NCats-1]
        
        # Linear predictor
        for (Cat in 1:(NCats-1)) {
          logit(p.gamma[Resp, Var, Cat]) <- kappa[sex[Resp], age[Resp], Cat, Var] + 
            theta[muni[Resp], Var] + psi[Resp, Var]
        }
      }
    }
    
    # Prior distributions
    
    # kappa[1:NSex, 1:NAges, 1:(NCats-1), 1:NVars] cut points
    # Monotonic transformation
    for (Var in 1:NVars) {
      for (SexGroup in 1:NSex) {
        for (AgeGroup in 1:NAges) {
          for (Cat in 1:(NCats-1)) {
            kappa[SexGroup, AgeGroup, Cat, Var] <- logit(sum(delta[SexGroup, AgeGroup, Var, 1:Cat]))
          }
          # delta[1:NSex, 1:NAges, 1:NVars, 1:NCats] Dirichlet prior
          delta[SexGroup, AgeGroup, Var, 1:NCats] ~ ddirch(ones[1:NCats])
        }
      }
    }
    
    # psi[1:NResp, 1:NVars] individual random effects
    for(Var in 1:NVars) {
      for (Resp in 1:NResp) {
        psi[Resp, Var] <- inprod(sub.Resp[Resp, ], M.Resp[, Var])
        # sub.Resp[1:NResp, 1:NVars] underlying individual REs
        sub.Resp[Resp, Var] ~ dnorm(0, 1)
      }
    }
    
    # theta[1:NMuni, 1:NVars] spatial random effects
    for (Var in 1:NVars) {
      for (Muni in 1:NMuni) {
        theta[Muni, Var] <- inprod(sub.Muni[Muni, ], M.Muni[, Var])
      }
      # sub.Muni[1:NMuni, 1:NVars] underlying spatial REs
      # LCAR distribution
      sub.Muni[1:NMuni, Var] ~ dcar_leroux(rho = rho[Var],
                                           sd.theta = 1,
                                           Lambda = Lambda[1:NMuni],
                                           from.to = from.to[1:NDist, 1:2])
    }
    
    # Hyperparameter of the spatial random effects
    for (Var in 1:NVars) {
      rho[Var] ~ dunif(0, 1)
    }
    
    # M.Resp[1:NVars, 1:NVars] and M.Muni[1:NVars, 1:NVars] M-matrices
    for (Var1 in 1:NVars) {
      for (Var2 in 1:NVars) {
        M.Resp[Var1, Var2] ~ dnorm(0, tau.M.Resp)
        M.Muni[Var1, Var2] ~ dnorm(0, tau.M.Muni)
      }
    }
    
    # Prior for precisions of M.Resp and M.Muni
    tau.M.Resp <- pow(sd.M.Resp, -2)
    sd.M.Resp ~ dhalfflat()
    
    tau.M.Muni <- pow(sd.M.Muni, -2)
    sd.M.Muni ~ dhalfflat()
    
    # Stochastic restrictions
    # Required vectors
    for (Var in 1:NVars) {
      for (Resp in 1:NResp) {
        sub.Muni.Resp[Resp, Var] <- sub.Muni[muni[Resp], Var]
      }
      
      # Zero-mean constraint for sub.Muni.Resp
      zero.sub.Muni.resp[Var] ~ dnorm(mean.sub.Munis.resp[Var], 10000)
      mean.sub.Munis.resp[Var] <- mean(sub.Muni.Resp[1:NResp, Var])
    }
    
  }
)

### Data to be loaded ###

modelData <- list(y = as.matrix(y), zero.sub.Muni.resp = rep(0, NVars))

modelConstants <- list(NResp = NResp, NCats = NCats, NVars = NVars, sex = sex, 
                       age = age, muni = muni, NSex = NSex, NAges = NAges, 
                       NMuni = NMuni, ones = ones, NDist = NDist, Lambda = Lambda, 
                       from.to = from.to)

### Parameters to be saved ###

modelParameters <- c("kappa", "theta", "M.Muni", "rho",
                     "sd.M.Muni", "psi", "M.Resp", "sd.M.Resp", 
                     "delta", "sub.Muni", "sub.Resp")

# Create a function with all the needed code
run_MCMC_allcode <- function(X, code, constants, data, monitors) {
  
  pacman::p_load(nimble, extraDistr, install = FALSE)
  
  dcar_leroux <- nimbleFunction(
    name = 'dcar_leroux',
    run = function(x = double(1),        # Spatial random effect (vector)
                   rho = double(0),      # Amount of spatial dependence (scalar)
                   sd.theta = double(0), # Standard deviation (scalar)
                   Lambda = double(1),   # Eigenvalues of matrix D - W
                   from.to = double(2),  # Matrix of distinct pairs of neighbors from.to[, 1] < from.to[, 2]
                   log = integer(0, default = 0)) {
      returnType(double(0))
      
      # Number of small areas
      NMuni <- dim(x)[1]
      # Number of distinct pairs of neighbors
      NDist <- dim(from.to)[1]
      # Required vectors
      x.from <- nimNumeric(NDist)
      x.to <- nimNumeric(NDist)
      for (Dist in 1:NDist) {
        x.from[Dist] <- x[from.to[Dist, 1]]
        x.to[Dist] <- x[from.to[Dist, 2]]
      }
      
      logDens <- sum(dnorm(x[1:NMuni], mean = 0, sd = sd.theta * pow(1 - rho, -1/2), log = TRUE)) -
        NMuni/2 * log(1 - rho) + 1/2 * sum(log(rho * (Lambda[1:NMuni] - 1) + 1)) - 
        1/2 * pow(sd.theta, -2) * rho * sum(pow(x.from[1:NDist] - x.to[1:NDist], 2))
      if(log) return(logDens)
      else return(exp(logDens))
    }
  )
  
  rcar_leroux <- nimbleFunction(
    name = 'rcar_leroux',
    run = function(n = integer(0),
                   rho = double(0),
                   sd.theta = double(0),
                   Lambda = double(1),
                   from.to = double(2)) {
      returnType(double(1))
      
      nimStop("user-defined distribution dcar_leroux provided without random generation function.")
      x <- nimNumeric(542)
      return(x)
    }
  )
  
  assign('dcar_leroux', dcar_leroux, envir = .GlobalEnv)
  assign('rcar_leroux', rcar_leroux, envir = .GlobalEnv)
  
  NSex <- constants$NSex
  NAges <- constants$NAges
  NCats <- constants$NCats
  ones <- constants$ones
  NResp <- constants$NResp
  NMuni <- constants$NMuni
  NVars <- constants$NVars
  
  # Let’s create the Nimble model, creates the nodes (inits should be passed now)
  model <- nimbleModel(code = code, 
                       constants = constants,
                       data = data, 
                       inits = list(delta = array(rdirichlet(NSex * NAges * NVars, ones),
                                                  dim = c(NSex, NAges, NVars, NCats)),
                                    rho = runif(NVars),
                                    sub.Resp = matrix(rnorm(NResp * NVars, sd = 0.01), nrow = NResp, ncol = NVars),
                                    M.Resp = matrix(rnorm(NVars * NVars, sd = 0.5), ncol = NVars, nrow = NVars),
                                    sd.M.Resp = runif(1, min = 0.2, max = 0.8),
                                    sub.Muni = matrix(rnorm(NMuni * NVars, sd = 0.01), nrow = NMuni, ncol = NVars),
                                    M.Muni = matrix(rnorm(NVars * NVars, sd = 0.5), ncol = NVars, nrow = NVars),
                                    sd.M.Muni = runif(1, min = 0.2, max = 0.8)), 
                       calculate = FALSE)
  
  # Compile the model, which means generating C++ code, compiling that code, and loading it back into R
  Cmodel <- compileNimble(model)
  
  # model$getParents(model$getNodeNames(dataOnly = TRUE), stochOnly = TRUE)
  
  # Configuration
  modelMCMCconfiguration <- configureMCMC(model, useConjugacy = FALSE,
                                          enableWAIC = TRUE)
  
  # Remove desire samplers
  modelMCMCconfiguration$removeSamplers(c("sub.Muni", "rho", "sd.M.Muni",
                                          "sd.M.Resp"))
  
  # Add slice/RW-MH sub.Muni[1:NMuni, 1:NVars] samplers
  sub.Munis <- matrix(nrow = NMuni, ncol = NVars)
  for (Var in 1:NVars) {
    for (Muni in 1:NMuni) {
      sub.Munis[Muni, Var] <- paste0("sub.Muni[",Muni,",",Var,"]")
    }
  }
  
  smuni <- sort(unique(constants$muni))
  for (Var in 1:NVars) {
    for (Muni in 1:NMuni) {
      ifelse(Muni %in% smuni,
             modelMCMCconfiguration$addSampler(target = sub.Munis[Muni, Var], type = "RW"),
             modelMCMCconfiguration$addSampler(target = sub.Munis[Muni, Var], type = "RW"))
    }
  }
  
  # Add slice rho sampler
  rhos <- character(NVars)
  for (Var in 1:NVars) {
    rhos[Var] <- paste0("rho[",Var,"]")
  }
  
  for (Var in 1:NVars) {
    modelMCMCconfiguration$addSampler(target = rhos[Var], type = "slice")
  }
  
  # Add slice sd.M.Muni and sd.M.Resp samplers
  modelMCMCconfiguration$addSampler(target = "sd.M.Resp", type = "slice")
  modelMCMCconfiguration$addSampler(target = "sd.M.Muni", type = "slice")
  
  # Add new monitors
  modelMCMCconfiguration$monitors <- c()
  modelMCMCconfiguration$addMonitors(monitors)
  # Build MCMC object
  modelMCMC <- buildMCMC(modelMCMCconfiguration)
  # Need to reset the nimbleFunctions in order to add the new MCMC
  CmodelMCMC <- compileNimble(modelMCMC, project = model,
                              resetFunctions = TRUE)
  # Results
  results <- runMCMC(CmodelMCMC, niter = 8000, nburnin = 2000, thin = 30, setSeed = X)
  
  return(results)
}

system.time(salnimble <- parLapply(cl = this_cluster, X = 1:n.chains, 
                                   fun = run_MCMC_allcode, 
                                   code = modelCode,
                                   constants = modelConstants,
                                   data = modelData,
                                   monitors = modelParameters))

# It's good practice to close the cluster when you're done with it.
stopCluster(this_cluster)

# 14.42h on a server with: niter = 8000, nburnin = 2000, thin = 30
# saveRDS(salnimble, file = file.path("results", "multi-2022-nimble-MH-corr-ire-8k-2k-30-WAIC.rds"))

#### Results ####

n.chains <- 5
labels <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12")
salnimble1 <- readRDS(file = file.path("results", "multi-2022-nimble-MH-indep-8k-2k-30-WAIC.rds"))
salnimble2 <- readRDS(file = file.path("results", "multi-2022-nimble-MH-corr-8k-2k-30-WAIC.rds"))
salnimble3 <- readRDS(file = file.path("results", "multi-2022-nimble-MH-corr-ire-8k-2k-30-WAIC.rds"))

#### Function: salnimble to salwinbugs for Model-Indep ####

NimToWin <- function(salnimble) {
  
  n.chains <- length(salnimble)
  n.sims <- n.chains * nrow(salnimble[[1]])
  
  kappa <- array(dim = c(n.sims, NSex, NAges, NCats, NVars))
  theta <- array(dim = c(n.sims, NMuni, NVars))
  rho <- matrix(nrow = n.sims, ncol = NVars)
  sd.theta <- matrix(nrow = n.sims, ncol = NVars)
  
  for (Var in 1:NVars) {
    for (Cat in 1:(NCats - 1)) {
      for (Sex in 1:NSex) {
        for (Age in 1:NAges) {
          kappa[, Sex, Age, Cat, Var] <- c(salnimble[[1]][,  paste0("kappa[", Sex, ", ", Age, ", ", Cat, ", ", Var, "]")],
                                           salnimble[[2]][,  paste0("kappa[", Sex, ", ", Age, ", ", Cat, ", ", Var, "]")],
                                           salnimble[[3]][,  paste0("kappa[", Sex, ", ", Age, ", ", Cat, ", ", Var, "]")],
                                           salnimble[[4]][,  paste0("kappa[", Sex, ", ", Age, ", ", Cat, ", ", Var, "]")],
                                           salnimble[[5]][,  paste0("kappa[", Sex, ", ", Age, ", ", Cat, ", ", Var, "]")])
        }
      }
    }
  }
  
  for (Var in 1:NVars) {
    for (Muni in 1:NMuni) {
      theta[, Muni, Var] <- c(salnimble[[1]][, paste0("theta[", Muni, ", ", Var, "]")], 
                              salnimble[[2]][, paste0("theta[", Muni, ", ", Var, "]")], 
                              salnimble[[3]][, paste0("theta[", Muni, ", ", Var, "]")], 
                              salnimble[[4]][, paste0("theta[", Muni, ", ", Var, "]")], 
                              salnimble[[5]][, paste0("theta[", Muni, ", ", Var, "]")])
    }
  }
  
  for (Var in 1:NVars) {
    rho[, Var] <- c(salnimble[[1]][, paste0("rho[", Var, "]")], 
                    salnimble[[2]][, paste0("rho[", Var, "]")],
                    salnimble[[3]][, paste0("rho[", Var, "]")], 
                    salnimble[[4]][, paste0("rho[", Var, "]")], 
                    salnimble[[5]][, paste0("rho[", Var, "]")])
  }
  
  for (Var in 1:NVars) {
    sd.theta[, Var] <- c(salnimble[[1]][, paste0("sd.theta[", Var, "]")], 
                         salnimble[[2]][, paste0("sd.theta[", Var, "]")],
                         salnimble[[3]][, paste0("sd.theta[", Var, "]")], 
                         salnimble[[4]][, paste0("sd.theta[", Var, "]")], 
                         salnimble[[5]][, paste0("sd.theta[", Var, "]")])
  }
  
  summary <- MCMCsummary(object = salnimble, round = 4)
  # summary <- "not available"
  sims.list <- list("kappa" = kappa, "theta" = theta, 
                    "sd.theta" = sd.theta, "rho" = rho)
  
  salwinbugs <- list("summary" = summary, "sims.list" = sims.list,
                     "n.chains" = n.chains, "n.sims" = n.sims)
  
  return(salwinbugs)
}

salwinbugs1 <- NimToWin(salnimble = salnimble1)

salwinbugs1$summary[startsWith(labels(salwinbugs1$summary)[[1]], "kappa"), ]

#### Function: salnimble to salwinbugs for Model-Corr ####

NimToWin <- function(salnimble) {
  
  n.chains <- length(salnimble)
  n.sims <- n.chains * nrow(salnimble[[1]])
  
  kappa <- array(dim = c(n.sims, NSex, NAges, NCats, NVars))
  theta <- array(dim = c(n.sims, NMuni, NVars))
  sd.M.Muni <- numeric(length = n.sims)
  M.Muni <- array(dim = c(n.sims, NVars, NVars))
  rho <- matrix(nrow = n.sims, ncol = NVars)
  
  for (Var in 1:NVars) {
    for (Cat in 1:(NCats - 1)) {
      for (Sex in 1:NSex) {
        for (Age in 1:NAges) {
          kappa[, Sex, Age, Cat, Var] <- c(salnimble[[1]][,  paste0("kappa[", Sex, ", ", Age, ", ", Cat, ", ", Var, "]")],
                                           salnimble[[2]][,  paste0("kappa[", Sex, ", ", Age, ", ", Cat, ", ", Var, "]")],
                                           salnimble[[3]][,  paste0("kappa[", Sex, ", ", Age, ", ", Cat, ", ", Var, "]")],
                                           salnimble[[4]][,  paste0("kappa[", Sex, ", ", Age, ", ", Cat, ", ", Var, "]")],
                                           salnimble[[5]][,  paste0("kappa[", Sex, ", ", Age, ", ", Cat, ", ", Var, "]")])
        }
      }
    }
  }
  
  for (Var in 1:NVars) {
    for (Muni in 1:NMuni) {
      theta[, Muni, Var] <- c(salnimble[[1]][, paste0("theta[", Muni, ", ", Var, "]")], 
                              salnimble[[2]][, paste0("theta[", Muni, ", ", Var, "]")], 
                              salnimble[[3]][, paste0("theta[", Muni, ", ", Var, "]")], 
                              salnimble[[4]][, paste0("theta[", Muni, ", ", Var, "]")], 
                              salnimble[[5]][, paste0("theta[", Muni, ", ", Var, "]")])
    }
  }
  
  sd.M.Muni <- c(salnimble[[1]][, "sd.M.Muni"], salnimble[[2]][, "sd.M.Muni"], 
                 salnimble[[3]][, "sd.M.Muni"], salnimble[[4]][, "sd.M.Muni"], 
                 salnimble[[5]][, "sd.M.Muni"])
  
  for (Var1 in 1:NVars) {
    for (Var2 in 1:NVars) {
      M.Muni[, Var1, Var2] <- c(salnimble[[1]][, paste0("M.Muni[", Var1, ", ", Var2, "]")], 
                                salnimble[[2]][, paste0("M.Muni[", Var1, ", ", Var2, "]")], 
                                salnimble[[3]][, paste0("M.Muni[", Var1, ", ", Var2, "]")], 
                                salnimble[[4]][, paste0("M.Muni[", Var1, ", ", Var2, "]")], 
                                salnimble[[5]][, paste0("M.Muni[", Var1, ", ", Var2, "]")])
    }
  }
  
  for (Var in 1:NVars) {
    rho[, Var] <- c(salnimble[[1]][, paste0("rho[", Var, "]")], 
                    salnimble[[2]][, paste0("rho[", Var, "]")],
                    salnimble[[3]][, paste0("rho[", Var, "]")], 
                    salnimble[[4]][, paste0("rho[", Var, "]")], 
                    salnimble[[5]][, paste0("rho[", Var, "]")])
  }
  
  summary <- MCMCsummary(object = salnimble, round = 4)
  # summary <- "not available"
  sims.list <- list("kappa" = kappa, "theta" = theta, "sd.M.Muni" = sd.M.Muni, 
                    "M.Muni" = M.Muni, "rho" = rho)
  
  salwinbugs <- list("summary" = summary, "sims.list" = sims.list,
                     "n.chains" = n.chains, "n.sims" = n.sims)
  
  return(salwinbugs)
}

salwinbugs2 <- NimToWin(salnimble = salnimble2)

salwinbugs2$summary[startsWith(labels(salwinbugs2$summary)[[1]], "kappa"), ]

#### Function: salnimble to salwinbugs for Model-Corr&IRE ####

NimToWin <- function(salnimble) {
  
  n.chains <- length(salnimble)
  n.sims <- n.chains * nrow(salnimble[[1]])
  
  kappa <- array(dim = c(n.sims, NSex, NAges, NCats, NVars))
  psi <- array(dim = c(n.sims, NResp, NVars))
  sd.M.Resp <- numeric(length = n.sims)
  M.Resp <- array(dim = c(n.sims, NVars, NVars))
  theta <- array(dim = c(n.sims, NMuni, NVars))
  sd.M.Muni <- numeric(length = n.sims)
  M.Muni <- array(dim = c(n.sims, NVars, NVars))
  rho <- matrix(nrow = n.sims, ncol = NVars)
  
  for (Var in 1:NVars) {
    for (Cat in 1:(NCats - 1)) {
      for (Sex in 1:NSex) {
        for (Age in 1:NAges) {
          kappa[, Sex, Age, Cat, Var] <- c(salnimble[[1]][,  paste0("kappa[", Sex, ", ", Age, ", ", Cat, ", ", Var, "]")],
                                           salnimble[[2]][,  paste0("kappa[", Sex, ", ", Age, ", ", Cat, ", ", Var, "]")],
                                           salnimble[[3]][,  paste0("kappa[", Sex, ", ", Age, ", ", Cat, ", ", Var, "]")],
                                           salnimble[[4]][,  paste0("kappa[", Sex, ", ", Age, ", ", Cat, ", ", Var, "]")],
                                           salnimble[[5]][,  paste0("kappa[", Sex, ", ", Age, ", ", Cat, ", ", Var, "]")])
        }
      }
    }
  }
  
  for (Var in 1:NVars) {
    for (Resp in 1:NResp) {
      psi[, Resp, Var] <- c(salnimble[[1]][, paste0("psi[", Resp, ", ", Var, "]")], 
                            salnimble[[2]][, paste0("psi[", Resp, ", ", Var, "]")], 
                            salnimble[[3]][, paste0("psi[", Resp, ", ", Var, "]")], 
                            salnimble[[4]][, paste0("psi[", Resp, ", ", Var, "]")], 
                            salnimble[[5]][, paste0("psi[", Resp, ", ", Var, "]")])
    }
  }
  
  sd.M.Resp <- c(salnimble[[1]][, "sd.M.Resp"], salnimble[[2]][, "sd.M.Resp"], 
                 salnimble[[3]][, "sd.M.Resp"], salnimble[[4]][, "sd.M.Resp"], 
                 salnimble[[5]][, "sd.M.Resp"])
  
  for (Var1 in 1:NVars) {
    for (Var2 in 1:NVars) {
      M.Resp[, Var1, Var2] <- c(salnimble[[1]][, paste0("M.Resp[", Var1, ", ", Var2, "]")], 
                                salnimble[[2]][, paste0("M.Resp[", Var1, ", ", Var2, "]")], 
                                salnimble[[3]][, paste0("M.Resp[", Var1, ", ", Var2, "]")], 
                                salnimble[[4]][, paste0("M.Resp[", Var1, ", ", Var2, "]")], 
                                salnimble[[5]][, paste0("M.Resp[", Var1, ", ", Var2, "]")])
    }
  }
  
  for (Var in 1:NVars) {
    for (Muni in 1:NMuni) {
      theta[, Muni, Var] <- c(salnimble[[1]][, paste0("theta[", Muni, ", ", Var, "]")], 
                              salnimble[[2]][, paste0("theta[", Muni, ", ", Var, "]")], 
                              salnimble[[3]][, paste0("theta[", Muni, ", ", Var, "]")], 
                              salnimble[[4]][, paste0("theta[", Muni, ", ", Var, "]")], 
                              salnimble[[5]][, paste0("theta[", Muni, ", ", Var, "]")])
    }
  }
  
  sd.M.Muni <- c(salnimble[[1]][, "sd.M.Muni"], salnimble[[2]][, "sd.M.Muni"],
                 salnimble[[3]][, "sd.M.Muni"], salnimble[[4]][, "sd.M.Muni"],
                 salnimble[[5]][, "sd.M.Muni"])
  
  for (Var1 in 1:NVars) {
    for (Var2 in 1:NVars) {
      M.Muni[, Var1, Var2] <- c(salnimble[[1]][, paste0("M.Muni[", Var1, ", ", Var2, "]")], 
                                salnimble[[2]][, paste0("M.Muni[", Var1, ", ", Var2, "]")], 
                                salnimble[[3]][, paste0("M.Muni[", Var1, ", ", Var2, "]")], 
                                salnimble[[4]][, paste0("M.Muni[", Var1, ", ", Var2, "]")], 
                                salnimble[[5]][, paste0("M.Muni[", Var1, ", ", Var2, "]")])
    }
  }
  
  for (Var in 1:NVars) {
    rho[, Var] <- c(salnimble[[1]][, paste0("rho[", Var, "]")], 
                    salnimble[[2]][, paste0("rho[", Var, "]")],
                    salnimble[[3]][, paste0("rho[", Var, "]")], 
                    salnimble[[4]][, paste0("rho[", Var, "]")], 
                    salnimble[[5]][, paste0("rho[", Var, "]")])
  }
  
  summary <- MCMCsummary(object = salnimble, round = 4)
  # summary <- "not available"
  sims.list <- list("kappa" = kappa, "theta" = theta, "M.Muni" = M.Muni, 
                    "rho" = rho, "sd.M.Muni" = sd.M.Muni, "psi" = psi, 
                    "sd.M.Resp" = sd.M.Resp, "M.Resp" = M.Resp)
  
  salwinbugs <- list("summary" = summary, "sims.list" = sims.list,
                     "n.chains" = n.chains, "n.sims" = n.sims)
  
  return(salwinbugs)
}

salwinbugs3 <- NimToWin(salnimble = salnimble3)

salwinbugs3$summary[startsWith(labels(salwinbugs3$summary)[[1]], "kappa"), ]

#### Convergence assessment for Model-Indep ####

which((salwinbugs1$summary[startsWith(labels(salwinbugs1$summary)[[1]], "kappa"), 6] > 1.1) | (salwinbugs1$summary[startsWith(labels(salwinbugs1$summary)[[1]], "kappa"), 7] < 100))
which((salwinbugs1$summary[startsWith(labels(salwinbugs1$summary)[[1]], "theta"), 6] > 1.1) | (salwinbugs1$summary[startsWith(labels(salwinbugs1$summary)[[1]], "theta"), 7] < 100))
which((salwinbugs1$summary[startsWith(labels(salwinbugs1$summary)[[1]], "sd.theta"), 6] > 1.1) | (salwinbugs1$summary[startsWith(labels(salwinbugs1$summary)[[1]], "sd.theta"), 7] < 100))
which((salwinbugs1$summary[startsWith(labels(salwinbugs1$summary)[[1]], "rho"), 6] > 1.1) | (salwinbugs1$summary[startsWith(labels(salwinbugs1$summary)[[1]], "rho"), 7] < 100))

MCMCsummary(object = salnimble1, params = "kappa",
            # exact = TRUE,
            # ISB = FALSE,
            round = 4)

MCMCtrace(object = salnimble1,
          pdf = FALSE, # no export to PDF
          ind = TRUE, # separate density lines per chain
          Rhat = TRUE,
          n.eff = TRUE,
          params = "sd.theta")

test <- "kappa"

MCMCtrace(object = salnimble1,
          pdf = FALSE, # no export to PDF
          ind = TRUE, # separate density lines per chain
          Rhat = TRUE,
          n.eff = TRUE,
          exact = TRUE,
          ISB = FALSE,
          params = rownames(salwinbugs1$summary[startsWith(labels(salwinbugs1$summary)[[1]], test), ])[(salwinbugs1$summary[startsWith(labels(salwinbugs1$summary)[[1]], test), 6] > 1.02) | (salwinbugs1$summary[startsWith(labels(salwinbugs1$summary)[[1]], test), 7] < 400)])

MCMCtrace(object = salnimble1,
          pdf = FALSE, # no export to PDF
          ind = TRUE, # separate density lines per chain
          Rhat = TRUE,
          n.eff = TRUE,
          exact = TRUE,
          ISB = FALSE,
          params = c("theta[526, 1]", "theta[526, 2]", "theta[526, 3]",
                     "theta[14, 1]", "theta[14, 2]", "theta[14, 3]",
                     "theta[65, 1]", "theta[65, 2]", "theta[65, 3]",
                     "theta[177, 1]", "theta[177, 2]", "theta[177, 3]"))

#### Convergence assessment for Model-Corr ####

which((salwinbugs2$summary[startsWith(labels(salwinbugs2$summary)[[1]], "kappa"), 6] > 1.1) | (salwinbugs2$summary[startsWith(labels(salwinbugs2$summary)[[1]], "kappa"), 7] < 100))
which((salwinbugs2$summary[startsWith(labels(salwinbugs2$summary)[[1]], "sd.M.Muni"), 6] > 1.1) | (salwinbugs2$summary[startsWith(labels(salwinbugs2$summary)[[1]], "sd.M.Muni"), 7] < 100))
which((salwinbugs2$summary[startsWith(labels(salwinbugs2$summary)[[1]], "theta"), 6] > 1.1) | (salwinbugs2$summary[startsWith(labels(salwinbugs2$summary)[[1]], "theta"), 7] < 100))
which((salwinbugs2$summary[startsWith(labels(salwinbugs2$summary)[[1]], "rho"), 6] > 1.1) | (salwinbugs2$summary[startsWith(labels(salwinbugs2$summary)[[1]], "rho"), 7] < 100))
which((salwinbugs2$summary[startsWith(labels(salwinbugs2$summary)[[1]], "M.Muni"), 6] > 1.1) | (salwinbugs2$summary[startsWith(labels(salwinbugs2$summary)[[1]], "M.Muni"), 7] < 100))

MCMCsummary(object = salnimble2, params = "kappa",
            # exact = TRUE,
            # ISB = FALSE,
            round = 4)

MCMCtrace(object = salnimble2,
          pdf = FALSE, # no export to PDF
          ind = TRUE, # separate density lines per chain
          Rhat = TRUE,
          n.eff = TRUE,
          params = "sd.M.Muni")

test <- "kappa"

MCMCtrace(object = salnimble2,
          pdf = FALSE, # no export to PDF
          ind = TRUE, # separate density lines per chain
          Rhat = TRUE,
          n.eff = TRUE,
          exact = TRUE,
          ISB = FALSE,
          params = rownames(salwinbugs2$summary[startsWith(labels(salwinbugs2$summary)[[1]], test), ])[(salwinbugs2$summary[startsWith(labels(salwinbugs2$summary)[[1]], test), 6] > 1.02) | (salwinbugs2$summary[startsWith(labels(salwinbugs2$summary)[[1]], test), 7] < 400)])

MCMCtrace(object = salnimble2,
          pdf = FALSE, # no export to PDF
          ind = TRUE, # separate density lines per chain
          Rhat = TRUE,
          n.eff = TRUE,
          exact = TRUE,
          ISB = FALSE,
          params = c("theta[526, 1]", "theta[526, 2]", "theta[526, 3]",
                     "theta[14, 1]", "theta[14, 2]", "theta[14, 3]",
                     "theta[65, 1]", "theta[65, 2]", "theta[65, 3]",
                     "theta[177, 1]", "theta[177, 2]", "theta[177, 3]"))

#### Convergence assessment for Model-Corr&IRE ####

which((salwinbugs3$summary[startsWith(labels(salwinbugs3$summary)[[1]], "kappa"), 6] > 1.1) | (salwinbugs3$summary[startsWith(labels(salwinbugs3$summary)[[1]], "kappa"), 7] < 100))
which((salwinbugs3$summary[startsWith(labels(salwinbugs3$summary)[[1]], "sd.M.Muni"), 6] > 1.1) | (salwinbugs3$summary[startsWith(labels(salwinbugs3$summary)[[1]], "sd.M.Muni"), 7] < 100))
which((salwinbugs3$summary[startsWith(labels(salwinbugs3$summary)[[1]], "theta"), 6] > 1.1) | (salwinbugs3$summary[startsWith(labels(salwinbugs3$summary)[[1]], "theta"), 7] < 100))
which((salwinbugs3$summary[startsWith(labels(salwinbugs3$summary)[[1]], "rho"), 6] > 1.1) | (salwinbugs3$summary[startsWith(labels(salwinbugs3$summary)[[1]], "rho"), 7] < 100))
which((salwinbugs3$summary[startsWith(labels(salwinbugs3$summary)[[1]], "M.Muni"), 6] > 1.1) | (salwinbugs3$summary[startsWith(labels(salwinbugs3$summary)[[1]], "M.Muni"), 7] < 100))
which((salwinbugs3$summary[startsWith(labels(salwinbugs3$summary)[[1]], "sd.M.Resp"), 6] > 1.1) | (salwinbugs3$summary[startsWith(labels(salwinbugs3$summary)[[1]], "sd.M.Resp"), 7] < 100))
which((salwinbugs3$summary[startsWith(labels(salwinbugs3$summary)[[1]], "psi"), 6] > 1.1) | (salwinbugs3$summary[startsWith(labels(salwinbugs3$summary)[[1]], "psi"), 7] < 100))
which((salwinbugs3$summary[startsWith(labels(salwinbugs3$summary)[[1]], "M.Resp"), 6] > 1.1) | (salwinbugs3$summary[startsWith(labels(salwinbugs3$summary)[[1]], "M.Resp"), 7] < 100))

MCMCsummary(object = salnimble3, params = "kappa",
            # exact = TRUE,
            # ISB = FALSE,
            round = 4)

MCMCtrace(object = salnimble3,
          pdf = FALSE, # no export to PDF
          ind = TRUE, # separate density lines per chain
          Rhat = TRUE,
          n.eff = TRUE,
          params = "sd.M.Muni")

test <- "kappa"

MCMCtrace(object = salnimble3,
          pdf = FALSE, # no export to PDF
          ind = TRUE, # separate density lines per chain
          Rhat = TRUE,
          n.eff = TRUE,
          exact = TRUE,
          ISB = FALSE,
          params = rownames(salwinbugs3$summary[startsWith(labels(salwinbugs3$summary)[[1]], test), ])[(salwinbugs3$summary[startsWith(labels(salwinbugs3$summary)[[1]], test), 6] > 1.02) | (salwinbugs3$summary[startsWith(labels(salwinbugs3$summary)[[1]], test), 7] < 400)])

MCMCtrace(object = salnimble3,
          pdf = FALSE, # no export to PDF
          ind = TRUE, # separate density lines per chain
          Rhat = TRUE,
          n.eff = TRUE,
          exact = TRUE,
          ISB = FALSE,
          params = c("theta[526, 1]", "theta[526, 2]", "theta[526, 3]",
                     "theta[14, 1]", "theta[14, 2]", "theta[14, 3]",
                     "theta[65, 1]", "theta[65, 2]", "theta[65, 3]",
                     "theta[177, 1]", "theta[177, 2]", "theta[177, 3]"))

MCMCtrace(object = salnimble3,
          pdf = FALSE, # no export to PDF
          ind = TRUE, # separate density lines per chain
          Rhat = TRUE,
          n.eff = TRUE,
          exact = TRUE,
          ISB = FALSE,
          params = c("psi[1, 1]", "psi[1, 2]", "psi[1, 3]",
                     "psi[100, 1]", "psi[100, 2]", "psi[100, 3]",
                     "psi[1000, 1]", "psi[1000, 2]", "psi[1000, 3]",
                     "psi[5485, 1]", "psi[5485, 2]", "psi[5485, 3]"))

#### Spatial correlation matrix for Model-Indep ####

salwinbugs <- salwinbugs1
thetasim <- salwinbugs$sims.list$theta
n.sims <- salwinbugs$n.sims

SurveyMapping.Corr <- function(thetasim) {
  
  Corr <- array(dim = c(n.sims, NVars, NVars))
  
  for (sim in 1:n.sims) {
    Corr[sim, , ] <- cor(thetasim[sim, , ])
  }
  return(Corr)
}

Corr <- SurveyMapping.Corr(thetasim = thetasim)

Corr.mean <- matrix(ncol = NVars, nrow = NVars)
Corr.quantileL <- matrix(ncol = NVars, nrow = NVars)
Corr.quantileU <- matrix(ncol = NVars, nrow = NVars)
for (Var1 in 1:NVars) {
  for (Var2 in 1:NVars) {
    Corr.mean[Var1, Var2] <- mean(Corr[, Var1, Var2])
    Corr.quantileL[Var1, Var2] <- quantile(Corr[, Var1, Var2], probs = 0.025)
    Corr.quantileU[Var1, Var2] <- quantile(Corr[, Var1, Var2], probs = 0.975)
  }
}

rm(Corr)

eigen(Corr.mean)
eigen(Corr.mean)$values[1]/NVars
eigen(Corr.mean)$values[2]/NVars

Corr.mean <- data.frame(Corr.mean); rownames(Corr.mean) <- labels;
colnames(Corr.mean) <- labels

# orden <- corrMatOrder(as.matrix(Corr.mean), order = "hclust", hclust.method = "ward.D2")
# orden <- c(1, 3, 4, 7, 8, 12, 2, 5, 6, 9, 10, 11)
orden <- c(3, 4, 1, 8, 7, 12, 11, 9, 10, 2, 5, 6)
# orden <- 1:NVars

Corr.mean.orden <- as.matrix(Corr.mean)
Corr.mean.orden <- Corr.mean.orden[orden, orden]

Corr.quantileL <- data.frame(Corr.quantileL); rownames(Corr.quantileL) <- labels;
colnames(Corr.quantileL) <- labels

Corr.quantileL.orden <- as.matrix(Corr.quantileL)
Corr.quantileL.orden <- Corr.quantileL.orden[orden, orden]

Corr.quantileU <- data.frame(Corr.quantileU); rownames(Corr.quantileU) <- labels;
colnames(Corr.quantileU) <- labels

Corr.quantileU.orden <- as.matrix(Corr.quantileU)
Corr.quantileU.orden <- Corr.quantileU.orden[orden, orden]

### Adding relevances ###

# replace on line 446 +0.35 y +0.15
# trace(corrplot, edit = TRUE)

Corr.mean.orden
Corr.quantileL.orden
Corr.quantileU.orden
Relevance <- matrix(as.numeric(Corr.quantileL.orden > 0 | Corr.quantileU.orden < 0), ncol = NVars, nrow = NVars, byrow = FALSE)
colnames(Relevance) <- rownames(Relevance) <-  colnames(Corr.mean.orden)
Relevance <- (Relevance - 1) * (-1)
for (Var in 1:NVars) { Relevance[Var, Var] <- 1 }

# First: ellipses in lower triangular
corrplot(as.matrix(Corr.mean.orden),
         type = "lower", method = "ellipse", 
         p.mat = Relevance, sig.level = 0.05, insig = "label_sig",
         pch.cex = 1.5, pch.col = "grey20",
         addCoef.col = "black", number.cex = 0.8,
         tl.pos = "d", tl.cex = 0.9, cl.pos = "r")

# Second: CI in upper triangular
corrplot(as.matrix(Corr.mean.orden),
         type = "upper", method = "square",
         diag = FALSE, add = TRUE, cl.pos = "n",
         plotCI = "rect", lowCI = as.matrix(Corr.quantileL.orden), 
         uppCI = as.matrix(Corr.quantileU.orden), rect.col = "navy", tl.pos = "n")

#### Spatial correlation matrix for Model-Corr and Model-Corr&IRE ####

# orden <- corrMatOrder(as.matrix(Corr.mean), order = "hclust", hclust.method = "ward.D2")
# orden <- c(1, 3, 4, 7, 8, 12, 2, 5, 6, 9, 10, 11)
orden <- c(3, 4, 1, 8, 7, 12, 11, 9, 10, 2, 5, 6)
# orden <- 1:NVars

SurveyMapping.Sigma.Muni <- function(salwinbugs) {
  
  n.sims <- salwinbugs$n.sims
  NVars <- dim(salwinbugs$sims.list$theta)[3]
  M.Muni <- salwinbugs$sims.list$M.Muni
  
  Sigma.Muni <- array(dim = c(n.sims, NVars, NVars))
  
  for (sim in 1:n.sims) {
    Sigma.Muni[sim, , ] <- t(M.Muni[sim, , ]) %*% M.Muni[sim, , ]
  }
  return(Sigma.Muni)
}

salwinbugs <- salwinbugs3
n.sims <- salwinbugs$n.sims
Sigma.Munisim <- SurveyMapping.Sigma.Muni(salwinbugs = salwinbugs)
Corr <- array(dim = c(n.sims, NVars, NVars))

# Set of the n.sims correlation matrices
for (sim in 1:n.sims) {
  Corr[sim, , ] <- diag(diag(Sigma.Munisim[sim, , ])^(-1/2)) %*% Sigma.Munisim[sim, , ] %*%  diag(diag(Sigma.Munisim[sim, , ])^(-1/2))
}

Corr.mean <- matrix(ncol = NVars, nrow = NVars)
Corr.quantileL <- matrix(ncol = NVars, nrow = NVars)
Corr.quantileU <- matrix(ncol = NVars, nrow = NVars)
Sigma.Munimean <- matrix(ncol = NVars, nrow = NVars)
for (Var1 in 1:NVars) {
  for (Var2 in 1:NVars) {
    Corr.mean[Var1, Var2] <- mean(Corr[, Var1, Var2])
    Corr.quantileL[Var1, Var2] <- quantile(Corr[, Var1, Var2], probs = 0.025)
    Corr.quantileU[Var1, Var2] <- quantile(Corr[, Var1, Var2], probs = 0.975)
    Sigma.Munimean[Var1, Var2] <- mean(Sigma.Munisim[, Var1, Var2])
  }
}

rm(Corr); rm(Sigma.Munisim)

eigen(Corr.mean)
eigen(Corr.mean)$values[1]/NVars
eigen(Corr.mean)$values[2]/NVars

Corr.mean <- data.frame(Corr.mean); rownames(Corr.mean) <- labels;
colnames(Corr.mean) <- labels

# orden <- corrMatOrder(as.matrix(Corr.mean), order = "hclust", hclust.method = "ward.D2")
# orden <- c(1, 3, 4, 7, 8, 12, 2, 5, 6, 9, 10, 11)
# orden <- c(3, 4, 1, 8, 7, 12, 11, 9, 10, 2, 5, 6)
# orden <- 1:NVars

Corr.mean.orden <- as.matrix(Corr.mean)
Corr.mean.orden <- Corr.mean.orden[orden, orden]

Corr.quantileL <- data.frame(Corr.quantileL); rownames(Corr.quantileL) <- labels;
colnames(Corr.quantileL) <- labels

Corr.quantileL.orden <- as.matrix(Corr.quantileL)
Corr.quantileL.orden <- Corr.quantileL.orden[orden, orden]

Corr.quantileU <- data.frame(Corr.quantileU); rownames(Corr.quantileU) <- labels;
colnames(Corr.quantileU) <- labels

Corr.quantileU.orden <- as.matrix(Corr.quantileU)
Corr.quantileU.orden <- Corr.quantileU.orden[orden, orden]

### Adding relevances ###

# replace on line 446 +0.35 y +0.15
# trace(corrplot, edit = TRUE)

Corr.mean.orden
Corr.quantileL.orden
Corr.quantileU.orden
Relevance <- matrix(as.numeric(Corr.quantileL.orden > 0 | Corr.quantileU.orden < 0), ncol = NVars, nrow = NVars, byrow = FALSE)
colnames(Relevance) <- rownames(Relevance) <-  colnames(Corr.mean.orden)
Relevance <- (Relevance - 1) * (-1)
for (Var in 1:NVars) { Relevance[Var, Var] <- 1 }

# First: ellipses in lower triangular
corrplot(as.matrix(Corr.mean.orden),
         type = "lower", method = "ellipse", 
         p.mat = Relevance, sig.level = 0.05, insig = "label_sig",
         pch.cex = 1.5, pch.col = "grey20",
         addCoef.col = "black", number.cex = 0.8,
         tl.pos = "d", tl.cex = 0.9, cl.pos = "r")

# Second: CI in upper triangular
corrplot(as.matrix(Corr.mean.orden),
         type = "upper", method = "square",
         diag = FALSE, add = TRUE, cl.pos = "n",
         plotCI = "rect", lowCI = as.matrix(Corr.quantileL.orden), 
         uppCI = as.matrix(Corr.quantileU.orden), rect.col = "navy", tl.pos = "n")

#### Individual correlation matrix for Model-Corr&IRE ####

# orden <- corrMatOrder(as.matrix(Corr.mean), order = "hclust", hclust.method = "ward.D2")
# orden <- c(1, 3, 4, 7, 8, 12, 2, 5, 6, 9, 10, 11)
# orden <- c(3, 4, 1, 8, 7, 12, 11, 9, 10, 2, 5, 6)
# orden <- 1:NVars

SurveyMapping.Sigma.Resp <- function(salwinbugs) {
  
  n.sims <- salwinbugs$n.sims
  NVars <- dim(salwinbugs$sims.list$theta)[3]
  M.Resp <- salwinbugs$sims.list$M.Resp
  
  Sigma.Resp <- array(dim = c(n.sims, NVars, NVars))
  
  for (sim in 1:n.sims) {
    Sigma.Resp[sim, , ] <- t(M.Resp[sim, , ]) %*% M.Resp[sim, , ]
  }
  return(Sigma.Resp)
}

salwinbugs <- salwinbugs3

n.sims <- salwinbugs$n.sims
Sigma.Respsim <- SurveyMapping.Sigma.Resp(salwinbugs = salwinbugs)
Corr <- array(dim = c(n.sims, NVars, NVars))

# Set of the n.sims correlation matrices
for (sim in 1:n.sims) {
  Corr[sim, , ] <- diag(diag(Sigma.Respsim[sim, , ])^(-1/2)) %*% Sigma.Respsim[sim, , ] %*%  diag(diag(Sigma.Respsim[sim, , ])^(-1/2))
}

Corr.mean <- matrix(ncol = NVars, nrow = NVars)
Corr.quantileL <- matrix(ncol = NVars, nrow = NVars)
Corr.quantileU <- matrix(ncol = NVars, nrow = NVars)
Sigma.Respmean <- matrix(ncol = NVars, nrow = NVars)
for (Var1 in 1:NVars) {
  for (Var2 in 1:NVars) {
    Corr.mean[Var1, Var2] <- mean(Corr[, Var1, Var2])
    Corr.quantileL[Var1, Var2] <- quantile(Corr[, Var1, Var2], probs = 0.025)
    Corr.quantileU[Var1, Var2] <- quantile(Corr[, Var1, Var2], probs = 0.975)
    Sigma.Respmean[Var1, Var2] <- mean(Sigma.Respsim[, Var1, Var2])
  }
}

eigen(Corr.mean)
eigen(Corr.mean)$values[1]/NVars
eigen(Corr.mean)$values[2]/NVars

Corr.mean <- data.frame(Corr.mean); rownames(Corr.mean) <- labels; colnames(Corr.mean) <- labels

orden <- corrMatOrder(as.matrix(Corr.mean), order = "hclust", hclust.method = "ward.D2")
# orden <- c(1, 3, 4, 7, 8, 12, 2, 5, 6, 9, 10, 11)
# orden <- c(3, 4, 1, 8, 7, 12, 11, 9, 10, 2, 5, 6)
# orden <- 1:NVars

Corr.mean.orden <- as.matrix(Corr.mean)
Corr.mean.orden <- Corr.mean.orden[orden, orden]

Corr.quantileL <- data.frame(Corr.quantileL); rownames(Corr.quantileL) <- labels;
colnames(Corr.quantileL) <- labels

Corr.quantileL.orden <- as.matrix(Corr.quantileL)
Corr.quantileL.orden <- Corr.quantileL.orden[orden, orden]

Corr.quantileU <- data.frame(Corr.quantileU); rownames(Corr.quantileU) <- labels;
colnames(Corr.quantileU) <- labels

Corr.quantileU.orden <- as.matrix(Corr.quantileU)
Corr.quantileU.orden <- Corr.quantileU.orden[orden, orden]

### Adding relevances ###

# replace on line 446 +0.35 y +0.15
# trace(corrplot, edit = TRUE)

Corr.mean.orden
Corr.quantileL.orden
Corr.quantileU.orden
Relevance <- matrix(as.numeric(Corr.quantileL.orden > 0 | Corr.quantileU.orden < 0), ncol = NVars, nrow = NVars, byrow = FALSE)
colnames(Relevance) <- rownames(Relevance) <-  colnames(Corr.mean.orden)
Relevance <- (Relevance - 1) * (-1)
for (Var in 1:NVars) { Relevance[Var, Var] <- 1 }

# First: ellipses in lower triangular
corrplot(as.matrix(Corr.mean.orden),
         type = "lower", method = "ellipse", 
         p.mat = Relevance, sig.level = 0.05, insig = "label_sig",
         pch.cex = 1.5, pch.col = "grey20",
         addCoef.col = "black", number.cex = 0.8,
         tl.pos = "d", tl.cex = 0.9, cl.pos = "r")

# Second: CI in upper triangular
corrplot(as.matrix(Corr.mean.orden),
         type = "upper", method = "square",
         diag = FALSE, add = TRUE, cl.pos = "n",
         plotCI = "rect", lowCI = as.matrix(Corr.quantileL.orden), 
         uppCI = as.matrix(Corr.quantileU.orden), rect.col = "navy", tl.pos = "n")

#### Number of respondents by small-area ####

# Does the municipality have any respondents?
carto_muni@data$index <- as.factor(as.numeric((1:NMuni %in% sort(unique(muni)))))
levels(carto_muni@data$index) <- c("No", "Yes")

# Number of respondents from each municipality
munif <- factor(muni, levels = 1:NMuni)
carto_muni@data$sampled <- as.numeric(table(munif))
breaks <- c(min(carto_muni@data$sampled) - 0.001, 0, 5, 10, 15, 20, 50, 100, max(carto_muni@data$sampled))
carto_muni@data$sampled <- cut(carto_muni@data$sampled, breaks = breaks, include.lowest = FALSE, right = TRUE)
levels(carto_muni@data$sampled) <- c("0", levels(carto_muni@data$sampled)[2:7], ">100")

spplot(carto_muni,
       c("sampled"),
       col.regions = colorRampPalette(brewer.pal(7,'Blues'))(8),
       cuts = 7,
       colorkey = list(key = list(labels = levels(carto_muni@data$sampled)),
                       width = 1.5, cex = 1.5, height = 0.75),
       par.settings = list(axis.line = list(col = 'transparent')),
       col = "black",
       lwd = 0.10)

#### Descriptive of thetamean - Boxplots ####

thetasim <- salwinbugs3$sims.list$theta

df <- data.frame("thetamean" = as.numeric(apply(thetasim, c(2, 3), mean)), "variable" = factor(rep(labels, each = NMuni), levels = labels))
# Use single color
ggplot(df, aes(x = variable, y = thetamean)) +
  geom_boxplot(fill = "grey80", color = "black") + 
  theme_bw() + 
  geom_hline(yintercept = 0, linetype = "dashed", col = "red")

as.numeric(with(df, by(thetamean, variable, sd)))

#### Maps of the RV - theta[1:NMuni, 1:NVars] ####

# Cartography of the Region of Valencia
load(file.path("data", "CartoCV.Rdata"))

NMods <- 3
thetasim <- array(dim = c(n.sims, NMuni, NVars, NMods))
thetasim[, , , 1] <- salwinbugs1$sims.list$theta
thetasim[, , , 2] <- salwinbugs2$sims.list$theta
thetasim[, , , 3] <- salwinbugs3$sims.list$theta

selection <- 1:4
NSel <- length(selection)
thetasim <- thetasim[, , selection, ]

# Fiveteen equal-probability intervals
breaks <- c(min(apply(thetasim, 2:4, mean)) - 0.001, quantile(apply(thetasim, 2:4, mean), probs = seq(1/15, 14/15, length.out = 14)), max(apply(thetasim, 2:4, mean)))
breaks <- c(-2.20, -0.5, -0.35, -0.25, -0.20, -0.15, -0.10, -0.05, 
            0.05, 0.10, 0.15, 0.20, 0.25, 0.35, 0.5, 2.20)

mod_labels <- c("Indep", "Corr", "CorrIRE")
for (Mod in 1:NMods) {
  for (Sel in 1:NSel) {
    carto_muni@data[[paste0(colnames(y)[selection[Sel]], "thetamean", mod_labels[Mod])]] <- cut(apply(thetasim[, , Sel, Mod], 2, mean), breaks = breaks, include.lowest = FALSE, right = TRUE)
    
    levels(carto_muni@data[[paste0(colnames(y)[selection[Sel]], "thetamean", mod_labels[Mod])]]) <- gsub("<=", "\u2264", c("<= -0.50", "(-0.50, -0.35]", "(-0.35, -0.25]", "(-0.25, -0.20]", 
                                                                                                                           "(-0.20, -0.15]", "(-0.15, -0.10]", "(-0.10, -0.05]", "(-0.05, 0.05]", 
                                                                                                                           "(0.05, 0.10]", "(0.10, 0.15]", "(0.15, 0.20]", "(0.20, 0.25]",
                                                                                                                           "(0.25, 0.35]", "(0.35, 0.50]", "> 0.50"))
  }
}

mod_labels <- c("CorrIRE", "Corr", "Indep")
all_labels <- matrix(nrow = NSel, ncol = NMods)
for (Sel in 1:NSel) {
  for (Mod in 1:NMods) {
    all_labels[Sel, Mod] <- paste0(labels[selection[Sel]], ".", mod_labels[Mod])
  }
}
all_labels <- as.character(all_labels)

spplot(carto_muni,
       c(colnames(carto_muni@data[, endsWith(colnames(carto_muni@data), "CorrIRE")]),
         colnames(carto_muni@data[, endsWith(colnames(carto_muni@data), "Corr")]),
         colnames(carto_muni@data[, endsWith(colnames(carto_muni@data), "Indep")])),
       names.attr = all_labels,
       col.regions = colorRampPalette(brewer.pal(7,'BrBG'))(15),
       cuts = 14,
       par.settings = list(layout.widths = list(left.padding = 0, right.padding = 0),
                           layout.heights = list(top.padding = 0, bottom.padding = 0),
                           axis.line = list(col = 'transparent')),
       strip = strip.custom(par.strip.text = list(cex = 0.95)),
       col = "black",
       lwd = 0.025,
       layout = c(NSel, NMods))

# Cartography of the Region of Valencia
load(file.path("data", "CartoCV.Rdata"))

NMods <- 3
thetasim <- array(dim = c(n.sims, NMuni, NVars, NMods))
thetasim[, , , 1] <- salwinbugs1$sims.list$theta
thetasim[, , , 2] <- salwinbugs2$sims.list$theta
thetasim[, , , 3] <- salwinbugs3$sims.list$theta

selection <- 1:NVars
NSel <- length(selection)
thetasim <- thetasim[, , selection, ]

# Fiveteen equal-probability intervals
breaks <- c(min(apply(thetasim, 2:4, mean)) - 0.001, quantile(apply(thetasim, 2:4, mean), probs = seq(1/15, 14/15, length.out = 14)), max(apply(thetasim, 2:4, mean)))
breaks <- c(-3.75, -0.75, -0.25, -0.20, -0.15, -0.10, -0.05, -0.02, 
            0.02, 0.05, 0.10, 0.15, 0.20, 0.25, 0.75, 3.75)

mod_labels <- c("Indep", "Corr", "CorrIRE")
for (Mod in 1:NMods) {
  for (Sel in 1:NSel) {
    carto_muni@data[[paste0(colnames(y)[selection[Sel]], "thetamean", mod_labels[Mod])]] <- cut(apply(thetasim[, , Sel, Mod], 2, mean), breaks = breaks, include.lowest = FALSE, right = TRUE)
    
    levels(carto_muni@data[[paste0(colnames(y)[selection[Sel]], "thetamean", mod_labels[Mod])]]) <- gsub("<=", "\u2264", c("<= -0.75", "(-0.75, -0.25]", "(-0.25, -0.20]", "(-0.20, -0.15]", 
                                                                                                                           "(-0.15, -0.10]", "(-0.10, -0.05]", "(-0.05, -0.02]", "(-0.02, 0.02]", 
                                                                                                                           "(0.02, 0.05]", "(0.05, 0.10]", "(0.10, 0.15]", "(0.15, 0.20]",
                                                                                                                           "(0.20, 0.25]", "(0.25, 0.75]", "> 0.75"))
  }
}

mod_labels <- c("CorrIRE", "Corr", "Indep")
all_labels <- matrix(nrow = NSel, ncol = NMods)
for (Sel in 1:NSel) {
  for (Mod in 1:NMods) {
    all_labels[Sel, Mod] <- paste0(labels[selection[Sel]], ".", mod_labels[Mod])
  }
}
all_labels <- as.character(all_labels)

spplot(carto_muni,
       c(colnames(carto_muni@data[, endsWith(colnames(carto_muni@data), "CorrIRE")]),
         colnames(carto_muni@data[, endsWith(colnames(carto_muni@data), "Corr")]),
         colnames(carto_muni@data[, endsWith(colnames(carto_muni@data), "Indep")])),
       names.attr = all_labels,
       col.regions = colorRampPalette(brewer.pal(7,'BrBG'))(15),
       cuts = 14,
       par.settings = list(layout.widths = list(left.padding = 0, right.padding = 0),
                           layout.heights = list(top.padding = 0, bottom.padding = 0),
                           axis.line = list(col = 'transparent')),
       strip = strip.custom(par.strip.text = list(cex = 0.95)),
       col = "black",
       lwd = 0.025,
       layout = c(NSel, NMods))

#### Maps of the RV - P(theta < 0 | y) ####

# Cartography of the Region of Valencia
load(file.path("data", "CartoCV.Rdata"))

NMods <- 3
mod_labels <- c("Indep", "Corr", "CorrIRE")
thetasim <- array(dim = c(n.sims, NMuni, NVars, NMods))
thetasim[, , , 1] <- salwinbugs1$sims.list$theta
thetasim[, , , 2] <- salwinbugs2$sims.list$theta
thetasim[, , , 3] <- salwinbugs3$sims.list$theta

# Checking when theta is LESS than zero
stepsim <- array(dim = c(n.sims, NMuni, NVars, NMods))
for (Mod in 1:NMods) {
  for (Var in 1:NVars) {
    for (sim in 1:n.sims) {
      for (Muni in 1:NMuni) {
        stepsim[sim, Muni, Var, Mod] <- ifelse(thetasim[sim, Muni, Var, Mod] < 0, 1, 0)
      }
    }
  }
}

selection <- 1:4
NSel <- length(selection)
stepsim <- stepsim[, , selection, ]

mod_labels <- c("Indep", "Corr", "CorrIRE")
for (Mod in 1:NMods) {
  for (Sel in 1:NSel) {
    carto_muni@data[[paste0(colnames(y)[selection[Sel]], "probmean", mod_labels[Mod])]] <- apply(stepsim[, , Sel, Mod], 2, mean)
  }
}

kk <- unlist(carto_muni@data[, startsWith(colnames(carto_muni@data), "P8_")])

limit <- max(kk) + 0.01

mod_labels <- c("CorrIRE", "Corr", "Indep")
all_labels <- matrix(nrow = NSel, ncol = NMods)
for (Sel in 1:NSel) {
  for (Mod in 1:NMods) {
    all_labels[Sel, Mod] <- paste0(labels[selection[Sel]], ".", mod_labels[Mod])
  }
}
all_labels <- as.character(all_labels)

spplot(carto_muni,
       c(colnames(carto_muni@data[, endsWith(colnames(carto_muni@data), "CorrIRE")]),
         colnames(carto_muni@data[, endsWith(colnames(carto_muni@data), "Corr")]),
         colnames(carto_muni@data[, endsWith(colnames(carto_muni@data), "Indep")])),
       names.attr = all_labels,
       col.regions = colorRampPalette(brewer.pal(7,'RdYlGn'))(15)[15:1],
       par.settings = list(axis.line = list(col = 'transparent')),
       strip = strip.custom(par.strip.text = list(cex = 0.95)),
       col = "black",
       at = seq(0, limit, length.out = 16),
       lwd = 0.025,
       layout = c(NSel, NMods))

# Cartography of the Region of Valencia
load(file.path("data", "CartoCV.Rdata"))

NMods <- 3
mod_labels <- c("Indep", "Corr", "CorrIRE")
thetasim <- array(dim = c(n.sims, NMuni, NVars, NMods))
thetasim[, , , 1] <- salwinbugs1$sims.list$theta
thetasim[, , , 2] <- salwinbugs2$sims.list$theta
thetasim[, , , 3] <- salwinbugs3$sims.list$theta

# Checking when theta is LESS than zero
stepsim <- array(dim = c(n.sims, NMuni, NVars, NMods))
for (Mod in 1:NMods) {
  for (Var in 1:NVars) {
    for (sim in 1:n.sims) {
      for (Muni in 1:NMuni) {
        stepsim[sim, Muni, Var, Mod] <- ifelse(thetasim[sim, Muni, Var, Mod] < 0, 1, 0)
      }
    }
  }
}

selection <- 1:NVars
NSel <- length(selection)
stepsim <- stepsim[, , selection, ]

mod_labels <- c("Indep", "Corr", "CorrIRE")
for (Mod in 1:NMods) {
  for (Sel in 1:NSel) {
    carto_muni@data[[paste0(colnames(y)[selection[Sel]], "probmean", mod_labels[Mod])]] <- apply(stepsim[, , Sel, Mod], 2, mean)
  }
}

kk <- unlist(carto_muni@data[, startsWith(colnames(carto_muni@data), "P8_")])

limit <- max(kk) + 0.01

mod_labels <- c("CorrIRE", "Corr", "Indep")
all_labels <- matrix(nrow = NSel, ncol = NMods)
for (Sel in 1:NSel) {
  for (Mod in 1:NMods) {
    all_labels[Sel, Mod] <- paste0(labels[selection[Sel]], ".", mod_labels[Mod])
  }
}
all_labels <- as.character(all_labels)

spplot(carto_muni,
       c(colnames(carto_muni@data[, endsWith(colnames(carto_muni@data), "CorrIRE")]),
         colnames(carto_muni@data[, endsWith(colnames(carto_muni@data), "Corr")]),
         colnames(carto_muni@data[, endsWith(colnames(carto_muni@data), "Indep")])),
       names.attr = all_labels,
       col.regions = colorRampPalette(brewer.pal(7,'RdYlGn'))(15)[15:1],
       par.settings = list(axis.line = list(col = 'transparent')),
       strip = strip.custom(par.strip.text = list(cex = 0.95)),
       col = "black",
       at = seq(0, limit, length.out = 16),
       lwd = 0.025,
       layout = c(NSel, NMods))

#### Maps of the RV - PCA ####

salwinbugs <- salwinbugs3
thetasim <- salwinbugs$sims.list$theta
thetamean <- apply(thetasim, 2:3, mean)
acp <- princomp(thetamean, cor = TRUE)
# summary(acp)
acp$loadings[, 1:3]
# plot(acp, type = "l")
thetaComp <- acp$scores[, 1:3]
cor(thetaComp)

carto_muni@data$ACP1 <- thetaComp[, 1]

# Fiveteen equal-probability intervals
breaks <- c(min(carto_muni@data$ACP1) - 0.001, quantile(carto_muni@data$ACP1, probs = seq(1/15, 14/15, length.out = 14)), max(carto_muni@data$ACP1))
breaks <- c(-17.50, -3.00, -2.50, -2.00, -1.50, -1.00, -0.50, -0.25, 
            0.25, 0.50, 1.00, 1.50, 2.00, 2.50, 3.00, 17.50)

carto_muni@data$ACP1 <- cut(carto_muni@data$ACP1, breaks = breaks, include.lowest = FALSE, right = TRUE)
levels(carto_muni@data$ACP1) <- c("Worse S1", "Better S2, ", "  ", "   ", "    ", 
                                  "     ", "      ", "       ", "        ",
                                  "         ", "          ", "           ",
                                  "               ", "Worse S2", "Better S1, ")

carto_muni@data$ACP2 <- thetaComp[, 2]

# Fiveteen equal-probability intervals
breaks <- c(min(carto_muni@data$ACP2) - 0.001, quantile(carto_muni@data$ACP2, probs = seq(1/15, 14/15, length.out = 14)), max(carto_muni@data$ACP2))

carto_muni@data$ACP2 <- cut(carto_muni@data$ACP2, breaks = breaks, include.lowest = FALSE, right = TRUE)
levels(carto_muni@data$ACP2) <- c("Worse MH", " ", "  ", "   ", "    ", 
                                  "     ", "      ", "       ", "        ",
                                  "         ", "          ", "           ",
                                  "               ", "                ", "Better MH")
rm(list = c("thetasim", "stepsim"))

grid.arrange(spplot(carto_muni,
                    c("ACP1"),
                    main = expression("(A)                                                                          "),
                    col.regions = colorRampPalette(brewer.pal(7,'RdYlBu'))(15),
                    cuts = 14,
                    par.settings = list(axis.line = list(col = 'transparent')),
                    strip = strip.custom(par.strip.text = list(cex = 0.95)),
                    col = "black",
                    lwd = 0.025),
             spplot(carto_muni,
                    c("ACP2"),
                    main = expression("(B)                                                                          "),
                    col.regions = colorRampPalette(brewer.pal(7,'Blues'))(15),
                    cuts = 14,
                    par.settings = list(axis.line = list(col = 'transparent')),
                    strip = strip.custom(par.strip.text = list(cex = 0.95)),
                    col = "black",
                    lwd = 0.025),
             ncol = 2)

#### Some comparisons ####

Var <- 5
MuniLevels <- carto_muni@data$NOMBRE_MUNI
target <- 1:100

ComparativeMuni <- data.frame("IndepMean" = apply(salwinbugs1$sims.list$theta[, , Var], 2, mean)[target], 
                              "IndepPIlower" = apply(salwinbugs1$sims.list$theta[, , Var], 2, quantile, prob = 0.025)[target],
                              "IndepPIupper" = apply(salwinbugs1$sims.list$theta[, , Var], 2, quantile, prob = 0.975)[target],
                              "CorrMean" = apply(salwinbugs2$sims.list$theta[, , Var], 2, mean)[target], 
                              "CorrPIlower" = apply(salwinbugs2$sims.list$theta[, , Var], 2, quantile, prob = 0.025)[target],
                              "CorrPIupper" = apply(salwinbugs2$sims.list$theta[, , Var], 2, quantile, prob = 0.975)[target],
                              "CorrIREMean" = apply(salwinbugs3$sims.list$theta[, , Var], 2, mean)[target], 
                              "CorrIREPIlower" = apply(salwinbugs3$sims.list$theta[, , Var], 2, quantile, prob = 0.025)[target],
                              "CorrIREPIupper" = apply(salwinbugs3$sims.list$theta[, , Var], 2, quantile, prob = 0.975)[target],
                              "Muni" = target)

apply(ComparativeMuni[, 1:9], 2, min)

comp_plot_muni <- ggplot() + 
  ylab("Municipality") + xlab("Spatial effect") + theme_bw() + coord_flip() + 
  geom_point(data = ComparativeMuni, aes(y = Muni, x = IndepMean, colour = "Indep"), size = 1.5) +
  geom_point(data = ComparativeMuni, aes(y = Muni, x = CorrMean, colour = "Corr"), size = 1.5) +
  geom_point(data = ComparativeMuni, aes(y = Muni, x = CorrIREMean, colour = "CorrIRE"), size = 1.5) +
  geom_errorbar(data = ComparativeMuni, aes(y = Muni,
                                            xmin = IndepPIlower,
                                            xmax = IndepPIupper), width = 0, color = 2) +
  geom_errorbar(data = ComparativeMuni, aes(y = Muni,
                                            xmin = CorrPIlower,
                                            xmax = CorrPIupper), width = 0, color = 3) +
  geom_errorbar(data = ComparativeMuni, aes(y = Muni,
                                            xmin = CorrIREPIlower,
                                            xmax = CorrIREPIupper), width = 0, color = 4) + 
  scale_x_continuous(limits = c(-2.75, 2.75), breaks = seq(-3, 3, by = 0.5)) + 
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, vjust = 0.3)) + ggtitle("Comparative")
comp_plot_muni

#### Model assessment for Model-Indep ####

# SurveyMapping.prlevels: 
# - computes the n.sims simulated probabilities for each respondent

SurveyMapping.prlevels <- function(salwinbugs) {
  
  n.sims <- salwinbugs$n.sims
  n.chains <- salwinbugs$n.chains
  p.gamma <- array(dim = c(n.sims, NResp, NVars, NCats - 1))
  prlevels <- array(dim = c(n.sims, NResp, NVars, NCats))
  for (sim in 1:n.sims) {
    for (Resp in 1:NResp) {
      for (Var in 1:NVars) {
        for (Cat in 1:(NCats - 1)) {
          p.gamma[sim, Resp, Var, Cat] <- 
            ilogit(salwinbugs$sims.list$kappa[sim, sex[Resp], age[Resp], Cat, Var] + 
                     salwinbugs$sims.list$sd.theta[sim, Var] * salwinbugs$sims.list$theta[sim, muni[Resp], Var])
        }
        
        prlevels[sim, Resp, Var, 1] <- p.gamma[sim, Resp, Var, 1]
        prlevels[sim, Resp, Var, NCats] <- 1 - p.gamma[sim, Resp, Var, NCats - 1]
        
        for (Cat in 2:(NCats - 1)) {
          prlevels[sim, Resp, Var, Cat] <- 
            p.gamma[sim, Resp, Var, Cat] - p.gamma[sim, Resp, Var, Cat-1]
        }
      }
    }
    
    if (sim %in% c(1, seq(n.sims/n.chains, n.sims, n.sims/n.chains))) {
      cat(sim, "of", n.sims, "simulations", "\n")
    } else {}
  }
  
  return(prlevels)
}

# This may take a minute
prlevels <- SurveyMapping.prlevels(salwinbugs = salwinbugs1)

#### Model assessment for Model-Corr ####

# SurveyMapping.prlevels: 
# - computes the n.sims simulated probabilities for each respondent

SurveyMapping.prlevels <- function(salwinbugs) {
  
  n.sims <- salwinbugs$n.sims
  n.chains <- salwinbugs$n.chains
  p.gamma <- array(dim = c(n.sims, NResp, NVars, NCats - 1))
  prlevels <- array(dim = c(n.sims, NResp, NVars, NCats))
  for (sim in 1:n.sims) {
    for (Resp in 1:NResp) {
      for (Var in 1:NVars) {
        for (Cat in 1:(NCats - 1)) {
          p.gamma[sim, Resp, Var, Cat] <- 
            ilogit(salwinbugs$sims.list$kappa[sim, sex[Resp], age[Resp], Cat, Var] + 
                     salwinbugs$sims.list$theta[sim, muni[Resp], Var])
        }
        
        prlevels[sim, Resp, Var, 1] <- p.gamma[sim, Resp, Var, 1]
        prlevels[sim, Resp, Var, NCats] <- 1 - p.gamma[sim, Resp, Var, NCats - 1]
        
        for (Cat in 2:(NCats - 1)) {
          prlevels[sim, Resp, Var, Cat] <- 
            p.gamma[sim, Resp, Var, Cat] - p.gamma[sim, Resp, Var, Cat-1]
        }
      }
    }
    
    if (sim %in% c(1, seq(n.sims/n.chains, n.sims, n.sims/n.chains))) {
      cat(sim, "of", n.sims, "simulations", "\n")
    } else {}
  }
  
  return(prlevels)
}

# This may take a minute
prlevels <- SurveyMapping.prlevels(salwinbugs = salwinbugs2)

#### Model assessment for Model-Corr&IRE ####

# SurveyMapping.prlevels: 
# - computes the n.sims simulated probabilities for each respondent

SurveyMapping.prlevels <- function(salwinbugs) {
  
  n.sims <- salwinbugs$n.sims
  n.chains <- salwinbugs$n.chains
  p.gamma <- array(dim = c(n.sims, NResp, NVars, NCats - 1))
  prlevels <- array(dim = c(n.sims, NResp, NVars, NCats))
  for (sim in 1:n.sims) {
    for (Resp in 1:NResp) {
      for (Var in 1:NVars) {
        for (Cat in 1:(NCats - 1)) {
          p.gamma[sim, Resp, Var, Cat] <- 
            ilogit(salwinbugs$sims.list$kappa[sim, sex[Resp], age[Resp], Cat, Var] + 
                     salwinbugs$sims.list$theta[sim, muni[Resp], Var] + 
                     salwinbugs$sims.list$psi[sim, Resp, Var])
        }
        
        prlevels[sim, Resp, Var, 1] <- p.gamma[sim, Resp, Var, 1]
        prlevels[sim, Resp, Var, NCats] <- 1 - p.gamma[sim, Resp, Var, NCats - 1]
        
        for (Cat in 2:(NCats - 1)) {
          prlevels[sim, Resp, Var, Cat] <- 
            p.gamma[sim, Resp, Var, Cat] - p.gamma[sim, Resp, Var, Cat-1]
        }
      }
    }
    
    if (sim %in% c(1, seq(n.sims/n.chains, n.sims, n.sims/n.chains))) {
      cat(sim, "of", n.sims, "simulations", "\n")
    } else {}
  }
  
  return(prlevels)
}

# This may take a minute
prlevels <- SurveyMapping.prlevels(salwinbugs = salwinbugs3)

### Validation ###

# Sample size of each municipality, sex and age group:
sample <- array(dim = c(NMuni, NSex, NAges))
for (Muni in 1:NMuni) {
  for (SexGroup in 1:NSex) {
    for (AgeGroup in 1:NAges) {
      sample[Muni, SexGroup, AgeGroup] <- sum(muni == Muni & sex == SexGroup & age == AgeGroup)
    }
  }
}

SurveyMapping.Validation <- function(prlevels, Muni) {
  
  NSamp <- sum(sample[Muni, , ])
  index <- which(muni == Muni)
  realvalue <- matrix(nrow = NVars, ncol = NCats)
  predictive <- array(dim = c(n.sims, NSamp, NVars))
  predictive.muni <- array(dim = c(n.sims, NVars, NCats))
  for (sim in 1:n.sims) {
    for (Var in 1:NVars) {
      for (Resp in 1:NSamp) {
        predictive[sim, Resp, Var] <- which(
          rmultinom(n = 1,
                    size = 1, 
                    prob = prlevels[sim, index[Resp], Var, ]) == 1)
      }
      predictive.muni[sim, Var, ] <- table(factor(predictive[sim, , Var], levels = 1:NCats))/NSamp * 100
    }
    
    if (sim %in% c(1, seq(n.sims/n.chains, n.sims, n.sims/n.chains))) {
      cat(sim, "of", n.sims, "simulations", "\n")
    } else {}
  }
  
  posteriormean <- round(apply(predictive.muni, 2:3, mean), 2)
  PInterval0.025 <- round(apply(predictive.muni, 2:3, quantile, prob = 0.025), 2)
  PInterval0.975 <- round(apply(predictive.muni, 2:3, quantile, prob = 0.975), 2)
  for (Var in 1:NVars) {
    realvalue[Var, ] <- round(table(factor(y[muni == Muni, Var], levels = 1:NCats))/NSamp * 100, 2)
  }
  return(list("mean" = posteriormean,
              "PI" = list("lower" = PInterval0.025, "upper" = PInterval0.975),
              "real" = realvalue))
  return(predictive.muni)
}

validation <- SurveyMapping.Validation(prlevels = prlevels, Muni = 526)
validation$mean
validation$PI
validation$real

# Four municipalities with the largest population in the RV
Munis <- order(apply(sample, 1, sum), decreasing = TRUE)[1:6]

validation <- list()
for (Muni in 1:length(Munis)) {
  set.seed(9747783)
  validation[[Muni]] <- SurveyMapping.Validation(prlevels = prlevels, Muni = Munis[Muni])
}

# saveRDS(validation, file = file.path("results", "multi-2022-nimble-MH-corr-ire-assessment.rds"))

#### WAIC for Model-Indep ####

# Let’s create the Nimble model, creates the nodes
modelWAIC <- nimbleModel(code = modelCode, 
                         constants = modelConstants,
                         data = modelData, 
                         inits = list(delta = array(rdirichlet(NSex * NAges * NVars, ones),
                                                    dim = c(NSex, NAges, NVars, NCats)),
                                      rho = runif(NVars),
                                      theta = matrix(rnorm(NMuni * NVars, sd = 0.1), nrow = NMuni, ncol = NVars),
                                      sd.theta = runif(NVars)), 
                         calculate = FALSE)
CmodelWAIC <- compileNimble(modelWAIC)         # calculateWAIC needs compiled model to exist
samples <- do.call(rbind, salnimble1)           # single matrix of samples
waic <- calculateWAIC(samples, modelWAIC)

# nimbleList object of type waicNimbleList
# Field "WAIC":
#   [1] 192525.4
# Field "lppd":
#   [1] -94783.11
# Field "pWAIC":
#   [1] 1479.595

#### WAIC for Model-Corr ####

# Let’s create the Nimble model, creates the nodes
modelWAIC <- nimbleModel(code = modelCode, 
                         constants = modelConstants,
                         data = modelData, 
                         inits = list(delta = array(rdirichlet(NSex * NAges * NVars, ones),
                                                    dim = c(NSex, NAges, NVars, NCats)),
                                      rho = runif(NVars),
                                      sub.Muni = matrix(rnorm(NMuni * NVars, sd = 0.01), nrow = NMuni, ncol = NVars),
                                      M.Muni = matrix(rnorm(NVars * NVars, sd = 0.5), ncol = NVars, nrow = NVars),
                                      sd.M.Muni = runif(1, min = 0.2, max = 0.8)), 
                         calculate = FALSE)
CmodelWAIC <- compileNimble(modelWAIC)         # calculateWAIC needs compiled model to exist
samples <- do.call(rbind, salnimble2)           # single matrix of samples
waic <- calculateWAIC(samples, modelWAIC)

# nimbleList object of type waicNimbleList
# Field "WAIC":
#   [1] 190447.6
# Field "lppd":
#   [1] -93940.28
# Field "pWAIC":
#   [1] 1283.548

#### WAIC for Model-Corr&IRE ####

# Let’s create the Nimble model, creates the nodes
modelWAIC <- nimbleModel(code = modelCode, 
                         constants = modelConstants,
                         data = modelData, 
                         inits = list(delta = array(rdirichlet(NSex * NAges * NVars, ones),
                                                    dim = c(NSex, NAges, NVars, NCats)),
                                      rho = runif(NVars),
                                      sub.Resp = matrix(rnorm(NResp * NVars, sd = 0.01), nrow = NResp, ncol = NVars),
                                      M.Resp = matrix(rnorm(NVars * NVars, sd = 0.5), ncol = NVars, nrow = NVars),
                                      sd.M.Resp = runif(1, min = 0.2, max = 0.8),
                                      sub.Muni = matrix(rnorm(NMuni * NVars, sd = 0.01), nrow = NMuni, ncol = NVars),
                                      M.Muni = matrix(rnorm(NVars * NVars, sd = 0.5), ncol = NVars, nrow = NVars),
                                      sd.M.Muni = runif(1, min = 0.2, max = 0.8)), 
                         calculate = FALSE)
CmodelWAIC <- compileNimble(modelWAIC)         # calculateWAIC needs compiled model to exist
samples <- do.call(rbind, salnimble3)           # single matrix of samples
waic <- calculateWAIC(samples, modelWAIC)

# nimbleList object of type waicNimbleList
# Field "WAIC":
#   [1] 103170.1
# Field "lppd":
#   [1] -36079.51
# Field "pWAIC":
#   [1] 15505.53
