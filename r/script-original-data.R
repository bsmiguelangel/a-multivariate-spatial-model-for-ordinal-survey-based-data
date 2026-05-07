#### Required packages ####

## install.packages("pacman")

pacman::p_load(foreign, readxl, faraway, spdep, sp, ggplot2, RColorBrewer, 
               graphics, ggpubr, leaflet, nimble, ggmcmc, extraDistr, 
               parallel, MCMCvis, gridExtra, corrplot, ggcorrplot, readr, 
               lattice, sf, patchwork, install = FALSE)

#### Health survey data loading: GHQ-12 items ####

HSRV2022 <- read.spss(file.path("data", "ESCV2022_UV_Matem.sav"), 
                      use.value.labels = TRUE, to.data.frame = TRUE)
# Sample size
NResp <- nrow(HSRV2022)

### GHQ-12 mental health items recoding ###

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
# Vector of ones
ones <- rep(1, NCats)

rm(list = c("P8_1", "P8_2", "P8_3", "P8_4", "P8_5", "P8_6", 
            "P8_7", "P8_8", "P8_9", "P8_10", "P8_11", "P8_12"))

# Non-response percentages
apply(y, 2, function(x) {sum(is.na(x))})/NResp * 100

### Covariates ###

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

#### Spatial neighbourhood structure ####

# Cartography of the Region of Valencia
load(file.path("data", "CartoCV.Rdata"))
# Cartography is sorted by municipality code
order(carto_muni$INE_MUN)-1:542
# Neighbourhood structure by contiguity
cv.nb <- poly2nb(carto_muni)

# Some extra neighbourhoods are added for Rincón de Ademuz comarca
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

# Number of neighbours of each municipality
nadj <- card(cv.nb)
# Neighbours of each municipality
map <- unlist(cv.nb)
# Sum of all the neighbour numbers of all municipalities
nadj.tot <- length(map)
# Cumulative sums of the number of neighbours of each municipality
index <- c(0, cumsum(nadj))

### Leroux CAR distribution in NIMBLE ###

# Diagonal matrix with the number of neighbours of each area
D <- diag(nadj)
# Adjacency matrix
W <- nb2mat(cv.nb, style = "B", zero.policy = TRUE)
# Eigenvalues of D-W
Lambda <- eigen(D - W)$values
# Identity matrix
I <- diag(rep(1, NMuni))

# All the neighbourhoods j ~ i where i < j
from.to <- cbind(rep(1:NMuni, times = nadj), map); colnames(from.to) <- c("from", "to")
from.to <- from.to[which(from.to[, 1] < from.to[, 2]), ]
NDist <- nrow(from.to)

dcar_leroux <- nimbleFunction(
  name = 'dcar_leroux',
  run = function(x = double(1),        # Spatial random effect (vector)
                 rho = double(0),      # Amount of spatial dependence (scalar)
                 sd.theta = double(0), # Standard deviation (scalar)
                 Lambda = double(1),   # Eigenvalues of matrix D - W
                 from.to = double(2),  # Matrix of distinct pairs of neighbours from.to[, 1] < from.to[, 2]
                 log = integer(0, default = 0)) {
    returnType(double(0))
    
    # Number of small areas
    NMuni <- dim(x)[1]
    # Number of distinct pairs of neighbours
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

#### Independent Model: Model-Indep ####

n.chains <- 5
this_cluster <- makeCluster(n.chains)

### Model code ###

source(file = file.path("models", "model-indep.R"))

### Model data ###

modelData <- list(y = as.matrix(y), zero.theta.resp = rep(0, NVars))

modelConstants <- list(NResp = NResp, NCats = NCats, NVars = NVars, 
                       sex = sex, age = age, muni = muni, NSex = NSex, 
                       NAges = NAges, NMuni = NMuni, ones = ones, 
                       NDist = NDist, Lambda = Lambda, from.to = from.to)

### Parameters to monitor ###

modelParameters <- c("kappa", "theta", "sd.theta", "rho", "delta")

# Create a function to run the MCMC
run_MCMC_allcode <- function(X, code, constants, data, monitors) {
  
  pacman::p_load(nimble, extraDistr, install = FALSE)
  
  dcar_leroux <- nimbleFunction(
    name = 'dcar_leroux',
    run = function(x = double(1),        # Spatial random effect (vector)
                   rho = double(0),      # Amount of spatial dependence (scalar)
                   sd.theta = double(0), # Standard deviation (scalar)
                   Lambda = double(1),   # Eigenvalues of matrix D - W
                   from.to = double(2),  # Matrix of distinct pairs of neighbours from.to[, 1] < from.to[, 2]
                   log = integer(0, default = 0)) {
      returnType(double(0))
      
      # Number of small areas
      NMuni <- dim(x)[1]
      # Number of distinct pairs of neighbours
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
  
  # Create the NIMBLE model and initialize its nodes
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
  
  # Remove desired samplers
  modelMCMCconfiguration$removeSamplers(c("theta", "rho", "sd.theta"))
  
  # Add RW-MH theta[1:NMuni, 1:NVars] samplers
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
             modelMCMCconfiguration$addSampler(target = thetas[Muni, Var], type = "RW"), # also slice
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
  
  # Add slice sd.theta sampler
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
  CmodelMCMC <- compileNimble(modelMCMC, project = model, resetFunctions = TRUE)
  # Results
  results <- runMCMC(CmodelMCMC, niter = 8000, nburnin = 2000, thin = 30, setSeed = X)
  
  return(results)
}

# system.time(salnimble1 <- parLapply(cl = this_cluster, X = 1:n.chains, 
#                                     fun = run_MCMC_allcode, 
#                                     code = modelCode,
#                                     constants = modelConstants,
#                                     data = modelData,
#                                     monitors = modelParameters))
# 
# # Close the cluster after the parallel computation
# stopCluster(this_cluster)

# 1.84h with: niter = 8000, nburnin = 2000, thin = 30
# saveRDS(salnimble1, file = file.path("results", "multi-2022-nimble-MH-indep-8k-2k-30-WAIC.rds"))

#### Correlated Model: Model-Corr ####

n.chains <- 5
this_cluster <- makeCluster(n.chains)

### Model code ###

source(file = file.path("models", "model-corr.R"))

### Model data ###

modelData <- list(y = as.matrix(y), zero.sub.Muni.resp = rep(0, NVars))

modelConstants <- list(NResp = NResp, NCats = NCats, NVars = NVars, 
                       sex = sex, age = age, muni = muni, NSex = NSex, 
                       NAges = NAges, NMuni = NMuni, ones = ones, 
                       NDist = NDist, Lambda = Lambda, from.to = from.to)

### Parameters to monitor ###

modelParameters <- c("kappa", "theta", "M.Muni", "sd.M.Muni", "rho", 
                     "delta", "sub.Muni")

# Create a function to run the MCMC
run_MCMC_allcode <- function(X, code, constants, data, monitors) {
  
  pacman::p_load(nimble, extraDistr, install = FALSE)
  
  dcar_leroux <- nimbleFunction(
    name = 'dcar_leroux',
    run = function(x = double(1),        # Spatial random effect (vector)
                   rho = double(0),      # Amount of spatial dependence (scalar)
                   sd.theta = double(0), # Standard deviation (scalar)
                   Lambda = double(1),   # Eigenvalues of matrix D - W
                   from.to = double(2),  # Matrix of distinct pairs of neighbours from.to[, 1] < from.to[, 2]
                   log = integer(0, default = 0)) {
      returnType(double(0))
      
      # Number of small areas
      NMuni <- dim(x)[1]
      # Number of distinct pairs of neighbours
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
  
  # Create the NIMBLE model and initialize its nodes
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
  
  # Remove desired samplers
  modelMCMCconfiguration$removeSamplers(c("sub.Muni", "rho", "sd.M.Muni"))
  
  # Add RW-MH sub.Muni[1:NMuni, 1:NVars] samplers
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
             modelMCMCconfiguration$addSampler(target = sub.Munis[Muni, Var], type = "RW"), # also slice
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
  CmodelMCMC <- compileNimble(modelMCMC, project = model, resetFunctions = TRUE)
  # Results
  results <- runMCMC(CmodelMCMC, niter = 8000, nburnin = 2000, thin = 30, setSeed = X)
  
  return(results)
}

# system.time(salnimble2 <- parLapply(cl = this_cluster, X = 1:n.chains, 
#                                     fun = run_MCMC_allcode, 
#                                     code = modelCode,
#                                     constants = modelConstants,
#                                     data = modelData,
#                                     monitors = modelParameters))
# 
# # Close the cluster after the parallel computation
# stopCluster(this_cluster)

# 6.18h with: niter = 8000, nburnin = 2000, thin = 30
# saveRDS(salnimble2, file = file.path("results", "multi-2022-nimble-MH-corr-8k-2k-30-WAIC.rds"))

#### Correlated with IREs Model: Model-Corr&IRE ####

n.chains <- 5
this_cluster <- makeCluster(n.chains)

### Model code ###

source(file = file.path("models", "model-corr&ire.R"))

### Model data ###

modelData <- list(y = as.matrix(y), zero.sub.Muni.resp = rep(0, NVars))

modelConstants <- list(NResp = NResp, NCats = NCats, NVars = NVars, sex = sex, 
                       age = age, muni = muni, NSex = NSex, NAges = NAges, 
                       NMuni = NMuni, ones = ones, NDist = NDist, Lambda = Lambda, 
                       from.to = from.to)

### Parameters to monitor ###

modelParameters <- c("kappa", "theta", "M.Muni", "rho",
                     "sd.M.Muni", "psi", "M.Resp", "sd.M.Resp", 
                     "delta", "sub.Muni", "sub.Resp")

# Create a function to run the MCMC
run_MCMC_allcode <- function(X, code, constants, data, monitors) {
  
  pacman::p_load(nimble, extraDistr, install = FALSE)
  
  dcar_leroux <- nimbleFunction(
    name = 'dcar_leroux',
    run = function(x = double(1),        # Spatial random effect (vector)
                   rho = double(0),      # Amount of spatial dependence (scalar)
                   sd.theta = double(0), # Standard deviation (scalar)
                   Lambda = double(1),   # Eigenvalues of matrix D - W
                   from.to = double(2),  # Matrix of distinct pairs of neighbours from.to[, 1] < from.to[, 2]
                   log = integer(0, default = 0)) {
      returnType(double(0))
      
      # Number of small areas
      NMuni <- dim(x)[1]
      # Number of distinct pairs of neighbours
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
  
  # Create the NIMBLE model and initialize its nodes
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
  
  # Remove desired samplers
  modelMCMCconfiguration$removeSamplers(c("sub.Muni", "rho", "sd.M.Muni",
                                          "sd.M.Resp"))
  
  # Add RW-MH sub.Muni[1:NMuni, 1:NVars] samplers
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
             modelMCMCconfiguration$addSampler(target = sub.Munis[Muni, Var], type = "RW"), # also slice
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

# system.time(salnimble3 <- parLapply(cl = this_cluster, X = 1:n.chains, 
#                                     fun = run_MCMC_allcode, 
#                                     code = modelCode,
#                                     constants = modelConstants,
#                                     data = modelData,
#                                     monitors = modelParameters))
# 
# # Close the cluster after the parallel computation
# stopCluster(this_cluster)

# 14.42h with: niter = 8000, nburnin = 2000, thin = 30
# saveRDS(salnimble3, file = file.path("results", "multi-2022-nimble-MH-corr-ire-8k-2k-30-WAIC.rds"))

#### Loading posterior samples ####

n.chains <- 5
n.sims <- 200 * n.chains
labels <- c("Item 1", "Item 2", "Item 3", "Item 4", "Item 5", "Item 6", "Item 7", "Item 8", "Item 9", "Item 10", "Item 11", "Item 12")
salnimble1 <- readRDS(file = file.path("results", "multi-2022-nimble-MH-indep-8k-2k-30-WAIC.rds"))
salnimble2 <- readRDS(file = file.path("results", "multi-2022-nimble-MH-corr-8k-2k-30-WAIC.rds"))
salnimble3 <- readRDS(file = file.path("results", "multi-2022-nimble-MH-corr-ire-8k-2k-30-WAIC.rds"))

#### Convert NIMBLE output to WinBUGS-style format: Model-Indep ####

NimToWin <- function(salnimble) {
  
  n.chains <- length(salnimble)
  n.sims <- n.chains * nrow(salnimble[[1]])
  
  kappa <- array(dim = c(n.sims, NSex, NAges, NCats - 1, NVars))
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
  sims.list <- list("kappa" = kappa, "theta" = theta, "sd.theta" = sd.theta, 
                    "rho" = rho)
  
  salwinbugs <- list("summary" = summary, "sims.list" = sims.list,
                     "n.chains" = n.chains, "n.sims" = n.sims)
  
  return(salwinbugs)
}

salwinbugs1 <- NimToWin(salnimble = salnimble1)

#### Convert NIMBLE output to WinBUGS-style format: Model-Corr ####

NimToWin <- function(salnimble) {
  
  n.chains <- length(salnimble)
  n.sims <- n.chains * nrow(salnimble[[1]])
  
  kappa <- array(dim = c(n.sims, NSex, NAges, NCats - 1, NVars))
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

#### Convert NIMBLE output to WinBUGS-style format: Model-Corr&IRE ####

NimToWin <- function(salnimble) {
  
  n.chains <- length(salnimble)
  n.sims <- n.chains * nrow(salnimble[[1]])
  
  kappa <- array(dim = c(n.sims, NSex, NAges, NCats - 1, NVars))
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

#### Posterior means of the spatial effects: Figures 1 and S13 ####

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

# Fifteen equal-probability intervals
breaks <- c(min(apply(thetasim, 2:4, mean)) - 0.001, quantile(apply(thetasim, 2:4, mean), probs = seq(1/15, 14/15, length.out = 14)), max(apply(thetasim, 2:4, mean)))
breaks <- c(-2.20, -0.5, -0.35, -0.25, -0.20, -0.15, -0.10, -0.05, 
            0.05, 0.10, 0.15, 0.20, 0.25, 0.35, 0.5, 2.20)
break_labels <- c("\u2264 -0.50", "(-0.50, -0.35]", "(-0.35, -0.25]", 
                  "(-0.25, -0.20]", "(-0.20, -0.15]", "(-0.15, -0.10]", 
                  "(-0.10, -0.05]", "(-0.05, 0.05]", "(0.05, 0.10]", 
                  "(0.10, 0.15]", "(0.15, 0.20]", "(0.20, 0.25]",
                  "(0.25, 0.35]", "(0.35, 0.50]", "> 0.50")

carto_muni_sf <- st_as_sf(carto_muni)
mod_labels <- c("Model-Indep", "Model-Corr", "Model-Corr&IRE")

res_list <- list()
for (Sel in 1:NSel) {
  for (Mod in 1:NMods) {
    tmp <- carto_muni_sf
    tmp$thetamean <- apply(thetasim[, , Sel, Mod], 2, mean)
    tmp$thetacat <- cut(tmp$thetamean, breaks = breaks, include.lowest = FALSE, right = TRUE, labels = break_labels)
    tmp$Item <- paste("Item", Sel)
    tmp$Model <- mod_labels[Mod]
    res_list[[length(res_list) + 1]] <- tmp
  }
}

plot_sf <- do.call(rbind, res_list)
plot_sf$Model <- factor(plot_sf$Model, levels = c("Model-Indep", "Model-Corr", "Model-Corr&IRE"))
plot_sf$Item <- factor(plot_sf$Item, levels = paste("Item", 1:NSel))
plot_sf$thetacat <- factor(plot_sf$thetacat, levels = break_labels)

ggplot(plot_sf) +
  geom_sf(aes(fill = thetacat), colour = "black", linewidth = 0.01) +
  facet_grid(Model ~ Item, switch = "y") +
  scale_fill_manual(values = colorRampPalette(brewer.pal(7, "BrBG"))(15), 
                    drop = FALSE, name = NULL, guide = guide_legend(reverse = TRUE)) +
  theme_void() +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y.left = element_text(size = 12, face = "bold", angle = 90, 
                                         margin = margin(r = 8, l = 8)),
        strip.placement = "outside", panel.spacing = unit(0.05, "lines"),
        legend.text = element_text(size = 9), legend.position = "right",
        legend.key.height = unit(0.9, "cm"), legend.key.width  = unit(0.7, "cm"),
        legend.spacing.y  = unit(0.08, "cm"))

# ggsave(file.path("figures", "BeltranSanchez1.png"), device = "png",
#        width = 10, height = 7, units = "in", dpi = 600)
# 
# ggsave(file.path("figures", "BeltranSanchez1.tiff"), device = "tiff",
#        width = 10, height = 7, units = "in", dpi = 600, compression = "lzw")
# 
# ggsave(file.path("figures", "BeltranSanchez1.eps"), device = cairo_ps,
#        width = 10, height = 7, units = "in", fallback_resolution = 600)

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

# Fifteen equal-probability intervals
breaks <- c(min(apply(thetasim, 2:4, mean)) - 0.001, quantile(apply(thetasim, 2:4, mean), probs = seq(1/15, 14/15, length.out = 14)), max(apply(thetasim, 2:4, mean)))
breaks <- c(-3.75, -0.75, -0.25, -0.20, -0.15, -0.10, -0.05, -0.02, 
            0.02, 0.05, 0.10, 0.15, 0.20, 0.25, 0.75, 3.75)
break_labels <- c("\u2264 -0.75", "(-0.75, -0.25]", "(-0.25, -0.20]", 
                  "(-0.20, -0.15]", "(-0.15, -0.10]", "(-0.10, -0.05]", 
                  "(-0.05, -0.02]", "(-0.02, 0.02]", "(0.02, 0.05]", 
                  "(0.05, 0.10]", "(0.10, 0.15]", "(0.15, 0.20]",
                  "(0.20, 0.25]", "(0.25, 0.75]", "> 0.75")

carto_muni_sf <- st_as_sf(carto_muni)
mod_labels <- c("Model-Indep", "Model-Corr", "Model-Corr&IRE")

res_list <- list()
for (Sel in 1:NSel) {
  for (Mod in 1:NMods) {
    tmp <- carto_muni_sf
    tmp$thetamean <- apply(thetasim[, , Sel, Mod], 2, mean)
    tmp$thetacat <- cut(tmp$thetamean, breaks = breaks, include.lowest = FALSE, right = TRUE, labels = break_labels)
    tmp$Item <- paste("Item", Sel)
    tmp$Model <- mod_labels[Mod]
    res_list[[length(res_list) + 1]] <- tmp
  }
}

plot_sf <- do.call(rbind, res_list)
plot_sf$Model <- factor(plot_sf$Model, levels = c("Model-Indep", "Model-Corr", "Model-Corr&IRE"))
plot_sf$Item <- factor(plot_sf$Item, levels = paste("Item", 1:NSel))
plot_sf$thetacat <- factor(plot_sf$thetacat, levels = break_labels)

ggplot(plot_sf) +
  geom_sf(aes(fill = thetacat), colour = "black", linewidth = 0.01) +
  facet_grid(Model ~ Item, switch = "y") +
  scale_fill_manual(values = colorRampPalette(brewer.pal(7, "BrBG"))(15), 
                    drop = FALSE, name = NULL, guide = guide_legend(reverse = TRUE)) +
  theme_void() +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y.left = element_text(size = 12, face = "bold", angle = 90, 
                                         margin = margin(r = 8, l = 8)),
        strip.placement = "outside", panel.spacing = unit(0.05, "lines"),
        legend.text = element_text(size = 9), legend.position = "right",
        legend.key.height = unit(0.9, "cm"), legend.key.width  = unit(0.7, "cm"),
        legend.spacing.y  = unit(0.08, "cm"))

# ggsave(file.path("figures", paste0("SupplementalMaterial", NVars + 1, ".png")),
#        device = "png", width = 16, height = 6, units = "in", dpi = 600)
# 
# ggsave(file.path("figures", paste0("SupplementalMaterial", NVars + 1, ".tiff")),
#        device = "tiff", width = 16, height = 6, units = "in", dpi = 600,
#        compression = "lzw")
# 
# ggsave(file.path("figures", paste0("SupplementalMaterial", NVars + 1, ".eps")),
#        device = cairo_ps, width = 16, height = 6, units = "in", 
#        fallback_resolution = 600)

#### Posterior probabilities of the spatial effects: Figures 2 and S14 ####

# Cartography of the Region of Valencia
load(file.path("data", "CartoCV.Rdata"))

NMods <- 3
mod_labels <- c("Indep", "Corr", "CorrIRE")
thetasim <- array(dim = c(n.sims, NMuni, NVars, NMods))
thetasim[, , , 1] <- salwinbugs1$sims.list$theta
thetasim[, , , 2] <- salwinbugs2$sims.list$theta
thetasim[, , , 3] <- salwinbugs3$sims.list$theta

# Checking when theta is LESS than zero
stepsim <- 1 * (thetasim < 0)

selection <- 1:4
NSel <- length(selection)
stepsim <- stepsim[, , selection, ]

carto_muni_sf <- st_as_sf(carto_muni)
mod_labels <- c("Model-Indep", "Model-Corr", "Model-Corr&IRE")

res_list <- list()
for (Sel in 1:NSel) {
  for (Mod in 1:NMods) {
    tmp <- carto_muni_sf
    tmp$probmean <- apply(stepsim[, , Sel, Mod], 2, mean)
    tmp$Item <- paste("Item", Sel)
    tmp$Model <- mod_labels[Mod]
    res_list[[length(res_list) + 1]] <- tmp
  }
}

plot_sf <- do.call(rbind, res_list)
plot_sf$Model <- factor(plot_sf$Model, levels = c("Model-Indep", "Model-Corr", "Model-Corr&IRE"))
plot_sf$Item <- factor(plot_sf$Item, levels = paste("Item", 1:NSel))

ggplot(plot_sf) +
  geom_sf(aes(fill = probmean), colour = "black", linewidth = 0.01) +
  facet_grid(Model ~ Item, switch = "y") +
  scale_fill_stepsn(colours = colorRampPalette(brewer.pal(7, "RdYlGn"))(15)[15:1], 
                    breaks = seq(0, 1, length.out = 16), limits = c(0, 1),
                    labels = c("0", "", "", "0.2", "", "", "0.4", "", 
                               "", "0.6", "", "", "0.8", "", "", "1"), 
                    guide = guide_colorsteps(title = NULL, even.steps = TRUE,
                                             show.limits = FALSE, ticks = TRUE,
                                             barheight = unit(14, "cm"),
                                             barwidth = unit(0.7, "cm"))) +
  theme_void() +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y.left = element_text(size = 12, face = "bold", angle = 90, 
                                         margin = margin(r = 8, l = 8)),
        strip.placement = "outside", panel.spacing = unit(0.05, "lines"),
        legend.text = element_text(size = 9), legend.position = "right")

# ggsave(file.path("figures", "BeltranSanchez2.png"), device = "png",
#        width = 10, height = 7, units = "in", dpi = 600)
# 
# ggsave(file.path("figures", "BeltranSanchez2.tiff"), device = "tiff",
#        width = 10, height = 7, units = "in", dpi = 600, compression = "lzw")
# 
# ggsave(file.path("figures", "BeltranSanchez2.eps"), device = cairo_ps,
#        width = 10, height = 7, units = "in", fallback_resolution = 600)

# Cartography of the Region of Valencia
load(file.path("data", "CartoCV.Rdata"))

NMods <- 3
mod_labels <- c("Indep", "Corr", "CorrIRE")
thetasim <- array(dim = c(n.sims, NMuni, NVars, NMods))
thetasim[, , , 1] <- salwinbugs1$sims.list$theta
thetasim[, , , 2] <- salwinbugs2$sims.list$theta
thetasim[, , , 3] <- salwinbugs3$sims.list$theta

# Checking when theta is LESS than zero
stepsim <- 1 * (thetasim < 0)

selection <- 1:NVars
NSel <- length(selection)
stepsim <- stepsim[, , selection, ]

carto_muni_sf <- st_as_sf(carto_muni)
mod_labels <- c("Model-Indep", "Model-Corr", "Model-Corr&IRE")

res_list <- list()
for (Sel in 1:NSel) {
  for (Mod in 1:NMods) {
    tmp <- carto_muni_sf
    tmp$probmean <- apply(stepsim[, , Sel, Mod], 2, mean)
    tmp$Item <- paste("Item", Sel)
    tmp$Model <- mod_labels[Mod]
    res_list[[length(res_list) + 1]] <- tmp
  }
}

plot_sf <- do.call(rbind, res_list)
plot_sf$Model <- factor(plot_sf$Model, levels = c("Model-Indep", "Model-Corr", "Model-Corr&IRE"))
plot_sf$Item <- factor(plot_sf$Item, levels = paste("Item", 1:NSel))

ggplot(plot_sf) +
  geom_sf(aes(fill = probmean), colour = "black", linewidth = 0.01) +
  facet_grid(Model ~ Item, switch = "y") +
  scale_fill_stepsn(colours = colorRampPalette(brewer.pal(7, "RdYlGn"))(15)[15:1], 
                    breaks = seq(0, 1, length.out = 16), limits = c(0, 1),
                    labels = c("0", "", "", "0.2", "", "", "0.4", "", 
                               "", "0.6", "", "", "0.8", "", "", "1"), 
                    guide = guide_colorsteps(title = NULL, even.steps = TRUE,
                                             show.limits = FALSE, ticks = TRUE,
                                             barheight = unit(14, "cm"),
                                             barwidth = unit(0.7, "cm"))) +
  theme_void() +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y.left = element_text(size = 12, face = "bold", angle = 90, 
                                         margin = margin(r = 8, l = 8)),
        strip.placement = "outside", panel.spacing = unit(0.05, "lines"),
        legend.text = element_text(size = 9), legend.position = "right")

# ggsave(file.path("figures", paste0("SupplementalMaterial", NVars + 2, ".png")),
#        device = "png", width = 16, height = 6, units = "in", dpi = 600)
# 
# ggsave(file.path("figures", paste0("SupplementalMaterial", NVars + 2, ".tiff")),
#        device = "tiff", width = 16, height = 6, units = "in", dpi = 600,
#        compression = "lzw")
# 
# ggsave(file.path("figures", paste0("SupplementalMaterial", NVars + 2, ".eps")),
#        device = cairo_ps, width = 16, height = 6, units = "in",
#        fallback_resolution = 600)

#### Municipality-level correlation matrix under Model-Corr&IRE: Figure 3 ####

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
Sigma.Munisim <- SurveyMapping.Sigma.Muni(salwinbugs = salwinbugs)
Corr <- array(dim = c(n.sims, NVars, NVars))

# Posterior sample of correlation matrices
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

Corr.mean <- data.frame(Corr.mean); rownames(Corr.mean) <- labels;
colnames(Corr.mean) <- labels

orden <- c(3, 4, 1, 8, 7, 12, 11, 9, 10, 2, 5, 6)

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

### Adding significances ###

# add on line 446 +0.35 y +0.15
# trace(corrplot, edit = TRUE)

Significance <- matrix(as.numeric(Corr.quantileL.orden > 0 | Corr.quantileU.orden < 0), ncol = NVars, nrow = NVars, byrow = FALSE)
colnames(Significance) <- rownames(Significance) <- colnames(Corr.mean.orden)
Significance <- (Significance - 1) * (-1)
for (Var in 1:NVars) { Significance[Var, Var] <- 1 }

# png(file.path("figures", "BeltranSanchez3.png"),
#     width = 9, height = 9, units = "in", res = 600)

# First: ellipses in lower triangular
corrplot(as.matrix(Corr.mean.orden),
         type = "lower", method = "ellipse", 
         p.mat = Significance, sig.level = 0.05, insig = "label_sig",
         pch.cex = 1.5, pch.col = "grey20",
         addCoef.col = "black", number.cex = 0.8,
         tl.pos = "d", tl.cex = 0.9, cl.pos = "r")

# Second: CI in upper triangular
corrplot(as.matrix(Corr.mean.orden),
         type = "upper", method = "square",
         diag = FALSE, add = TRUE, cl.pos = "n",
         plotCI = "rect", lowCI = as.matrix(Corr.quantileL.orden), 
         uppCI = as.matrix(Corr.quantileU.orden), rect.col = "navy", tl.pos = "n")

# dev.off()

# tiff(file.path("figures", "BeltranSanchez3.tiff"),
#      width = 9, height = 9, units = "in", res = 600, compression = "lzw")
# 
# # First: ellipses in lower triangular
# corrplot(as.matrix(Corr.mean.orden),
#          type = "lower", method = "ellipse", 
#          p.mat = Significance, sig.level = 0.05, insig = "label_sig",
#          pch.cex = 1.5, pch.col = "grey20",
#          addCoef.col = "black", number.cex = 0.8,
#          tl.pos = "d", tl.cex = 0.9, cl.pos = "r")
# 
# # Second: CI in upper triangular
# corrplot(as.matrix(Corr.mean.orden),
#          type = "upper", method = "square",
#          diag = FALSE, add = TRUE, cl.pos = "n",
#          plotCI = "rect", lowCI = as.matrix(Corr.quantileL.orden), 
#          uppCI = as.matrix(Corr.quantileU.orden), rect.col = "navy", tl.pos = "n")
# 
# dev.off()
# 
# setEPS()
# postscript(file.path("figures", "BeltranSanchez3.eps"),
#            width = 9, height = 9, horizontal = FALSE, onefile = FALSE)
# 
# # First: ellipses in lower triangular
# corrplot(as.matrix(Corr.mean.orden),
#          type = "lower", method = "ellipse", 
#          p.mat = Significance, sig.level = 0.05, insig = "label_sig",
#          pch.cex = 1.5, pch.col = "grey20",
#          addCoef.col = "black", number.cex = 0.8,
#          tl.pos = "d", tl.cex = 0.9, cl.pos = "r")
# 
# # Second: CI in upper triangular
# corrplot(as.matrix(Corr.mean.orden),
#          type = "upper", method = "square",
#          diag = FALSE, add = TRUE, cl.pos = "n",
#          plotCI = "rect", lowCI = as.matrix(Corr.quantileL.orden), 
#          uppCI = as.matrix(Corr.quantileU.orden), rect.col = "navy", tl.pos = "n")
# 
# dev.off()

#### Individual-level correlation matrix under Model-Corr&IRE: Figure 4 ####

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

# Posterior sample of correlation matrices
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

Corr.mean <- data.frame(Corr.mean); rownames(Corr.mean) <- labels; colnames(Corr.mean) <- labels

orden <- corrMatOrder(as.matrix(Corr.mean), order = "hclust", hclust.method = "ward.D2")
# orden <- c(3, 4, 1, 8, 7, 12, 11, 9, 10, 2, 5, 6)

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

### Adding significances ###

# add on line 446 +0.35 y +0.15
# trace(corrplot, edit = TRUE)

Significance <- matrix(as.numeric(Corr.quantileL.orden > 0 | Corr.quantileU.orden < 0), ncol = NVars, nrow = NVars, byrow = FALSE)
colnames(Significance) <- rownames(Significance) <- colnames(Corr.mean.orden)
Significance <- (Significance - 1) * (-1)
for (Var in 1:NVars) { Significance[Var, Var] <- 1 }

# png(file.path("figures", "BeltranSanchez4.png"),
#     width = 9, height = 9, units = "in", res = 600)

# First: ellipses in lower triangular
corrplot(as.matrix(Corr.mean.orden),
         type = "lower", method = "ellipse", 
         p.mat = Significance, sig.level = 0.05, insig = "label_sig",
         pch.cex = 1.5, pch.col = "grey20",
         addCoef.col = "black", number.cex = 0.8,
         tl.pos = "d", tl.cex = 0.9, cl.pos = "r")

# Second: CI in upper triangular
corrplot(as.matrix(Corr.mean.orden),
         type = "upper", method = "square",
         diag = FALSE, add = TRUE, cl.pos = "n",
         plotCI = "rect", lowCI = as.matrix(Corr.quantileL.orden), 
         uppCI = as.matrix(Corr.quantileU.orden), rect.col = "navy", tl.pos = "n")

# dev.off()

# tiff(file.path("figures", "BeltranSanchez4.tiff"),
#      width = 9, height = 9, units = "in", res = 600, compression = "lzw")
# 
# # First: ellipses in lower triangular
# corrplot(as.matrix(Corr.mean.orden),
#          type = "lower", method = "ellipse", 
#          p.mat = Significance, sig.level = 0.05, insig = "label_sig",
#          pch.cex = 1.5, pch.col = "grey20",
#          addCoef.col = "black", number.cex = 0.8,
#          tl.pos = "d", tl.cex = 0.9, cl.pos = "r")
# 
# # Second: CI in upper triangular
# corrplot(as.matrix(Corr.mean.orden),
#          type = "upper", method = "square",
#          diag = FALSE, add = TRUE, cl.pos = "n",
#          plotCI = "rect", lowCI = as.matrix(Corr.quantileL.orden), 
#          uppCI = as.matrix(Corr.quantileU.orden), rect.col = "navy", tl.pos = "n")
# 
# dev.off()
# 
# setEPS()
# postscript(file.path("figures", "BeltranSanchez4.eps"),
#            width = 9, height = 9, horizontal = FALSE, onefile = FALSE)
# 
# # First: ellipses in lower triangular
# corrplot(as.matrix(Corr.mean.orden),
#          type = "lower", method = "ellipse", 
#          p.mat = Significance, sig.level = 0.05, insig = "label_sig",
#          pch.cex = 1.5, pch.col = "grey20",
#          addCoef.col = "black", number.cex = 0.8,
#          tl.pos = "d", tl.cex = 0.9, cl.pos = "r")
# 
# # Second: CI in upper triangular
# corrplot(as.matrix(Corr.mean.orden),
#          type = "upper", method = "square",
#          diag = FALSE, add = TRUE, cl.pos = "n",
#          plotCI = "rect", lowCI = as.matrix(Corr.quantileL.orden), 
#          uppCI = as.matrix(Corr.quantileU.orden), rect.col = "navy", tl.pos = "n")
# 
# dev.off()

#### Municipality-level correlation matrix under Model-Corr: Figure S15 ####

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

salwinbugs <- salwinbugs2
n.sims <- salwinbugs$n.sims
Sigma.Munisim <- SurveyMapping.Sigma.Muni(salwinbugs = salwinbugs)
Corr <- array(dim = c(n.sims, NVars, NVars))

# Posterior sample of correlation matrices
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

Corr.mean <- data.frame(Corr.mean); rownames(Corr.mean) <- labels;
colnames(Corr.mean) <- labels

Corr.mean.orden <- as.matrix(Corr.mean)
Corr.mean.orden <- Corr.mean.orden[orden, orden]

Corr.quantileL <- data.frame(Corr.quantileL); rownames(Corr.quantileL) <- labels;
colnames(Corr.quantileL) <- labels

orden <- c(3, 4, 1, 8, 7, 12, 11, 9, 10, 2, 5, 6)

Corr.quantileL.orden <- as.matrix(Corr.quantileL)
Corr.quantileL.orden <- Corr.quantileL.orden[orden, orden]

Corr.quantileU <- data.frame(Corr.quantileU); rownames(Corr.quantileU) <- labels;
colnames(Corr.quantileU) <- labels

Corr.quantileU.orden <- as.matrix(Corr.quantileU)
Corr.quantileU.orden <- Corr.quantileU.orden[orden, orden]

### Adding significances ###

# add on line 446 +0.35 y +0.15
# trace(corrplot, edit = TRUE)

Significance <- matrix(as.numeric(Corr.quantileL.orden > 0 | Corr.quantileU.orden < 0), ncol = NVars, nrow = NVars, byrow = FALSE)
colnames(Significance) <- rownames(Significance) <- colnames(Corr.mean.orden)
Significance <- (Significance - 1) * (-1)
for (Var in 1:NVars) { Significance[Var, Var] <- 1 }

# png(file.path("figures", paste0("SupplementalMaterial", NVars + 3, ".png")),
#     width = 9, height = 9, units = "in", res = 600)

# First: ellipses in lower triangular
corrplot(as.matrix(Corr.mean.orden),
         type = "lower", method = "ellipse", 
         p.mat = Significance, sig.level = 0.05, insig = "label_sig",
         pch.cex = 1.5, pch.col = "grey20",
         addCoef.col = "black", number.cex = 0.8,
         tl.pos = "d", tl.cex = 0.9, cl.pos = "r")

# Second: CI in upper triangular
corrplot(as.matrix(Corr.mean.orden),
         type = "upper", method = "square",
         diag = FALSE, add = TRUE, cl.pos = "n",
         plotCI = "rect", lowCI = as.matrix(Corr.quantileL.orden), 
         uppCI = as.matrix(Corr.quantileU.orden), rect.col = "navy", tl.pos = "n")

# dev.off()

# tiff(file.path("figures", paste0("SupplementalMaterial", NVars + 3, ".tiff")),
#      width = 9, height = 9, units = "in", res = 600, compression = "lzw")
# 
# # First: ellipses in lower triangular
# corrplot(as.matrix(Corr.mean.orden),
#          type = "lower", method = "ellipse", 
#          p.mat = Significance, sig.level = 0.05, insig = "label_sig",
#          pch.cex = 1.5, pch.col = "grey20",
#          addCoef.col = "black", number.cex = 0.8,
#          tl.pos = "d", tl.cex = 0.9, cl.pos = "r")
# 
# # Second: CI in upper triangular
# corrplot(as.matrix(Corr.mean.orden),
#          type = "upper", method = "square",
#          diag = FALSE, add = TRUE, cl.pos = "n",
#          plotCI = "rect", lowCI = as.matrix(Corr.quantileL.orden), 
#          uppCI = as.matrix(Corr.quantileU.orden), rect.col = "navy", tl.pos = "n")
# 
# dev.off()
# 
# # setEPS()
# # postscript(file.path("figures", paste0("SupplementalMaterial", NVars + 3, ".eps")),
# #            width = 9, height = 9, horizontal = FALSE, onefile = FALSE)
# 
# # First: ellipses in lower triangular
# corrplot(as.matrix(Corr.mean.orden),
#          type = "lower", method = "ellipse", 
#          p.mat = Significance, sig.level = 0.05, insig = "label_sig",
#          pch.cex = 1.5, pch.col = "grey20",
#          addCoef.col = "black", number.cex = 0.8,
#          tl.pos = "d", tl.cex = 0.9, cl.pos = "r")
# 
# # Second: CI in upper triangular
# corrplot(as.matrix(Corr.mean.orden),
#          type = "upper", method = "square",
#          diag = FALSE, add = TRUE, cl.pos = "n",
#          plotCI = "rect", lowCI = as.matrix(Corr.quantileL.orden), 
#          uppCI = as.matrix(Corr.quantileU.orden), rect.col = "navy", tl.pos = "n")
# 
# dev.off()

#### Principal component analysis of the spatial effects: Figure 5 ####

salwinbugs <- salwinbugs3
thetasim <- salwinbugs$sims.list$theta
thetamean <- apply(thetasim, 2:3, mean)
pca <- princomp(thetamean, cor = TRUE)
# summary(pca)
pca$loadings[, 1:2]
# plot(pca, type = "l")
thetaComp <- pca$scores[, 1:2]
cor(thetaComp)

carto_muni_sf <- st_as_sf(carto_muni)
carto_muni_sf$pca1_num <- thetaComp[, 1]

# Fifteen equal-probability intervals
breaks <- c(min(carto_muni_sf$pca1_num) - 0.001, quantile(carto_muni_sf$pca1_num, probs = seq(1/15, 14/15, length.out = 14)), max(carto_muni_sf$pca1_num))
breaks <- c(-17.50, -3.00, -2.50, -2.00, -1.50, -1.00, -0.50, -0.25, 
            0.25, 0.50, 1.00, 1.50, 2.00, 2.50, 3.00, 17.50)
labels <- c("Worse S1", "Better S2,", " ", "  ", "   ", 
            "    ", "     ", "      ", "       ", "        ",
            "         ", "          ", "           ", "Worse S2", "Better S1,")

carto_muni_sf$pca1_cat <- cut(carto_muni_sf$pca1_num, breaks = breaks, include.lowest = FALSE, right = TRUE, labels = labels)

p1 <- ggplot(carto_muni_sf) + 
  geom_sf(aes(fill = pca1_cat), colour = "black", linewidth = 0.01) +
  scale_fill_manual(values = colorRampPalette(brewer.pal(7, "RdYlBu"))(15),
                    drop = FALSE, name = NULL, guide = guide_legend(reverse = TRUE)) +
  labs(title = "(A)") + theme_void() +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0),
        legend.text = element_text(size = 14), legend.position = "right",
        legend.key.height = unit(0.9, "cm"), legend.key.width = unit(0.7, "cm"),
        legend.spacing.y = unit(0.08, "cm"))

carto_muni_sf$pca2_num <- thetaComp[, 2]

# Fifteen equal-probability intervals
breaks <- c(min(carto_muni_sf$pca2_num) - 0.001, quantile(carto_muni_sf$pca2_num, probs = seq(1/15, 14/15, length.out = 14)), max(carto_muni_sf$pca2_num))
labels <- c("Better MH", " ", "  ", "   ", "    ", 
            "     ", "      ", "       ", "        ",
            "         ", "          ", "           ",
            "            ", "             ", "Worse MH")

carto_muni_sf$pca2_cat <- cut(carto_muni_sf$pca2_num, breaks = breaks, include.lowest = FALSE, right = TRUE)
carto_muni_sf$pca2_cat <- factor(carto_muni_sf$pca2_cat, levels = rev(levels(carto_muni_sf$pca2_cat)), labels = labels)

p2 <- ggplot(carto_muni_sf) +
  geom_sf(aes(fill = pca2_cat), colour = "black", linewidth = 0.01) +
  scale_fill_manual(values = colorRampPalette(brewer.pal(7, "Oranges"))(15),
                    drop = FALSE, name = NULL, guide = guide_legend(reverse = TRUE)) +
  labs(title = "(B)") + theme_void() +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0),
        legend.text = element_text(size = 14), legend.position = "right",
        legend.key.height = unit(0.9, "cm"), legend.key.width = unit(0.7, "cm"),
        legend.spacing.y = unit(0.08, "cm"))
p1 + p2

# ggsave(file.path("figures", "BeltranSanchez5.png"), device = "png",
#        width = 10, height = 7, units = "in", dpi = 600)
# 
# ggsave(file.path("figures", "BeltranSanchez5.tiff"), device = "tiff", 
#        width = 10, height = 7, units = "in", dpi = 600, compression = "lzw")
# 
# ggsave(file.path("figures", "BeltranSanchez5.eps"), device = cairo_ps,
#        width = 10, height = 7, units = "in", fallback_resolution = 600)

#### Cut points by sex and age group: Figures S1–S12 ####

round(salwinbugs3$summary, 4)[startsWith(labels(salwinbugs3$summary)[[1]], "kappa"), ]
kappamean <- apply(salwinbugs3$sims.list$kappa, 2:5, mean)
kappaCI <- apply(salwinbugs3$sims.list$kappa, 2:5, quantile, probs = c(0.025, 0.975))
x <- seq(from = -10, to = 10, length.out = 1000)

for (Var in 1:NVars) {
  p <- vector("list", NAges)
  for (AgeGroup in 1:NAges) {
    AgeLevel <- levels(ageC)[AgeGroup]
    df <- data.frame("x" = x, "y" = dlogis(x))
    lines <- data.frame("intercepts" = as.numeric(kappamean[, AgeGroup, , Var]),
                        "Sex" = rep(c("Male", "Female"), NCats - 1))
    ic <- data.frame("lower" = as.numeric(kappaCI[1, , AgeGroup, , Var]),
                     "upper" = as.numeric(kappaCI[2, , AgeGroup, , Var]),
                     "Sex" = rep(c("Male", "Female"), NCats - 1))
    lines$Sex <- factor(lines$Sex, levels = c("Male", "Female"))
    ic$Sex <- factor(ic$Sex, levels = c("Male", "Female"))
    p[[AgeGroup]] <- ggplot() + 
      geom_line(data = df, mapping = aes(x = x, y = y)) + 
      geom_rect(data = ic, mapping = aes(xmin = lower, xmax = upper, 
                                         ymin = -0.1, ymax = 0.30, fill = Sex), 
                alpha = 0.25) +
      geom_vline(data = lines, 
                 mapping = aes(xintercept = intercepts, linetype = Sex, color = Sex)) +
      scale_fill_manual(name = "95% CI", breaks = c("Male", "Female"), 
                        values = c("Male" = "blue3", "Female" = "red3")) +
      scale_color_manual(name = "Mean", breaks = c("Male", "Female"), 
                         values = c("Male" = "blue3", "Female" = "red3")) +
      scale_linetype_manual(name = "Mean", breaks = c("Male", "Female"),
                            values = c("Male" = "dashed", "Female" = "twodash")) +
      labs(title = substitute(paste("Age group ", a), list(a = AgeLevel)), x = "x", y = "Density") +
      coord_cartesian(ylim = c(0, 0.25)) + theme_bw() + scale_x_continuous(breaks = seq(-10, 10, by = 2))
  }
  
  kappa <- ggarrange(plotlist = p, nrow = 2, ncol = 4, common.legend = TRUE, 
                     legend = "bottom")
  print(kappa)
  
  # ggsave(file.path("figures", paste0("SupplementalMaterial", Var, ".png")),
  #        plot = kappa, device = "png", width = 12, height = 6.5,
  #        units = "in", dpi = 600)
  # 
  # ggsave(file.path("figures", paste0("SupplementalMaterial", Var, ".tiff")),
  #        plot = kappa, device = "tiff", width = 12, height = 6.5,
  #        units = "in", dpi = 600, compression = "lzw")
  # 
  # ggsave(file.path("figures", paste0("SupplementalMaterial", Var, ".eps")),
  #        plot = kappa, device = cairo_ps, width = 12, height = 6.5,
  #        units = "in", fallback_resolution = 600)
}

#### Model assessment: Table 2 ####

### Model-Indep ###

# SurveyMapping.prlevels1: 
# - computes the n.sims simulated probabilities for each respondent under Model-Indep

SurveyMapping.prlevels1 <- function(salwinbugs) {
  
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
            p.gamma[sim, Resp, Var, Cat] - p.gamma[sim, Resp, Var, Cat - 1]
        }
      }
    }
    
    if (sim %in% c(1, seq(n.sims/n.chains, n.sims, n.sims/n.chains))) {
      cat(sim, "of", n.sims, "simulations", "\n")
    } else {}
  }
  
  return(prlevels)
}

prlevels1 <- SurveyMapping.prlevels1(salwinbugs = salwinbugs1)

### Model-Corr ###

# SurveyMapping.prlevels2: 
# - computes the n.sims simulated probabilities for each respondent under Model-Corr

SurveyMapping.prlevels2 <- function(salwinbugs) {
  
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
            p.gamma[sim, Resp, Var, Cat] - p.gamma[sim, Resp, Var, Cat - 1]
        }
      }
    }
    
    if (sim %in% c(1, seq(n.sims/n.chains, n.sims, n.sims/n.chains))) {
      cat(sim, "of", n.sims, "simulations", "\n")
    } else {}
  }
  
  return(prlevels)
}

prlevels2 <- SurveyMapping.prlevels2(salwinbugs = salwinbugs2)

### Model-Corr&IRE ###

# SurveyMapping.prlevels3: 
# - computes the n.sims simulated probabilities for each respondent under Model-Corr&IRE

SurveyMapping.prlevels3 <- function(salwinbugs) {
  
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
            p.gamma[sim, Resp, Var, Cat] - p.gamma[sim, Resp, Var, Cat - 1]
        }
      }
    }
    
    if (sim %in% c(1, seq(n.sims/n.chains, n.sims, n.sims/n.chains))) {
      cat(sim, "of", n.sims, "simulations", "\n")
    } else {}
  }
  
  return(prlevels)
}

prlevels3 <- SurveyMapping.prlevels3(salwinbugs = salwinbugs3)

### Validation ###

# Sample size by municipality, sex, and age group
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

# Six municipalities with the largest population in the Region of Valencia
Munis <- order(apply(sample, 1, sum), decreasing = TRUE)[1:6]

# validation1 <- list()
# for (Muni in 1:length(Munis)) {
#   set.seed(9747783)
#   validation1[[Muni]] <- SurveyMapping.Validation(prlevels = prlevels1, Muni = Munis[Muni])
# }

# saveRDS(validation1, file = file.path("results", "multi-2022-nimble-MH-indep-assessment.rds"))

# validation2 <- list()
# for (Muni in 1:length(Munis)) {
#   set.seed(9747783)
#   validation2[[Muni]] <- SurveyMapping.Validation(prlevels = prlevels2, Muni = Munis[Muni])
# }

# saveRDS(validation2, file = file.path("results", "multi-2022-nimble-MH-corr-assessment.rds"))

# validation3 <- list()
# for (Muni in 1:length(Munis)) {
#   set.seed(9747783)
#   validation3[[Muni]] <- SurveyMapping.Validation(prlevels = prlevels3, Muni = Munis[Muni])
# }

# saveRDS(validation3, file = file.path("results", "multi-2022-nimble-MH-corr-ire-assessment.rds"))

validation1 <- readRDS(file = file.path("results", "multi-2022-nimble-MH-indep-assessment.rds"))
validation2 <- readRDS(file = file.path("results", "multi-2022-nimble-MH-corr-assessment.rds"))
validation3 <- readRDS(file = file.path("results", "multi-2022-nimble-MH-corr-ire-assessment.rds"))

# Item 5
Var <- 5
# Valencia
Muni <- 1
# Results
validation1[[Muni]]$mean[Var, ]; validation1[[Muni]]$PI$lower[Var, ]; validation1[[Muni]]$PI$upper[Var, ]
validation2[[Muni]]$mean[Var, ]; validation2[[Muni]]$PI$lower[Var, ]; validation2[[Muni]]$PI$upper[Var, ]
validation3[[Muni]]$mean[Var, ]; validation3[[Muni]]$PI$lower[Var, ]; validation3[[Muni]]$PI$upper[Var, ]
validation3[[Muni]]$real[Var, ]

#### WAIC computation ####

### Model-Indep ###

source(file = file.path("models", "model-indep.R"))

modelData <- list(y = as.matrix(y), zero.theta.resp = rep(0, NVars))

modelConstants <- list(NResp = NResp, NCats = NCats, NVars = NVars, 
                       sex = sex, age = age, muni = muni, NSex = NSex, 
                       NAges = NAges, NMuni = NMuni, ones = ones, 
                       NDist = NDist, Lambda = Lambda, from.to = from.to)

# Create the NIMBLE model and initialize its nodes
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
waic1 <- calculateWAIC(samples, modelWAIC)
waic1

# nimbleList object of type waicNimbleList
# Field "WAIC":
#   [1] 192525.4
# Field "lppd":
#   [1] -94783.11
# Field "pWAIC":
#   [1] 1479.595

### Model-Corr ###

source(file = file.path("models", "model-corr.R"))

modelData <- list(y = as.matrix(y), zero.sub.Muni.resp = rep(0, NVars))

modelConstants <- list(NResp = NResp, NCats = NCats, NVars = NVars, 
                       sex = sex, age = age, muni = muni, NSex = NSex, 
                       NAges = NAges, NMuni = NMuni, ones = ones, 
                       NDist = NDist, Lambda = Lambda, from.to = from.to)

# Create the NIMBLE model and initialize its nodes
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
waic2 <- calculateWAIC(samples, modelWAIC)
waic2

# nimbleList object of type waicNimbleList
# Field "WAIC":
#   [1] 190447.6
# Field "lppd":
#   [1] -93940.28
# Field "pWAIC":
#   [1] 1283.548

### Model-Corr&IRE ###

source(file = file.path("models", "model-corr&ire.R"))

modelData <- list(y = as.matrix(y), zero.sub.Muni.resp = rep(0, NVars))

modelConstants <- list(NResp = NResp, NCats = NCats, NVars = NVars, sex = sex, 
                       age = age, muni = muni, NSex = NSex, NAges = NAges, 
                       NMuni = NMuni, ones = ones, NDist = NDist, Lambda = Lambda, 
                       from.to = from.to)

# Create the NIMBLE model and initialize its nodes
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
waic3 <- calculateWAIC(samples, modelWAIC)
waic3

# nimbleList object of type waicNimbleList
# Field "WAIC":
#   [1] 103170.1
# Field "lppd":
#   [1] -36079.51
# Field "pWAIC":
#   [1] 15505.53

#### Convergence assessment ####

### Model-Indep ###

which((salwinbugs1$summary[startsWith(labels(salwinbugs1$summary)[[1]], "kappa"), 6] > 1.1) | (salwinbugs1$summary[startsWith(labels(salwinbugs1$summary)[[1]], "kappa"), 7] < 100))
which((salwinbugs1$summary[startsWith(labels(salwinbugs1$summary)[[1]], "theta"), 6] > 1.1) | (salwinbugs1$summary[startsWith(labels(salwinbugs1$summary)[[1]], "theta"), 7] < 100))
which((salwinbugs1$summary[startsWith(labels(salwinbugs1$summary)[[1]], "sd.theta"), 6] > 1.1) | (salwinbugs1$summary[startsWith(labels(salwinbugs1$summary)[[1]], "sd.theta"), 7] < 100))
which((salwinbugs1$summary[startsWith(labels(salwinbugs1$summary)[[1]], "rho"), 6] > 1.1) | (salwinbugs1$summary[startsWith(labels(salwinbugs1$summary)[[1]], "rho"), 7] < 100))

salwinbugs1$summary[startsWith(labels(salwinbugs1$summary)[[1]], "kappa"), ]
salwinbugs1$summary[startsWith(labels(salwinbugs1$summary)[[1]], "theta"), ]

MCMCtrace(object = salnimble1,
          pdf = FALSE, # no export to PDF
          ind = TRUE, # separate density lines per chain
          Rhat = TRUE,
          n.eff = TRUE,
          params = "sd.theta")

test <- "theta"

MCMCtrace(object = salnimble1,
          pdf = FALSE, # no export to PDF
          ind = TRUE, # separate density lines per chain
          Rhat = TRUE,
          n.eff = TRUE,
          exact = TRUE,
          ISB = FALSE,
          params = rownames(salwinbugs1$summary[startsWith(labels(salwinbugs1$summary)[[1]], test), ])[(salwinbugs1$summary[startsWith(labels(salwinbugs1$summary)[[1]], test), 6] > 1.03) | (salwinbugs1$summary[startsWith(labels(salwinbugs1$summary)[[1]], test), 7] < 200)])

### Model-Corr ###

which((salwinbugs2$summary[startsWith(labels(salwinbugs2$summary)[[1]], "kappa"), 6] > 1.1) | (salwinbugs2$summary[startsWith(labels(salwinbugs2$summary)[[1]], "kappa"), 7] < 100))
which((salwinbugs2$summary[startsWith(labels(salwinbugs2$summary)[[1]], "sd.M.Muni"), 6] > 1.1) | (salwinbugs2$summary[startsWith(labels(salwinbugs2$summary)[[1]], "sd.M.Muni"), 7] < 100))
which((salwinbugs2$summary[startsWith(labels(salwinbugs2$summary)[[1]], "theta"), 6] > 1.1) | (salwinbugs2$summary[startsWith(labels(salwinbugs2$summary)[[1]], "theta"), 7] < 100))
which((salwinbugs2$summary[startsWith(labels(salwinbugs2$summary)[[1]], "rho"), 6] > 1.1) | (salwinbugs2$summary[startsWith(labels(salwinbugs2$summary)[[1]], "rho"), 7] < 100))
which((salwinbugs2$summary[startsWith(labels(salwinbugs2$summary)[[1]], "M.Muni"), 6] > 1.1) | (salwinbugs2$summary[startsWith(labels(salwinbugs2$summary)[[1]], "M.Muni"), 7] < 100))

salwinbugs2$summary[startsWith(labels(salwinbugs2$summary)[[1]], "kappa"), ]
salwinbugs2$summary[startsWith(labels(salwinbugs2$summary)[[1]], "theta"), ]

MCMCtrace(object = salnimble2,
          pdf = FALSE, # no export to PDF
          ind = TRUE, # separate density lines per chain
          Rhat = TRUE,
          n.eff = TRUE,
          params = "sd.M.Muni")

test <- "theta"

MCMCtrace(object = salnimble2,
          pdf = FALSE, # no export to PDF
          ind = TRUE, # separate density lines per chain
          Rhat = TRUE,
          n.eff = TRUE,
          exact = TRUE,
          ISB = FALSE,
          params = rownames(salwinbugs2$summary[startsWith(labels(salwinbugs2$summary)[[1]], test), ])[(salwinbugs2$summary[startsWith(labels(salwinbugs2$summary)[[1]], test), 6] > 1.03) | (salwinbugs2$summary[startsWith(labels(salwinbugs2$summary)[[1]], test), 7] < 200)])

### Model-Corr&IRE ###

which((salwinbugs3$summary[startsWith(labels(salwinbugs3$summary)[[1]], "kappa"), 6] > 1.1) | (salwinbugs3$summary[startsWith(labels(salwinbugs3$summary)[[1]], "kappa"), 7] < 100))
which((salwinbugs3$summary[startsWith(labels(salwinbugs3$summary)[[1]], "sd.M.Muni"), 6] > 1.1) | (salwinbugs3$summary[startsWith(labels(salwinbugs3$summary)[[1]], "sd.M.Muni"), 7] < 100))
which((salwinbugs3$summary[startsWith(labels(salwinbugs3$summary)[[1]], "theta"), 6] > 1.1) | (salwinbugs3$summary[startsWith(labels(salwinbugs3$summary)[[1]], "theta"), 7] < 100))
which((salwinbugs3$summary[startsWith(labels(salwinbugs3$summary)[[1]], "rho"), 6] > 1.1) | (salwinbugs3$summary[startsWith(labels(salwinbugs3$summary)[[1]], "rho"), 7] < 100))
which((salwinbugs3$summary[startsWith(labels(salwinbugs3$summary)[[1]], "M.Muni"), 6] > 1.1) | (salwinbugs3$summary[startsWith(labels(salwinbugs3$summary)[[1]], "M.Muni"), 7] < 100))
which((salwinbugs3$summary[startsWith(labels(salwinbugs3$summary)[[1]], "sd.M.Resp"), 6] > 1.1) | (salwinbugs3$summary[startsWith(labels(salwinbugs3$summary)[[1]], "sd.M.Resp"), 7] < 100))
which((salwinbugs3$summary[startsWith(labels(salwinbugs3$summary)[[1]], "psi"), 6] > 1.1) | (salwinbugs3$summary[startsWith(labels(salwinbugs3$summary)[[1]], "psi"), 7] < 100))
which((salwinbugs3$summary[startsWith(labels(salwinbugs3$summary)[[1]], "M.Resp"), 6] > 1.1) | (salwinbugs3$summary[startsWith(labels(salwinbugs3$summary)[[1]], "M.Resp"), 7] < 100))

salwinbugs3$summary[startsWith(labels(salwinbugs3$summary)[[1]], "kappa"), ]
salwinbugs3$summary[startsWith(labels(salwinbugs3$summary)[[1]], "theta"), ]
salwinbugs3$summary[startsWith(labels(salwinbugs3$summary)[[1]], "psi"), ]

MCMCtrace(object = salnimble3,
          pdf = FALSE, # no export to PDF
          ind = TRUE, # separate density lines per chain
          Rhat = TRUE,
          n.eff = TRUE,
          params = "sd.M.Muni")

MCMCtrace(object = salnimble3,
          pdf = FALSE, # no export to PDF
          ind = TRUE, # separate density lines per chain
          Rhat = TRUE,
          n.eff = TRUE,
          params = "sd.M.Resp")

test <- "theta"

MCMCtrace(object = salnimble3,
          pdf = FALSE, # no export to PDF
          ind = TRUE, # separate density lines per chain
          Rhat = TRUE,
          n.eff = TRUE,
          exact = TRUE,
          ISB = FALSE,
          params = rownames(salwinbugs3$summary[startsWith(labels(salwinbugs3$summary)[[1]], test), ])[(salwinbugs3$summary[startsWith(labels(salwinbugs3$summary)[[1]], test), 6] > 1.03) | (salwinbugs3$summary[startsWith(labels(salwinbugs3$summary)[[1]], test), 7] < 200)])

test <- "psi"

MCMCtrace(object = salnimble3,
          pdf = FALSE, # no export to PDF
          ind = TRUE, # separate density lines per chain
          Rhat = TRUE,
          n.eff = TRUE,
          exact = TRUE,
          ISB = FALSE,
          params = rownames(salwinbugs3$summary[startsWith(labels(salwinbugs3$summary)[[1]], test), ])[(salwinbugs3$summary[startsWith(labels(salwinbugs3$summary)[[1]], test), 6] > 1.03) | (salwinbugs3$summary[startsWith(labels(salwinbugs3$summary)[[1]], test), 7] < 200)])
