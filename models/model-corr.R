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