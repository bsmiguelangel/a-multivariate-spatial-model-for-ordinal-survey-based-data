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