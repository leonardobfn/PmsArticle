gkumar_method_1 <- function(formula,data,n) {
  # n=132
  nn = nrow(data)
  if(n>=nn){
    stop("n must be smaller than nrow(data) ")
  }
  data.n = data[1:n,]
  # formula = RH~log(TBS) + log(t) + sent+cost | semester
  mf <- model.frame(Formula::Formula(formula), data = data.n)
  y <- model.response(mf)
  cov_a <-
    model.matrix(Formula::Formula(formula), data = data.n, rhs = 1)
  cov_delta <-
    model.matrix(Formula::Formula(formula), data = data.n, rhs = 2)

  ncx <- ncol(cov_a)
  ncv <- ncol(cov_delta)
  par_names <- c(paste0(colnames(cov_a), "_nu"),
                 paste0(colnames(cov_delta), "_delta"),
                 "alpha")

  start_aux_betas = try(coef(lm.fit(cov_a, -log(-log(y))), silent = T))

  if (class(start_aux_betas) != "try-error") {
    par.cov.Gbase <-
      try(optim(
        par =  c(start_aux_betas, rep(-.1, ncv)),
        fn = log.kumar.gbase,
        control = list(fnscale = -1),
        method = "BFGS",
        # method = "L-BFGS-B",
        # lower = c(rep(-Inf,6),0.25),
        # upper = c(rep(Inf,6),0.95),
        x = cov_a,
        w = cov_delta,
        y = y,
        ncx = ncx,
        ncv = ncv
      ),
      silent = T)

    if (class(par.cov.Gbase) != "try-error" &
        par.cov.Gbase$value == 0) {
      theta.start = c(rep(0.20, ncx), rep(-.1, ncv))
    }

    if (class(par.cov.Gbase) == "try-error") {
      theta.start = c(rep(0.20, ncx), rep(-.1, ncv))
    } else{
      theta.start = par.cov.Gbase$par
    }
  } else{
    theta.start = c(rep(0.2, ncx), rep(-.1, ncv))
  }

  # if(START == "gbase"){
  #   theta.start = c(par.cov.Gbase$par, 0.5)
  # }else{
  #   theta.start = c(start_aux_betas,rep(-.1, ncv), 0.5)
  # }
  theta.start = c(par.cov.Gbase$par, 0.5)
  value.start = theta.start
  iter = 0
  names(theta.start) = par_names

    emv <-
      try(optim(
        par = theta.start,
        fn = log.f.cond.all,
        control = list(fnscale = -1),
        method = "BFGS",
        # method = "L-BFGS-B",
        # lower = c(rep(-Inf,ncx+ncv),0.25),
        # upper = c(rep(Inf,ncx+ncv),0.95),
        x = cov_a,
        w = cov_delta,
        y = y,
        ncx = ncx,
        ncv = ncv,
        hessian = T
      ),
      silent = T)

    if(class(emv)!="try-error") {
      if (emv$convergence == 0) {
        emv$par[ncx + ncv + 1] = min(0.99,emv$par[ncx + ncv + 1])
        sdd = sqrt(diag(-solve(emv$hessian)))
        emv.lower = round(emv$par - 1.95 * sdd, 4)
        emv.upper = round(emv$par + 1.95 * sdd, 4)
        emv.upper[ncx + ncv + 1] = min(c(1, emv.upper[ncx + ncv + 1]))
        interval = paste0("(", emv.lower, ",", emv.upper, ")")
        results = data.frame(
          Estimates = round(emv$par, 4),
          sd = round(sdd, 4),
          Interval = interval
        )
        AIC = -2*emv$value + 2*(ncx+ncv+1)
        BIC = -2*emv$value + log(length(y))*(ncx+ncv+1)
      }
    }else{stop("error in optim")}

  return(
    list(
      coefficients = results,
      fomula = formula,
      AIC = AIC,
      BIC = BIC,
      data = tibble(data),
      data.n = tibble(data.n),
      n=n,
      optim.results = emv
    )
  )
}
