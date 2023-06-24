gkumar_method_1_per <- function(formula, data, erro,n) {
  #n=132
  nn = nrow(data)
  if(n>=nn){
    stop("n must be smaller than nrow(data) ")
  }
  data.n = data[1:n,]
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
  repeat {
    beta <- theta.start[c(1:ncx)]
    lambda <- theta.start[c((1 + ncx):(ncx + ncv))]
    alpha <- theta.start[-c((1):(ncx + ncv))]

    emv.beta <-
      try(optim(
        par = beta,
        fn = log.f.cond.beta,
        control = list(fnscale = -1),
        method = "BFGS",
        # method = "L-BFGS-B",
        # lower = c(rep(-Inf,ncx+ncv),0.25),
        # upper = c(rep(Inf,ncx+ncv),0.95),
        x = cov_a,
        w = cov_delta,
        y = y,
        lambda = lambda,
        alpha = alpha,
        ncx = ncx,
        ncv = ncv,
        hessian = T
      ),
      silent = T)

    emv.lambda <-
      try(optim(
        par = lambda,
        fn = log.f.cond.lambda,
        control = list(fnscale = -1),
        method = "BFGS",
        # method = "L-BFGS-B",
        # lower = c(rep(-Inf,ncx+ncv),0.25),
        # upper = c(rep(Inf,ncx+ncv),0.95),
        x = cov_a,
        w = cov_delta,
        y = y,
        beta = emv.beta$par,
        alpha = alpha,
        hessian = T
      ),
      silent = T)


    emv.alpha <-
      try(optim(
        par = alpha,
        fn = log.f.cond.alpha,
        control = list(fnscale = -1),
        #method = "BFGS",
        method = "L-BFGS-B",
        lower = c(0.1),
        upper = c(.99),
        x = cov_a,
        w = cov_delta,
        y = y,
        ncx = ncx,
        ncv = ncv,
        theta = c(emv.beta$par, emv.lambda$par),
        hessian = T
      ),
      silent = T)

    # if (class(emv.alpha) != "try-error" & emv.alpha$value == 0) {
    #   estimates.aux = list()
    #   estimates.aux$par = rep(NA, ncx + ncv + 1)
    #   break
    # }
    #
    # if (class(emv.alpha) == "try-error") {
    #   estimates.aux = list()
    #   estimates.aux$par = rep(NA, ncx + ncv + 1)
    #   break
    # } else{
    #   theta.up <- c(emv.beta$par, emv.lambda$par, emv.alpha$par)
    # }

    if (class(emv.alpha) == "try-error") {
      estimates.aux = list()
      estimates.aux$par = rep(NA, ncx + ncv + 1)
      break
    }

    if (class(emv.alpha) != "try-error" & emv.alpha$value == 0) {
      estimates.aux = list()
      estimates.aux$par = rep(NA, ncx + ncv + 1)
      break
    }



    theta.up <- c(emv.beta$par, emv.lambda$par, emv.alpha$par)


    crit <- sum(((theta.up - theta.start) / theta.start) ^ 2)

    if (crit < erro) {
      estimates.aux = list()
      estimates.aux$par = c(emv.beta$par, emv.lambda$par, emv.alpha$par)
      break
    } else{
      iter = iter + 1
      theta.start = c(emv.beta$par, emv.lambda$par, emv.alpha$par)
      if (iter > 50) {
        estimates.aux = list()
        estimates.aux$par = c(emv.beta$par, emv.lambda$par, emv.alpha$par)
        break
      }
    }

  }

  sdd_beta = sqrt(diag(-solve(emv.beta$hessian)))
  sdd_gamma = sqrt(diag(-solve(emv.lambda$hessian)))
  sdd_alpha = sqrt(diag(-solve(emv.alpha$hessian)))
  sdd = c(sdd_beta,sdd_gamma,sdd_alpha)
  emv.lower = round(estimates.aux$par-1.96*sdd,4)
  emv.upper = round(estimates.aux$par+1.96*sdd,4)
  emv.upper[ncx+ncv+1] = min( c(1, emv.upper[ncx+ncv+1]  )  )
  interval = paste0("(",emv.lower,",",emv.upper,")")
  results = data.frame(Estimates = round(estimates.aux$par,4),sd=round(sdd,4),Interval = interval)

  return(
    list(
      coefficients = results,
      fomula = formula,
      data = tibble(data),
      data.n = tibble(data.n),
      n=n
    )
  )
}
