gkumar_method_2 <- function(formula, data, erro,n) {
  #data = results$data
  # n = 144
  # erro = 10^(-4)
  nn = nrow(data)
  if(n>=nn){
    stop("n must be smaller than nrow(data) ")
  }
  data.n = data[1:n,]
  # formula =  RH ~ log(TBS) + log(t) + sent+cost | semester
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
    theta.start = c(rep(0.20, ncx), rep(-.1, ncv))
  }


  theta.start = c(par.cov.Gbase$par, 0.50)
  #theta.start = c(start_aux_betas,rep(-0.1,ncv),0.5)
  value.start = theta.start

  m1 = gkumar_method_1(formula = formula,data=data,n = n)
  estimates.aux = list()
  estimates.aux$par = m1$optim.results$par
  estimates.aux$par[ncx + ncv + 1] = min(0.99,estimates.aux$par[ncx + ncv + 1])
  alpha.up <- estimates.aux$par[-c(1:(ncx + ncv))]
  par.covariates.start <- estimates.aux$par[c(1:(ncx + ncv))]
  emv.alpha = list()
  emv.alpha$hessian <- -solve(m1$optim.results$hessian)[length(estimates.aux$par),length(estimates.aux$par)]

  # alpha.up <- estimates.aux$par[-c(1:(ncx + ncv))]
  #theta.start = c(start_aux_betas, rep(-0.1,ncv) )
  #par.covariates.start <- theta.start[c(1:(ncx + ncv))]
  #value.start = c(estimates.aux$par[c(1:(ncx + ncv))],0.5)
  iter = 0

  repeat {
    if (length(which(is.na(estimates.aux$par) == T)) > 0) {
      par.covariates.up = list()
      par.covariates.up$hessian = matrix(0,ncx+ncv,ncx+ncv)
      emv.alpha = list()
      emv.alpha$hessian = c(0)
      emv <- rep(NA, ncx + ncv + 1)
      break
    }
    #       # Step E-----
    v <- V(
      theta = par.covariates.start,
      x = cov_a,
      w = cov_delta,
      y = y,
      ncx=ncx,
      ncv=ncv
    )
    derivate_numerator <- try(d_vn(N = length(y) + 1,
                                   v = v,
                                   alpha = alpha.up)$dvn)

    if (class(derivate_numerator) == "try-error" |
        is.finite(derivate_numerator) == F |
        derivate_numerator == 0) {
      par.covariates.up = list()
      par.covariates.up$hessian = matrix(0,ncx+ncv,ncx+ncv)
      emv.alpha = list()
      emv.alpha$hessian = c(0)
      emv <- rep(NA, ncx + ncv + 1)
      break
    }

    derivate_denominator <- try(d_vn(N = length(y),
                                     v = v,
                                     alpha = alpha.up)$dvn)

    if (class(derivate_denominator) == "try-error" |
        is.finite(derivate_denominator) == F |
        derivate_denominator == 0) {
      par.covariates.up = list()
      par.covariates.up$hessian = matrix(0,ncx+ncv,ncx+ncv)
      emv.alpha = list()
      emv.alpha$hessian = c(0)
      emv <- rep(NA, ncx + ncv + 1)
      break
    }

    Esp.z <- -(derivate_numerator / derivate_denominator)

    # Step M----

    par.covariates.up <-
      try(optim(
        par = par.covariates.start,
        fn = log.like_conditionl_covariates_EM,
        control = list(fnscale = -1),
        method = "BFGS",
        x = cov_a,
        w = cov_delta,
        y = y,
        Etil1 = as.numeric(Esp.z),
        hessian = T,
        ncx = ncx,
        ncv=ncv
      ),
      silent = T)

    if (class(par.covariates.up) == "try-error") {
      par.covariates.up = list()
      par.covariates.up$hessian = matrix(0,ncx+ncv,ncx+ncv)
      emv.alpha = list()
      emv.alpha$hessian = c(0)
      emv <- rep(NA, ncx + ncv + 1)
      break
    }

    if (class(par.covariates.up) != "try-error" &
        par.covariates.up$value == 0) {
      par.covariates.up = list()
      par.covariates.up$hessian = matrix(0,ncx+ncv,ncx+ncv)
      emv.alpha = list()
      emv.alpha$hessian = c(0)
      emv <- rep(NA, ncx + ncv + 1)
      break
    }

    crit <-
      sum(((par.covariates.up$par - par.covariates.start) / par.covariates.start
      ) ^ 2)
    if (crit < erro) {
      emv <- round(c(par.covariates.up$par, alpha.up),6)
      break
    }
    else{
      par.covariates.start <- par.covariates.up$par
      iter = iter + 1
      if(iter > 50){
        par.covariates.up = list()
        par.covariates.up$hessian = matrix(0,ncx+ncv,ncx+ncv)
        emv.alpha = list()
        emv.alpha$hessian = c(0)
        emv <- rep(NA, ncx + ncv + 1)
        break
      }
    }
  }


  if(length(which(is.na(emv)==T)   )>0){

    theta.start = c(rep(.001,ncx), rep(-.1, ncv))
    par.covariates.start <- theta.start[c(1:(ncx + ncv))]
    #value.start = c(estimates.aux$par[c(1:(ncx + ncv))],0.5)
    iter = 0

    repeat {
      if (length(which(is.na(estimates.aux$par) == T)) > 0) {
        par.covariates.up = list()
        par.covariates.up$hessian = matrix(0,ncx+ncv,ncx+ncv)
        emv.alpha = list()
        emv.alpha$hessian = c(0)
        emv <- rep(NA, ncx + ncv + 1)
        break
      }
      #       # Step E-----
      v <- V(
        theta = par.covariates.start,
        x = cov_a,
        w = cov_delta,
        y = y,
        ncx=ncx,
        ncv=ncv
      )
      derivate_numerator <- try(d_vn(N = length(y) + 1,
                                     v = v,
                                     alpha = alpha.up)$dvn)

      if (class(derivate_numerator) == "try-error" |
          is.finite(derivate_numerator) == F |
          derivate_numerator == 0) {
        par.covariates.up = list()
        par.covariates.up$hessian = matrix(0,ncx+ncv,ncx+ncv)
        emv.alpha = list()
        emv.alpha$hessian = c(0)
        emv <- rep(NA, ncx + ncv + 1)
        break
      }

      derivate_denominator <- try(d_vn(N = length(y),
                                       v = v,
                                       alpha = alpha.up)$dvn)

      if (class(derivate_denominator) == "try-error" |
          is.finite(derivate_denominator) == F |
          derivate_denominator == 0) {
        par.covariates.up = list()
        par.covariates.up$hessian = matrix(0,ncx+ncv,ncx+ncv)
        emv.alpha = list()
        emv.alpha$hessian = c(0)
        emv <- rep(NA, ncx + ncv + 1)
        break
      }

      Esp.z <- -(derivate_numerator / derivate_denominator)

      # Step M----

      par.covariates.up <-
        try(optim(
          par = par.covariates.start,
          fn = log.like_conditionl_covariates_EM,
          control = list(fnscale = -1),
          method = "BFGS",
          x = cov_a,
          w = cov_delta,
          y = y,
          Etil1 = as.numeric(Esp.z),
          hessian = T,
          ncx = ncx,
          ncv=ncv
        ),
        silent = T)

      if (class(par.covariates.up) == "try-error") {
        par.covariates.up = list()
        par.covariates.up$hessian = matrix(0,ncx+ncv,ncx+ncv)
        emv.alpha = list()
        emv.alpha$hessian = c(0)
        emv <- rep(NA, ncx + ncv + 1)
        break
      }

      if (class(par.covariates.up) != "try-error" &
          par.covariates.up$value == 0) {
        par.covariates.up = list()
        par.covariates.up$hessian = matrix(0,ncx+ncv,ncx+ncv)
        emv.alpha = list()
        emv.alpha$hessian = c(0)
        emv <- rep(NA, ncx + ncv + 1)
        break
      }

      crit <-
        sum(((par.covariates.up$par - par.covariates.start) / par.covariates.start
        ) ^ 2)
      if (crit < erro) {
        emv <- round(c(par.covariates.up$par, alpha.up),6)
        break
      }
      else{
        par.covariates.start <- par.covariates.up$par
        iter = iter + 1
        if(iter > 50){
          par.covariates.up = list()
          par.covariates.up$hessian = matrix(0,ncx+ncv,ncx+ncv)
          emv.alpha = list()
          emv.alpha$hessian = c(0)
          emv <- rep(NA, ncx + ncv + 1)
          break
        }
      }
    }

    theta.start = c(par.cov.Gbase$par, 0.5)
    value.start = theta.start
    iter = 0
    m1 = gkumar_method_1(formula = formula,data=data,n = n,erro=erro)
    estimates.aux = list()
    estimates.aux$par = m1$optim.results$par
    estimates.aux$par[ncx + ncv + 1] = min(0.99,estimates.aux$par[ncx + ncv + 1])
    alpha.up <- estimates.aux$par[-c(1:(ncx + ncv))]
    emv.alpha = list()
    emv.alpha$hessian <- -solve(m1$optim.results$hessian)[length(estimates.aux$par),length(estimates.aux$par)]

  }



  if(length(which(is.na(emv) == T)) == 0){
    names(emv) = par_names
    names(par.covariates.up$par) = par_names[(1:(ncx+ncv))]
    rownames(par.covariates.up$hessian) = par_names[c(1:(ncx+ncv))]
    colnames(par.covariates.up$hessian) = par_names[c(1:(ncx+ncv))]
    names(emv.alpha$hessian) = par_names[-c(1:(ncx+ncv))]

    sdd_cov = sqrt(diag(-solve(par.covariates.up$hessian)))
    sdd_alpha = sqrt(emv.alpha$hessian)
    sdd = c(sdd_cov,sdd_alpha)
    emv.lower = round(emv-1.95*sdd,4)
    emv.upper = round(emv+1.95*sdd,4)
    emv.upper[ncx+ncv+1] = min( c(1, emv.upper[ncx+ncv+1]  )  )
    interval = paste0("(",emv.lower,",",emv.upper,")")
    results = data.frame(Estimates = round(emv,4),sd=round(sdd,4),Interval = interval)
    AIC = -2*par.covariates.up$value + 2*(ncx+ncv+1)
    BIC = -2*par.covariates.up$value + log(length(y))*(ncx+ncv+1)
  }else{
    stop("erro in optim")
  }

  return(
    list(
      coefficients = results,
      # par.covariates = par.covariates.up,
      # Alpha = emv.alpha,
      fomula = formula,
      data = tibble(data),
      data.n = tibble(data.n),
      n=n,
      AIC = AIC,
      BIC = BIC,
      convergence = par.covariates.up$convergence,
      value = par.covariates.up$value
    )
  )
}
