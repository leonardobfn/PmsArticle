snowfall.estimates_Method_2 = function(steps,
                                       model,
                                       alpha.value,
                                       erro = 10 ^ (-4),
                                       formula,
                                       wd,
                                       n)
{

  # wd= wd.
  # model = 1
  # alpha.value = "alpha65"
  # formula = FORMULA
  # steps = 1461
  # n = n.

  wd_sample = paste0(wd,"/Data_simulation/Model_",model,"/simulations/n",n)
  wd_results_emv = paste0(wd,"/Data_simulation/Model_",model,"/estimates/Method_",2,"/estimates/n",n,"/",alpha.value)
  wd_results_hessian = paste0(wd,"/Data_simulation/Model_",model,"/estimates/Method_",2,"/hessian/n",n,"/",alpha.value)
  wd_covariates = paste0(wd,"/Data_simulation/")

  setwd(wd)
  setwd(wd_sample)
  file.sample = paste0(alpha.value, "/data", steps, ".txt")
  data.sample = read.table(file.sample)
  setwd(wd)
  setwd(wd_covariates)
  covi = read.table("covariates.txt")[1:n, ]

  covi$semester <- as.factor(covi$semester)

  data = data.frame(covi, RH = data.sample$y)
  mf <- model.frame(Formula::Formula(formula), data = data)
  y <- model.response(mf)
  cov_a <-
    model.matrix(Formula::Formula(formula),
                 data = data,
                 rhs = 1)
  cov_delta <-
    model.matrix(Formula::Formula(formula),
                 data = data,
                 rhs = 2)

  ncx <- ncol(cov_a)

  ncv <- ncol(cov_delta)
  par_names <- c(paste0(colnames(cov_a), "_nu"),
                 paste0(colnames(cov_delta), "_delta"),
                 "alpha")
  par_real = read.table(paste0("real_par_model_", model, ".txt"))


  if(model==2 | model==4){
    start_aux_betas = try(coef(lm(-log(-log(RH)) ~ sent+cost-1, data = data)))
  }
  if(model==1 | model==3|model==5){
    start_aux_betas = try(coef(lm(-log(-log(RH)) ~ sent+cost, data = data)))
  }
  if(model==6){
    start_aux_betas = try(coef(lm(-log(-log(RH)) ~ t, data = data)))
  }
  if(model==7|model==8){
    start_aux_betas = try(coef(lm(-log(-log(RH)) ~ log(t), data = data)))
  }

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
      theta.start = c(rep(0.20,ncx) , rep(-.1, ncv))
    }

    if (class(par.cov.Gbase) == "try-error") {
      theta.start = c(rep(0.20,ncx) , rep(-.1, ncv))
    } else{
      theta.start = par.cov.Gbase$par
    }
  } else{
    theta.start = c(rep(0.20,ncx) , rep(-.1, ncv))
  }


  theta.start = c(par.cov.Gbase$par, 0.5)

  iter = 0
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
      theta.start = c(emv.beta$par, emv.lambda$par, emv.alpha$par)
      iter = iter + 1
      if(iter>50){
        estimates.aux = list()
        estimates.aux$par = rep(NA, ncx + ncv + 1)
        break
      }
    }

  }


  alpha.up <- estimates.aux$par[-c(1:(ncx + ncv))]
  par.covariates.start <- estimates.aux$par[c(1:(ncx + ncv))]
  # alpha.up <- estimates.aux$par[-c(1:(ncx + ncv))]
  # theta.start = c(1, 0.20, -0.02 , rep(-.1, ncv))
  # par.covariates.start <- theta.start[c(1:(ncx + ncv))]
  value.start = c(estimates.aux$par[c(1:(ncx + ncv))],0.5)
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
      emv <- round(c(par.covariates.up$par, alpha.up),3)
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




  setwd(wd)
  setwd(wd_results_emv)
  #steps=1
  file.estimates.emv  = file.estimates.emv  = paste0(steps,"_estimates_",alpha.value,"_n",n, ".txt")

  estimates = data.frame(
    steps = rep(steps, ncx + ncv + 1),
    alpha = rep(data.sample$alpha[1], ncx + ncv + 1),
    par_name = par_names,
    par_real = c(par_real[, 1], data.sample$alpha[1]),
    emv,
    Method = rep("Method 2", ncx + ncv + 1),
    n = rep(n, ncx + ncv + 1),
    Model = rep(paste0("Model ", model), ncx + ncv +
                  1),
    value.start = round(value.start, 3)
  )

  write.table(
    estimates,
    file.estimates.emv,
    col.names = F,
    row.names = F,
    append = F,
    quote = T,
  )
  setwd(wd)

  setwd(wd_results_hessian)

  file.hessian.alpha = paste0(steps,"_hessian_", alpha.value, "_n", n, ".txt")
  file.hessian.cov = paste0(steps,"_hessian_cov_", alpha.value, "_n", n, ".txt")

  hessianAlpha = cbind(steps, round(emv.alpha$hessian,3))
  hessianCov = cbind(steps, round(par.covariates.up$hessian,3))

  write.table(
    hessianAlpha,
    file.hessian.alpha,
    col.names = F,
    row.names = F,
    quote = T,
    append = F
  )

  write.table(
    hessianCov,
    file.hessian.cov,
    col.names = F,
    row.names = F,
    quote = T,
    append = F
  )

  setwd(wd)
}
