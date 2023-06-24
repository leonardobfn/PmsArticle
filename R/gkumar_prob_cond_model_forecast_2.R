gkumar_prob_cond_model_forecast_2 <- function(results,t,h.forecast,h.before,...) {
  # devtools::load_all()
  # results = readRDS("Data_real/results_method_1_model_1.rds")
  # coef(results)
  # data  = results$data.n
  # formula = results$fomula
  # n = results$n
  # estimates = results$coefficients[, 1]
  # mf <- model.frame(Formula::Formula(formula), data = data)
  # y <- model.response(mf)
  # cov_a <-
  #   model.matrix(Formula::Formula(formula), data = data, rhs = 1)
  # cov_delta <-
  #   model.matrix(Formula::Formula(formula), data = data, rhs = 2)
  #
  # ncx <- ncol(cov_a)
  # ncv <- ncol(cov_delta)
  #
  # beta <- estimates[c(1:ncx)]
  # lambda <- estimates[c((1 + ncx):(ncx + ncv))]
  # alpha <- estimates[-c((1):(ncx + ncv))]
  #
  # nu = exp(cov_a %*% beta)
  # delta = exp(cov_delta %*% lambda)
  # b = 1 / delta

  #h.forecast = 12
  FORESCAST = gkumar_forecast_mean(results,h = h.forecast,...)
  data  = results$data
  formula = results$fomula
  estimates = results$coefficients[, 1]
  n = results$n
  mf <- model.frame(Formula::Formula(formula), data = data)
  y <- model.response(mf)
  cov_a <-
    model.matrix(Formula::Formula(formula), data = data, rhs = 1)
  cov_delta <-
    model.matrix(Formula::Formula(formula), data = data, rhs = 2)

  ncx <- ncol(cov_a)
  ncv <- ncol(cov_delta)

  beta <- estimates[c(1:ncx)]
  lambda <- estimates[c((1 + ncx):(ncx + ncv))]
  alpha <- estimates[-c((1):(ncx + ncv))]

  nu = exp(cov_a %*% beta)
  delta = exp(cov_delta %*% lambda)
  b = 1 / delta
  y.complet = FORESCAST$y.all
  names(y.complet) = 1:(n+h.forecast)
  #h.before = 2
  pro.cond = gkumar_prob_cond_h_2(y.complet,nu = nu,b = b,h = h.before,alpha=alpha)
  if(length( which(is.na(pro.cond$y) ))>0){
    warning(paste0("t>n"))
  }
  #return(list(pro.cond,y.complet,h.forecast,h.before,FORESCAST$y.forecast))
  return(pro.cond)
}
