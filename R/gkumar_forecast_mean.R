gkumar_forecast_mean = function(results, h) {
  # MC = T
  # h=12
  # n.mc = 5
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


  #if(MC==F){
    y.all = NULL
    y.all = c(y[1:n])
    Esp = y.forecast = NULL
  for(t in n:(n+(h-1))){
    #t=(n+1)
    G.base = extraDistr::pkumar(y.all[t], a = nu[t], b = b[t],lower.tail = F)
    LAMBDA = -log(G.base)
    Esp[t+1] = alpha^(-1)*LAMBDA^(1-alpha)
    y.forecast[t+1] =  (1-exp(-Esp[t+1])^(1/b[t+1]))^(1/nu[t+1])
    y.all[t+1] = y.forecast[t+1]
  }
 # }

  # else{
  #   y.all = y.forecast = sd.forecast=NULL
  #   y.all = c(y[1:n])
  #
  #   for (t in n:(n + (h - 1))) {
  #     #t = n
  #     S = n.mc;K=n.mc
  #     y.forecast.mc_1 = matrix(0,S,K)
  #     set.seed(SEED)
  #     for(k in 1:K){
  #       for (s in 1:S) {
  #         y.s = runif(1)
  #         y.forecast.mc_1[s,k] = prob.cond(
  #           y.t.1 = y.all[t],
  #           y.t = y.s,
  #           at1 = nu[t],
  #           at = nu[t + 1],
  #           bt1 = b[t],
  #           bt = b[t + 1],
  #           alpha
  #         )
  #       }
  #     }
  #     y.forecast[t + 1] = mean(colMeans(y.forecast.mc_1),na.rm=T)
  #     sd.forecast[t + 1] = sd(colMeans(y.forecast.mc_1),na.rm=T)
  #     y.all[t + 1] = y.forecast[t + 1]
  #   }
  #   }
  # if(MC==T){
  #   res = list(y.forecast = y.forecast[(n + 1):length(y.forecast)],
  #              y.all = y.all,
  #              SD = sd.forecast[(n + 1):length(sd.forecast)])
  #
  # }else{
    res = list(y.forecast = y.forecast[(n + 1):length(y.forecast)],
               y.all = y.all
    )
  #}

  return(res)
}
