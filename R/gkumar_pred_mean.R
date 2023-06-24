gkumar_pred_mean <- function(results) {
  data  = results$data.n
  formula = results$fomula
  estimates = results$coefficients[, 1]
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
  #if (MC == F) {
    Esp = y.pred = NULL
    for (t in 2:length(y)) {
      G.base = extraDistr::pkumar(y[t - 1],
                                  a = nu[t - 1],
                                  b = b[t - 1],
                                  lower.tail = F)
      LAMBDA = -log(G.base)
      Esp[t] = alpha ^ (-1) * LAMBDA ^ (1 - alpha)
      y.pred[t] =  (1 - exp(-Esp[t]) ^ (1 / b[t])) ^ (1 / nu[t])
    }
  #}


  # if (MC == T) {
  #   y.pred = sd.pred = NULL
  #   for (t in 2:length(y)) {
  #     S = n.mc
  #     K = n.mc
  #     y.pred.mc_1 = matrix(0, S, K)
  #     for (k in 1:K) {
  #       for (s in 1:S) {
  #         y.s = runif(1)
  #         y.pred.mc_1[s, k] = prob.cond(
  #           y.t.1 = y[t - 1],
  #           y.t = y.s,
  #           at1 = nu[t - 1],
  #           at = nu[t],
  #           bt1 = b[t],
  #           bt = b[t],
  #           alpha
  #         )
  #       }
  #     }
  #     y.pred [t] = mean(colMeans(y.pred.mc_1), na.rm = T)
  #     sd.pred[t] = sd(colMeans(y.pred.mc_1), na.rm = T)
  #   }
  # }


  # if (MC == T) {
  #   res = list(y.pred = y.pred,
  #              sd.pred = sd.pred)
  # } else{
    res = list(y.pred = y.pred)
  #}
  res =
    return(res)
}
