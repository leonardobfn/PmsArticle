gkumar_resid <- function(results) {
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
  prob = NULL
  for (i in 2:length(y)) {
    prob[i] = prob.cond(y.t.1 = y[i - 1],
                            y[i],
                            nu[i - 1],
                            nu[i],
                            b[i - 1],
                            b[i],
                            alpha)
  }
  prob = na.omit(prob)
  return(qnorm(prob))
  #return(prob)
}
