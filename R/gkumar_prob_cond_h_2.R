gkumar_prob_cond_h_2 <- function(y, nu, b, h, alpha) {
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
  #h = 1
  prob = NULL
  #y = y.complet
  h.seq = 1:h
  if (h > 1) {
    for (t in (h + 1):length(y)) {
      #t = 3

      t.seq = t - h.seq

      id.last = t - h

      at.before.cond = nu[id.last]
      bt.before.cond = b[id.last]

      at.before = nu[c(t.seq[-length(t.seq)])]
      bt.before = b[c(t.seq[-length(t.seq)])]

      at = nu[t]
      bt = b[t]

      G.base.t.before.cond =  extraDistr::pkumar(y[id.last],
                                                 a = at.before.cond,
                                                 b = bt.before.cond,
                                                 lower.tail = F)

      G.base.t.before =  extraDistr::pkumar(y[c(t.seq[-length(t.seq)])],
                                            a = at.before,
                                            b = bt.before,
                                            lower.tail = F)

      G.base.t = extraDistr::pkumar(y[t],
                                    a = at,
                                    b = bt ,
                                    lower.tail = F)

      LAMBDA.t.before.cond = -log(G.base.t.before.cond)
      LAMBDA.t = -log(c(G.base.t, G.base.t.before))

      prob[t] = (
        LAMBDA.t.before.cond ^ (1 - alpha) *
          exp(
            LAMBDA.t.before.cond ^ alpha - (LAMBDA.t.before.cond + sum(LAMBDA.t)) ^ alpha
          ) *
          (LAMBDA.t.before.cond + sum(LAMBDA.t)) ^ (alpha - 1)
      )

    }

  }
  if (h == 1) {
    for (t in (h + 1):length(y)) {
      #t = 3

      t.seq = t - h.seq

      id.last = t - h

      at.before.cond = nu[id.last]
      bt.before.cond = b[id.last]

      at = nu[t]
      bt = b[t]

      G.base.t.before.cond =  extraDistr::pkumar(y[id.last],
                                                 a = at.before.cond,
                                                 b = bt.before.cond,
                                                 lower.tail = F)

      G.base.t = extraDistr::pkumar(y[t],
                                    a = at,
                                    b = bt ,
                                    lower.tail = F)

      LAMBDA.t.before.cond = -log(G.base.t.before.cond)
      LAMBDA.t = -log(c(G.base.t))

      prob[t] = (
        LAMBDA.t.before.cond ^ (1 - alpha) *
          exp(
            LAMBDA.t.before.cond ^ alpha - (LAMBDA.t.before.cond + sum(LAMBDA.t)) ^ alpha
          ) *
          (LAMBDA.t.before.cond + sum(LAMBDA.t)) ^ (alpha - 1)
      )

    }
  }

  return(list(prob = prob,
              h = h,
              y = y))
}
