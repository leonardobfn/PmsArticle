gkumar_pred_quantile <- function(results, quant) {
  #quant = "runif"


  formula = results$fomula
  if(quant=="runif"){
  n = nrow(data)#results$n
  data  = results$data
  }else{
    data  = results$data.n
    n = results$n
  }

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


  nn = n
  if(quant=="runif"){
    yt.y1 = matrix(0, nn, 1)
  }else{
    yt.y1 = matrix(y, nn, 1)
  }

  a = nu
  at1 = a[1]
  bt1 = b[1]

  if (quant == "runif") {
    u.quant = call("runif", 1)
  } else{
    u.quant = quant
  }

  if (quant == "runif") {
    repeat {
      u1 = eval(u.quant)
      y1 = (1 - exp(-(1 / bt1) * (-log(1 - u1)) ^ (1 / alpha))) ^ (1 / at1)
      yt.y1[1] <-  y1
      if (format(y1) != 1 & format(y1) != 0)
        break
    }
  }

  for (i in 2:nn) {
    #i = 84
    at1 = a[i - 1]
    bt1 = b[i - 1]
    at = a[i]
    bt = b[i]

    # repeat{
    #   u2 = runif(1,0,1)
    #   if(abs(u2-0)>0.02 & abs(u2-1)>0.02)
    #     break
    # }

    u2 = eval(u.quant)

    int.yt = try (uniroot(
      p.cond,
      interval = c(0, 1),
      u = u2,
      y.t.1 = yt.y1[i - 1],
      at1 = at1,
      at = at,
      bt1 = bt1,
      bt = bt,
      alpha = alpha
    ),
    silent = T)

    if (class(int.yt) == "try-error" & i == 2) {
      repeat {
        repeat {
          u1 = eval(u.quant)
          #G = (1 - .8 ^ (1 / b[1])) ^ a[1]
          y1 = (1 - exp(-(1 / bt1) * (-log(1 - u1)) ^ (1 / alpha))) ^ (1 / at1)
          yt.y1[1] <-  y1
          if (format(y1) != 1 & format(y1) != 0)
            break
        }

        u2 = eval(u.quant)

        int.yt = try (uniroot(
          p.cond,
          interval = c(0, 1),
          u = u2,
          y.t.1 = yt.y1[i - 1],
          at1 = at1,
          at = at,
          bt1 = bt1,
          bt = bt,
          alpha = alpha
        ),
        silent = T)

        if (class(int.yt) != "try-error") {
          break
        }
      }
    }
    #int.yt = list()
    #int.yt$root = 0.000000e+00
    if (class(int.yt) == "try-error") {
      int.yt = list()
      int.yt$root = 0
      test2 = "try-error"
    } else{
      test2 = "no-error"
    }

    if (class(int.yt) != "try-error" &
        i == 2 &
        (format(int.yt$root) == 0 | format(int.yt$root) == 1)) {
      test = "try-error"
      while (test == "try-error") {
        repeat {
          u1 = eval(u.quant)
          #G = (1 - .8 ^ (1 / b[1])) ^ a[1]
          y1 = (1 - exp(-(1 / bt1) * (-log(1 - u1)) ^ (1 / alpha))) ^ (1 / at1)
          yt.y1[1] <-  y1
          if (format(y1) != 1 & format(y1) != 0)
            break
        }

        u2 = eval(u.quant)

        int.yt = try (uniroot(
          p.cond,
          interval = c(0, 1),
          u = u2,
          y.t.1 = yt.y1[i - 1],
          at1 = at1,
          at = at,
          bt1 = bt1,
          bt = bt,
          alpha = alpha
        ),
        silent = T)
        test = class(int.yt)
        if (test != "try-error") {
          if (format(int.yt$root) != 0 & format(int.yt$root) != 1)
            break
          else{
            test = "try-error"
          }
        }
        # if (class(int.yt) != "try-error") {
        #   break
        # }
      }
    }

    if ((class(int.yt) == "try-error" |
         test2 == "try-error") & i > 2) {
      repeat {
        u2 = eval(u.quant)
        at1 = a[i - 2]
        bt1 = b[i - 2]
        at = a[i - 1]
        bt = b[i - 1]

        int.yt = try (uniroot(
          p.cond,
          interval = c(0, 1),
          u = u2,
          y.t.1 = yt.y1[i - 2],
          at1 = at1,
          at = at,
          bt1 = bt1,
          bt = bt,
          alpha = alpha
        ),
        silent = T)
        if (class(int.yt) == "try-error") {
          int.yt = list()
          int.yt$root <- NA

        }

        yt.y1[i - 1] <- int.yt$root

        u2 = eval(u.quant)
        at1 = a[i - 1]
        bt1 = b[i - 1]
        at = a[i]
        bt = b[i]

        int.yt = try (uniroot(
          p.cond,
          interval = c(0, 1),
          u = u2,
          y.t.1 = yt.y1[i - 1],
          at1 = at1,
          at = at,
          bt1 = bt1,
          bt = bt,
          alpha = alpha
        ),
        silent = T)

        if (class(int.yt) != "try-error") {
          break
        }
      }
    }

    if (class(int.yt) != "try-error" &
        i > 2 &
        (format(int.yt$root) == 0 | format(int.yt$root) == 1)) {
      test = "try-error"
      while (test == "try-error") {
        u2 = eval(u.quant)
        at1 = a[i - 2]
        bt1 = b[i - 2]
        at = a[i - 1]
        bt = b[i - 1]

        int.yt = try (uniroot(
          p.cond,
          interval = c(0, 1),
          u = u2,
          y.t.1 = yt.y1[i - 2],
          at1 = at1,
          at = at,
          bt1 = bt1,
          bt = bt,
          alpha = alpha
        ),
        silent = T)

        if (class(int.yt) == "try-error") {
          int.yt = list()
          int.yt$root <- NA
        }

        yt.y1[i - 1] <- int.yt$root

        u2 = eval(u.quant)
        at1 = a[i - 1]
        bt1 = b[i - 1]
        at = a[i]
        bt = b[i]

        int.yt = try (uniroot(
          p.cond,
          interval = c(0, 1),
          u = u2,
          y.t.1 = yt.y1[i - 1],
          at1 = at1,
          at = at,
          bt1 = bt1,
          bt = bt,
          alpha = alpha
        ),
        silent = T)

        test = class(int.yt)

        if (test != "try-error") {
          if (format(int.yt$root) != 0 & format(int.yt$root) != 1)
            break
          else{
            test = "try-error"
          }
        }

        # if (class(int.yt) != "try-error") {
        #   break
        # }

      }
    }

    yt.y1[i] <- int.yt$root
  }

  y.pred = yt.y1
  # plot.ts(results$data.n$RH,col=2)
  # lines(y.pred)

  return(y.pred)

}
