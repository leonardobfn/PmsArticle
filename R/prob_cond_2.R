prob_cond_2 = function(u,
                       y.t.2,
                       y.t.1,
                       y.t,
                       at1,
                       at2,
                       at,
                       bt1,
                       bt2,
                       bt,
                       alpha) {

  G.base.t.2 = extraDistr::pkumar(y.t.2,
                                  a = at2,
                                  b = bt2,
                                  lower.tail = F)
  G.base.t.1 = extraDistr::pkumar(y.t.1,
                                  a = at1,
                                  b = bt1,
                                  lower.tail = F)
  G.base.t = extraDistr::pkumar(y.t,
                                a = at,
                                b = bt,
                                lower.tail = F)
  LAMBDA.t.2 = -log(G.base.t.2)
  LAMBDA.t.1 = -log(G.base.t.1)
  LAMBDA.t = -log(G.base.t)

  g.base.t.2 = extraDistr::dkumar(y.t.2, a = at2, b = bt2)
  g.base.t.1 = extraDistr::dkumar(y.t.1, a = at1, b = bt1)
  lambda.t2 = g.base.t.2 / G.base.t.2
  lambda.t1 = g.base.t.1 / G.base.t.1

  SUM.LAMBDA.f = LAMBDA.t.2 + LAMBDA.t.1
  SUM.LAMBDA.F = LAMBDA.t.2 + LAMBDA.t.1 + LAMBDA.t


  ff = alpha * lambda.t1 * lambda.t2 * exp(-SUM.LAMBDA.f ^ alpha) * (SUM.LAMBDA.f) ^
    (alpha - 2) *
    (alpha * SUM.LAMBDA.f ^ alpha + (1 - alpha))

  FF = lambda.t1 * lambda.t2 * exp(-SUM.LAMBDA.F) *
    (alpha ^ 2 * SUM.LAMBDA.F ^ (2 * (alpha - 1)) - alpha*(alpha - 1) * SUM.LAMBDA.F ^
       (alpha - 2))

  return(1 - u - (FF / ff))
}
