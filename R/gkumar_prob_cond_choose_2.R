gkumar_prob_cond_choose_2 <- function(results,t.choose="NULL",y.choose="NULL",h.before) {
  # t.choose = y.choose = "NULL"
   # t.choose = c(135,133)
   # y.choose = c(0.70,0.70)
  # t = 3
  id.t.choose = length(which(t.choose=="NULL"))
  id.y.choose = length(which(y.choose=="NULL"))
  # if(id.y.choose==0 & id.t.choose==0){
  #   if((t %in% t.choose )==F){
  #     stop("t is not in t.choose")
  #   }
  # }
  if(id.y.choose==0 & id.t.choose==0){
    if(length(t.choose)!=length(y.choose)){
      stop("length(t.choose) is not equal length(y.choose)")
    }
  }
  data  = results$data
  formula = results$fomula
  estimates = results$coefficients[, 1]
  n = results$n
  if(t.choose[1]!="NULL"){
    ask. = t.choose>n
    if(length(  which(ask.==T)   )>0){
      FORESCAST = gkumar_forecast_mean(results,h = max(t.choose)-n,F)
    }
  }


  mf <- model.frame(Formula::Formula(formula), data = data)
  y <- model.response(mf)
  y = y[1:n]
  if(t.choose[1]!="NULL"){
  if(length(  which(ask.==T)   )>0){
    y = FORESCAST$y.all
  }
    }
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
  if(t.choose[1]!="NULL"){
  if(id.y.choose==0 & id.t.choose==0){
    y[t.choose] <- y.choose
  }
    }

  pro.cond = gkumar_prob_cond_h_2(y,nu = nu,b = b,h = h.before,alpha=alpha)
  pro.cond$t.choose = t.choose
  pro.cond$y.choose = y.choose
  #pro.cond$forecast = FORESCAST$y.forecast
  # if(length( which(is.na(pro.cond$y) ))>0){
  #   warning(paste0("t>n,","try the function: gkumar_prob_cond_model_forecast"))
  # }
  return(pro.cond)
}
