rm(list = ls())
devtools::load_all() # load the functions
require(tidyverse)
require(extraDistr)
require(reshape2)
data(Manaus)# load the data
data = Manaus

results_1 = readRDS("method_1.rds")
i = 1
# forecast/pred
repeat{


data.simu = gkumar_pred_quantile(results_1, quant = "runif") # simulated
formula = RH ~ log(TBS) + sent + cost | log(t) # definition of model
data$RH[1:results_1$n] = data.simu[1:132]
erro = 10 ^ (-4) # error for the convergence criterion

results_simulated = try(gkumar_method_1(formula,data,n=132),silent = T)

# -----------

if(class(results_simulated)=="try-error" ){
    next
}

# if(class(results_simulated)=="try-error" ){
#   results_s1 = try(gkumar_method_1_per(formula,erro = erro,data,n=132),silent = T)
#   if(class(results_s1)=="try-error" ){
#     next
#   }
# }


lf = length(which(is.finite(results_simulated$coefficients$sd )==F))
if(lf>=1){
  next
}
if(results_simulated$coefficients[7,1]==0.9900){
  next
}
if(results_simulated$coefficients[7,1]<=0){
  next
}
#----------

forecast_median_cond = gkumar_pred_quantile_forecast(results_simulated,h=12,quant = 0.5)
forecast_reverse_risk = gkumar_forecast_mean(results_simulated,h=12)

pred_median_cond = gkumar_pred_quantile(results_simulated,quant = 0.5) # estimation
pred_reverse_risk = gkumar_pred_mean(results_simulated)

fore_sim = data.frame(
  Method = "Method 1",
  id_simu = i,
  tt = 1:144,
  data.simu = data.simu,
  forecast_median_cond = forecast_median_cond$y.all,
  forecast_reverse_risk = forecast_reverse_risk$y.all
)


pred_sim = data.frame(
  Method = "Method 1",
  id_simu = i,
  tt = 2:132,
  data.simu = data.simu[2:132],
  pred_median_cond = pred_median_cond[-1],
  pred_reverse_risk =pred_reverse_risk$y.pred[-1]
)

write.table(fore_sim,"fore_sim_method_1.txt",append = T,col.names = F)
write.table(pred_sim,"pred_sim_method_1.txt",append = T,col.names = F)
n.row = nrow(read.table("fore_sim_method_1.txt"))
if(n.row==144000){
  break
}
i = i + 1
}



# 2 -----------------------------------------------------------------------

rm(list = ls())
devtools::load_all() # load the functions
require(tidyverse)
require(extraDistr)
require(reshape2)
data(Manaus)# load the data
data = Manaus

results_2 = readRDS("method_2.rds")
i = 1
# forecast/pred
repeat{


  data.simu = gkumar_pred_quantile(results_2, quant = "runif") # simulated
  formula = RH ~ log(TBS) + sent + cost | log(t) # definition of model
  data$RH[1:results_2$n] = data.simu[1:132]
  erro = 10 ^ (-4) # error for the convergence criterion

  results_simulated = try(gkumar_method_2(formula,erro = erro,data,n=132),silent = T)

  # -----------
  if(class(results_simulated)=="try-error" ){
  next
    }



  lf = length(which(is.finite(results_simulated$coefficients$sd )==F))
  if(lf>=1){
    next
  }
  if(results_simulated$coefficients[7,1]==0.9900){
    next
  }
  if(results_simulated$coefficients[7,1]<=0){
    next
  }
  #----------

  forecast_median_cond = gkumar_pred_quantile_forecast(results_simulated,h=12,quant = 0.5)
  forecast_reverse_risk = gkumar_forecast_mean(results_simulated,h=12)

  pred_median_cond = gkumar_pred_quantile(results_simulated,quant = 0.5) # estimation
  pred_reverse_risk = gkumar_pred_mean(results_simulated)

  fore_sim = data.frame(
    Method = "Method 2",
    id_simu = i,
    tt = 1:144,
    data.simu = data.simu,
    forecast_median_cond = forecast_median_cond$y.all,
    forecast_reverse_risk = forecast_reverse_risk$y.all
  )


  pred_sim = data.frame(
    Method = "Method 2",
    id_simu = i,
    tt = 2:132,
    data.simu = data.simu[2:132],
    pred_median_cond = pred_median_cond[-1],
    pred_reverse_risk =pred_reverse_risk$y.pred[-1]
  )

  write.table(fore_sim,"fore_sim_method_2.txt",append = T,col.names = F)
  write.table(pred_sim,"pred_sim_method_2.txt",append = T,col.names = F)
  n.row = nrow(read.table("fore_sim_method_2.txt"))
  if(n.row==144000){
    break
  }
  i = i + 1
}
