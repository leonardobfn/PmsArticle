rm(list = ls())
# packages--------------
require(tidyverse)
require(dplyr)
require(extraDistr)
require(devtools)
require(ggplot2)
require(latex2exp) # for graphics
devtools::load_all() # load the functions
data(Manaus)# load the data
data = Manaus

formula = RH ~ log(TBS) + sent + cost | log(t) # definition of model

erro = 10 ^ (-4) # error for the convergence criterion

results_1 = gkumar_method_1(formula,data,n=132)
results_2 = gkumar_method_2(formula,data,erro,n=132)

coef(results_1)
coef(results_2)
saveRDS(results_1,"method_1.rds")
saveRDS(results_2,"method_2.rds")




# ------ forecast-----

forecast_median_cond = gkumar_pred_quantile_forecast(results,h=12,quant = 0.5)

forecast_reverse_risk = gkumar_forecast_mean(results,h=12)

# ------------ Prediction-----------

pred_median_cond = gkumar_pred_quantile(results,quant = 0.5)
pred_reverse_risk = gkumar_pred_mean(results)


# -- Conditional Probability order 1----------
prob_cond_1 = gkumar_prob_cond_1(results, yt2 = 0.7) # P(Y_t>0.70 |Y_{t-1} = y_{t-1})
prob_cond_1_ts = ts(prob_cond_1,start = 2009,frequency = 12)
require(latex2exp) # to YLAB
YLAB= TeX(sprintf(
  "$P( Y_t>0.70|Y_{t-1}=y_{t-1} )$"
))
boxplot(prob_cond_1_ts~results$data.n$month_names,xlab = "Month",ylab=YLAB)
abline(h=c(0.50),lty = 2)

# -- Conditional Probability order 2----------
prob_cond_2 = NULL
h.before = 2
t_start = h.before + 1
n = results$n
for (t in t_start:n) {
  t.vector = c(t - 1, t)
  prob_cond_2[t] = gkumar_prob_cond_choose_2(
    results,
    h.before = h.before,
    t.choose = t.vector, # time instants where the values defined in "y.choose" will be placed
    y.choose = c(0.70, 0.70) # betwen (0,1)
  )$prob[t] # P( Y_t>0.70,Y_{t-1}>0.70|Y_{t-2}=y_{t-2} )
}

prob_cond_2_ts = ts(c(prob_cond_2),start = 2009,frequency = 12)
YLAB = TeX(sprintf(
  "$P( Y_t>0.70,Y_{t-1}>0.70|Y_{t-2}=y_{t-2} )$"
))
boxplot(prob_cond_2_ts~results$data.n$month_names,xlab="Month",ylab=YLAB)
abline(h=c(0.25),lty = 2)



# ------- Other results ------------

forecast_q_cond = gkumar_pred_quantile_forecast(results,h=12,quant = .75) #q = 0.75
pred_q_cond = gkumar_pred_quantile(results,quant = 0.75) #q = 0.75


prob_cond_1 = gkumar_prob_cond_1(results) # P(Y_t>y_{t}|Y_{t-1} = y_{t-1})
prob_cond_1 = 1-gkumar_prob_cond_1(results,yt2 = 0.30) # P(Y_t<0.30|Y_{t-1} = y_{t-1})
prob_cond_1 = gkumar_prob_cond_1(results,yt1 = 0.50,yt2 = 0.60) # P(Y_t>0.60|Y_{t-1} = 0.50)


prob_cond_2 = gkumar_prob_cond_choose_2(results,h.before = 2)#P( Y_t>y{t},Y_{t-1}>y{t-1}|Y_{t-2}=y_{t-2} )


