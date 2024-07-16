rm(list = ls())
results_1 = readRDS("method_1.rds")
results_2 = readRDS("method_2.rds")

y.full = results_1$data$RH
y.n = results_1$data.n$RH

# ------ forecast-----

forecast_median_cond.1 = gkumar_pred_quantile_forecast(results_1, h = 12, quant = 0.5)
forecast_reverse_risk.1 = gkumar_forecast_mean(results_1, h = 12)

forecast_median_cond.2 = gkumar_pred_quantile_forecast(results_2, h = 12, quant = 0.5)
forecast_reverse_risk.2 = gkumar_forecast_mean(results_2, h = 12)


# ------------ Prediction-----------

pred_median_cond.1 = gkumar_pred_quantile(results_1, quant = 0.5)
pred_reverse_risk.1 = gkumar_pred_mean(results_1)

pred_median_cond.2 = gkumar_pred_quantile(results_2, quant = 0.5)
pred_reverse_risk.2 = gkumar_pred_mean(results_2)



# Graphics ----------------------------------------------------------------
forec = NULL
forec = rbind(
  forecast_median_cond.1$y.forecast,
  forecast_reverse_risk.1$y.forecast,
  forecast_median_cond.2$y.forecast,
  forecast_reverse_risk.2$y.forecast
) |> data.frame()

forec = forec |> mutate(
  m = c("median_cond", "RR", "median_cond", "RR"),
  Method = c("Method 1", "Method 1", "Method 2", "Method 2")
)

forec = reshape2::melt(forec) |> arrange(m) |> select(-variable)

y.data.frame = data.frame(
  m = rep("Real", each = 2) |> rep(2),
  Method = forec$Method,
  value = rep(y.full[133:144], each = 2) |> rep(2)
)

fore.all = rbind(forec, y.data.frame)
fore.all$h = rep(1L:12L, each = 2) |> rep(4) |> as.integer()

fore.all = fore.all %>% mutate(m = factor(m, levels = c("Real", "RR", "median_cond")))

ggplot(data = fore.all, aes(h, value, linetype = m)) + geom_line() +
  geom_point() +
  scale_x_continuous(breaks =  1:12) +
  scale_linetype_discrete(labels = c("Real", "Reverse Risk", "Conditional Median")) +
  facet_wrap( ~ Method) +
  ylab("Relative Humidity") +
  theme_bw() +
  theme(legend.position = "top", legend.title = element_blank())
