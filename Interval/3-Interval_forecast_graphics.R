rm(list = ls())
devtools::load_all() # load the functions
require(tidyverse)
require(extraDistr)
require(reshape2)
require(lubridate)
data(Manaus)# load the data
data = Manaus


results1 = readRDS("method_1.rds")
results2 = readRDS("method_2.rds")

data.sim.1 = read.table("Interval/fore_sim_method_1.txt")
data.sim.2 = read.table("Interval/fore_sim_method_2.txt")


data.sim = rbind(data.sim.1,data.sim.2)
dados = data.sim |>
  group_by(V2, V3) |>
  slice(133:144) |>
  mutate(erro_forecast_median = V5 - V6,
         erro_forecast_rr = V5 - V7)

forecast_median_1 = gkumar_pred_quantile_forecast(results1,quant = 0.5,h = 12)$y.forecast
forecast_rr_1 = gkumar_forecast_mean(results1,h = 12)$y.forecast

forecast_median_2 = gkumar_pred_quantile_forecast(results2,quant = 0.5,h = 12)$y.forecast
forecast_rr_2 = gkumar_forecast_mean(results2,h = 12)$y.forecast


dados$forecast_median = c(rep(forecast_median_1,1000),rep(forecast_median_2,1000))
dados$forecast_rr =c(rep(forecast_rr_1,1000),rep(forecast_rr_2,1000))


dados.resultys  =  dados |> mutate(V3 = as.factor(V3), V4 = as.factor(V4)) |>
  group_by(V2, V4) |>
  reframe(
    y_sim_median = forecast_median + sample(erro_forecast_median , 1000,replace=T),
    y_sim_rr = forecast_rr + sample(erro_forecast_rr , 1000,replace=T),
  ) |>
  group_by(V2, V4) |>
  reframe(
    LI.median = quantile(y_sim_median, 0.025),
    LS.median = quantile(y_sim_median, 0.975),
    LI.rr = quantile(y_sim_rr, 0.025),
    LS.rr = quantile(y_sim_rr, 0.975)
  ) |> mutate(Real = data$RH[133:144] |> rep(2),
              Month = make_date(2020,1:12) |> rep(2),
              forecast_median = c(rep(forecast_median_1,1),rep(forecast_median_2,1)),
              forecast_rr = c(rep(forecast_rr_1,1),rep(forecast_rr_2,1))
             ) |>
  pivot_longer(cols = c(3,5),names_to = "LI_type",values_to = "LI") |>
  pivot_longer(cols = c(3,4),names_to = "LS_type",values_to = "LS") |>
  pivot_longer(cols = c(3,5,6),names_to = "lines",values_to = "real_fore") |>
  mutate(Interval = str_sub(LS_type,4) )
H =c("#4D4D4D","#E6E6E6")
dados.resultys.2 = dados.resultys |> filter(lines!="Real")
dados.resultys.3 = dados.resultys |> filter(lines =="Real")

dados.resultys |> ggplot(aes(x=Month,y= real_fore)) + scale_x_date(date_labels = '%b %y',name = "Date") +
  geom_point(size=1.5) +
  geom_line(aes(linetype = lines),linewidth=0.2) +
  scale_linetype_manual(values=c("longdash", "dotted","solid"),name="Forecast:",labels=c("Median","Reverse Risk","Real"))+
  geom_ribbon(aes(x=Month,ymax=LS,ymin=LI,fill=Interval),color="grey",alpha=0.15)+
  #scale_color_manual(values=H,name="Interval:",labels = c("Median","Reverse Risk"))+
  facet_grid(.~V2)+
  theme_bw() +
  theme(legend.position = "top",legend.direction = "horizontal")+
  ylab("Relative Humidity")+
  scale_fill_discrete(name="Interval:",labels = c("Median","Reverse Risk"))


