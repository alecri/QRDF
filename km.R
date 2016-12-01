library(ggplot2)
library(survival)
library(plotly)

surv.data_bio <- summary(survfit(Surv(time, status) ~ 1, data = subset(terapi_basdata, preparat == "Benepali")))
surv.data_all <- summary(survfit(Surv(time, status) ~ 1, data = terapi_basdata))
surv.data <- data.frame()
surv.data <-rbind(
  with(surv.data_bio, data.frame(preparat = "preparat", time, surv, upper, lower)),
  with(surv.data_all, data.frame(preparat = "all", time, surv, upper, lower))
  )


ggplotly(
  #+geom_step(aes(y = lower), linetype = "dotted") +
  #geom_step(aes(y = upper), linetype = "dotted")
)


