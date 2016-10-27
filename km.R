library(ggplot2)
library(survival)
library(plotly)

fit <- survfit(Surv(time, status) ~ sex, data = lung)
fit1 <- survfit(Surv(time, status) ~ 1, data = subset(lung, sex == 1))
fit2 <- survfit(Surv(time, status) ~ 1, data = subset(lung, sex == 2))

surv.data1 <- summary(fit1)
surv.data2 <- summary(fit2)
surv.data <- data.frame()

surv.data <-rbind(
  with(surv.data1, data.frame(female = "female", time, surv, upper, lower)),
  with(surv.data2, data.frame(female = "male", time, surv, upper, lower))
  )

ggplotly(
  ggplot(surv.data, aes(x = time, y = surv, col = female)) + 
    geom_step() + 
    geom_step(aes(y = lower), linetype = "dotted") +
    geom_step(aes(y = upper), linetype = "dotted")
  )