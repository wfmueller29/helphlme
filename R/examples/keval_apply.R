library(dplyr)
library(ggplot2)

prep_data <- helphlme::prep_hlme2(lcmm::data_hlme,
                             vars = c("Time"),
                             center = FALSE,
                             scale = FALSE)

model1 <- helphlme::hlme2(
  data = prep_data,
  fixed = Y ~ Time + X1 + X2,
  mixture = ~ Time + X1 + X2,
  random = ~Time,
  subject = "ID",
  ng = 2
)

model2 <- helphlme::hlme2(
  data = prep_data,
  fixed = Y ~ Time + X1 + X2,
  mixture = ~ Time + X1 + X2,
  random = ~Time,
  subject = "ID",
  ng = 3
)

model_list <- list(model1 = model1, model2 = model2)

pred <- lcpred(prep_data, age_vars = "Time", fixcov = c("X1" = 0, "X2" = 0))

keval_apply(pred, model_list, age = "Time")

