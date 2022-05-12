library(dplyr)
library(ggplot2)

model1 <- helphlme::hlme2(
  data = lcmm::data_hlme,
  fixed = Y ~ Time + X1 + X2,
  mixture = Y ~ Time + X1 + X2,
  random = ~Time,
  subject = "ID",
  ng = 2
)

model2 <- helphlme::hlme2(
  data = lcmm::data_hlme,
  fixed = Y ~ Time + X1 + X2,
  mixture = Y ~ Time + X1 + X2,
  random = ~Time,
  subject = "ID",
  ng = 3
)

model_list <- list(model1 = model1, model2 = model2)



compare_models(model_list)
