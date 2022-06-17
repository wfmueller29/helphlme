library(lcmm)

testthat::test_that("prep_hlme works with centering data", {
                      data <- lcmm::data_hlme
                      m_time <- mean(data$Time)
                      data$Time_ns <- data$time
                      data$Time <- data$Time - m_time
                      data_test <- helphlme::prep_hlme(df = lcmm::data_hlme, 
                                          vars = "Time",
                                          center = TRUE,
                                          scale = FALSE
                      )

                      test <- setequal(data$Time,  data_test$Time)

                      testthat::expect_true(test)
})


testthat::test_that("prep_hlme works with centering data and ref_data", {
                      data <- lcmm::data_hlme
                      m_time <- mean(data$Time)
                      data$Time_ns <- data$time
                      data$Time <- data$Time - m_time
                      data_test <- data[1:20,]
                      data_test <- helphlme::prep_hlme(df = data_test, 
                                          vars = "Time",
                                          center = TRUE,
                                          scale = FALSE,
                                          ref_data = data
                      )

                      test <- all.equal(data$Time[1:20],  data_test$Time)

                      testthat::expect_true(test)
})
