# Lets see if hlme2 provides us with the same results
library(lcmm)
library(helphlme)
library(callframe)

test_that("hlme2 works outside loop for ng = 1", {
  our_model <- helphlme::hlme2(
    data = data_hlme,
    fixed = Y ~ X1 + X2 + X3,
    mixture = ~ X1 + X2 + X3,
    random = ~1,
    ng = 1,
    idiag = FALSE,
    nwg = FALSE,
    subject = "ID"
  )
  # we do not include nwg = FALSE argument in their model because
  # this argument is removed by hlme2 when n = 1
  their_model <- lcmm::hlme(
    data = data_hlme,
    fixed = Y ~ X1 + X2 + X3,
    random = ~1,
    ng = 1,
    idiag = FALSE,
    subject = "ID"
  )
  nearly_equal <- all.equal(our_model, their_model)
  testthat::expect_true(nearly_equal)
})

test_that("hlme2 works for ng > 1", {
  set.seed(100)
  our_model <- helphlme::hlme2(
    data = data_hlme,
    fixed = Y ~ X1 + X2 + X3,
    mixture = ~ X1 + X2 + X3,
    random = ~1,
    ng = 2,
    idiag = FALSE,
    nwg = FALSE,
    subject = "ID"
  )
  # we do not include nwg = FALSE argument in their model because
  # this argument is removed by hlme2 when n = 1
  set.seed(100)
  betas <- lcmm::hlme(
    data = data_hlme,
    fixed = Y ~ X1 + X2 + X3,
    random = ~1,
    ng = 1,
    idiag = FALSE,
    subject = "ID"

  )
  their_model <- lcmm::hlme(
    data = data_hlme,
    fixed = Y ~ X1 + X2 + X3,
    mixture = ~ X1 + X2 + X3,
    random = ~1,
    ng = 2,
    idiag = FALSE,
    nwg = FALSE,
    subject = "ID",
    B = betas
  )
  nearly_equal <- all.equal(our_model, their_model)
  testthat::expect_true(nearly_equal)
})

