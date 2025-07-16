# 02_reg_pipeline/reg_chi_squared_test.R

reg_chi_squared_test <- function(fire_rescue_clean) {
  # Chi-squared test to examine the association between age_band and extrication
  chisq_test_result <- chisq.test(table(
    fire_rescue_clean$age_band,
    fire_rescue_clean$extrication
  ))
  # Print the results of the Chi-squared test
  print(chisq_test_result)
  # Return the Chi-squared test result
  chisq_test_result
}
