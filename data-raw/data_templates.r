# FHD bootstraps
FHD_bootstrap_template <- data.frame(height_m = 0:499, matrix(0, nrow = 500, ncol = 200, dimnames = list(NULL,  paste0("bootId_", 1:200))))

usethis::use_data(FHD_bootstrap_template, overwrite = TRUE)



# FHD estimates
FHD_template <- data.frame(height_m = 0:499, prop = 0)

usethis::use_data(FHD_template, overwrite = TRUE)



# Monthly densities reference point estimates
monthDens_pctls_template <- data.frame(
  Percentiles = c(1, 2.5, 5, 10, 25, 50, 75, 90, 95, 97.5, 99),
  matrix(0, nrow = 11, ncol = 12, dimnames = list(NULL,  month.name))
)

usethis::use_data(monthDens_pctls_template, overwrite = TRUE)



# Monthly densities random samples
monthDens_draws_template <- data.frame(matrix(0, nrow = 1000, ncol = 12, dimnames = list(NULL,  month.name)))

usethis::use_data(monthDens_draws_template, overwrite = TRUE)
