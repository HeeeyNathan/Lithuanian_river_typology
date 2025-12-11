# ===============================================================================
# SUPPLEMENTARY FUNCTIONS AND DOCUMENTATION
# ===============================================================================
#
# This file provides:
#   1. Detailed explanation of the Tisseyre et al. (2018) methodology
#   2. Helper functions for diagnostics
#   3. Cross-validation functions for block kriging
#   4. Guidance on interpreting results
#
# ===============================================================================

# ===============================================================================
# METHODOLOGY EXPLANATION
# ===============================================================================
#
# The Tisseyre et al. (2018) methodology addresses: "How to define the optimal
# grid size to map high resolution spatial data?"
#
# KEY CONCEPTS:
#
# 1. VARIOGRAM DECOMPOSITION
#    - Total variance = Nugget (C0) + Partial Sill (C1)
#    - Nugget (C0): Random noise, measurement error, micro-scale variation
#    - Partial Sill (C1): Spatially structured variance (the "signal")
#    - Range (a): Distance at which spatial autocorrelation approaches zero
#
# 2. CHANGE OF SUPPORT
#    When aggregating point data (Zp) into blocks of size v (Zv):
#    - Nugget decreases: C0v = C0p / (r * L²)
#      where r = spatial resolution (points/km²), L = grid side length
#    - Structured variance also decreases: C1v = C1p * exp(-L/(2a))
#      (for exponential variogram)
#
# 3. OPTIMISATION OBJECTIVE
#    Find grid size L that maximizes:
#      fv = PNR + PS
#    where:
#      PNR = (C0p - C0v) / C0p  (proportion of noise removed)
#      PS = C1v / C1p            (proportion of signal retained)
#
#    The optimal L balances:
#    - Removing noise (wants larger L)
#    - Retaining spatial structure (wants smaller L)
#
# 4. SIGNAL-TO-NOISE RATIO
#    S/N = C1v / C0v
#    Cambardella et al. (1994) suggest S/N > 3 indicates strong spatial structure
#
# APPLICATION TO YOUR DATA:
#
# Your Lithuanian river data has:
# - ~170 sites across ~65,000 km² → very sparse (~0.003 pts/km²)
# - Average site spacing of ~100-150 km
# - This is VERY different from the paper's precision agriculture context
#   (>4000 pts/ha = 0.0004 km spacing)
#
# IMPORTANT CONSIDERATIONS:
# - With such sparse data, optimal grid sizes will be much larger
# - The variogram range should reflect the spatial scale of your process
# - Block kriging is still valid, but interpret results carefully
# - Consider whether your spatial resolution can support the analysis
#
# ===============================================================================

# ===============================================================================
# CROSS-VALIDATION FOR BLOCK KRIGING
# ===============================================================================

#' Leave-one-out cross-validation for kriging
#' @param data Data frame with Xkm, Ykm, and response variable
#' @param var_name Name of variable
#' @param vgm_model Variogram model
#' @param block_size Block size (NULL for point kriging)
#' @return Data frame with observed, predicted, and error statistics

krige_loocv <- function(data, var_name, vgm_model, block_size = NULL) {

 # Prepare data
 obs_data <- data |>
   dplyr::select(Xkm, Ykm, value = !!sym(var_name)) |>
   filter(!is.na(value))

 n <- nrow(obs_data)
 predictions <- numeric(n)
 variances <- numeric(n)

 for (i in 1:n) {
   # Training data (all except point i)
   train_data <- obs_data[-i, ]
   coordinates(train_data) <- ~ Xkm + Ykm

   # Test point
   test_point <- obs_data[i, ]
   coordinates(test_point) <- ~ Xkm + Ykm

   tryCatch({
     if (is.null(block_size)) {
       # Point kriging
       krige_result <- krige(
         formula = value ~ 1,
         locations = train_data,
         newdata = test_point,
         model = vgm_model,
         debug.level = 0
       )
     } else {
       # Block kriging
       krige_result <- krige(
         formula = value ~ 1,
         locations = train_data,
         newdata = test_point,
         model = vgm_model,
         block = c(block_size, block_size),
         debug.level = 0
       )
     }
     predictions[i] <- krige_result$var1.pred
     variances[i] <- krige_result$var1.var
   }, error = function(e) {
     predictions[i] <- NA
     variances[i] <- NA
   })
 }

 # Calculate statistics
 observed <- obs_data$value
 residuals <- observed - predictions

 result <- data.frame(
   observed = observed,
   predicted = predictions,
   variance = variances,
   residual = residuals,
   std_residual = residuals / sqrt(variances)
 )

 # Summary statistics
 attr(result, "summary") <- list(
   n = n,
   ME = mean(residuals, na.rm = TRUE),           # Mean Error (bias)
   RMSE = sqrt(mean(residuals^2, na.rm = TRUE)), # Root Mean Square Error
   MAE = mean(abs(residuals), na.rm = TRUE),     # Mean Absolute Error
   R2 = cor(observed, predictions, use = "complete.obs")^2,
   MSDR = mean((residuals^2) / variances, na.rm = TRUE)  # Should be ~1
 )

 return(result)
}

#' Compare point kriging vs block kriging via cross-validation
compare_kriging_methods <- function(data, var_name, vgm_model, block_sizes = c(5, 10, 20)) {

 results <- list()

 # Point kriging
 cat("Running point kriging CV...\n")
 cv_point <- krige_loocv(data, var_name, vgm_model, block_size = NULL)
 results[["point"]] <- attr(cv_point, "summary")
 results[["point"]]$method <- "Point kriging"
 results[["point"]]$block_size <- 0

 # Block kriging at different sizes
 for (bs in block_sizes) {
   cat(paste0("Running block kriging CV (", bs, " km)...\n"))
   cv_block <- krige_loocv(data, var_name, vgm_model, block_size = bs)
   results[[paste0("block_", bs)]] <- attr(cv_block, "summary")
   results[[paste0("block_", bs)]]$method <- paste0("Block kriging (", bs, " km)")
   results[[paste0("block_", bs)]]$block_size <- bs
 }

 # Combine results
 comparison <- do.call(rbind, lapply(results, as.data.frame))
 rownames(comparison) <- NULL

 return(comparison)
}

# ===============================================================================
# DIAGNOSTIC PLOTS
# ===============================================================================

#' Plot cross-validation results
plot_cv_results <- function(cv_result) {

 summary_stats <- attr(cv_result, "summary")

 p1 <- ggplot(cv_result, aes(x = observed, y = predicted)) +
   geom_point(alpha = 0.6) +
   geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
   labs(
     title = "Observed vs Predicted",
     subtitle = paste0("R² = ", round(summary_stats$R2, 3),
                       ", RMSE = ", round(summary_stats$RMSE, 3)),
     x = "Observed", y = "Predicted"
   ) +
   theme_minimal()

 p2 <- ggplot(cv_result, aes(x = predicted, y = residual)) +
   geom_point(alpha = 0.6) +
   geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
   labs(
     title = "Residuals vs Predicted",
     subtitle = paste0("ME = ", round(summary_stats$ME, 4)),
     x = "Predicted", y = "Residual"
   ) +
   theme_minimal()

 p3 <- ggplot(cv_result, aes(x = std_residual)) +
   geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
   geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
   labs(
     title = "Standardised Residuals",
     subtitle = paste0("MSDR = ", round(summary_stats$MSDR, 3), " (should be ~1)"),
     x = "Standardised Residual", y = "Count"
   ) +
   theme_minimal()

 p4 <- ggplot(cv_result, aes(sample = std_residual)) +
   stat_qq() +
   stat_qq_line(color = "red") +
   labs(
     title = "Q-Q Plot",
     x = "Theoretical Quantiles", y = "Sample Quantiles"
   ) +
   theme_minimal()

 combined <- (p1 | p2) / (p3 | p4)

 return(combined)
}

# ===============================================================================
# ALTERNATIVE: USING geoR PACKAGE
# ===============================================================================
#
# The geoR package provides more control over variogram fitting and kriging.
# Here's an alternative implementation using geoR:
#
# library(geoR)
#
# # Convert data to geodata object
# geo_data <- as.geodata(
#   data[, c("Xkm", "Ykm", "temp_annual")],
#   coords.col = 1:2,
#   data.col = 3
# )
#
# # Fit variogram using ML/REML
# vario_fit <- likfit(
#   geo_data,
#   ini.cov.pars = c(var(geo_data$data), 50),  # Initial sill and range
#   nugget = 0.5 * var(geo_data$data),
#   cov.model = "exponential"
# )
#
# # Block kriging
# pred_grid <- expand.grid(
#   Xkm = seq(xmin, xmax, by = grid_size),
#   Ykm = seq(ymin, ymax, by = grid_size)
# )
#
# kc <- krige.conv(
#   geo_data,
#   loc = pred_grid,
#   krige = krige.control(
#     type.krige = "OK",  # Ordinary kriging
#     cov.pars = vario_fit$cov.pars,
#     nugget = vario_fit$nugget,
#     cov.model = "exponential"
#   ),
#   output = output.control(
#     n.pred = nrow(pred_grid),
#     quantile = NULL,
#     simulations.predictive = NULL
#   )
# )
#
# ===============================================================================

# ===============================================================================
# INTERPRETATION GUIDANCE
# ===============================================================================
#
# VARIOGRAM INTERPRETATION:
#
# 1. Nugget effect (C0):
#    - High nugget → large measurement error or fine-scale variation
#    - Low nugget → measurements are precise, variation is smooth
#    - Nugget ratio = C0 / (C0 + C1)
#      * < 25%: Strong spatial dependence
#      * 25-75%: Moderate spatial dependence
#      * > 75%: Weak spatial dependence
#
# 2. Range (practical range):
#    - Distance beyond which observations are essentially uncorrelated
#    - If range << average site spacing: poor spatial resolution
#    - If range >> study extent: trend, not stationarity
#
# 3. Sill (C0 + C1):
#    - Total variance (should match sample variance approximately)
#    - If sill < sample variance: trend present
#    - If sill > sample variance: outliers or non-stationarity
#
# OPTIMAL GRID SIZE INTERPRETATION:
#
# 1. Very small L_opt (< 5 km):
#    - Strong spatial structure
#    - High data resolution
#    - Fine-scale patterns preserved
#
# 2. Large L_opt (> 20 km):
#    - Weak spatial structure OR
#    - Low data resolution OR
#    - Large nugget effect
#
# 3. L_opt close to practical range:
#    - Warning: losing most spatial structure
#    - Consider if aggregation is appropriate
#
# SIGNAL-TO-NOISE RATIO:
#
# - S/N > 3: Strong spatial signal, good for mapping
# - S/N 1-3: Moderate signal, proceed with caution
# - S/N < 1: Noise dominates, spatial interpolation may not be meaningful
#
# FOR YOUR DATA:
#
# Given your sparse sampling (~170 sites, ~130 km spacing):
# - Expect large optimal grid sizes (likely 20-50 km)
# - S/N ratios may be low
# - Consider whether block kriging adds value over simpler methods
# - Validate results with cross-validation
# - Be cautious about interpreting fine-scale patterns
#
# ===============================================================================

# ===============================================================================
# PRACTICAL RECOMMENDATIONS
# ===============================================================================
#
# 1. VARIOGRAM FITTING:
#    - Use multiple years pooled for robust variogram estimation
#    - Check if year-specific variograms are similar
#    - Consider anisotropy (directional effects)
#
# 2. GRID SIZE SELECTION:
#    - Use Tisseyre method as starting point
#    - Validate with cross-validation
#    - Consider practical constraints (management units, etc.)
#
# 3. TEMPORAL AVERAGING:
#    - Your approach of annual kriging + temporal averaging is sound
#    - Accounts for inter-annual variability
#    - Provides uncertainty estimates (CV across years)
#
# 4. VALIDATION:
#    - Always perform cross-validation
#    - Check RMSE, R², and MSDR
#    - Compare with simpler methods (e.g., IDW)
#
# 5. ALTERNATIVES TO CONSIDER:
#    - Universal kriging (if covariates like elevation matter)
#    - Regression kriging (combine regression + kriging of residuals)
#    - Gaussian process regression (more flexible covariance)
#
# ===============================================================================
