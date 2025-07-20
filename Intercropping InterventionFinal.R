#### Combined Monoculture vs. Intercropping Simulation ####

# 1) Load required packages
library(decisionSupport)
library(ggplot2)
library(tidyr)


set.seed()   # I could not make this arguments for that function,
#  I did not understand

# runif(5,1,10)

# 2) Read the unified input table
input_data <- read.csv("Input_File.csv", stringsAsFactors = FALSE)

# 3) Convert to estimate object and draw one set of inputs into the environment
estimates <- as.estimate(input_data)

make_variables <- function(est, n = 1) {
  x <- random(rho = est, n = n)
  for (nm in colnames(x)) {
    assign(nm, as.numeric(x[1, nm]), envir = .GlobalEnv)
  }
}

make_variables(estimates, n = 1)

# 4) Define a single model function that computes both scenarios
model_function <- function() {
  
#Mono culture Initial Cost
  
  mono_seed_ts <- vv(Maize_Seeds_Cost, var_CV = var_CV, n = n_years)

#Mono culture recurring Cost  
  mono_recur_val <- Pest_Weed_Management_Mono + Crop_Maintenance_Mono +
                    Irrigation_Mono
  
  pest_mono <- vv(Pest_Weed_Management_Mono, var_CV = var_CV, n = n_years)
  cropm_mono <- vv(Crop_Maintenance_Mono, var_CV = var_CV, n = n_years)
  irr_mono <- vv(Irrigation_Mono, var_CV = var_CV, n = n_years)
  
  mono_recur_ts  <- c( vv(var_mean = mono_recur_val,
                    var_CV = var_CV, n = n_years), 0)
  
  mono_cost_ts   <- mono_seed_ts + pest_mono + cropm_mono + irr_mono
  
  mono_pest_ts    <- chance_event(
    chance       = Pest_Disease_Chance_MY,
    value_if     = Maize_Yield * (1 - Pest_Disease_Effect_MY),
    value_if_not = Maize_Yield,
    n            = n_years )
  
  mono_climate_ts <- chance_event(
    chance       = Extreme_Climate_Chance_MY,
    value_if     = mono_pest_ts * (1 - Extreme_Climate_Events_MY),
    value_if_not = mono_pest_ts,
    n            = n_years )
  
  mono_rev_base_ts <- vv(
    var_mean = mono_climate_ts * Maize_Price,
    var_CV   = var_CV,
    n        = n_years )
  mono_phl_factor  <- chance_event(
    chance       = Post_Harvest_Losses,
    value_if     = 1 - Reduction_Sale_PHL,
    value_if_not = 1,
    n            = n_years )
  mono_mf_factor   <- chance_event(
    chance       = Market_Fluctuation,
    value_if     = 1 - Reduction_Sales_MF,
    value_if_not = 1,
    n            = n_years )
  
  mono_rev_ts      <- mono_rev_base_ts * mono_phl_factor * mono_mf_factor
  
  Cashflow_Monoculture    <- mono_rev_ts - mono_cost_ts #cashflowis vs profit?
  CumCashflow_Monoculture <- cumsum(Cashflow_Monoculture)
  NPV_Monoculture         <- discount(Cashflow_Monoculture,
                                      discount_rate = discount_rate,
                                      calculate_NPV = TRUE)
  
  
  
   
  ## -- Intercropping intervention --
  
  int_seed_ts <- vv(Total_Seeds_Cost, var_CV = var_CV, n = n_years)
  
  ini_int_investment <- Training_Capacity_Int
  
  # Training Capacity reducing from the 2nd year in a relative trend
  int_initial_investment_val <- vv( var_mean      = Training_Capacity_Int,
                                var_CV         = 0,
                                n              = n_years,
                                relative_trend = -40 )
  
  ini_int_cost <- int_initial_investment_val + int_seed_ts
  
  #Intercropping Recurr Start Here
  
  int_recur_val <- Pest_Weed_Management_Int + Crop_Maintenance_Int +
                   Irrigation_Int + Additional_Labor_Int
  
  pest_int <- vv(Pest_Weed_Management_Int, var_CV = var_CV, n = n_years)
  cropm_int <- vv(Crop_Maintenance_Int, var_CV = var_CV, n = n_years)
  irr_int <- vv(Irrigation_Int, var_CV = var_CV, n = n_years)
  addl_int <- vv(Additional_Labor_Int, var_CV = var_CV, n = n_years)
  
  int_recur_ts  <- c( vv(var_mean = int_recur_val, var_CV = var_CV, n = n_years), 0)
  
  int_cost_ts   <- ini_int_cost + pest_int + cropm_int + irr_int + addl_int
  
  
  maize_adj_ts  <- chance_event(
    chance       = Pest_Disease_Chance_MY,
    value_if     = Maize_Yield * (1 - Pest_Disease_Effect_MY),
    value_if_not = Maize_Yield,
    n            = n_years )
  maize_cl_ts   <- chance_event(
    chance       = Extreme_Climate_Chance_MY,
    value_if     = maize_adj_ts * (1 - Extreme_Climate_Events_MY),
    value_if_not = maize_adj_ts,
    n            = n_years )
  cowpea_adj_ts <- chance_event(
    chance       = Pest_Disease_Chance_CY,
    value_if     = Cowpea_Yield * (1 - Pest_Disease_Effect_CY),
    value_if_not = Cowpea_Yield,
    n            = n_years )
  cowpea_cl_ts  <- chance_event(
    chance       = Extreme_Climate_Chance_CY,
    value_if     = cowpea_adj_ts * (1 - Extreme_Climate_Events_CY),
    value_if_not = cowpea_adj_ts,
    n            = n_years )
  yellow_adj_ts <- chance_event(
    chance       = Pest_Disease_Chance_YB,
    value_if     = Yellow_Beans_Yield * (1 - Pest_Disease_Effect_YB),
    value_if_not = Yellow_Beans_Yield,
    n            = n_years )
  yellow_cl_ts  <- chance_event(
    chance       = Extreme_Climate_Chance_YB,
    value_if     = yellow_adj_ts * (1 - Extreme_Climate_Events_YB),
    value_if_not = yellow_adj_ts,
    n            = n_years)
  
  int_rev_base_ts <- vv(
    var_mean = (maize_cl_ts * Maize_Price) +
      (cowpea_cl_ts * Cowpea_Price) +
      (yellow_cl_ts * Yellow_Beans_Price),
    var_CV   = var_CV,
    n        = n_years )
  int_phl_factor  <- chance_event(
    chance       = Post_Harvest_Losses,
    value_if     = 1 - Reduction_Sale_PHL,
    value_if_not = 1,
    n            = n_years )
  int_mf_factor   <- chance_event(
    chance       = Market_Fluctuation,
    value_if     = 1 - Reduction_Sales_MF,
    value_if_not = 1,
    n            = n_years )
  int_rev_ts      <- int_rev_base_ts * int_phl_factor * int_mf_factor
  
  Cashflow_Intercropping    <- int_rev_ts - int_cost_ts
  CumCashflow_Intercropping <- cumsum(Cashflow_Intercropping)
  NPV_Intercropping         <- discount(Cashflow_Intercropping,
                                        discount_rate = discount_rate,
                                        calculate_NPV = TRUE)
  
  
  # return both scenarios
  # — Decision difference —
  Cashflow_Decision    <- Cashflow_Intercropping - Cashflow_Monoculture
  CumCashflow_Decision <- cumsum(Cashflow_Decision)
  NPV_Decision         <- NPV_Intercropping - NPV_Monoculture
  
  # 5) Return everything
  return(list(
    NPV_Monoculture        = NPV_Monoculture,
    NPV_Intercropping      = NPV_Intercropping,
    NPV_Decision           = NPV_Decision,
    Cashflow_Monoculture   = Cashflow_Monoculture,
    Cashflow_Intercropping = Cashflow_Intercropping,
    Cashflow_Decision      = Cashflow_Decision,
    CumCashflow_Monoculture   = CumCashflow_Monoculture,
    CumCashflow_Intercropping = CumCashflow_Intercropping,
    CumCashflow_Decision      = CumCashflow_Decision
  ))
}

# 5) Run a single Monte Carlo simulation for both scenarios
combined_simulation <- mcSimulation(
  estimate          = estimates,
  model_function    = model_function,
  numberOfModelRuns = 10000,
  functionSyntax    = "plainNames"
)

# 6) Plot both NPV distributions together

plot_distributions(
  mcSimulation_object = combined_simulation,
  vars                = c("NPV_Monoculture","NPV_Intercropping"),
  method              = "hist_simple_overlay", base_size = 7
)
plot_distributions(
  mcSimulation_object = combined_simulation,
  vars                = c("NPV_Monoculture","NPV_Intercropping"),
  method              = "smooth_simple_overlay", base_size = 7
)



# 1) Combine inputs and both NPVs into one data frame
df_evpi <- data.frame(
  combined_simulation$x,
  NPV_Monoculture   = combined_simulation$y[, "NPV_Monoculture"],
  NPV_Intercropping = combined_simulation$y[, "NPV_Intercropping"]
)

# 2) Calculate EVPI for all inputs, starting at Monoculture_NPV
EVPI_results <- multi_EVPI(
  mc            = df_evpi,
  first_out_var = "NPV_Monoculture"
)

# 3) Inspect the EVPI table
print(EVPI_results)

# 4) Plot EVPI for both scenarios side by side
plot_evpi(
  EVPIresults   = EVPI_results,
  decision_vars = c("NPV_Monoculture", "NPV_Intercropping")
)



#### Cashflow (Annual Profit) Comparison for Both Scenarios ####

# 1) Plot the annual profit (“cashflow”) time series for both scenarios
plot_cashflow(
  mcSimulation_object = combined_simulation,
  cashflow_var_name   = c("Cashflow_Monoculture", "Cashflow_Intercropping"),
  x_axis_name         = "Year",
  y_axis_name         = "Annual Profit (USD/ha)",
  color_25_75         = "blue",
  color_5_95          = "lightblue",
  color_median        = "darkblue",
  facet_labels        = c("Monoculture", "Intercropping")
)

# 2) Identify the exact column names for Year-n_years
mono_cols <- grep("^Cashflow_Monoculture\\.", names(combined_simulation$y), value = TRUE)
int_cols  <- grep("^Cashflow_Intercropping\\.", names(combined_simulation$y), value = TRUE)

final_year_col_mono <- mono_cols[grepl(paste0("\\.", n_years, "$"), mono_cols)][1]
final_year_col_int  <- int_cols[ grepl(paste0("\\.", n_years, "$"), int_cols)][1]

# 3) Extract those columns as numeric vectors
mono_final <- combined_simulation$y[[final_year_col_mono]]
int_final  <- combined_simulation$y[[final_year_col_int ]]

# 4) Compute the 30th and 70th percentiles for Year-n_years
mono_q <- quantile(mono_final, probs = c(0.3, 0.7))
int_q  <- quantile(int_final,  probs = c(0.3, 0.7))

# 5) Print the results
cat("Monoculture Year", n_years, "Profit 30th & 70th percentiles:\n")
print(mono_q)
cat("\nIntercropping Year", n_years, "Profit 30th & 70th percentiles:\n")
print(int_q)


#### Additional Result Graphs ####

# 1) Side-by-side boxplots of NPV
npv_df <- data.frame(
  Monoculture   = combined_simulation$y[, "NPV_Monoculture"],
  Intercropping = combined_simulation$y[, "NPV_Intercropping"]
)
npv_long <- pivot_longer(
  npv_df,
  cols      = everything(),
  names_to  = "Scenario",
  values_to = "NPV"
)
ggplot(npv_long, aes(x = Scenario, y = NPV, fill = Scenario)) +
  geom_boxplot(alpha = 0.6) +
  theme_minimal(base_size = 14) +
  labs(
    title = "NPV Distribution: Monoculture vs. Intercropping",
    x     = NULL,
    y     = "Net Present Value (USD/ha)"
  ) +
  scale_fill_manual(values = c("grey40", "forestgreen")) +
  theme(legend.position = "none")
if (!require("pls")) install.packages("pls")
library(pls)
# Create a data frame with all input variables and the NPV difference as response
pls_data <- data.frame(
  combined_simulation$x,  # All input variables
  NPV_Difference = combined_simulation$y[, "NPV_Decision"]  # Response variable
)
# 3) Run PLS regression
pls_model <- plsr(
  NPV_Difference ~ .,  # Model formula: NPV difference explained by all inputs
  data = pls_data,
  scale = TRUE,       # Standardize variables
  validation = "CV",  # Cross-validation
  segments = 10       # Number of cross-validation segments
)
 #Determine optimal number of components
# Plot cross-validated RMSEP to choose number of components
plot(RMSEP(pls_model), legendpos = "topright", main = "Cross-Validated RMSEP")
# 5) Summary of the PLS model
summary(pls_model)
# 1) Install and load required packages
if (!require("pls")) install.packages("pls")
if (!require("vip")) install.packages("vip")  # For VIP scores
if (!require("ggplot2")) install.packages("ggplot2")

library(pls)
library(vip)
library(ggplot2)
# 2) Prepare data for PLS analysis
pls_data <- data.frame(
  combined_simulation$x,  # All input variables
  NPV_Difference = combined_simulation$y[, "NPV_Decision"]  # Response variable
)
#3) Run PLS regression
pls_model <- plsr(
  NPV_Difference ~ .,  
  data = pls_data,
  scale = TRUE,       
  validation = "CV",  
  segments = 10       
)
# Plot cross-validated RMSEP
plot(RMSEP(pls_model), legendpos = "topright", main = "Cross-Validated RMSEP")
# Select number of components with lowest RMSEP
optimal_comp <- which.min(RMSEP(pls_model)$val[1,,]) - 1


# 5) Calculate VIP scores manually (alternative if vip package still has issues)
vip_scores <- function(object) {
  if (object$method != "oscorespls")
    stop("Only implemented for orthogonal scores algorithm")
  if (nrow(object$Yloadings) > 1)
    stop("Only implemented for single-response models")
  
  SS <- c(object$Yloadings)^2 * colSums(object$scores^2)
  Wnorm2 <- colSums(object$loading.weights^2)
  SSW <- sweep(object$loading.weights^2, 2, SS / Wnorm2, "*")
  vip <- sqrt(nrow(SSW) * apply(SSW, 1, cumsum) / cumsum(SS))
  t(vip)
}
# Calculate VIP scores
vip_values <- vip_scores(pls_model)[optimal_comp,]
vip_df <- data.frame(
  Variable = names(vip_values),
  VIP_Score = vip_values
)





#### Robust PLS Analysis for Key Drivers of NPV Difference ####

# 1) Load required packages (install first if needed)
if (!require("pls")) install.packages("pls")
if (!require("ggplot2")) install.packages("ggplot2")
library(pls)
library(ggplot2)

# 2) Prepare data for PLS analysis
pls_data <- data.frame(
  combined_simulation$x,  # All input variables
  NPV_Difference = combined_simulation$y[, "NPV_Decision"]  # Response variable
)

# 3) Run PLS regression
pls_model <- plsr(
  NPV_Difference ~ .,
  data = pls_data,
  scale = TRUE,
  validation = "CV",
  segments = 10
)

# 4) Determine optimal number of components
# Plot cross-validated RMSEP
validationplot(pls_model, val.type = "RMSEP", legendpos = "topright")
optimal_comp <- which.min(RMSEP(pls_model)$val[1,,]) - 1
cat("Optimal number of components:", optimal_comp, "\n")

# 5) Calculate VIP scores manually (more reliable implementation)
calculate_vip <- function(pls_model) {
  # Get the weights
  w <- pls_model$loading.weights
  # Get the scores
  s <- pls_model$scores
  # Get the Y-loadings
  c <- pls_model$Yloadings
  
  # Calculate squared Y-variances
  ss <- c^2 * colSums(s^2)
  
  # Calculate VIP scores
  vip_score <- sqrt(nrow(w) * apply(sweep(w^2, 2, ss / colSums(w^2), "*"), 1, sum) / sum(ss))
  
  names(vip_score) <- rownames(w)
  return(vip_score)
}

# Get VIP scores for optimal components
vip_scores <- calculate_vip(pls_model)
vip_df <- data.frame(
  Variable = names(vip_scores),
  VIP_Score = vip_scores,
  row.names = NULL
)

# Sort by VIP score
vip_df <- vip_df[order(-vip_df$VIP_Score), ]

# 6) Plot VIP scores (top 15 variables)
ggplot(head(vip_df, 15), aes(x = reorder(Variable, VIP_Score), y = VIP_Score)) +
  geom_col(fill = "steelblue", width = 0.7) +
  coord_flip() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(title = "Variable Importance in Projection (VIP) Scores",
       subtitle = paste("Optimal components:", optimal_comp),
       x = "Input Variables",
       y = "VIP Score") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

# 7) Get and plot coefficients
coefficients <- coef(pls_model, ncomp = optimal_comp)
coef_df <- data.frame(
  Variable = rownames(coefficients),
  Coefficient = as.numeric(coefficients),
  row.names = NULL
)

# Sort by absolute coefficient value and take top 15
coef_df <- coef_df[order(-abs(coef_df$Coefficient)), ]
coef_df <- head(coef_df, 15)

# Plot coefficients
ggplot(coef_df, aes(x = reorder(Variable, Coefficient), y = Coefficient)) +
  geom_col(aes(fill = Coefficient > 0), width = 0.7) +
  scale_fill_manual(values = c("tomato", "steelblue")) +
  coord_flip() +
  labs(title = "PLS Regression Coefficients",
       subtitle = paste("For", optimal_comp, "components"),
       x = "Input Variables",
       y = "Coefficient Value") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")
# Load libraries
library(pls)
library(plsVarSel)
library(decisionSupport)

# Run MC simulation (if not done already)
mc_sim <- mcSimulation(
  estimate = read.csv("Input_File.csv"),
  model_function = decision_function,
  numberOfModelRuns = 10000,
  functionSyntax = "plainNames"
)

# Prepare data
X <- as.data.frame(mc_sim$x)
Y <- as.data.frame(mc_sim$y)$NPV_intercropping

# Run PLS
pls_model <- plsr(Y ~ ., data = X, scale = TRUE, validation = "CV")

# Explore results
summary(pls_model)
explvar(pls_model)
plot(RMSEP(pls_model), legendpos = "topright")
plot(scores(pls_model), main = "Scores Plot")
plot(loadings(pls_model), main = "Loadings Plot")

# VIP scores
VIP(pls_model)





# Load required packages
if (!require("pls")) install.packages("pls")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
library(pls)
library(ggplot2)
library(dplyr)

# Prepare data - combine inputs with NPV difference outcome
pls_data <- data.frame(
  combined_simulation$x,
  NPV_Difference = combined_simulation$y[,"NPV_Decision"]
)

# Remove any columns with zero variance (constants)
pls_data <- pls_data[, apply(pls_data, 2, var) > 0]

# Run PLS regression with 10-fold cross-validation
set.seed(123) # For reproducibility
pls_model <- plsr(
  NPV_Difference ~ .,
  data = pls_data,
  scale = TRUE,       # Standardize variables
  validation = "CV",  # Cross-validation
  segments = 10,      # Number of CV folds
  jackknife = TRUE    # For coefficient statistics
)

# Determine optimal number of components
validationplot(pls_model, val.type = "RMSEP", legendpos = "topright")
optimal_comp <- which.min(pls_model$validation$PRESS[1,]) - 1
cat("Optimal number of PLS components:", optimal_comp, "\n")

## VIP Score Calculation and Visualization
# Robust VIP calculation function
calculate_vip <- function(pls_model) {
  # Extract model components
  W <- pls_model$loading.weights
  T <- pls_model$scores
  Q <- pls_model$Yloadings
  
  # Calculate VIP scores
  SS <- c(Q)^2 * colSums(T^2)
  Wnorm2 <- colSums(W^2)
  SSW <- sweep(W^2, 2, SS / Wnorm2, "*")
  vip_scores <- sqrt(nrow(SSW) * rowSums(SSW) / sum(SS))
  
  names(vip_scores) <- rownames(W)
  return(vip_scores)
}

# Calculate and sort VIP scores
vip_scores <- calculate_vip(pls_model)
vip_df <- data.frame(
  Variable = names(vip_scores),
  VIP = vip_scores,
  row.names = NULL
) %>% arrange(desc(VIP))

# Plot VIP scores
ggplot(head(vip_df, 15), aes(x = reorder(Variable, VIP), y = VIP)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(title = "Variable Importance in Projection (VIP) Scores",
       subtitle = paste("Top 15 influential variables for", optimal_comp, "PLS components"),
       x = "",
       y = "VIP Score") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

## PLS Coefficients Analysis
# Get coefficients with jackknife statistics
coef_stats <- jack.test(pls_model, ncomp = optimal_comp)
coef_df <- data.frame(
  Variable = rownames(coef_stats$coefficients),
  Coefficient = coef_stats$coefficients[,1],
  Std.Error = coef_stats$sd[,1],
  t.value = coef_stats$tvalues[,1],
  p.value = coef_stats$pvalues[,1]
) %>% 
  mutate(Significance = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01 ~ "**",
    p.value < 0.05 ~ "*",
    TRUE ~ ""
  ))

# Sort by absolute coefficient value
coef_df <- coef_df %>% 
  arrange(desc(abs(Coefficient))) %>% 
  head(15)

# Plot coefficients with error bars
ggplot(coef_df, aes(x = reorder(Variable, Coefficient), y = Coefficient)) +
  geom_col(aes(fill = Coefficient > 0), alpha = 0.8) +
  geom_errorbar(aes(ymin = Coefficient - 1.96*Std.Error, 
                    ymax = Coefficient + 1.96*Std.Error),
                width = 0.2) +
  geom_text(aes(label = Significance, 
                y = ifelse(Coefficient > 0, 
                           Coefficient + 1.96*Std.Error + 0.1,
                           Coefficient - 1.96*Std.Error - 0.1)),
            size = 5) +
  scale_fill_manual(values = c("tomato", "steelblue")) +
  coord_flip() +
  labs(title = "PLS Regression Coefficients with 95% Confidence Intervals",
       subtitle = paste("For", optimal_comp, "PLS components"),
       x = "",
       y = "Coefficient Value") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

## Score and Loading Plots (Component Analysis)
# Extract scores and loadings
scores <- scores(pls_model)[,1:2]
loadings <- loadings(pls_model)[,1:2]

# Create biplot
par(mfrow = c(1,1))
biplot(scores, loadings, 
       main = "PLS Component Biplot",
       xlab = "Component 1",
       ylab = "Component 2",
       col = c("blue", "red"))

# MONOCULTURE PLS ANALYSIS (SINGLE SCRIPT)
# Load required packages
if (!require("pls")) install.packages("pls")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
library(pls)
library(ggplot2)
library(dplyr)

# 1. Prepare Monoculture Data
mono_data <- data.frame(
  combined_simulation$x,
  NPV_Monoculture = combined_simulation$y[,"NPV_Monoculture"]
) %>% 
  select(-contains("Cowpea"), -contains("Yellow_Beans"), -contains("Int")) %>%
  select(where(~var(.) > 0))  # Remove constant columns

# 2. Run PLS Regression
set.seed(123)
pls_mono <- plsr(
  NPV_Monoculture ~ .,
  data = mono_data,
  scale = TRUE,
  validation = "CV",
  segments = 10,
  jackknife = TRUE
)

# 3. Determine Optimal Components
optimal_comp <- which.min(pls_mono$validation$PRESS[1,]) - 1
cat("Optimal PLS components:", optimal_comp, "\n")

# 4. VIP Score Calculation
W <- pls_mono$loading.weights
T <- pls_mono$scores
Q <- pls_mono$Yloadings
SS <- c(Q)^2 * colSums(T^2)
vip_scores <- sqrt(nrow(W) * rowSums(sweep(W^2, 2, SS / colSums(W^2), "*")) / sum(SS))
vip_df <- data.frame(Variable = names(vip_scores), VIP = vip_scores) %>% 
  arrange(desc(VIP))

# 5. Coefficient Analysis
coef_df <- data.frame(
  Variable = rownames(pls_mono$coefficients),
  Coefficient = pls_mono$coefficients[,,optimal_comp],
  row.names = NULL
) %>% arrange(desc(abs(Coefficient)))

# 6. Visualizations
# VIP Plot
ggplot(head(vip_df, 15), aes(x = reorder(Variable, VIP), y = VIP)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "tomato") +
  labs(title = "Top Drivers of Monoculture Profitability",
       subtitle = paste("VIP Scores |", optimal_comp, "Components"),
       x = "", y = "VIP Score") +
  coord_flip() +
  theme_minimal(base_size = 12)

# Coefficient Plot
ggplot(head(coef_df, 15), aes(x = reorder(Variable, Coefficient), y = Coefficient)) +
  geom_col(aes(fill = Coefficient > 0), alpha = 0.8) +
  scale_fill_manual(values = c("steelblue", "tomato")) +
  labs(title = "Impact Direction on NPV",
       x = "", y = "Coefficient Value") +
  coord_flip() +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

# 7. Print Key Results
cat("\n=== TOP 5 PROFIT DRIVERS ===\n")
print(head(vip_df, 5))

cat("\n=== TOP COST RISKS ===\n")
coef_df %>% 
  filter(Coefficient < 0) %>% 
  head(5) %>% 
  print()
