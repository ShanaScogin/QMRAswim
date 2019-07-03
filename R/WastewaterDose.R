#' This function calculates how much wastewater a swimmer is exposed to based on a 
#' single indicator concentration value
#' 
#' @title Dose of Wastewater
#' @description Calculate how much wastewater a swimmer is exposed to based on a single 
#' indicator concentration value. Water dose is from "Water ingestion during swimming 
#' activities in a pool: a pilot study", lognormal distribution with a mean of 2.92 
#' and a standard deviation of 1.43 (Dufor et al. 2006).
#' @param indic_enviro_conc The concentration of the indicator in environmental 
#' waters-the dependent variable in log10 Copies/L
#' @param indic_sewage_dist Which distribution, log uniform: \code{'lunif'}  or 
#' log normal: \code{'lnorm'} the indicator concentration in sewage follows.
#' @param i_alpha First value for distrubution, if lunif, alpha is the min concentration, 
#' for lnorm it is the alpha value.
#' @param i_beta Second value for distribution, if lunif, beta is the max concentration, 
#' for lnorm it is the beta value.
#' @param seed Optional paramter. Sets the seed of R's random number generator, which 
#' is useful for creating simulations that can be reproduced. Default is 1
#' @param count Optional parameter. How many Monte Carlo samplings. Default is 10,000
#' @return Wastewater dose, \code{WWdose}, in Liters. Vector contains  \code{count} 
#' number of samplings.
#' @export
#' @examples
#' \dontrun{
#'}

WastewaterDose <- function(indic_enviro_conc, 
                           indic_sewage_dist = 'lunif',
                           i_alpha,
                           i_beta,
                           seed = 1,
                           count = 10000){
  count <- count
  set.seed(seed)
  if (indic_sewage_dist == 'lunif'){
    Ci_Sew <- runif(count, min=i_alpha, max=i_beta)
  } else {
    if (indic_sewage_dist == 'lnorm'){
      Ci_Sew <- rlnorm(count, i_alpha, i_beta)
    } else {
      stop("Invalid entry for indic_sewage_dist distribution, must be either 'lunif' or 'lnorm'")
    }
  }
  EnvWaterDose <-(rlnorm(count,meanlog = 2.92, sdlog = 1.43))
  fractionWW <-(indic_enviro_conc / 10 ^ Ci_Sew)
  WWdose <-(EnvWaterDose / 1000 * (fractionWW)) 
  ###exposure volume while swimming event occurs in mL changed to L/indicator 
  ###concentration changed divided by the range of concentration in sewage
}





