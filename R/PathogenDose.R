
##need WWDose fron WastewaterDose.r
#' @title  Probability of Illness from exposure to wastewater
#' @description Calculates the probability of illness for a specific pathogen based the amount of wastewater consumed during the swimming event
#' @param doseresp Choose which dose response model to use. Type \code{"ex"} for single parameter exponential, \code{"bp"} for two parameter beta-poisson, or \code{"f1"} for the two parameter hypergeometric1F1, commonly used for norovirus
#' @param alpha First parameter for the dose response
#' @param beta Second parameter for the dose response. Leave blank if using single paramter exponential dose response
#' @param Pillinf The fraction of infections that become illnesses. If Pillinf is a distribution not a single value, use the function Fraction to generate a monte carlo table. Can be a single value or a monte carlo vector generated from the \code{frac} function. P _ill/inf The probability of illness if infected. Fraction in decimal form. i.e. for norovirus, 60 percent of infections become illnesses, so \code{frac=0.6} (McBride et al. 2013)
#' @param path_sewage_dist The distribution of the pathogen in sewage. Choose either \code{'lunif'} for a uniform distribution or \code{'lnorm'} for a normal distribution
#' @param p_alpha The first parameter value for the distribution of pathogen in sewage. For \code{'lunif'} alpha_sew is the min concentration in log 10 copies/L, for \code{'lnorm'} it is the mean value in log 10 copies/L.
#' @param p_beta The second parameter value for the distribution of pathogen in sewage. For \code{'lunif'} beta_sew is the max concentration in log 10 copies/L, for \code{'lnorm'} it is the standard deviation in log 10 units.
#' @param dose Returned dose from Wastewater Dose function. Defaults to WWdose
#' @return Probability of illness \code{pill1}. Vector contains \code{count} number of samplings. To calculate illness from \code{n} number of pathogens, use the formula totpill=1-((1-pill1)(1-pill2)(1-pill3)(1-pill4)...)
#' @export
Probability_Illness<-function(doseresp='f1',alpha,beta=0,Pillinf,path_sewage_dist='lunif',p_alpha,p_beta,dose=WWdose){### low high log conc/L in wastewater pathogen,doseresp can be 1 of 3, alpha beta are dose response parameters, frac is fraction of people who are infected who get sick ect.
  dose<-WWdose
  if (path_sewage_dist== 'lunif'){
    Cp_Sew<-log(runif(count,min=p_alpha,max=p_beta))
  } else if (path_sewage_dist == 'lnorm'){
    Cp_Sew<-rlnorm(count,p_alpha,p_beta)
  }  else {
    stop("Invalid entry distribution, must be either 'lunif' or 'lnorm'")
  }
  e1<<-WWdose*10^Cp_Sew
  if (doseresp == 'ex')  {
    pi1<<- fu1(alpha,e1)
  } else if (doseresp=='bp') {
    pi1<<- f2(alpha, beta, e1)
  } else if (doseresp=='f1') {
    pi1<<- f3(alpha, beta, e1)
  }  else{
    stop("Invalid entry for doseresp")
  }
  pill1<<-pi1*Pillinf
  return(pill1)
}

fu1<-function(x,N){ ##single parameter exponential
  1-(exp(-x*N))
}
f2<-function(a,b,N){##two parameter beta-poisson
  1-(1+N/a)^-b
}
f3<-function(a,b,N){###hypergeometric from packageCharFun
  1- hypergeom1F1(-N,a,b)
}

###Total probability of illness
###totpill<-1-((1-pill1)*(1-pill2)*(1-pill3)*(1-pill4))

