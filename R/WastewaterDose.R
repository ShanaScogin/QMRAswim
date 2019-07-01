
#needed inputs:

###indicator conc range in wastewater
###indicator conc in environmental water
###pathogen concentration range in wastewater
###pathogen dose response function and parameters
###fraction of illnesses given infection for pathogen



WastewaterDose<-function(seed =1,count,min,max,indicatorconc){
  set.seed(seed)#randomness
  indicatorrange<-runif(count,min=b,max=c)
  waterdose<-rlnorm(count,meanlog=2.92,sdlog=1.43) ###exposure volume while swimming event occurs
  WWfrac<-indicatorconc/(10^indicatorrange)
  WWdose<-waterdose/1000*WWfrac
  }
PathogenDose<-function(low,high,doseresp="f1",alpha,beta=0,frac){### low high log conc/L in wastewater pathogen,doseresp can be 1 of 3, alpha beta are dose response parameters, frac is fraction of people who are infected who get sick ect.
  a1<-runif(count,low,high)
  e1<-WWdose*10^a1
  if (doseresp="f1") {
    pi1<- f1(alpha,e1)
  } else{
    if (doseresp="f2") {
      pi1<- f2(alpha, beta, e1)
    }
  } else {
    if (doseresp="f3") {
      pi1<- f3(alpha, beta, e1)
    }
  } else{
    print("Invalid entry for doseresp")
  }
  pill1<-pi1*frac
}

###To calculate probability of infection, use the formula 1-((1-pathogena)*(1-pathogenb)*(1-pathogenc)...)
####probability of infection
f1<-function(x,N){ ##single parameter exponential
  1-exp(-x*N)
}
f2<-function(a,b,N){##two parameter beta-poisson
  1-(1+N/a)^-b
}
f3<-function(a,b,N){###hypergeometric from packageCharFun
  1- hypergeom1F1(-N,a,b)
}

###Total probability of illness
#pill<-1-((1-pill1)*(1-pill2)*(1-pill3)*(1-pill4))

