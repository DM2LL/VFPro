# Updated by Siamak Yousefi, 5/23/2018
# University of Tennessee Health Science Center (UTHSC)
# This program is an implementation of VFPro (based on Gaussian mixxture modeling expectation maximization and statisticsl progression of patterns (POP) 
# Two sample eyes are selected from Rotterdsm dataset, one eye is progressing based on VFPro, the other not
# please send any comment to siamak.yousefi@uthsc.edu
# If you are using this package/program, please cite at least two of the following articles

# Yousefi S, M. Balasubramanian, M.H. Goldbaum, F.A. Medeiros, L.M. Zangwill, R.N. Weinreb, J.M. Liebmann, C.A. Girkin, and C. Bowd, 
# "Unsupervised Gaussian mixture model with expectation maximization for detecting glaucomatous progression in standard automated perimetry visual fields," 
# Translational Vision Science and Technology (TVST), vol. 5, no. 3, pp. 1-19, 2016.

# Yousefi S, Goldbaum, M.H., Balasubramanian, M., Jung, T.P., Weinreb, R.N., Medeiros, F.A., Zangwill, L.M., Liebmann, J.M., Girkin, C.A., and Bowd, C., 
# "Glaucoma progression detection using structural retinal nerve fiber layer measurements and functional visual field points,"
# IEEE transactions on Biomedical Engineering TBME, vol. 61, pp. 1143-1154, 2014.

# Yousefi S, Goldbaum MH, Balasubramanian M Weinreb RN, Medeiros FA, Zangwill LM, Liebmann JM, Girkin CA, and Bowd C, 
# "Learning from data: recognizing glaucomatous defect patterns and detecting progression from visual field measurements. 
# IEEE transactions on Biomedical Engineering TBME, vol. 61, pp. 2112-2124, 2014.


rm(list=ls())
load( "vfProParameters.Rdata" )


# libraries
library(visualFields)
library(lubridate)
library(colortools)
library(MESS)  # area under a curve
library(eeptools)  # age_calc
library(corpcor) # Pseudoinverse of matrices

# init parameters
td.labels= paste("TD_", 1:52, sep="")
sample.data.ivan= sample.data.ivan.2  # sample.data.ivan.1 is not progressed while sample.data.ivan.2 is progressed based on VFPro


if (sum(diff(diff(order(sample.data.ivan$Age)))) != 0) print("visit dates are not ordered")
if (nrow(sample.data.ivan) < 5 )  print("small number of visits")
age= sample.data.ivan$Age        # it should be in years, it could be visit dates as well
pValue.slope.results <- NULL
eye.progressed.result <- NULL

for (j in 1: 20){ # loop GEM Axes (20 rows)
  # projecting VFs on GEM axes
  y = as.matrix(sample.data.ivan[, td.labels], nrow=52, ncol= n.visits.k) %*% as.matrix(gem.axes.ivan[j, ], nrow=1, ncol= 52)
  # linear regression
  lm.result = summary( lm(  y ~ age  ) )    # Added by Siamak:   lm(formula = y ~ x)
  pValue = lm.result$coefficients[2,4]
  slope = lm.result$coefficients[2,1]
  pValue.slope.results <- c(pValue.slope.results, c(pValue, slope))
  # eye is progressed or not?
  if ( slope < 0 & pValue < crit.p.value ) {
    eye.progressed.result <- c(eye.progressed.result, 1)
  }
  else {
    eye.progressed.result <- c(eye.progressed.result, 0)
  }
  print( paste0(sample.data.ivan$IdEye[1],': gem axis ', j,', pvalue: ',round(pValue,5),', slope: ',round(slope,3),", critPvalue: ",
                round(crit.p.value,5),", critSlope: ", 0," timeToProg: ", round((age[nrow(sample.data.ivan)]-age[1]),2),', progressed: ',eye.progressed.result[j]) ) 
}
# checking whether eye progressed 
progressed.flag= 0  # eye hasnot progressed
# if at least three gem axes confirm progressing, then labeled as progressed
if ( sum( eye.progressed.result, na.rm= TRUE ) >= 3 ){  
  progressed.flag= 1
  print("Eye is progressed")
}
