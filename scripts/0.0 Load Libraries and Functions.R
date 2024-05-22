
# Load Libraries ----------------------------------------------------------

library(janitor)
library(BayesFactor)
library(bayestestR)
library(ez)
library(openxlsx)
library(tidyverse)
library(Superpower)

# Useful Functions --------------------------------------------------------

calculate_d = function(h, fa, correction_value = .025)
{
  if(length(h[h==1])>0)
  {
    h[h==1] = 1-correction_value
  }
  
  if(length(fa[fa==0])>0)
  {
    fa[fa==0] = correction_value
  }
  
  return(qnorm(h) - qnorm(fa))
}

calculate_c = function(h, fa, correction_value = .025)
{
  if(length(h[h==1])>0)
  {
    h[h==1] = 1-correction_value
  }
  
  if(length(fa[fa==0])>0)
  {
    fa[fa==0] = correction_value
  }

  return(-.5 * (qnorm(h) + qnorm(fa)))
}

