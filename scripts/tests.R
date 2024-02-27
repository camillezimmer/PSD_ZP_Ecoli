library(moments)
library(psych)
library(AID)
library(onewaytests)
library(MASS)

bxk12 <- cumulants_cleaned %>% 
  filter(strain_id == 'ka') %>% 
  select(strain_id, strain_run, run_id, z_ave)

describe(zp ~ strain_id, data = zp_cleaned)

nor.test(zp ~ strain_id, data = zp_cleaned)

homog.test(zp ~ strain_id, data = zp_cleaned, method = "Bartlett")

out <- boxcoxfr(cumulants_cleaned$z_ave, cumulants_cleaned$strain_id, 
                lambda = seq(-5, 5, 0.01), tau = 0.000001) 


mc <- cumulants_cleaned %>% 
  filter(strain_id == 'mc') %>%
  dplyr::select(strain_id, z_ave)%>% 
  describe(z_ave ~ strain_id, data = .)
