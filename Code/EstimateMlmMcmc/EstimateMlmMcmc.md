<!-- Specify the report's official name, goal & description. -->
# Diagnosis of Coupon Pit Depth

**Report Description**: This report is more for the diagnosis of the MCMC.  The `CouponDepth` report is intended for general audiences.



<!-- Point knitr to the underlying code file so it knows where to look for the chunks. -->



<!-- Load the packages.  Suppress the output when loading packages. --> 



<!-- Load any Global Functions declared in the R file.  Suppress the output. --> 



<!-- Declare any global functions specific to a Rmd output.  Suppress the output. --> 



<!-- Load the dataset.   -->



<!-- Tweak the dataset.   -->



<!-- Prepare the parameters to be sent to the MCMC.   -->



## 1. Parameters
**chainCount**: 8

**iterationCount**: 10000 (The number used for estimate -ie, it doesn't include burn-in.)

**burninCount**: 2500





## 2. MCMCglmm


```

                      MCMC iteration = 0

                      MCMC iteration = 1000

                      MCMC iteration = 2000

                      MCMC iteration = 3000

                      MCMC iteration = 4000

                      MCMC iteration = 5000

                      MCMC iteration = 6000

                      MCMC iteration = 7000

                      MCMC iteration = 8000

                      MCMC iteration = 9000

                      MCMC iteration = 10000

                      MCMC iteration = 11000

                      MCMC iteration = 12000

                      MCMC iteration = 0

                      MCMC iteration = 1000

                      MCMC iteration = 2000

                      MCMC iteration = 3000

                      MCMC iteration = 4000

                      MCMC iteration = 5000

                      MCMC iteration = 6000

                      MCMC iteration = 7000

                      MCMC iteration = 8000

                      MCMC iteration = 9000

                      MCMC iteration = 10000

                      MCMC iteration = 11000

                      MCMC iteration = 12000

                      MCMC iteration = 0

                      MCMC iteration = 1000

                      MCMC iteration = 2000

                      MCMC iteration = 3000

                      MCMC iteration = 4000

                      MCMC iteration = 5000

                      MCMC iteration = 6000

                      MCMC iteration = 7000

                      MCMC iteration = 8000

                      MCMC iteration = 9000

                      MCMC iteration = 10000

                      MCMC iteration = 11000

                      MCMC iteration = 12000

                      MCMC iteration = 0

                      MCMC iteration = 1000

                      MCMC iteration = 2000

                      MCMC iteration = 3000

                      MCMC iteration = 4000

                      MCMC iteration = 5000

                      MCMC iteration = 6000

                      MCMC iteration = 7000

                      MCMC iteration = 8000

                      MCMC iteration = 9000

                      MCMC iteration = 10000

                      MCMC iteration = 11000

                      MCMC iteration = 12000

                      MCMC iteration = 0

                      MCMC iteration = 1000

                      MCMC iteration = 2000

                      MCMC iteration = 3000

                      MCMC iteration = 4000

                      MCMC iteration = 5000

                      MCMC iteration = 6000

                      MCMC iteration = 7000

                      MCMC iteration = 8000

                      MCMC iteration = 9000

                      MCMC iteration = 10000

                      MCMC iteration = 11000

                      MCMC iteration = 12000

                      MCMC iteration = 0

                      MCMC iteration = 1000

                      MCMC iteration = 2000

                      MCMC iteration = 3000

                      MCMC iteration = 4000

                      MCMC iteration = 5000

                      MCMC iteration = 6000

                      MCMC iteration = 7000

                      MCMC iteration = 8000

                      MCMC iteration = 9000

                      MCMC iteration = 10000

                      MCMC iteration = 11000

                      MCMC iteration = 12000

                      MCMC iteration = 0

                      MCMC iteration = 1000

                      MCMC iteration = 2000

                      MCMC iteration = 3000

                      MCMC iteration = 4000

                      MCMC iteration = 5000

                      MCMC iteration = 6000

                      MCMC iteration = 7000

                      MCMC iteration = 8000

                      MCMC iteration = 9000

                      MCMC iteration = 10000

                      MCMC iteration = 11000

                      MCMC iteration = 12000

                      MCMC iteration = 0

                      MCMC iteration = 1000

                      MCMC iteration = 2000

                      MCMC iteration = 3000

                      MCMC iteration = 4000

                      MCMC iteration = 5000

                      MCMC iteration = 6000

                      MCMC iteration = 7000

                      MCMC iteration = 8000

                      MCMC iteration = 9000

                      MCMC iteration = 10000

                      MCMC iteration = 11000

                      MCMC iteration = 12000
```

```
Time difference of 3.51 hours
```

```

 Iterations = 2501:12500
 Thinning interval  = 1
 Sample size  = 10000 

 DIC: 815367 

 G-structure:  ~CouponID

         post.mean l-95% CI u-95% CI eff.samp
CouponID      6.38     4.18     8.87     8461

 R-structure:  ~units

      post.mean l-95% CI u-95% CI eff.samp
units      10.4     10.3     10.4    10000

 Location effects: ProbeDepth ~ 1 + Treatment 

                        post.mean l-95% CI u-95% CI eff.samp  pMCMC    
(Intercept)                -6.206   -7.196   -5.232    10000 <1e-04 ***
TreatmentAcetateOnly       -0.020   -2.302    2.260    11119  0.989    
TreatmentMethane           -0.786   -3.283    1.580    10000  0.529    
TreatmentSulfideAcetate    -2.179   -3.923   -0.448    10000  0.016 *  
TreatmentSulfideOnly        0.319   -1.312    1.843    10000  0.685    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```

Iterations = 2501:12500
Thinning interval = 1 
Number of chains = 8 
Sample size per chain = 10000 

1. Empirical mean and standard deviation for each variable,
   plus standard error of the mean:

                           Mean    SD Naive SE Time-series SE
(Intercept)             -6.2058 0.514  0.00182        0.00183
TreatmentAcetateOnly    -0.0198 1.152  0.00407        0.00407
TreatmentMethane        -0.7996 1.243  0.00439        0.00440
TreatmentSulfideAcetate -2.1742 0.894  0.00316        0.00316
TreatmentSulfideOnly     0.3136 0.813  0.00287        0.00286

2. Quantiles for each variable:

                         2.5% 15.87%    25%     50%     75% 84.13%  97.5%
(Intercept)             -7.21 -6.714 -6.549 -6.2063 -5.8621 -5.695 -5.198
TreatmentAcetateOnly    -2.30 -1.156 -0.784 -0.0221  0.7463  1.123  2.252
TreatmentMethane        -3.23 -2.037 -1.632 -0.7988  0.0355  0.437  1.636
TreatmentSulfideAcetate -3.94 -3.061 -2.774 -2.1722 -1.5762 -1.291 -0.417
TreatmentSulfideOnly    -1.29 -0.489 -0.226  0.3125  0.8541  1.118  1.915
```

```
R-Hat:
```

```
Potential scale reduction factors:

                        Point est. Upper C.I.
(Intercept)                      1          1
TreatmentAcetateOnly             1          1
TreatmentMethane                 1          1
TreatmentSulfideAcetate          1          1
TreatmentSulfideOnly             1          1

Multivariate psrf

1
```

![plot of chunk RunMCMCglmm](FigureDiagnosisRmd/RunMCMCglmm.png) 

```
Effective Size:
```

```
            (Intercept)    TreatmentAcetateOnly        TreatmentMethane TreatmentSulfideAcetate    TreatmentSulfideOnly 
                  78964                   80226                   79827                   80289                   80756 
```


### For and explanation of the graphs, see http://xavier-fim.net/packages/ggmcmc/

```
Error: subscript out of bounds
```

```
Error: subscript out of bounds
```

![plot of chunk GraphJagsSelect](FigureDiagnosisRmd/GraphJagsSelect1.png) 

```
Error: subscript out of bounds
```

![plot of chunk GraphJagsSelect](FigureDiagnosisRmd/GraphJagsSelect2.png) 



```
Warning: Removed 8 rows containing missing values (geom_point).
```

![plot of chunk GraphJagsAll](FigureDiagnosisRmd/GraphJagsAll1.png) ![plot of chunk GraphJagsAll](FigureDiagnosisRmd/GraphJagsAll2.png) ![plot of chunk GraphJagsAll](FigureDiagnosisRmd/GraphJagsAll3.png) ![plot of chunk GraphJagsAll](FigureDiagnosisRmd/GraphJagsAll4.png) 


## 3. Display Syntax of Models


```
function( x ) {
  MCMCglmm(fixed = ProbeDepth ~ 1 + Treatment,
           random = ~ CouponID,
           data = dsProbe, 
           nitt = iterationCount + burninCount,
           burnin = burninCount,
           thin = thinCount)
}
```


## --Session Info--

```
Report created by Will at 2014-02-15, 11:58:28 -0600
```

```
R Under development (unstable) (2014-02-10 r64961)
Platform: x86_64-w64-mingw32/x64 (64-bit)

locale:
[1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252    LC_MONETARY=English_United States.1252
[4] LC_NUMERIC=C                           LC_TIME=English_United States.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] ggmcmc_0.5.1    ggplot2_0.9.3.1 reshape2_1.2.2  plyr_1.8.0.99   MCMCglmm_2.17   corpcor_1.6.6   ape_3.0-11      Matrix_1.1-0    tensorA_0.36   
[10] coda_0.16-1     lattice_0.20-24 knitr_1.5      

loaded via a namespace (and not attached):
 [1] colorspace_1.2-4   dichromat_2.0-0    digest_0.6.4       evaluate_0.5.1     formatR_0.10       grid_3.1.0         gtable_0.1.2      
 [8] labeling_0.2       MASS_7.3-29        munsell_0.4.2      nlme_3.1-113       proto_0.3-10       RColorBrewer_1.0-5 Rcpp_0.11.0       
[15] scales_0.2.3       stringr_0.6.2      tools_3.1.0       
```

