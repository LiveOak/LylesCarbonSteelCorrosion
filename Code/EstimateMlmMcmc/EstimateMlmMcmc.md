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
**chainCount**: 4

**iterationCount**: 5000 (The number used for estimate -ie, it doesn't include burn-in.)

**burninCount**: 1250





## 2. MCMCglmm


```

                      MCMC iteration = 0

                      MCMC iteration = 1000

                      MCMC iteration = 2000

                      MCMC iteration = 3000

                      MCMC iteration = 4000

                      MCMC iteration = 5000

                      MCMC iteration = 6000

                      MCMC iteration = 0

                      MCMC iteration = 1000

                      MCMC iteration = 2000

                      MCMC iteration = 3000

                      MCMC iteration = 4000

                      MCMC iteration = 5000

                      MCMC iteration = 6000

                      MCMC iteration = 0

                      MCMC iteration = 1000

                      MCMC iteration = 2000

                      MCMC iteration = 3000

                      MCMC iteration = 4000

                      MCMC iteration = 5000

                      MCMC iteration = 6000

                      MCMC iteration = 0

                      MCMC iteration = 1000

                      MCMC iteration = 2000

                      MCMC iteration = 3000

                      MCMC iteration = 4000

                      MCMC iteration = 5000

                      MCMC iteration = 6000
```

```
Time difference of 13.72 mins
```

```

 Iterations = 1251:6250
 Thinning interval  = 1
 Sample size  = 5000 

 DIC: 815367 

 G-structure:  ~CouponID

         post.mean l-95% CI u-95% CI eff.samp
CouponID      6.38     4.12     8.69     4026

 R-structure:  ~units

      post.mean l-95% CI u-95% CI eff.samp
units      10.4     10.3     10.4     5000

 Location effects: ProbeDepth ~ 1 + Treatment 

                        post.mean l-95% CI u-95% CI eff.samp  pMCMC    
(Intercept)               -6.2073  -7.2372  -5.1880     5000 <2e-04 ***
TreatmentAcetateOnly      -0.0299  -2.2630   2.1734     5000  0.972    
TreatmentMethane          -0.8075  -3.3131   1.5091     5000  0.507    
TreatmentSulfideAcetate   -2.1741  -3.9497  -0.4624     5000  0.015 *  
TreatmentSulfideOnly       0.3232  -1.2145   1.9578     5000  0.702    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```

Iterations = 1251:6250
Thinning interval = 1 
Number of chains = 4 
Sample size per chain = 5000 

1. Empirical mean and standard deviation for each variable,
   plus standard error of the mean:

                           Mean    SD Naive SE Time-series SE
(Intercept)             -6.2090 0.515  0.00364        0.00358
TreatmentAcetateOnly    -0.0156 1.144  0.00809        0.00809
TreatmentMethane        -0.7918 1.246  0.00881        0.00859
TreatmentSulfideAcetate -2.1819 0.891  0.00630        0.00627
TreatmentSulfideOnly     0.3086 0.816  0.00577        0.00573

2. Quantiles for each variable:

                         2.5% 15.87%    25%     50%     75% 84.13%  97.5%
(Intercept)             -7.21 -6.717 -6.550 -6.2085 -5.8634 -5.700 -5.189
TreatmentAcetateOnly    -2.26 -1.158 -0.782 -0.0183  0.7507  1.116  2.218
TreatmentMethane        -3.23 -2.036 -1.625 -0.7904  0.0351  0.442  1.669
TreatmentSulfideAcetate -3.94 -3.068 -2.780 -2.1817 -1.5781 -1.294 -0.456
TreatmentSulfideOnly    -1.27 -0.503 -0.250  0.3066  0.8498  1.128  1.913
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
                  20692                   20000                   21080                   20206                   20354 
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
Warning: Removed 20 rows containing missing values (geom_point).
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
Report created by Will at 2014-02-15, 01:27:03 -0600
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

