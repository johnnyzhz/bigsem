---
title: Network analysis
layout: home
nav_order: 1
parent: How to use the online app
---

# Use the online app for network analysis


The network data analysis can also be conducted using our online app available at: [https://bigsem.psychstat.org/app](https://bigsem.psychstat.org/app) . To use the app, one need to register as a user to protect the data of the users. Once logging in, a user with work with an interface like below:

![netapp1.png](/assets/images/sAYY964EFPojK7N9-netapp1.png)

### Organizing data

Organizing the data for analysis is the first step for using the app or R package. In R, the data are provided as a list with a non-network component and a network component. To conveniently organize the data online, we developed a simple app.

To use the app, one first upload the non-network data and network data sets as separate files. Then, in the app, one selects the corresponding data files. An example is given below with two networks - friendship and WeChat networks. Note that the new data set will be saved as R data with the provided name, i.e., `mynetworkdata.RData` in this example.

![netapp2.png](/assets/images/hMXdh9xrNLd60HEl-netapp2.png)

### Conducting the analysis

We use a simple example to illustrate the use of the online app. To conduct the analysis, we need to first draw the path diagram of the model. Here, we create a latent happiness factor (happy.f) from the 4-item measure of global subjective happiness. We then use the friendship network to predict the happiness factor.

![netapp3.png](/assets/images/2reFGiLC2duE6DKw-netapp3.png)

For the network analysis, one needs to choose the software to use, here "NetworkSEM". Then, one selects the Data File "network.RData".

![netapp4.png](/assets/images/D3cqqm03rmDck254-netapp4.png)

For the network statistics based method, one need to choose what statistics to use. Here, one can specify them in the "Control" field. In this example, we use `netstats = degree, betweenness, closeness` to allow the use of the three network statistics.

![netapp5.png](/assets/images/Xq3wjYGF3znG6ADr-netapp5.png)

To run the analysis, one clicks on the green triangle in the left panel. The output of the analysis is given below. The output has several parts:

- The basic information, particularly, the user and the analysis id `7cf61d4792351966add082d56368301d`.
- The descriptive statistics for numerical variables in the non-network data set.
- The information on the networks.
- The basic model information
- The results from fitting the model.

BigSEM started at 15:36:50 on Oct 22, 2024.  
=====================================  
Please refresh your browser for complete output of complex data analysis.

The current analysis was conducted by the BigSEM user **johnny**.  
To contact us, make sure to include the ticket # for this analysis 7cf61d4792351966add082d56368301d

  
**Descriptive statistics (N=165, p=59)**

```
                   Mean        sd     Min       Max   Skewness Kurtosis
gender          0.55152   0.49885   0.000    1.0000 -0.2071631   1.0429
gpa             3.27293   0.48805   1.173    4.2200 -0.6399076   4.2619
age            21.64242   0.85505  18.000   24.0000 -0.1255522   4.5903
weight         62.29091  14.16756  37.000  110.0000  0.9021334   3.2265
height        169.54545   8.15808 155.000  188.0000  0.3186553   1.9660
smoke           0.26061   0.44030   0.000    1.0000  1.0907192   2.1897
drink           0.41212   0.49372   0.000    1.0000  0.3570735   1.1275
wechat        157.32927 180.36548   0.000 1000.0000  2.9199355  11.9943
id             83.00000  47.77552   1.000  165.0000  0.0000000   1.7999
personality1    2.81818   1.06652   1.000    5.0000 -0.0869982   2.4384
personality2    2.61818   1.22710   1.000    5.0000  0.3212422   2.0339
personality3    2.45455   0.98436   1.000    5.0000  0.4540597   2.8503
personality4    2.64242   0.98743   1.000    5.0000  0.1910639   2.5725
personality5    3.03636   1.15764   1.000    5.0000 -0.0235915   2.2242
personality6    3.07879   1.12612   1.000    5.0000  0.1017642   2.3871
personality7    3.27273   1.16537   1.000    5.0000 -0.1954555   2.1881
personality8    2.36970   1.13816   1.000    5.0000  0.5103888   2.4850
personality9    2.75758   0.94451   1.000    5.0000  0.3684034   3.1224
personality10   3.01212   1.08194   1.000    5.0000  0.0049198   2.5241
personality11   2.89697   1.20276   1.000    5.0000  0.0931915   2.2009
personality12   3.78788   1.08081   1.000    5.0000 -0.4433181   2.2537
personality13   2.61818   1.03283   1.000    5.0000  0.3473757   2.9438
personality14   3.80000   1.04298   1.000    5.0000 -0.5964333   2.8276
personality15   3.42424   1.11613   1.000    5.0000 -0.3898210   2.5711
personality16   2.65455   1.20292   1.000    5.0000  0.2450516   2.2534
personality17   2.31515   0.98033   1.000    5.0000  0.3493841   2.6210
personality18   3.59394   0.99937   1.000    5.0000 -0.1128832   2.1067
personality19   3.82424   0.94966   1.000    5.0000 -0.5435870   3.1673
personality20   3.12121   1.06946   1.000    5.0000  0.0874853   2.4055
depress1        0.98788   0.55202   0.000    3.0000  0.6478164   5.7357
depress2        0.61818   0.58926   0.000    3.0000  0.5205043   3.3723
depress3        0.76364   0.78002   0.000    3.0000  0.8239322   3.2396
depress4        0.91515   0.59884   0.000    3.0000  0.3722678   4.0971
depress5        0.70303   0.67376   0.000    3.0000  0.6728525   3.3429
depress6        0.80606   0.69753   0.000    3.0000  0.7141707   3.7965
depress7        0.66667   0.70998   0.000    3.0000  0.8848909   3.5949
lone1           1.04848   0.77935   0.000    3.0000  0.2260045   2.3813
lone2           1.26667   0.88437   0.000    3.0000  0.1437581   2.2374
lone3           1.03030   0.87251   0.000    3.0000  0.2729773   2.0401
lone4           1.29091   0.90404   0.000    3.0000  0.1403947   2.1952
lone5           1.27879   0.88750   0.000    3.0000  0.0558801   2.1521
lone6           0.85455   0.79828   0.000    3.0000  0.5543989   2.5604
lone7           0.98788   0.85531   0.000    3.0000  0.3749858   2.2210
lone8           1.64242   0.89682   0.000    3.0000 -0.2540419   2.3354
lone9           1.00000   0.86954   0.000    3.0000  0.3907138   2.2320
lone10          0.88485   0.76832   0.000    3.0000  0.5218129   2.7655
happy1          5.34545   1.31897   1.000    7.0000 -0.8142547   3.6334
happy2          5.25455   1.30969   1.000    7.0000 -0.7392627   3.2077
happy3          5.24848   1.30387   2.000    7.0000 -0.4342157   2.6097
happy4          3.89091   1.65654   1.000    7.0000  0.1177261   2.2404
lone            1.12848   0.56674   0.000    2.6000 -0.0868936   2.8135
depress         0.78009   0.41754   0.000    1.8571  0.1401042   2.5266
happy           4.93485   0.86774   2.500    7.0000  0.2112938   3.2653
p.e             2.91364   0.78605   1.000    5.0000  0.1731648   3.4108
p.c             3.53182   0.69743   2.000    5.0000  0.2454618   2.4799
p.i             3.53788   0.68721   1.500    5.0000 -0.2099051   2.6462
p.a             3.55606   0.61259   1.750    5.0000  0.0235716   2.8378
p.n             2.87576   0.63835   1.000    4.7500  0.1728206   3.3815
bmi            21.50942   3.84812  15.401   39.5197  1.5035276   6.1558
              Missing Rate
gender           0.0000000
gpa              0.0000000
age              0.0000000
weight           0.0000000
height           0.0000000
smoke            0.0000000
drink            0.0000000
wechat           0.0060606
id               0.0000000
personality1     0.0000000
personality2     0.0000000
personality3     0.0000000
personality4     0.0000000
personality5     0.0000000
personality6     0.0000000
personality7     0.0000000
personality8     0.0000000
personality9     0.0000000
personality10    0.0000000
personality11    0.0000000
personality12    0.0000000
personality13    0.0000000
personality14    0.0000000
personality15    0.0000000
personality16    0.0000000
personality17    0.0000000
personality18    0.0000000
personality19    0.0000000
personality20    0.0000000
depress1         0.0000000
depress2         0.0000000
depress3         0.0000000
depress4         0.0000000
depress5         0.0000000
depress6         0.0000000
depress7         0.0000000
lone1            0.0000000
lone2            0.0000000
lone3            0.0000000
lone4            0.0000000
lone5            0.0000000
lone6            0.0000000
lone7            0.0000000
lone8            0.0000000
lone9            0.0000000
lone10           0.0000000
happy1           0.0000000
happy2           0.0000000
happy3           0.0000000
happy4           0.0000000
lone             0.0000000
depress          0.0000000
happy            0.0000000
p.e              0.0000000
p.c              0.0000000
p.i              0.0000000
p.a              0.0000000
p.n              0.0000000
bmi              0.0000000
```

  
**Network data information**

```
        #row #col
friends  165  165
wechat   165  165
```

**Model information**  
Observed non-network variables: happy1 happy2 happy3 happy4 .  
Observed network variables: friends .  
Latent variables: happy.f .  
The weight is: 0 .

**Results**

```
lavaan 0.6-18 ended normally after 66 iterations

  Estimator                                         ML
  Optimization method                           NLMINB
  Number of model parameters                        11

  Number of observations                           165

Model Test User Model:
                                                      
  Test statistic                                14.749
  Degrees of freedom                                11
  P-value (Chi-square)                           0.194

Model Test Baseline Model:

  Test statistic                               162.858
  Degrees of freedom                                18
  P-value                                        0.000

User Model versus Baseline Model:

  Comparative Fit Index (CFI)                    0.974
  Tucker-Lewis Index (TLI)                       0.958

Loglikelihood and Information Criteria:

  Loglikelihood user model (H0)              -1077.697
  Loglikelihood unrestricted model (H1)      -1070.322
                                                      
  Akaike (AIC)                                2177.394
  Bayesian (BIC)                              2211.559
  Sample-size adjusted Bayesian (SABIC)       2176.733

Root Mean Square Error of Approximation:

  RMSEA                                          0.045
  90 Percent confidence interval - lower         0.000
  90 Percent confidence interval - upper         0.099
  P-value H_0: RMSEA <= 0.050                    0.498
  P-value H_0: RMSEA >= 0.080                    0.170

Standardized Root Mean Square Residual:

  SRMR                                           0.039

Parameter Estimates:

  Standard errors                             Standard
  Information                                 Expected
  Information saturated (h1) model          Structured

Latent Variables:
                   Estimate  Std.Err  z-value  P(>|z|)
  happy.f =~                                          
    happy4            1.000                           
    happy3           -4.933    5.032   -0.980    0.327
    happy2           -7.445    7.547   -0.986    0.324
    happy1           -8.133    8.251   -0.986    0.324

Regressions:
                   Estimate  Std.Err  z-value  P(>|z|)
  happy.f ~                                           
    friends.degree   -0.024    0.037   -0.655    0.513
    frinds.btwnnss    0.019    0.029    0.654    0.513
    friends.clsnss    0.011    0.027    0.401    0.689

Variances:
                   Estimate  Std.Err  z-value  P(>|z|)
   .happy4            2.708    0.299    9.070    0.000
   .happy3            1.219    0.147    8.306    0.000
   .happy2            0.633    0.150    4.207    0.000
   .happy1            0.450    0.167    2.701    0.007
   .happy.f           0.019    0.039    0.494    0.621

```

  
=====================================  
BigSEM ended at 15:36:50 on Oct 22, 2024
