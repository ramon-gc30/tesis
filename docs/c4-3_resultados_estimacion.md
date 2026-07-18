# Resultados de la estimación


``` r
library(here)

source(here::here("scripts", "3_regresion_v1.R"))

remove("jovenes_reg")
```

Resultados de la estimación

``` r
resultados_jov
```


    Call:
    svyglm(formula = log(ingreso_mensual) ~ anios_estudios + over_obj + 
        exper + I(exper^2) + log(horas_lab) + mujer + jefe_hogar + 
        regiones + campo_amplio + division + pos_ocu + tamanio + 
        informal + jornada + unidad_eco, design = jovenes_reg)

    Survey design:
    Called via srvyr

    Coefficients:
                    Estimate Std. Error t value Pr(>|t|)    
    (Intercept)     7.699922   0.265452  29.007  < 2e-16 ***
    anios_estudios  0.043461   0.013173   3.299 0.000976 ***
    over_obj       -0.190892   0.030858  -6.186 6.62e-10 ***
    exper           0.064394   0.015796   4.077 4.63e-05 ***
    I(exper^2)     -0.003561   0.001090  -3.266 0.001100 ** 
    log(horas_lab)  0.109717   0.063098   1.739 0.082122 .  
    mujer          -0.105034   0.021318  -4.927 8.60e-07 ***
    jefe_hogar      0.127815   0.022008   5.808 6.70e-09 ***
    regiones2       0.118809   0.030828   3.854 0.000118 ***
    regiones3       0.044905   0.029456   1.524 0.127451    
    regiones4       0.209951   0.025908   8.104 6.55e-16 ***
    regiones5       0.070149   0.027979   2.507 0.012197 *  
    campo_amplio02  0.052810   0.051128   1.033 0.301699    
    campo_amplio03  0.063253   0.037032   1.708 0.087678 .  
    campo_amplio04  0.117370   0.033967   3.455 0.000554 ***
    campo_amplio05  0.034070   0.075342   0.452 0.651141    
    campo_amplio06  0.100391   0.055858   1.797 0.072352 .  
    campo_amplio07  0.111333   0.036577   3.044 0.002347 ** 
    campo_amplio08  0.077450   0.101590   0.762 0.445867    
    campo_amplio09  0.065465   0.038081   1.719 0.085655 .  
    campo_amplio10  0.077222   0.045381   1.702 0.088884 .  
    campo_amplio99  0.215205   0.101490   2.120 0.034014 *  
    division2      -0.092856   0.046689  -1.989 0.046772 *  
    division3      -0.201238   0.054281  -3.707 0.000212 ***
    division4      -0.282692   0.056987  -4.961 7.24e-07 ***
    division5      -0.183837   0.060155  -3.056 0.002254 ** 
    division6      -0.328396   0.148325  -2.214 0.026869 *  
    division7      -0.205931   0.075897  -2.713 0.006684 ** 
    division8      -0.352066   0.061081  -5.764 8.68e-09 ***
    division9      -0.420335   0.071032  -5.918 3.47e-09 ***
    pos_ocu2        0.436066   0.060499   7.208 6.48e-13 ***
    pos_ocu3        0.095728   0.047980   1.995 0.046077 *  
    tamanio4        0.104681   0.034614   3.024 0.002505 ** 
    tamanio5        0.151180   0.039826   3.796 0.000149 ***
    tamanio6        0.277740   0.045617   6.089 1.22e-09 ***
    tamanio7        0.369643   0.054541   6.777 1.36e-11 ***
    tamanio8        0.011337   0.039463   0.287 0.773915    
    informal1      -0.305055   0.025616 -11.909  < 2e-16 ***
    jornada3        0.470724   0.098589   4.775 1.85e-06 ***
    jornada4        0.495275   0.122823   4.032 5.60e-05 ***
    jornada5        0.485265   0.139641   3.475 0.000515 ***
    unidad_eco2    -0.168041   0.032739  -5.133 2.96e-07 ***
    unidad_eco3    -0.122774   0.045088  -2.723 0.006490 ** 
    unidad_eco4    -0.042328   0.114278  -0.370 0.711101    
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    (Dispersion parameter for gaussian family taken to be 0.2376794)

    Number of Fisher Scoring iterations: 2

Resultados de la estimación con términos de interacción relacionadas a
la sobrecalificación y a los años de escolaridad

``` r
resultados_edu
```


    Call:
    svyglm(formula = log(ingreso_mensual) ~ over_obj * anios_estudios + 
        exper + I(exper^2) + log(horas_lab) + mujer + jefe_hogar + 
        regiones + campo_amplio + division + pos_ocu + tamanio + 
        informal + jornada + unidad_eco, design = jovenes_reg)

    Survey design:
    Called via srvyr

    Coefficients:
                             Estimate Std. Error t value Pr(>|t|)    
    (Intercept)              7.262543   0.320527  22.658  < 2e-16 ***
    over_obj                 0.753597   0.383393   1.966 0.049397 *  
    anios_estudios           0.070250   0.017581   3.996 6.54e-05 ***
    exper                    0.062881   0.015605   4.030 5.67e-05 ***
    I(exper^2)              -0.003467   0.001079  -3.213 0.001323 ** 
    log(horas_lab)           0.108254   0.063096   1.716 0.086274 .  
    mujer                   -0.105128   0.021284  -4.939 8.08e-07 ***
    jefe_hogar               0.127288   0.021922   5.806 6.75e-09 ***
    regiones2                0.117131   0.030702   3.815 0.000138 ***
    regiones3                0.044167   0.029454   1.499 0.133807    
    regiones4                0.210215   0.025821   8.141 4.83e-16 ***
    regiones5                0.072181   0.027880   2.589 0.009652 ** 
    campo_amplio02           0.055674   0.051248   1.086 0.277369    
    campo_amplio03           0.061744   0.036976   1.670 0.095006 .  
    campo_amplio04           0.114768   0.034043   3.371 0.000754 ***
    campo_amplio05           0.031680   0.076187   0.416 0.677565    
    campo_amplio06           0.102252   0.056189   1.820 0.068848 .  
    campo_amplio07           0.113193   0.036581   3.094 0.001983 ** 
    campo_amplio08           0.079960   0.102361   0.781 0.434744    
    campo_amplio09           0.058367   0.037603   1.552 0.120679    
    campo_amplio10           0.068970   0.045499   1.516 0.129619    
    campo_amplio99           0.210386   0.100574   2.092 0.036498 *  
    division2               -0.093582   0.046005  -2.034 0.041983 *  
    division3               -0.204795   0.053974  -3.794 0.000150 ***
    division4               -0.284108   0.056539  -5.025 5.20e-07 ***
    division5               -0.188232   0.059921  -3.141 0.001691 ** 
    division6               -0.340637   0.149006  -2.286 0.022289 *  
    division7               -0.214782   0.075813  -2.833 0.004628 ** 
    division8               -0.354887   0.060668  -5.850 5.22e-09 ***
    division9               -0.426655   0.071307  -5.983 2.33e-09 ***
    pos_ocu2                 0.437853   0.060217   7.271 4.07e-13 ***
    pos_ocu3                 0.091532   0.047846   1.913 0.055795 .  
    tamanio4                 0.104623   0.034504   3.032 0.002440 ** 
    tamanio5                 0.153075   0.039843   3.842 0.000123 ***
    tamanio6                 0.275557   0.045091   6.111 1.06e-09 ***
    tamanio7                 0.376213   0.054415   6.914 5.27e-12 ***
    tamanio8                 0.011080   0.039554   0.280 0.779396    
    informal1               -0.305363   0.025711 -11.877  < 2e-16 ***
    jornada3                 0.473875   0.098243   4.824 1.45e-06 ***
    jornada4                 0.499166   0.122575   4.072 4.72e-05 ***
    jornada5                 0.489433   0.139465   3.509 0.000453 ***
    unidad_eco2             -0.168917   0.032661  -5.172 2.40e-07 ***
    unidad_eco3             -0.120794   0.045319  -2.665 0.007712 ** 
    unidad_eco4             -0.057986   0.120867  -0.480 0.631423    
    over_obj:anios_estudios -0.056957   0.023015  -2.475 0.013365 *  
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    (Dispersion parameter for gaussian family taken to be 0.237156)

    Number of Fisher Scoring iterations: 2

Resultados de la estimación con términos de interacción relacionadas a
la sobrecalificación y al género

``` r
resultados_muj
```


    Call:
    svyglm(formula = log(ingreso_mensual) ~ anios_estudios + over_obj * 
        mujer + exper + I(exper^2) + log(horas_lab) + jefe_hogar + 
        regiones + campo_amplio + division + pos_ocu + tamanio + 
        informal + jornada + unidad_eco, design = jovenes_reg)

    Survey design:
    Called via srvyr

    Coefficients:
                    Estimate Std. Error t value Pr(>|t|)    
    (Intercept)     7.690155   0.265102  29.008  < 2e-16 ***
    anios_estudios  0.043119   0.013109   3.289 0.001011 ** 
    over_obj       -0.166496   0.036872  -4.515 6.45e-06 ***
    mujer          -0.078404   0.032136  -2.440 0.014729 *  
    exper           0.064236   0.015761   4.076 4.66e-05 ***
    I(exper^2)     -0.003551   0.001088  -3.263 0.001110 ** 
    log(horas_lab)  0.109059   0.062890   1.734 0.082954 .  
    jefe_hogar      0.128852   0.022019   5.852 5.15e-09 ***
    regiones2       0.119177   0.030813   3.868 0.000111 ***
    regiones3       0.046292   0.029431   1.573 0.115795    
    regiones4       0.210145   0.025888   8.117 5.86e-16 ***
    regiones5       0.070912   0.027936   2.538 0.011166 *  
    campo_amplio02  0.054202   0.051160   1.059 0.289435    
    campo_amplio03  0.066892   0.036643   1.826 0.067977 .  
    campo_amplio04  0.119702   0.034291   3.491 0.000486 ***
    campo_amplio05  0.038211   0.075014   0.509 0.610500    
    campo_amplio06  0.104494   0.055768   1.874 0.061024 .  
    campo_amplio07  0.113704   0.036685   3.099 0.001949 ** 
    campo_amplio08  0.078200   0.101634   0.769 0.441669    
    campo_amplio09  0.067406   0.038283   1.761 0.078347 .  
    campo_amplio10  0.077701   0.045304   1.715 0.086384 .  
    campo_amplio99  0.217936   0.099245   2.196 0.028140 *  
    division2      -0.092805   0.046392  -2.000 0.045503 *  
    division3      -0.196527   0.054002  -3.639 0.000276 ***
    division4      -0.280628   0.056683  -4.951 7.62e-07 ***
    division5      -0.182635   0.059908  -3.049 0.002311 ** 
    division6      -0.336406   0.148633  -2.263 0.023656 *  
    division7      -0.210669   0.075878  -2.776 0.005515 ** 
    division8      -0.358382   0.061153  -5.860 4.89e-09 ***
    division9      -0.421406   0.070620  -5.967 2.57e-09 ***
    pos_ocu2        0.439274   0.060681   7.239 5.16e-13 ***
    pos_ocu3        0.097024   0.048155   2.015 0.043974 *  
    tamanio4        0.104309   0.034648   3.011 0.002620 ** 
    tamanio5        0.153051   0.039614   3.864 0.000113 ***
    tamanio6        0.279903   0.045492   6.153 8.17e-10 ***
    tamanio7        0.373274   0.054314   6.872 7.03e-12 ***
    tamanio8        0.011037   0.039460   0.280 0.779717    
    informal1      -0.304715   0.025666 -11.873  < 2e-16 ***
    jornada3        0.471611   0.098199   4.803 1.61e-06 ***
    jornada4        0.495496   0.122452   4.046 5.27e-05 ***
    jornada5        0.485537   0.139244   3.487 0.000493 ***
    unidad_eco2    -0.170882   0.032708  -5.225 1.81e-07 ***
    unidad_eco3    -0.122059   0.044974  -2.714 0.006670 ** 
    unidad_eco4    -0.047690   0.111501  -0.428 0.668883    
    over_obj:mujer -0.048602   0.040076  -1.213 0.225276    
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    (Dispersion parameter for gaussian family taken to be 0.2375453)

    Number of Fisher Scoring iterations: 2

Resultados de la estimación con términos de interacción relacionadas a
la sobrecalificación y al campo amplio de formación académica

``` r
resultados_cam
```


    Call:
    svyglm(formula = log(ingreso_mensual) ~ anios_estudios + over_obj * 
        campo_amplio + exper + I(exper^2) + log(horas_lab) + mujer + 
        jefe_hogar + regiones + division + pos_ocu + tamanio + informal + 
        jornada + unidad_eco, design = jovenes_reg)

    Survey design:
    Called via srvyr

    Coefficients:
                             Estimate Std. Error t value Pr(>|t|)    
    (Intercept)              7.743914   0.266763  29.029  < 2e-16 ***
    anios_estudios           0.043449   0.013003   3.341 0.000839 ***
    over_obj                -0.274320   0.058850  -4.661 3.22e-06 ***
    campo_amplio02           0.030777   0.065752   0.468 0.639751    
    campo_amplio03           0.003349   0.050777   0.066 0.947411    
    campo_amplio04           0.083674   0.052551   1.592 0.111390    
    campo_amplio05          -0.057380   0.124587  -0.461 0.645135    
    campo_amplio06           0.149858   0.085897   1.745 0.081108 .  
    campo_amplio07           0.027232   0.051804   0.526 0.599144    
    campo_amplio08           0.021760   0.121973   0.178 0.858416    
    campo_amplio09           0.035333   0.049142   0.719 0.472179    
    campo_amplio10          -0.024933   0.057303  -0.435 0.663498    
    campo_amplio99          -0.086892   0.088221  -0.985 0.324700    
    exper                    0.064006   0.015671   4.084 4.49e-05 ***
    I(exper^2)              -0.003550   0.001082  -3.281 0.001043 ** 
    log(horas_lab)           0.107175   0.063070   1.699 0.089318 .  
    mujer                   -0.107544   0.021116  -5.093 3.64e-07 ***
    jefe_hogar               0.127700   0.021899   5.831 5.82e-09 ***
    regiones2                0.117689   0.030554   3.852 0.000119 ***
    regiones3                0.047816   0.029082   1.644 0.100203    
    regiones4                0.210024   0.025968   8.088 7.46e-16 ***
    regiones5                0.071495   0.028350   2.522 0.011703 *  
    division2               -0.098617   0.047469  -2.077 0.037805 *  
    division3               -0.199319   0.054408  -3.663 0.000251 ***
    division4               -0.280460   0.057030  -4.918 9.02e-07 ***
    division5               -0.185315   0.060130  -3.082 0.002067 ** 
    division6               -0.341933   0.147511  -2.318 0.020486 *  
    division7               -0.215006   0.076461  -2.812 0.004942 ** 
    division8               -0.361007   0.062172  -5.807 6.75e-09 ***
    division9               -0.421639   0.071628  -5.887 4.19e-09 ***
    pos_ocu2                 0.438913   0.060597   7.243 5.01e-13 ***
    pos_ocu3                 0.101517   0.048098   2.111 0.034849 *  
    tamanio4                 0.102742   0.034964   2.939 0.003312 ** 
    tamanio5                 0.152180   0.039660   3.837 0.000126 ***
    tamanio6                 0.279588   0.045556   6.137 9.01e-10 ***
    tamanio7                 0.380356   0.055232   6.886 6.38e-12 ***
    tamanio8                 0.015474   0.039240   0.394 0.693346    
    informal1               -0.302359   0.025873 -11.686  < 2e-16 ***
    jornada3                 0.474379   0.098069   4.837 1.35e-06 ***
    jornada4                 0.508589   0.122098   4.165 3.16e-05 ***
    jornada5                 0.496374   0.138862   3.575 0.000354 ***
    unidad_eco2             -0.174176   0.032871  -5.299 1.21e-07 ***
    unidad_eco3             -0.122372   0.045193  -2.708 0.006796 ** 
    unidad_eco4             -0.020193   0.109218  -0.185 0.853325    
    over_obj:campo_amplio02  0.044687   0.100739   0.444 0.657354    
    over_obj:campo_amplio03  0.115803   0.075272   1.538 0.123996    
    over_obj:campo_amplio04  0.071448   0.068617   1.041 0.297802    
    over_obj:campo_amplio05  0.192760   0.139669   1.380 0.167609    
    over_obj:campo_amplio06 -0.106330   0.102640  -1.036 0.300271    
    over_obj:campo_amplio07  0.152470   0.070044   2.177 0.029540 *  
    over_obj:campo_amplio08  0.102328   0.179900   0.569 0.569511    
    over_obj:campo_amplio09  0.060500   0.074323   0.814 0.415672    
    over_obj:campo_amplio10  0.152799   0.079959   1.911 0.056060 .  
    over_obj:campo_amplio99  0.416387   0.130934   3.180 0.001481 ** 
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    (Dispersion parameter for gaussian family taken to be 0.2365574)

    Number of Fisher Scoring iterations: 2

Resultados de la estimación con términos de interacción relacionadas a
la sobrecalificación y a la región geográfica

``` r
resultados_reg
```


    Call:
    svyglm(formula = log(ingreso_mensual) ~ anios_estudios + over_obj * 
        regiones + exper + I(exper^2) + log(horas_lab) + mujer + 
        jefe_hogar + campo_amplio + division + pos_ocu + tamanio + 
        informal + jornada + unidad_eco, design = jovenes_reg)

    Survey design:
    Called via srvyr

    Coefficients:
                        Estimate Std. Error t value Pr(>|t|)    
    (Intercept)         7.706736   0.264789  29.105  < 2e-16 ***
    anios_estudios      0.043051   0.013105   3.285 0.001026 ** 
    over_obj           -0.184921   0.042118  -4.391 1.15e-05 ***
    regiones2           0.143212   0.046324   3.092 0.002002 ** 
    regiones3           0.038748   0.041686   0.930 0.352664    
    regiones4           0.194324   0.035783   5.431 5.86e-08 ***
    regiones5           0.072487   0.044237   1.639 0.101353    
    exper               0.064403   0.015714   4.098 4.22e-05 ***
    I(exper^2)         -0.003551   0.001084  -3.275 0.001063 ** 
    log(horas_lab)      0.108442   0.063367   1.711 0.087077 .  
    mujer              -0.105109   0.021321  -4.930 8.48e-07 ***
    jefe_hogar          0.126931   0.022246   5.706 1.22e-08 ***
    campo_amplio02      0.051673   0.050847   1.016 0.309558    
    campo_amplio03      0.060412   0.036897   1.637 0.101622    
    campo_amplio04      0.113296   0.033877   3.344 0.000831 ***
    campo_amplio05      0.027514   0.075749   0.363 0.716448    
    campo_amplio06      0.094222   0.054450   1.730 0.083613 .  
    campo_amplio07      0.108547   0.036454   2.978 0.002918 ** 
    campo_amplio08      0.074112   0.101690   0.729 0.466153    
    campo_amplio09      0.060828   0.037934   1.604 0.108872    
    campo_amplio10      0.074744   0.045198   1.654 0.098250 .  
    campo_amplio99      0.215836   0.102395   2.108 0.035089 *  
    division2          -0.090200   0.045767  -1.971 0.048795 *  
    division3          -0.200624   0.053670  -3.738 0.000187 ***
    division4          -0.279319   0.056340  -4.958 7.35e-07 ***
    division5          -0.180935   0.059808  -3.025 0.002496 ** 
    division6          -0.328049   0.145960  -2.248 0.024647 *  
    division7          -0.206039   0.075419  -2.732 0.006318 ** 
    division8          -0.354738   0.060524  -5.861 4.87e-09 ***
    division9          -0.416936   0.069856  -5.968 2.55e-09 ***
    pos_ocu2            0.433451   0.060735   7.137 1.08e-12 ***
    pos_ocu3            0.091117   0.047968   1.900 0.057547 .  
    tamanio4            0.103998   0.034716   2.996 0.002751 ** 
    tamanio5            0.148992   0.039888   3.735 0.000189 ***
    tamanio6            0.276428   0.045585   6.064 1.42e-09 ***
    tamanio7            0.370395   0.054917   6.745 1.70e-11 ***
    tamanio8            0.010478   0.039504   0.265 0.790828    
    informal1          -0.305326   0.025674 -11.893  < 2e-16 ***
    jornada3            0.471719   0.098764   4.776 1.83e-06 ***
    jornada4            0.497710   0.123121   4.042 5.36e-05 ***
    jornada5            0.489486   0.139847   3.500 0.000469 ***
    unidad_eco2        -0.167908   0.032776  -5.123 3.12e-07 ***
    unidad_eco3        -0.118921   0.045119  -2.636 0.008421 ** 
    unidad_eco4        -0.048545   0.110582  -0.439 0.660686    
    over_obj:regiones2 -0.047164   0.061344  -0.769 0.442021    
    over_obj:regiones3  0.012550   0.053802   0.233 0.815560    
    over_obj:regiones4  0.031634   0.046957   0.674 0.500541    
    over_obj:regiones5 -0.002742   0.053899  -0.051 0.959430    
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    (Dispersion parameter for gaussian family taken to be 0.2374611)

    Number of Fisher Scoring iterations: 2

Resultados de la prueba Wald

Años de escolaridad

``` r
contraste_over_edu
```

    Wald test for over_obj:anios_estudios
     in svyglm(formula = log(ingreso_mensual) ~ over_obj * anios_estudios + 
        exper + I(exper^2) + log(horas_lab) + mujer + jefe_hogar + 
        regiones + campo_amplio + division + pos_ocu + tamanio + 
        informal + jornada + unidad_eco, design = jovenes_reg)
    F =  6.124274  on  1  and  5334  df: p= 0.013365 

Género

``` r
contraste_over_muj
```

    Wald test for over_obj:mujer
     in svyglm(formula = log(ingreso_mensual) ~ anios_estudios + over_obj * 
        mujer + exper + I(exper^2) + log(horas_lab) + jefe_hogar + 
        regiones + campo_amplio + division + pos_ocu + tamanio + 
        informal + jornada + unidad_eco, design = jovenes_reg)
    F =  1.470781  on  1  and  5334  df: p= 0.22528 

Campo de formación académica

``` r
contraste_over_cam
```

    Wald test for over_obj:campo_amplio
     in svyglm(formula = log(ingreso_mensual) ~ anios_estudios + over_obj * 
        campo_amplio + exper + I(exper^2) + log(horas_lab) + mujer + 
        jefe_hogar + regiones + division + pos_ocu + tamanio + informal + 
        jornada + unidad_eco, design = jovenes_reg)
    F =  2.183209  on  10  and  5325  df: p= 0.016136 

Regiones

``` r
contraste_over_reg
```

    Wald test for over_obj:regiones
     in svyglm(formula = log(ingreso_mensual) ~ anios_estudios + over_obj * 
        regiones + exper + I(exper^2) + log(horas_lab) + mujer + 
        jefe_hogar + campo_amplio + division + pos_ocu + tamanio + 
        informal + jornada + unidad_eco, design = jovenes_reg)
    F =  0.4768821  on  4  and  5331  df: p= 0.75276 
