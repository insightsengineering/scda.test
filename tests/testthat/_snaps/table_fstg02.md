# FSTG02 table variant 1 (Subgroup Analysis of Survival Duration) is produced correctly

    Code
      res
    Output
      Baseline Risk Factors                          B: Placebo               A: Drug X                                    
                                      Total n    n    Median (Months)    n    Median (Months)   Hazard Ratio   95% Wald CI 
      —————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      All Patients                      268     134         NA          134         NA              1.00       (0.69, 1.44)
      Sex                                                                                                                  
        F                               161     82          NA          79          NA              0.79       (0.49, 1.28)
        M                               107     52          NA          55          9.6             1.39       (0.78, 2.47)
      Categorical Level Biomarker 2                                                                                        
        LOW                             95      45          NA          50          9.3             1.14       (0.64, 2.02)
        MEDIUM                          93      56          NA          37          NA              0.97       (0.52, 1.82)
        HIGH                            80      33          NA          47          NA              0.97       (0.45, 2.12)

# FSTG02 table variant 2 (specifying class variables and options for the treatment variable)

    Code
      res
    Output
      Baseline Risk Factors                      Placebo                 Drug X                                      
                                Total n    n    Median (Months)    n    Median (Months)   Hazard Ratio   95% Wald CI 
      ———————————————————————————————————————————————————————————————————————————————————————————————————————————————
      All Patients                268     134         NA          134         NA              1.00       (0.69, 1.44)
      Sex                                                                                                            
        M                         107     52          NA          55          9.6             1.39       (0.78, 2.47)
        F                         161     82          NA          79          NA              0.79       (0.49, 1.28)
      Stratification Factor 1                                                                                        
        C                         94      45          NA          49          NA              0.75       (0.41, 1.38)
        B                         92      45          NA          47          NA              1.34       (0.71, 2.54)
        A                         82      44          NA          38          NA              1.02       (0.53, 1.97)

# FSTG02 table variant 3 (selecting columns and changing the alpha level)

    Code
      res
    Output
      Baseline Risk Factors                                                
                                      Total n   Hazard Ratio   90% Wald CI 
      —————————————————————————————————————————————————————————————————————
      All Patients                      268         1.00       (0.74, 1.36)
      Sex                                                                  
        F                               161         0.79       (0.53, 1.19)
        M                               107         1.39       (0.86, 2.25)
      Categorical Level Biomarker 2                                        
        LOW                             95          1.14       (0.71, 1.84)
        MEDIUM                          93          0.97       (0.58, 1.64)
        HIGH                            80          0.97       (0.51, 1.87)

# FSTG02 table variant 4 (fixed symbol size) is produced correctly

    Code
      res
    Output
      Baseline Risk Factors                          B: Placebo               A: Drug X                                    
                                      Total n    n    Median (Months)    n    Median (Months)   Hazard Ratio   95% Wald CI 
      —————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      All Patients                      268     134         NA          134         NA              1.00       (0.69, 1.44)
      Sex                                                                                                                  
        F                               161     82          NA          79          NA              0.79       (0.49, 1.28)
        M                               107     52          NA          55          9.6             1.39       (0.78, 2.47)
      Categorical Level Biomarker 2                                                                                        
        LOW                             95      45          NA          50          9.3             1.14       (0.64, 2.02)
        MEDIUM                          93      56          NA          37          NA              0.97       (0.52, 1.82)
        HIGH                            80      33          NA          47          NA              0.97       (0.45, 2.12)

