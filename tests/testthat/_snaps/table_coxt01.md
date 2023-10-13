# 1. Cox Regression

    Code
      res
    Output
                                    n    Hazard Ratio      95% CI      p-value
      ————————————————————————————————————————————————————————————————————————
      Treatment:                                                              
        ARM A vs control (ARM B)   247       0.97       (0.66, 1.43)   0.8934 
      Covariate:                                                              
        Sex                        247       0.98       (0.67, 1.43)   0.8970 
        Race                       247       0.98       (0.67, 1.44)   0.9239 
        Age                        247       0.95       (0.65, 1.40)   0.7948 

# 2. Cox Regression (with Interaction Term)

    Code
      res
    Output
                                         n    Hazard Ratio      95% CI      p-value   Interaction p-value
      ———————————————————————————————————————————————————————————————————————————————————————————————————
      Treatment:                                                                                         
        ARM A vs control (ARM B)        247       0.97       (0.66, 1.43)   0.8934                       
      Covariate:                                                                                         
        Sex                             247                                                 0.1455       
            F                                     0.77       (0.47, 1.27)                                
            M                                     1.38       (0.75, 2.52)                                
        Race                            247                                                 0.6850       
            ASIAN                                 1.05       (0.63, 1.75)                                
            BLACK OR AFRICAN AMERICAN             1.08       (0.51, 2.29)                                
            WHITE                                 0.67       (0.27, 1.71)                                
        Age                             247                                                 0.7878       
            34                                    0.95       (0.65, 1.40)                                

# 3. Cox Regression (specifying covariates)

    Code
      res
    Output
                                         n    Hazard Ratio      95% CI      p-value   Interaction p-value
      ———————————————————————————————————————————————————————————————————————————————————————————————————
      Treatment:                                                                                         
        ARM A vs control (ARM B)        247       0.97       (0.66, 1.43)   0.8934                       
      Covariate:                                                                                         
        Sex                             247                                                 0.1455       
            F                                     0.77       (0.47, 1.27)                                
            M                                     1.38       (0.75, 2.52)                                
        Race                            247                                                 0.6850       
            ASIAN                                 1.05       (0.63, 1.75)                                
            BLACK OR AFRICAN AMERICAN             1.08       (0.51, 2.29)                                
            WHITE                                 0.67       (0.27, 1.71)                                
        Age                             247                                                 0.7878       
            30                                    0.98       (0.63, 1.51)                                
            40                                    0.91       (0.54, 1.51)                                
            50                                    0.84       (0.32, 2.20)                                

# 4. Cox Regression (setting strata, ties, and alpha level)

    Code
      res
    Output
                                         n    Hazard Ratio      90% CI      p-value   Interaction p-value
      ———————————————————————————————————————————————————————————————————————————————————————————————————
      Treatment:                                                                                         
        ARM A vs control (ARM B)        247       0.97       (0.71, 1.34)   0.8934                       
      Covariate:                                                                                         
        Sex                             247                                                 0.1455       
            F                                     0.77       (0.47, 1.27)                                
            M                                     1.38       (0.75, 2.52)                                
        Race                            247                                                 0.6850       
            ASIAN                                 1.05       (0.63, 1.75)                                
            BLACK OR AFRICAN AMERICAN             1.08       (0.51, 2.29)                                
            WHITE                                 0.67       (0.27, 1.71)                                
        Age                             247                                                 0.7878       
            30                                    0.98       (0.68, 1.41)                                
            40                                    0.91       (0.59, 1.39)                                
            50                                    0.84       (0.38, 1.88)                                

