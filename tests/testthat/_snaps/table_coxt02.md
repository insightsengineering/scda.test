# COXT02 default variant 1 is produced correctly

    Code
      res
    Output
                                    Hazard Ratio      95% CI      p-value
      ———————————————————————————————————————————————————————————————————
      Treatment:                                                         
        ARMCD (reference = ARM B)                                 0.2643 
          ARM A                         0.96       (0.66, 1.42)   0.8536 
          ARM C                         1.27       (0.88, 1.83)   0.2010 
      Covariate:                                                         
        Sex (reference = F)                                              
          M                             1.09       (0.80, 1.48)   0.5987 
        Age                                                              
          All                           0.99       (0.97, 1.01)   0.5104 

# COXT02 variant 5 is produced correctly

    Code
      res
    Output
                                    Hazard Ratio      90% CI   
      —————————————————————————————————————————————————————————
      Treatment:                                               
        ARMCD (reference = ARM B)                              
          ARM A                         0.97       (0.71, 1.35)
          ARM C                         1.27       (0.93, 1.73)
      Covariate:                                               
        Sex (reference = F)                                    
          M                             1.08       (0.83, 1.40)
        Age                                                    
          All                           0.99       (0.98, 1.01)

