# PKCT01 is produced correctly

    Code
      res
    Output
                                                       n     Mean   SD    SE    CV (%)   CV % Geometric Mean
      ——————————————————————————————————————————————————————————————————————————————————————————————————————
      Xanomeline High Dose                                                                                  
        Pharmacokinetic concentration of Xanomeline   1339   0.6    0.7   0.0   108.9            NA         
        Xanomeline Patch Dose                         247    54.0   0.0   0.0    0.0             0.0        
      Xanomeline Low Dose                                                                                   
        Pharmacokinetic concentration of Xanomeline   1343   0.6    0.7   0.0   109.3            NA         
        Xanomeline Patch Dose                         251    54.0   0.0   0.0    0.0             0.0        

---

    Code
      res
    Output
                                                      n_blq
      —————————————————————————————————————————————————————
      Xanomeline High Dose                                 
        Pharmacokinetic concentration of Xanomeline    333 
        Xanomeline Patch Dose                           0  
      Xanomeline Low Dose                                  
        Pharmacokinetic concentration of Xanomeline    335 
        Xanomeline Patch Dose                           0  

# Specific PKCT01 features are present

    Code
      res
    Output
      Summary of PK Concentrations by Nominal Time and Treatment: PK Evaluable
       Protocol: xxxxx
      Analyte:  Pharmacokinetic concentration of Xanomeline Treatment: Xanomeline High Dose
      Analyte:  Xanomeline Patch Dose Treatment: Xanomeline Low Dose
      
      ————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      Cohort/Treatment                            Number                                                                                                      
        Visit                                       of                                                                                                        
          Norminal Time from First Dose    n    <LTR/BLQ>s   Mean      SD      CV (%) Mean   Geometric Mean   CV % Geometric Mean   Median   Minimum   Maximum
      ————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      Xanomeline High Dose                                                                                                                                    
        BASELINE                                                                                                                                              
          0                               144       72        27      27.1        100.3            NE                 NE              27        0        54   
          0.08                            72        0         0.1    0.00489       4.9            0.1                 4.9            0.1     0.0912     0.112 
          0.5                             72        0        0.544   0.0241        4.4           0.543                4.4           0.543     0.499     0.603 
          1                               72        0        0.927    0.037        4.0           0.926                4.0           0.926     0.859     1.02  
          1.5                             72        0         1.2    0.0438        3.7            1.2                 3.6            1.2      1.12      1.31  
          2                               72        0        1.39    0.0474        3.4            1.39                3.4            1.38     1.31       1.5  
          4                               72        0        1.73    0.0521        3.0            1.73                3.0            1.73     1.65      1.84  
          6                               72        0        1.82    0.0538        3.0            1.82                3.0            1.82     1.74      1.92  
          8                               72        0        1.84    0.0545        3.0            1.84                3.0            1.84     1.76      1.94  
          12                              72        0        0.551   0.0341        6.2            0.55                6.2           0.554     0.486     0.619 
          16                              72        0        0.165   0.0181       11.0           0.164               11.1           0.165     0.134     0.198 
          24                              216       0         18      25.5        141.6          0.225             191466.4         0.0177    0.01       54   
          36                              72        72       0.005      0          0.0           0.005                0.0           0.005     0.005     0.005 
          48                              216      144        18      25.5        141.7          0.111             1518935.1        0.005     0.005      54   
      ————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      
      NE: Not Estimable

