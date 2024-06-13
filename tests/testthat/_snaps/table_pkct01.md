# PKCT01 is produced correctly

    Code
      res
    Output
                                                       n     Mean   SD    SE    CV (%)   CV % Geometric Mean
      ——————————————————————————————————————————————————————————————————————————————————————————————————————
      Xanomeline High Dose                                                                                  
        Pharmacokinetic concentration of Xanomeline   1675   3.7    7.7   0.2   206.7            NA         
        Xanomeline Patch Dose                         247    54.0   0.0   0.0    0.0             0.0        
      Xanomeline Low Dose                                                                                   
        Pharmacokinetic concentration of Xanomeline   1679   3.7    7.6   0.2   207.0            NA         
        Xanomeline Patch Dose                         251    54.0   0.0   0.0    0.0             0.0        

---

    Code
      res
    Output
                                                      n_blq
      —————————————————————————————————————————————————————
      Xanomeline High Dose                                 
        Pharmacokinetic concentration of Xanomeline    393 
        Xanomeline Patch Dose                           0  
      Xanomeline Low Dose                                  
        Pharmacokinetic concentration of Xanomeline    402 
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
          3                               72        0        26.8     1.51         5.6            26.8                5.6            26.8     24.5      29.2  
          4                               72        0        1.73    0.0521        3.0            1.73                3.0            1.73     1.65      1.84  
          6                               72        0        1.82    0.0538        3.0            1.82                3.0            1.82     1.74      1.92  
          8                               72        0        1.84    0.0545        3.0            1.84                3.0            1.84     1.76      1.94  
          9                               72        0        22.7     1.69         7.4            22.6                7.5            22.6      20       25.3  
          12                              72        0        0.551   0.0341        6.2            0.55                6.2           0.554     0.486     0.619 
          16                              72        0        0.165   0.0181       11.0           0.164               11.1           0.165     0.134     0.198 
          18                              72        0        14.3     2.04        14.3            14.2               14.6            14.3     11.1      17.4  
          24                              216       0         18      25.5        141.6          0.225             191466.4         0.0177    0.01       54   
          36                              72        72       0.005      0          0.0           0.005                0.0           0.005     0.005     0.005 
          37                              72        51       0.278    0.531       191.0          0.0208             1414.0          0.005     0.005     1.78  
          48                              216      144        18      25.5        141.7          0.111             1518935.1        0.005     0.005      54   
      ————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      
      NE: Not Estimable

