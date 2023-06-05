# PKCT01 is produced correctly

    Code
      res
    Output
                         n     Mean    SD    SE    CV (%)   CV % Geometric Mean
      —————————————————————————————————————————————————————————————————————————
      A: Drug X                                                                
        Plasma Drug X   1340   7.1    6.6    0.2    92.9            NA         
        Plasma Drug Y    0      NA     NA    NA      NA             NA         
        Urine Drug X    670    1.0    1.9    0.1   188.0            NA         
        Urine Drug Y    670    1.0    1.9    0.1   188.0            NA         
      C: Combination                                                           
        Plasma Drug X   1320   7.2    6.7    0.2    93.7            NA         
        Plasma Drug Y   1320   14.4   13.5   0.4    93.7            NA         
        Urine Drug X    660    1.0    1.9    0.1   189.6            NA         
        Urine Drug Y    660    1.0    1.9    0.1   189.6            NA         

---

    Code
      res
    Output
                        n_blq
      ———————————————————————
      A: Drug X              
        Plasma Drug X    268 
        Plasma Drug Y     0  
        Urine Drug X     268 
        Urine Drug Y     268 
      C: Combination         
        Plasma Drug X    264 
        Plasma Drug Y    264 
        Urine Drug X     264 
        Urine Drug Y     264 

# Specific PKCT01 features are present

    Code
      res
    Output
      Summary of PK Concentrations by Nominal Time and Treatment: PK Evaluable
       Protocol: xxxxx
      Analyte:  Plasma Drug X Treatment: A: Drug X
      Analyte:  Urine Drug X Treatment: C: Combination
      Analyte:  Urine Drug Y Treatment: A: Drug X
      Analyte:  Plasma Drug Y Treatment: C: Combination
      
      ————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      Cohort/Treatment                                                                                                                                        
        Visit                                                                                                                                                 
          Norminal Time from First Dose           Number                                                                                                      
                                                    of                                                                                                        
                                           n    <LTR/BLQ>s    Mean      SD     CV (%) Mean   Geometric Mean   CV % Geometric Mean   Median   Minimum   Maximum
      ————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      A: Drug X                                                                                                                                               
        Day 1                                                                                                                                                 
          0                               402      402         0        0          NE              NE                 NE              0         0         0   
          0.5                             134       0         12.6     1.51       12.0            12.5               12.2            12.6     9.72      15.6  
          1                               134       0         16.2     1.63       10.0            16.1               10.1            16.2     12.6      19.9  
          1.5                             134       0         15.6     1.46        9.3            15.6                9.3            15.5     12.3       19   
          2                               134       0         13.4     1.35       10.1            13.4               10.0            13.3     10.8      16.5  
          3                               134       0         8.47     1.25       14.7            8.38               15.0            8.4      5.88      10.9  
          4                               402       0         4.79     1.01       21.2            4.69               21.9            4.79      2.7      7.09  
          8                               402       0        0.348    0.179       51.6           0.303               58.2           0.318     0.076     0.866 
          12                              402       0        0.0224   0.0189      84.4           0.0156              111.2          0.017     0.002     0.083 
        Day 2                                                                                                                                                 
          24                              402      402         0        0          NE              NE                 NE              0         0         0   
      ————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      
      NE: Not Estimable

