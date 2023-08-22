# TTET01 default variant is produced correctly

    Code
      res
    Output
                                          A: Drug X        B: Placebo      C: Combination
                                           (N=134)           (N=134)          (N=132)    
      ———————————————————————————————————————————————————————————————————————————————————
      Patients with event (%)             58 (43.3%)       58 (43.3%)        69 (52.3%)  
        Earliest contributing event                                                      
          Death                               58               58                69      
      Patients without event (%)          76 (56.7%)       76 (56.7%)        63 (47.7%)  
      Time to Event (Months)                                                             
        Median                                NA               NA               9.4      
          95% CI                          (9.3, NA)         (9.4, NA)        (7.6, NA)   
        25% and 75%-ile                    5.6, NA           5.6, NA          5.0, NA    
        Range (censored)                 9.9 to 16.4       9.9 to 16.3      9.9 to 16.3  
        Range (event)                     0.5 to 9.6       0.9 to 9.6        0.5 to 9.8  
      Unstratified Analysis                                                              
        p-value (log-rank)                                   0.9998            0.1541    
        Hazard Ratio                                          1.00              1.29     
        95% CI                                            (0.69, 1.44)      (0.91, 1.83) 
      6 Months                                                                           
        Patients remaining at risk            97               97                90      
        Event Free Rate (%)                 72.39             72.39            68.18     
        95% CI                          (64.82, 79.96)   (64.82, 79.96)    (60.24, 76.13)
        Difference in Event Free Rate                         0.00             -4.21     
          95% CI                                         (-10.71, 10.71)   (-15.18, 6.77)
          p-value (Z-test)                                   1.0000            0.4525    
      12 Months                                                                          
        Patients remaining at risk            49               48                37      
        Event Free Rate (%)                 56.72             56.72            47.73     
        95% CI                          (48.33, 65.11)   (48.33, 65.11)    (39.21, 56.25)
        Difference in Event Free Rate                         0.00             -8.99     
          95% CI                                         (-11.86, 11.86)   (-20.95, 2.97)
          p-value (Z-test)                                   1.0000            0.1406    

# TTET01 variant 2: selecting sections to display

    Code
      res
    Output
                                       A: Drug X        B: Placebo     C: Combination
                                        (N=134)          (N=134)          (N=132)    
      ———————————————————————————————————————————————————————————————————————————————
      Patients with event (%)          58 (43.3%)       58 (43.3%)       69 (52.3%)  
      Patients without event (%)       76 (56.7%)       76 (56.7%)       63 (47.7%)  
      Time to Event (Months)                                                         
        Median                             NA               NA              9.4      
          95% CI                       (9.3, NA)        (9.4, NA)        (7.6, NA)   
        25% and 75%-ile                 5.6, NA          5.6, NA          5.0, NA    
        Range (censored)              9.9 to 16.4      9.9 to 16.3      9.9 to 16.3  
        Range (event)                  0.5 to 9.6       0.9 to 9.6       0.5 to 9.8  
      Unstratified Analysis                                                          
        p-value (log-rank)                                0.9998           0.1541    
        Hazard Ratio                                       1.00             1.29     
        95% CI                                         (0.69, 1.44)     (0.91, 1.83) 
      12 Months                                                                      
        Patients remaining at risk         49               48               37      
        Event Free Rate (%)              56.72            56.72            47.73     
        95% CI                       (48.33, 65.11)   (48.33, 65.11)   (39.21, 56.25)

# TTET01 variant 3: modifying analysis details like conftype, ties, alpha level

    Code
      res
    Output
                                        A: Drug X        B: Placebo      C: Combination
                                         (N=134)           (N=134)          (N=132)    
      —————————————————————————————————————————————————————————————————————————————————
      Patients with event (%)           58 (43.3%)       58 (43.3%)        69 (52.3%)  
        Earliest contributing event                                                    
          Death                             58               58                69      
      Patients without event (%)        76 (56.7%)       76 (56.7%)        63 (47.7%)  
      Time to Event (Months)                                                           
        Median                              NA               NA               9.4      
          90% CI                        (9.6, NA)         (9.6, NA)        (7.7, NA)   
        25% and 75%-ile                  5.6, NA           5.6, NA          5.0, NA    
        Range (censored)               9.9 to 16.4       9.9 to 16.3      9.9 to 16.3  
        Range (event)                   0.5 to 9.6       0.9 to 9.6        0.5 to 9.8  
      Unstratified Analysis                                                            
        p-value (log-rank)                                 0.9998            0.1541    
        Hazard Ratio                                        1.00              1.29     
        95% CI                                          (0.69, 1.44)      (0.91, 1.83) 
      12 Months                                                                        
        Patients remaining at risk          49               48                37      
        Event Free Rate (%)               56.72             56.72            47.73     
        90% CI                        (49.37, 63.41)   (49.37, 63.41)    (40.42, 54.66)
      Difference in Event Free Rate                         0.00             -8.99     
        97.5% CI                                       (-13.57, 13.57)   (-22.66, 4.69)
        p-value (Z-test)                                   1.0000            0.1406    

# TTET01 variant 4: with stratified analysis

    Code
      res
    Output
                                          A: Drug X        B: Placebo      C: Combination
                                           (N=134)           (N=134)          (N=132)    
      ———————————————————————————————————————————————————————————————————————————————————
      Patients with event (%)             58 (43.3%)       58 (43.3%)        69 (52.3%)  
        Earliest contributing event                                                      
          Death                               58               58                69      
      Patients without event (%)          76 (56.7%)       76 (56.7%)        63 (47.7%)  
      Time to Event (Months)                                                             
        Median                                NA               NA               9.4      
          95% CI                          (9.3, NA)         (9.4, NA)        (7.6, NA)   
        25% and 75%-ile                    5.6, NA           5.6, NA          5.0, NA    
        Range (censored)                 9.9 to 16.4       9.9 to 16.3      9.9 to 16.3  
        Range (event)                     0.5 to 9.6       0.9 to 9.6        0.5 to 9.8  
      Unstratified Analysis                                                              
        p-value (log-rank)                                   0.9998            0.1541    
        Hazard Ratio                                          1.00              1.29     
        95% CI                                            (0.69, 1.44)      (0.91, 1.83) 
      Stratified Analysis                                                                
        p-value (log-rank)                                   0.9978            0.1733    
        Hazard Ratio                                          1.00              1.27     
        95% CI                                            (0.69, 1.44)      (0.90, 1.81) 
      12 Months                                                                          
        Patients remaining at risk            49               48                37      
        Event Free Rate (%)                 56.72             56.72            47.73     
        95% CI                          (48.33, 65.11)   (48.33, 65.11)    (39.21, 56.25)
        Difference in Event Free Rate                         0.00             -8.99     
          95% CI                                         (-11.86, 11.86)   (-20.95, 2.97)
          p-value (Z-test)                                   1.0000            0.1406    

# TTET01 variant 5: modifying time point

    Code
      res
    Output
                                          A: Drug X        B: Placebo      C: Combination
                                           (N=134)           (N=134)          (N=132)    
      ———————————————————————————————————————————————————————————————————————————————————
      Patients with event (%)             58 (43.3%)       58 (43.3%)        69 (52.3%)  
        Earliest contributing event                                                      
          Death                               58               58                69      
      Patients without event (%)          76 (56.7%)       76 (56.7%)        63 (47.7%)  
      Time to Event (Months)                                                             
        Median                                NA               NA               9.4      
          95% CI                          (9.3, NA)         (9.4, NA)        (7.6, NA)   
        25% and 75%-ile                    5.6, NA           5.6, NA          5.0, NA    
        Range (censored)                 9.9 to 16.4       9.9 to 16.3      9.9 to 16.3  
        Range (event)                     0.5 to 9.6       0.9 to 9.6        0.5 to 9.8  
      Unstratified Analysis                                                              
        p-value (log-rank)                                   0.9998            0.1541    
        Hazard Ratio                                          1.00              1.29     
        95% CI                                            (0.69, 1.44)      (0.91, 1.83) 
      6 Months                                                                           
        Patients remaining at risk            97               97                90      
        Event Free Rate (%)                 72.39             72.39            68.18     
        95% CI                          (64.82, 79.96)   (64.82, 79.96)    (60.24, 76.13)
        Difference in Event Free Rate                         0.00             -4.21     
          95% CI                                         (-10.71, 10.71)   (-15.18, 6.77)
          p-value (Z-test)                                   1.0000            0.4525    

# TTET01 variant 6: requesting more than one p-value

    Code
      res
    Output
                                          A: Drug X        B: Placebo      C: Combination 
                                           (N=134)          (N=134)           (N=132)     
      ————————————————————————————————————————————————————————————————————————————————————
      Patients with event (%)              79 (59%)        87 (64.9%)       116 (87.9%)   
        Earliest contributing event                                                       
          Death                               79               87               116       
      Patients without event (%)           55 (41%)        47 (35.1%)        16 (12.1%)   
      Time to Event (Months)                                                              
        Median                               41.4             27.5              11.1      
          95% CI                         (27.7, 54.7)     (17.3, 30.2)      (9.6, 15.9)   
        25% and 75%-ile                   15.4, 75.2       9.5, 54.9         5.3, 25.2    
        Range (censored)                 0.4 to 154.7     0.9 to 91.0       0.3 to 49.4   
        Range (event)                    0.3 to 116.4     0.0 to 122.4      0.1 to 101.6  
      Unstratified Analysis                                                               
        p-value (log-rank)                                   0.0334           <0.0001     
        p-value (wald)                                       0.0342           <0.0001     
        p-value (likelihood)                                 0.0341           <0.0001     
          Hazard Ratio                                        1.39              2.75      
            95% CI                                        (1.03, 1.90)      (2.05, 3.70)  
      12 Months                                                                           
        Patients remaining at risk            92               83                56       
        Event Free Rate (%)                 78.03            70.32             46.39      
        95% CI                          (70.82, 85.24)   (62.27, 78.37)    (37.59, 55.18) 
        Difference in Event Free Rate                        -7.71             -31.64     
          95% CI                                         (-18.51, 3.10)   (-43.01, -20.26)
          p-value (Z-test)                                   0.1622           <0.0001     
