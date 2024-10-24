# EXT01 default variant with numeric parameters is produced correctly

    Code
      res
    Output
                                                      Placebo      Xanomeline High Dose   Xanomeline Low Dose
      Parameter Category (Drug A/Drug B)               (N=86)             (N=72)                (N=96)       
      ———————————————————————————————————————————————————————————————————————————————————————————————————————
      Drug A                                                                                                 
        Overall duration (days)                                                                              
          n                                              39                 43                    50         
          Mean (SD)                                 84.1 (40.5)        80.3 (43.5)            78.8 (41.4)    
          Median                                        81.0               81.0                  85.5        
          Min - Max                                 9.0 - 142.0        9.0 - 148.0            7.0 - 145.0    
        Total number of doses administered                                                                   
          n                                              39                 43                    50         
          Mean (SD)                                 141.6 (65.9)       109.3 (66.6)           90.3 (70.3)    
          Median                                       182.0               95.0                  71.5        
          Min - Max                                 0.0 - 210.0        15.0 - 190.0           0.0 - 193.0    
        Total number of missed doses during study                                                            
          n                                              39                 43                    50         
          Mean (SD)                                  10.2 (5.9)         9.8 (5.9)              9.4 (6.2)     
          Median                                        11.0               9.0                   10.0        
          Min - Max                                  0.0 - 20.0         0.0 - 20.0            0.0 - 20.0     
      Drug B                                                                                                 
        Overall duration (days)                                                                              
          n                                              47                 29                    46         
          Mean (SD)                                 68.6 (33.8)        78.4 (42.1)            79.8 (44.9)    
          Median                                        65.0               69.0                  82.5        
          Min - Max                                 7.0 - 134.0        11.0 - 148.0           1.0 - 149.0    
        Total number of doses administered                                                                   
          n                                              47                 29                    46         
          Mean (SD)                                 153.0 (59.1)       116.5 (64.8)           81.1 (71.5)    
          Median                                       182.0              114.0                  60.5        
          Min - Max                                 7.0 - 198.0        22.0 - 200.0           1.0 - 212.0    
        Total number of missed doses during study                                                            
          n                                              47                 29                    46         
          Mean (SD)                                  9.1 (6.5)          10.7 (6.1)            10.2 (6.5)     
          Median                                        9.0                12.0                  10.0        
          Min - Max                                  0.0 - 20.0         1.0 - 20.0            0.0 - 20.0     

# EXT01 variant: with both numeric and categorical parameters

    Code
      res
    Output
                                               Placebo      Xanomeline High Dose   Xanomeline Low Dose
      Parameter Category (Drug A/Drug B)        (N=86)             (N=72)                (N=96)       
      ————————————————————————————————————————————————————————————————————————————————————————————————
      Drug A                                                                                          
        Overall duration (days)                                                                       
          n                                       39                 43                    50         
          Mean (SD)                          84.1 (40.5)        80.3 (43.5)            78.8 (41.4)    
          Median                                 81.0               81.0                  85.5        
          Min - Max                          9.0 - 142.0        9.0 - 148.0            7.0 - 145.0    
        Overall duration (days)                                                                       
          n                                       39                 43                    50         
          0 - 30                              4 (10.3%)           6 (14%)               10 (20%)      
          31 - 60                             6 (15.4%)          10 (23.3%)              8 (16%)      
          61 - 90                             13 (33.3%)         9 (20.9%)              12 (24%)      
          >= 91                                16 (41%)          18 (41.9%)             20 (40%)      
        Total dose administered                                                                       
          n                                       39                 43                    50         
          Mean (SD)                           0.0 (0.0)       8305.3 (5230.6)        4877.3 (3798.0)  
          Median                                 0.0               7263.0                3861.0       
          Min - Max                           0.0 - 0.0       810.0 - 14607.0         0.0 - 10422.0   
        Total number of doses administered                                                            
          n                                       39                 43                    50         
          Mean (SD)                          141.6 (65.9)       109.3 (66.6)           90.3 (70.3)    
          Median                                182.0               95.0                  71.5        
          Min - Max                          0.0 - 210.0        15.0 - 190.0           0.0 - 193.0    
      Drug B                                                                                          
        Overall duration (days)                                                                       
          n                                       47                 29                    46         
          Mean (SD)                          68.6 (33.8)        78.4 (42.1)            79.8 (44.9)    
          Median                                 65.0               69.0                  82.5        
          Min - Max                          7.0 - 134.0        11.0 - 148.0           1.0 - 149.0    
        Overall duration (days)                                                                       
          n                                       47                 29                    46         
          0 - 30                               8 (17%)           5 (17.2%)              7 (15.2%)     
          31 - 60                             13 (27.7%)         7 (24.1%)             10 (21.7%)     
          61 - 90                             11 (23.4%)         6 (20.7%)              9 (19.6%)     
          >= 91                               15 (31.9%)         11 (37.9%)            20 (43.5%)     
        Total dose administered                                                                       
          n                                       47                 29                    46         
          Mean (SD)                           0.0 (0.0)       8395.1 (5203.2)        4379.9 (3859.6)  
          Median                                 0.0               6966.0                3267.0       
          Min - Max                           0.0 - 0.0       810.0 - 15417.0        54.0 - 11448.0   
        Total number of doses administered                                                            
          n                                       47                 29                    46         
          Mean (SD)                          153.0 (59.1)       116.5 (64.8)           81.1 (71.5)    
          Median                                182.0              114.0                  60.5        
          Min - Max                          7.0 - 198.0        22.0 - 200.0           1.0 - 212.0    

# EXT01 variant: with user specified categories for missed doses

    Code
      res
    Output
                                               Placebo      Xanomeline High Dose   Xanomeline Low Dose
      Parameter Category (Drug A/Drug B)        (N=86)             (N=72)                (N=96)       
      ————————————————————————————————————————————————————————————————————————————————————————————————
      Drug A                                                                                          
        Overall duration (days)                                                                       
          n                                       39                 43                    50         
          Mean (SD)                          84.1 (40.5)        80.3 (43.5)            78.8 (41.4)    
          Median                                 81.0               81.0                  85.5        
          Min - Max                          9.0 - 142.0        9.0 - 148.0            7.0 - 145.0    
        Overall duration (days)                                                                       
          n                                       39                 43                    50         
          0 - 30                              4 (10.3%)           6 (14%)               10 (20%)      
          31 - 60                             6 (15.4%)          10 (23.3%)              8 (16%)      
          61 - 90                             13 (33.3%)         9 (20.9%)              12 (24%)      
          >= 91                                16 (41%)          18 (41.9%)             20 (40%)      
        Total dose administered                                                                       
          n                                       39                 43                    50         
          Mean (SD)                           0.0 (0.0)       8305.3 (5230.6)        4877.3 (3798.0)  
          Median                                 0.0               7263.0                3861.0       
          Min - Max                           0.0 - 0.0       810.0 - 14607.0         0.0 - 10422.0   
        Total number of doses administered                                                            
          n                                       39                 43                    50         
          Mean (SD)                          141.6 (65.9)       109.3 (66.6)           90.3 (70.3)    
          Median                                182.0               95.0                  71.5        
          Min - Max                          0.0 - 210.0        15.0 - 190.0           0.0 - 193.0    
        Missed Doses                                                                                  
          n                                       39                 43                    50         
          At least 1 missed dose               37 (43%)          42 (58.3%)             48 (50%)      
          At least 5 missed doses             32 (37.2%)         34 (47.2%)            36 (37.5%)     
          At least 10 missed doses            21 (24.4%)         20 (27.8%)            26 (27.1%)     
          At least 15 missed doses            10 (11.6%)         13 (18.1%)            12 (12.5%)     
      Drug B                                                                                          
        Overall duration (days)                                                                       
          n                                       47                 29                    46         
          Mean (SD)                          68.6 (33.8)        78.4 (42.1)            79.8 (44.9)    
          Median                                 65.0               69.0                  82.5        
          Min - Max                          7.0 - 134.0        11.0 - 148.0           1.0 - 149.0    
        Overall duration (days)                                                                       
          n                                       47                 29                    46         
          0 - 30                               8 (17%)           5 (17.2%)              7 (15.2%)     
          31 - 60                             13 (27.7%)         7 (24.1%)             10 (21.7%)     
          61 - 90                             11 (23.4%)         6 (20.7%)              9 (19.6%)     
          >= 91                               15 (31.9%)         11 (37.9%)            20 (43.5%)     
        Total dose administered                                                                       
          n                                       47                 29                    46         
          Mean (SD)                           0.0 (0.0)       8395.1 (5203.2)        4379.9 (3859.6)  
          Median                                 0.0               6966.0                3267.0       
          Min - Max                           0.0 - 0.0       810.0 - 15417.0        54.0 - 11448.0   
        Total number of doses administered                                                            
          n                                       47                 29                    46         
          Mean (SD)                          153.0 (59.1)       116.5 (64.8)           81.1 (71.5)    
          Median                                182.0              114.0                  60.5        
          Min - Max                          7.0 - 198.0        22.0 - 200.0           1.0 - 212.0    
        Missed Doses                                                                                  
          n                                       47                 29                    46         
          At least 1 missed dose              42 (48.8%)         29 (40.3%)            42 (43.8%)     
          At least 5 missed doses              31 (36%)          22 (30.6%)            33 (34.4%)     
          At least 10 missed doses            22 (25.6%)          18 (25%)              24 (25%)      
          At least 15 missed doses             12 (14%)          11 (15.3%)            16 (16.7%)     

