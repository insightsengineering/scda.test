# EXT01 default variant with numeric parameters is produced correctly

    Code
      res
    Output
                                                      Placebo      Xanomeline High Dose   Xanomeline Low Dose
      Parameter Category (Drug A/Drug B)               (N=86)             (N=72)                (N=96)       
      ———————————————————————————————————————————————————————————————————————————————————————————————————————
      Drug A                                                                                                 
        Overall duration (days)                                                                              
          n                                              44                 43                    52         
          Mean (SD)                                 82.8 (38.6)        74.1 (42.9)            81.9 (44.1)    
          Median                                        84.5               69.0                  88.5        
          Min - Max                                 7.0 - 142.0        9.0 - 148.0            1.0 - 149.0    
        Total number of doses administered                                                                   
          n                                              44                 43                    52         
          Mean (SD)                                 148.9 (61.8)       116.5 (63.8)           78.3 (70.6)    
          Median                                       182.0               99.0                  60.0        
          Min - Max                                 0.0 - 198.0        19.0 - 200.0           1.0 - 198.0    
        Total number of missed doses during study                                                            
          n                                              44                 43                    52         
          Mean (SD)                                  9.1 (6.0)          10.2 (5.8)             9.4 (5.9)     
          Median                                        9.0                10.0                  10.0        
          Min - Max                                  0.0 - 20.0         1.0 - 20.0            0.0 - 20.0     
      Drug B                                                                                                 
        Overall duration (days)                                                                              
          n                                              42                 29                    44         
          Mean (SD)                                 68.1 (35.4)        87.6 (41.8)            76.3 (41.8)    
          Median                                        64.5               85.0                  77.5        
          Min - Max                                 9.0 - 139.0        28.0 - 148.0          12.0 - 148.0    
        Total number of doses administered                                                                   
          n                                              42                 29                    44         
          Mean (SD)                                 146.6 (63.2)       105.9 (68.6)           94.9 (70.4)    
          Median                                       182.0               83.0                  76.5        
          Min - Max                                 14.0 - 210.0       15.0 - 196.0           0.0 - 212.0    
        Total number of missed doses during study                                                            
          n                                              42                 29                    44         
          Mean (SD)                                  10.2 (6.5)         10.1 (6.3)            10.1 (6.9)     
          Median                                        10.5               10.0                  10.0        
          Min - Max                                  0.0 - 20.0         0.0 - 20.0            0.0 - 20.0     

# EXT01 variant: with both numeric and categorical parameters

    Code
      res
    Output
                                               Placebo      Xanomeline High Dose   Xanomeline Low Dose
      Parameter Category (Drug A/Drug B)        (N=86)             (N=72)                (N=96)       
      ————————————————————————————————————————————————————————————————————————————————————————————————
      Drug A                                                                                          
        Overall duration (days)                                                                       
          n                                       44                 43                    52         
          Mean (SD)                          82.8 (38.6)        74.1 (42.9)            81.9 (44.1)    
          Median                                 84.5               69.0                  88.5        
          Min - Max                          7.0 - 142.0        9.0 - 148.0            1.0 - 149.0    
        Overall duration (days)                                                                       
          n                                       44                 43                    52         
          0 - 30                              5 (11.4%)          9 (20.9%)              9 (17.3%)     
          31 - 60                             6 (13.6%)          10 (23.3%)             8 (15.4%)     
          61 - 90                             15 (34.1%)         8 (18.6%)             11 (21.2%)     
          >= 91                               18 (40.9%)         16 (37.2%)            24 (46.2%)     
        Total dose administered                                                                       
          n                                       44                 43                    52         
          Mean (SD)                           0.0 (0.0)       8874.2 (5018.7)        4228.6 (3813.8)  
          Median                                 0.0               7452.0                3240.0       
          Min - Max                           0.0 - 0.0       1134.0 - 15417.0       54.0 - 10692.0   
        Total number of doses administered                                                            
          n                                       44                 43                    52         
          Mean (SD)                          148.9 (61.8)       116.5 (63.8)           78.3 (70.6)    
          Median                                182.0               99.0                  60.0        
          Min - Max                          0.0 - 198.0        19.0 - 200.0           1.0 - 198.0    
      Drug B                                                                                          
        Overall duration (days)                                                                       
          n                                       42                 29                    44         
          Mean (SD)                          68.1 (35.4)        87.6 (41.8)            76.3 (41.8)    
          Median                                 64.5               85.0                  77.5        
          Min - Max                          9.0 - 139.0        28.0 - 148.0          12.0 - 148.0    
        Overall duration (days)                                                                       
          n                                       42                 29                    44         
          0 - 30                              7 (16.7%)           2 (6.9%)              8 (18.2%)     
          31 - 60                              13 (31%)          7 (24.1%)             10 (22.7%)     
          61 - 90                             9 (21.4%)          7 (24.1%)             10 (22.7%)     
          >= 91                                13 (31%)          13 (44.8%)            16 (36.4%)     
        Total dose administered                                                                       
          n                                       42                 29                    44         
          Mean (SD)                           0.0 (0.0)       7551.6 (5408.6)        5123.9 (3803.9)  
          Median                                 0.0               5103.0                4131.0       
          Min - Max                           0.0 - 0.0       810.0 - 14931.0         0.0 - 11448.0   
        Total number of doses administered                                                            
          n                                       42                 29                    44         
          Mean (SD)                          146.6 (63.2)       105.9 (68.6)           94.9 (70.4)    
          Median                                182.0               83.0                  76.5        
          Min - Max                          14.0 - 210.0       15.0 - 196.0           0.0 - 212.0    

# EXT01 variant: with user specified categories for missed doses

    Code
      res
    Output
                                               Placebo      Xanomeline High Dose   Xanomeline Low Dose
      Parameter Category (Drug A/Drug B)        (N=86)             (N=72)                (N=96)       
      ————————————————————————————————————————————————————————————————————————————————————————————————
      Drug A                                                                                          
        Overall duration (days)                                                                       
          n                                       44                 43                    52         
          Mean (SD)                          82.8 (38.6)        74.1 (42.9)            81.9 (44.1)    
          Median                                 84.5               69.0                  88.5        
          Min - Max                          7.0 - 142.0        9.0 - 148.0            1.0 - 149.0    
        Overall duration (days)                                                                       
          n                                       44                 43                    52         
          0 - 30                              5 (11.4%)          9 (20.9%)              9 (17.3%)     
          31 - 60                             6 (13.6%)          10 (23.3%)             8 (15.4%)     
          61 - 90                             15 (34.1%)         8 (18.6%)             11 (21.2%)     
          >= 91                               18 (40.9%)         16 (37.2%)            24 (46.2%)     
        Total dose administered                                                                       
          n                                       44                 43                    52         
          Mean (SD)                           0.0 (0.0)       8874.2 (5018.7)        4228.6 (3813.8)  
          Median                                 0.0               7452.0                3240.0       
          Min - Max                           0.0 - 0.0       1134.0 - 15417.0       54.0 - 10692.0   
        Total number of doses administered                                                            
          n                                       44                 43                    52         
          Mean (SD)                          148.9 (61.8)       116.5 (63.8)           78.3 (70.6)    
          Median                                182.0               99.0                  60.0        
          Min - Max                          0.0 - 198.0        19.0 - 200.0           1.0 - 198.0    
        Missed Doses                                                                                  
          n                                       44                 43                    52         
          At least 1 missed dose              40 (46.5%)         43 (59.7%)            51 (53.1%)     
          At least 5 missed doses             33 (38.4%)         34 (47.2%)            37 (38.5%)     
          At least 10 missed doses            21 (24.4%)         23 (31.9%)            27 (28.1%)     
          At least 15 missed doses             8 (9.3%)          16 (22.2%)            13 (13.5%)     
      Drug B                                                                                          
        Overall duration (days)                                                                       
          n                                       42                 29                    44         
          Mean (SD)                          68.1 (35.4)        87.6 (41.8)            76.3 (41.8)    
          Median                                 64.5               85.0                  77.5        
          Min - Max                          9.0 - 139.0        28.0 - 148.0          12.0 - 148.0    
        Overall duration (days)                                                                       
          n                                       42                 29                    44         
          0 - 30                              7 (16.7%)           2 (6.9%)              8 (18.2%)     
          31 - 60                              13 (31%)          7 (24.1%)             10 (22.7%)     
          61 - 90                             9 (21.4%)          7 (24.1%)             10 (22.7%)     
          >= 91                                13 (31%)          13 (44.8%)            16 (36.4%)     
        Total dose administered                                                                       
          n                                       42                 29                    44         
          Mean (SD)                           0.0 (0.0)       7551.6 (5408.6)        5123.9 (3803.9)  
          Median                                 0.0               5103.0                4131.0       
          Min - Max                           0.0 - 0.0       810.0 - 14931.0         0.0 - 11448.0   
        Total number of doses administered                                                            
          n                                       42                 29                    44         
          Mean (SD)                          146.6 (63.2)       105.9 (68.6)           94.9 (70.4)    
          Median                                182.0               83.0                  76.5        
          Min - Max                          14.0 - 210.0       15.0 - 196.0           0.0 - 212.0    
        Missed Doses                                                                                  
          n                                       42                 29                    44         
          At least 1 missed dose              39 (45.3%)         28 (38.9%)            39 (40.6%)     
          At least 5 missed doses             30 (34.9%)         22 (30.6%)            32 (33.3%)     
          At least 10 missed doses            22 (25.6%)         15 (20.8%)             23 (24%)      
          At least 15 missed doses            14 (16.3%)         8 (11.1%)             15 (15.6%)     

