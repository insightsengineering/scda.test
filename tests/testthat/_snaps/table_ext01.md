# EXT01 default variant with numeric parameters is produced correctly

    Code
      res
    Output
                                                      Placebo      Xanomeline High Dose   Xanomeline Low Dose
      Parameter Category (Drug A/Drug B)               (N=86)             (N=72)                (N=96)       
      ———————————————————————————————————————————————————————————————————————————————————————————————————————
      Drug A                                                                                                 
        Overall duration (days)                                                                              
          n                                              36                 33                    46         
          Mean (SD)                                 80.5 (40.6)        75.2 (44.6)            77.0 (42.3)    
          Median                                        86.5               66.0                  76.5        
          Min - Max                                 7.0 - 142.0        11.0 - 148.0           1.0 - 149.0    
        Total number of doses administered                                                                   
          n                                              36                 33                    46         
          Mean (SD)                                 157.5 (53.1)       102.2 (65.0)           65.0 (62.0)    
          Median                                       183.0               83.0                  44.0        
          Min - Max                                 7.0 - 198.0        15.0 - 196.0           1.0 - 193.0    
        Total number of missed doses during study                                                            
          n                                              36                 33                    46         
          Mean (SD)                                  8.9 (5.3)          9.6 (5.4)             10.7 (5.9)     
          Median                                        8.5                10.0                  10.0        
          Min - Max                                  0.0 - 20.0         1.0 - 20.0            0.0 - 20.0     
      Drug B                                                                                                 
        Overall duration (days)                                                                              
          n                                              50                 39                    50         
          Mean (SD)                                 72.1 (35.2)        83.2 (41.2)            81.4 (43.7)    
          Median                                        69.5               85.0                  90.0        
          Min - Max                                 9.0 - 139.0        9.0 - 148.0            3.0 - 145.0    
        Total number of doses administered                                                                   
          n                                              50                 39                    50         
          Mean (SD)                                 140.8 (67.6)       120.7 (65.6)          105.1 (73.3)    
          Median                                       181.5              142.0                  104.5       
          Min - Max                                 0.0 - 210.0        19.0 - 200.0           0.0 - 212.0    
        Total number of missed doses during study                                                            
          n                                              50                 39                    50         
          Mean (SD)                                  10.1 (6.8)         10.6 (6.4)             8.8 (6.7)     
          Median                                        11.5               12.0                   8.5        
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
          n                                       36                 33                    46         
          Mean (SD)                          80.5 (40.6)        75.2 (44.6)            77.0 (42.3)    
          Median                                 86.5               66.0                  76.5        
          Min - Max                          7.0 - 142.0        11.0 - 148.0           1.0 - 149.0    
        Overall duration (days)                                                                       
          n                                       36                 33                    46         
          0 - 30                              5 (13.9%)          7 (21.2%)              8 (17.4%)     
          31 - 60                             8 (22.2%)          8 (24.2%)             10 (21.7%)     
          61 - 90                             6 (16.7%)          6 (18.2%)             12 (26.1%)     
          >= 91                               17 (47.2%)         12 (36.4%)            16 (34.8%)     
        Total dose administered                                                                       
          n                                       36                 33                    46         
          Mean (SD)                           0.0 (0.0)       7745.7 (5125.2)        3512.3 (3346.7)  
          Median                                 0.0               6156.0                2376.0       
          Min - Max                           0.0 - 0.0       810.0 - 14931.0        54.0 - 10422.0   
        Total number of doses administered                                                            
          n                                       36                 33                    46         
          Mean (SD)                          157.5 (53.1)       102.2 (65.0)           65.0 (62.0)    
          Median                                183.0               83.0                  44.0        
          Min - Max                          7.0 - 198.0        15.0 - 196.0           1.0 - 193.0    
      Drug B                                                                                          
        Overall duration (days)                                                                       
          n                                       50                 39                    50         
          Mean (SD)                          72.1 (35.2)        83.2 (41.2)            81.4 (43.7)    
          Median                                 69.5               85.0                  90.0        
          Min - Max                          9.0 - 139.0        9.0 - 148.0            3.0 - 145.0    
        Overall duration (days)                                                                       
          n                                       50                 39                    50         
          0 - 30                               7 (14%)           4 (10.3%)               9 (18%)      
          31 - 60                              11 (22%)          9 (23.1%)               8 (16%)      
          61 - 90                              18 (36%)          9 (23.1%)               9 (18%)      
          >= 91                                14 (28%)          17 (43.6%)             24 (48%)      
        Total dose administered                                                                       
          n                                       50                 39                    50         
          Mean (SD)                           0.0 (0.0)       8845.6 (5244.2)        5675.4 (3956.4)  
          Median                                 0.0              10692.0                5643.0       
          Min - Max                           0.0 - 0.0       810.0 - 15417.0         0.0 - 11448.0   
        Total number of doses administered                                                            
          n                                       50                 39                    50         
          Mean (SD)                          140.8 (67.6)       120.7 (65.6)          105.1 (73.3)    
          Median                                181.5              142.0                  104.5       
          Min - Max                          0.0 - 210.0        19.0 - 200.0           0.0 - 212.0    

# EXT01 variant: with user specified categories for missed doses

    Code
      res
    Output
                                               Placebo      Xanomeline High Dose   Xanomeline Low Dose
      Parameter Category (Drug A/Drug B)        (N=86)             (N=72)                (N=96)       
      ————————————————————————————————————————————————————————————————————————————————————————————————
      Drug A                                                                                          
        Overall duration (days)                                                                       
          n                                       36                 33                    46         
          Mean (SD)                          80.5 (40.6)        75.2 (44.6)            77.0 (42.3)    
          Median                                 86.5               66.0                  76.5        
          Min - Max                          7.0 - 142.0        11.0 - 148.0           1.0 - 149.0    
        Overall duration (days)                                                                       
          n                                       36                 33                    46         
          0 - 30                              5 (13.9%)          7 (21.2%)              8 (17.4%)     
          31 - 60                             8 (22.2%)          8 (24.2%)             10 (21.7%)     
          61 - 90                             6 (16.7%)          6 (18.2%)             12 (26.1%)     
          >= 91                               17 (47.2%)         12 (36.4%)            16 (34.8%)     
        Total dose administered                                                                       
          n                                       36                 33                    46         
          Mean (SD)                           0.0 (0.0)       7745.7 (5125.2)        3512.3 (3346.7)  
          Median                                 0.0               6156.0                2376.0       
          Min - Max                           0.0 - 0.0       810.0 - 14931.0        54.0 - 10422.0   
        Total number of doses administered                                                            
          n                                       36                 33                    46         
          Mean (SD)                          157.5 (53.1)       102.2 (65.0)           65.0 (62.0)    
          Median                                183.0               83.0                  44.0        
          Min - Max                          7.0 - 198.0        15.0 - 196.0           1.0 - 193.0    
        Missed Doses                                                                                  
          n                                       36                 33                    46         
          At least 1 missed dose              35 (40.7%)         33 (45.8%)            44 (45.8%)     
          At least 5 missed doses             29 (33.7%)         25 (34.7%)            39 (40.6%)     
          At least 10 missed doses            15 (17.4%)         17 (23.6%)             25 (26%)      
          At least 15 missed doses              6 (7%)           9 (12.5%)             15 (15.6%)     
      Drug B                                                                                          
        Overall duration (days)                                                                       
          n                                       50                 39                    50         
          Mean (SD)                          72.1 (35.2)        83.2 (41.2)            81.4 (43.7)    
          Median                                 69.5               85.0                  90.0        
          Min - Max                          9.0 - 139.0        9.0 - 148.0            3.0 - 145.0    
        Overall duration (days)                                                                       
          n                                       50                 39                    50         
          0 - 30                               7 (14%)           4 (10.3%)               9 (18%)      
          31 - 60                              11 (22%)          9 (23.1%)               8 (16%)      
          61 - 90                              18 (36%)          9 (23.1%)               9 (18%)      
          >= 91                                14 (28%)          17 (43.6%)             24 (48%)      
        Total dose administered                                                                       
          n                                       50                 39                    50         
          Mean (SD)                           0.0 (0.0)       8845.6 (5244.2)        5675.4 (3956.4)  
          Median                                 0.0              10692.0                5643.0       
          Min - Max                           0.0 - 0.0       810.0 - 15417.0         0.0 - 11448.0   
        Total number of doses administered                                                            
          n                                       50                 39                    50         
          Mean (SD)                          140.8 (67.6)       120.7 (65.6)          105.1 (73.3)    
          Median                                181.5              142.0                  104.5       
          Min - Max                          0.0 - 210.0        19.0 - 200.0           0.0 - 212.0    
        Missed Doses                                                                                  
          n                                       50                 39                    50         
          At least 1 missed dose              44 (51.2%)         38 (52.8%)            46 (47.9%)     
          At least 5 missed doses             34 (39.5%)         31 (43.1%)            30 (31.2%)     
          At least 10 missed doses            28 (32.6%)         21 (29.2%)             25 (26%)      
          At least 15 missed doses            16 (18.6%)         15 (20.8%)            13 (13.5%)     

