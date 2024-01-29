# LBT13 variant 1: LOW works as expected

    Code
      res
    Output
      Parameter Code                                                                          
        Visit                                                                                 
          NCI CTCAE Grade at Visit      Placebo     Xanomeline High Dose   Xanomeline Low Dose
            Baseline NCI CTCAE Grade     (N=86)            (N=72)                (N=96)       
      ————————————————————————————————————————————————————————————————————————————————————————
      ALB                                                                                     
        POST-BASELINE MINIMUM                                                                 
          Not Low (n)                      66                67                    62         
            Not Low                    66 (100%)         67 (100%)              62 (100%)     
          1 (n)                            15                5                     13         
            Not Low                    14 (93.3%)         5 (100%)             12 (92.3%)     
            1                           1 (6.7%)             0                  1 (7.7%)      
          2 (n)                            2                 0                      0         
            Not Low                    1 (50.0%)             0                      0         
            1                          1 (50.0%)             0                      0         

# LBT13 variant 2: HIGH works as expected

    Code
      res
    Output
      Parameter Code                                                                                          
        Visit                                                                                                 
          NCI CTCAE Grade at Visit      Placebo    Screen Failure   Xanomeline High Dose   Xanomeline Low Dose
            Baseline NCI CTCAE Grade    (N=86)         (N=0)               (N=72)                (N=96)       
      ————————————————————————————————————————————————————————————————————————————————————————————————————————
      ALB                                                                                                     
        POST-BASELINE MAXIMUM                                                                                 
          Not High (n)                    83             0                   72                    75         
            Not High                   83 (100%)         0               72 (100%)              75 (100%)     

# LBT13 variant 3: LOW without baseline missing works as expected

    Code
      res
    Output
      Parameter Code                                                                                           
        Visit                                                                                                  
          NCI CTCAE Grade at Visit      Placebo     Screen Failure   Xanomeline High Dose   Xanomeline Low Dose
            Baseline NCI CTCAE Grade     (N=86)         (N=0)               (N=72)                (N=96)       
      —————————————————————————————————————————————————————————————————————————————————————————————————————————
      ALB                                                                                                      
        POST-BASELINE MINIMUM                                                                                  
          Not Low (n)                      66             0                   67                    62         
            Not Low                    66 (100%)          0               67 (100%)              62 (100%)     
          1 (n)                            15             0                   5                     13         
            Not Low                    14 (93.3%)         0                5 (100%)             12 (92.3%)     
            1                           1 (6.7%)          0                   0                  1 (7.7%)      
          2 (n)                            2              0                   0                      0         
            Not Low                    1 (50.0%)          0                   0                      0         
            1                          1 (50.0%)          0                   0                      0         

# LBT13 variant 4: HIGH with missing baseline considered as grade 0 works as expected

    Code
      res
    Output
      Parameter Code                                                                                          
        Visit                                                                                                 
          NCI CTCAE Grade at Visit      Placebo    Screen Failure   Xanomeline High Dose   Xanomeline Low Dose
            Baseline NCI CTCAE Grade    (N=86)         (N=0)               (N=72)                (N=96)       
      ————————————————————————————————————————————————————————————————————————————————————————————————————————
      ALB                                                                                                     
        POST-BASELINE MAXIMUM                                                                                 
          Not High (n)                    83             0                   72                    75         
            Not High                   83 (100%)         0               72 (100%)              75 (100%)     

# LBT14 variant 5: HIGH with filled in grades works as expected

    Code
      res
    Output
      Parameter Code                                                                                          
        Visit                                                                                                 
          NCI CTCAE Grade at Visit      Placebo    Screen Failure   Xanomeline High Dose   Xanomeline Low Dose
            Baseline NCI CTCAE Grade    (N=86)         (N=0)               (N=72)                (N=96)       
      ————————————————————————————————————————————————————————————————————————————————————————————————————————
      ALB                                                                                                     
        POST-BASELINE MAXIMUM                                                                                 
          Not High (n)                    83             0                   72                    75         
            Not High                   83 (100%)         0               72 (100%)              75 (100%)     
            1                              0             0                   0                      0         
            2                              0             0                   0                      0         
            3                              0             0                   0                      0         
            4                              0             0                   0                      0         
            Missing                        0             0                   0                      0         
          1 (n)                            0             0                   0                      0         
            Not High                       0             0                   0                      0         
            1                              0             0                   0                      0         
            2                              0             0                   0                      0         
            3                              0             0                   0                      0         
            4                              0             0                   0                      0         
            Missing                        0             0                   0                      0         
          2 (n)                            0             0                   0                      0         
            Not High                       0             0                   0                      0         
            1                              0             0                   0                      0         
            2                              0             0                   0                      0         
            3                              0             0                   0                      0         
            4                              0             0                   0                      0         
            Missing                        0             0                   0                      0         
          3 (n)                            0             0                   0                      0         
            Not High                       0             0                   0                      0         
            1                              0             0                   0                      0         
            2                              0             0                   0                      0         
            3                              0             0                   0                      0         
            4                              0             0                   0                      0         
            Missing                        0             0                   0                      0         
          4 (n)                            0             0                   0                      0         
            Not High                       0             0                   0                      0         
            1                              0             0                   0                      0         
            2                              0             0                   0                      0         
            3                              0             0                   0                      0         
            4                              0             0                   0                      0         
            Missing                        0             0                   0                      0         
          Missing (n)                      0             0                   0                      0         
            Not High                       0             0                   0                      0         
            1                              0             0                   0                      0         
            2                              0             0                   0                      0         
            3                              0             0                   0                      0         
            4                              0             0                   0                      0         
            Missing                        0             0                   0                      0         

