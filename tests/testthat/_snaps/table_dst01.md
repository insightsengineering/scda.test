# DST01 default variant is produced correctly

    Code
      res
    Output
                                        A: Drug X    B: Placebo   C: Combination   All Patients
                                         (N=134)      (N=134)        (N=132)         (N=400)   
      —————————————————————————————————————————————————————————————————————————————————————————
      COMPLETED                         68 (50.7%)   66 (49.3%)     73 (55.3%)     207 (51.7%) 
      ONGOING                           24 (17.9%)   28 (20.9%)     21 (15.9%)      73 (18.2%) 
      DISCONTINUED                      42 (31.3%)   40 (29.9%)     38 (28.8%)     120 (30.0%) 
        ADVERSE EVENT                    3 (2.2%)     6 (4.5%)       5 (3.8%)       14 (3.5%)  
        DEATH                           25 (18.7%)   23 (17.2%)     22 (16.7%)      70 (17.5%) 
        LACK OF EFFICACY                 2 (1.5%)     2 (1.5%)       3 (2.3%)        7 (1.8%)  
        PHYSICIAN DECISION               2 (1.5%)     3 (2.2%)       2 (1.5%)        7 (1.8%)  
        PROTOCOL VIOLATION               5 (3.7%)     3 (2.2%)        4 (3%)         12 (3%)   
        WITHDRAWAL BY PARENT/GUARDIAN     4 (3%)      2 (1.5%)       1 (0.8%)        7 (1.8%)  
        WITHDRAWAL BY SUBJECT            1 (0.7%)     1 (0.7%)       1 (0.8%)        3 (0.8%)  

# DST01 variants 2 and 3 are produced correctly

    Code
      res
    Output
                                          A: Drug X    B: Placebo   C: Combination   All Patients
                                           (N=134)      (N=134)        (N=132)         (N=400)   
      ———————————————————————————————————————————————————————————————————————————————————————————
      COMPLETED                           68 (50.7%)   66 (49.3%)     73 (55.3%)     207 (51.7%) 
      ONGOING                             24 (17.9%)   28 (20.9%)     21 (15.9%)      73 (18.2%) 
      DISCONTINUED                        42 (31.3%)   40 (29.9%)     38 (28.8%)     120 (30.0%) 
        Safety                                                                                   
          ADVERSE EVENT                    3 (2.2%)     6 (4.5%)       5 (3.8%)       14 (3.5%)  
          DEATH                           25 (18.7%)   23 (17.2%)     22 (16.7%)      70 (17.5%) 
        Non-Safety                                                                               
          LACK OF EFFICACY                 2 (1.5%)     2 (1.5%)       3 (2.3%)        7 (1.8%)  
          PHYSICIAN DECISION               2 (1.5%)     3 (2.2%)       2 (1.5%)        7 (1.8%)  
          PROTOCOL VIOLATION               5 (3.7%)     3 (2.2%)        4 (3%)         12 (3%)   
          WITHDRAWAL BY PARENT/GUARDIAN     4 (3%)      2 (1.5%)       1 (0.8%)        7 (1.8%)  
          WITHDRAWAL BY SUBJECT            1 (0.7%)     1 (0.7%)       1 (0.8%)        3 (0.8%)  

---

    Code
      res
    Output
                                          A: Drug X    B: Placebo   C: Combination   All Patients
                                           (N=134)      (N=134)        (N=132)         (N=400)   
      ———————————————————————————————————————————————————————————————————————————————————————————
      COMPLETED                           68 (50.7%)   66 (49.3%)     73 (55.3%)     207 (51.7%) 
      ONGOING                             24 (17.9%)   28 (20.9%)     21 (15.9%)      73 (18.2%) 
      DISCONTINUED                        42 (31.3%)   40 (29.9%)     38 (28.8%)     120 (30.0%) 
        Safety                                                                                   
          ADVERSE EVENT                    3 (2.2%)     6 (4.5%)       5 (3.8%)       14 (3.5%)  
          DEATH                           25 (18.7%)   23 (17.2%)     22 (16.7%)      70 (17.5%) 
        Non-Safety                                                                               
          LACK OF EFFICACY                 2 (1.5%)     2 (1.5%)       3 (2.3%)        7 (1.8%)  
          PHYSICIAN DECISION               2 (1.5%)     3 (2.2%)       2 (1.5%)        7 (1.8%)  
          PROTOCOL VIOLATION               5 (3.7%)     3 (2.2%)        4 (3%)         12 (3%)   
          WITHDRAWAL BY PARENT/GUARDIAN     4 (3%)      2 (1.5%)       1 (0.8%)        7 (1.8%)  
          WITHDRAWAL BY SUBJECT            1 (0.7%)     1 (0.7%)       1 (0.8%)        3 (0.8%)  
      COMPLETED                           47 (35.1%)   48 (35.8%)     47 (35.6%)     142 (35.5%) 
      ONGOING                             36 (26.9%)   41 (30.6%)     42 (31.8%)     119 (29.8%) 
      DISCONTINUED                        51 (38.1%)   45 (33.6%)     43 (32.6%)     139 (34.8%) 

