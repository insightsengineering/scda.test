# Pagination with specific column widths and minimum lines per page

    Code
      pag_res[9:10]
    Output
      [[1]]
                            C: Combination
                               (N=132)    
      ————————————————————————————————————
      cl A.1                              
        - Any Grade -         89 (67.4%)  
        Grade 1-2             89 (67.4%)  
        1                     39 (29.5%)  
        2                     50 (37.9%)  
          dcd A.1.1.1.2                   
            - Any Grade -     50 (37.9%)  
            Grade 1-2         50 (37.9%)  
            2                 50 (37.9%)  
      
      [[2]]
                            A: Drug X 
                             (N=134)  
      ————————————————————————————————
      cl B.2                          
        - Any Grade -       79 (59.0%)
        Grade 1-2           30 (22.4%)
        1                   30 (22.4%)
        Grade 3-4           49 (36.6%)
        3                   49 (36.6%)
          dcd B.2.2.3.1               
            - Any Grade -   48 (35.8%)
            Grade 1-2       48 (35.8%)
            1               48 (35.8%)
      

# Pagination works also if table is decorated

    Code
      cat(res1)
    Output
      main title with some new 
      line
      sub
      -------
      titles
      
           —————————————————————————————————————————————
           SOME         A: Drug       B:      C: Combina
           TOPLEFT         X       Placebo       tion   
                        (N=134)    (N=134)     (N=132)  
           —————————————————————————————————————————————
           - Any          122        123         120    
           Grade -      (91.0%)    (91.8%)     (90.9%)  
           Grade 1-2       13         19      11 (8.3%) 
                         (9.7%)    (14.2%)              
           1            7 (5.2%)   9 (6.7%)    4 (3.0%) 
           2            6 (4.5%)      10       7 (5.3%) 
                                    (7.5%)              
           Grade 3-4       33         34      34 (25.8%)
                        (24.6%)    (25.4%)              
           3               18         14      16 (12.1%)
                        (13.4%)    (10.4%)              
           4               15         20      18 (13.6%)
                        (11.2%)    (14.9%)              
                                     {1}                
           Grade 5         76         70      75 (56.8%)
                        (56.7%)    (52.2%)              
           cl A.1                                       
             - Any         78         75      89 (67.4%)
             Grade -    (58.2%)    (56.0%)              
             Grade         78         75      89 (67.4%)
             1-2        (58.2%)    (56.0%)              
             1             30         27      39 (29.5%)
                        (22.4%)    (20.1%)              
             2             48         48      50 (37.9%)
                        (35.8%)    (35.8%)              
               dcd A.                                   
               1.1.1.                                   
               1                                        
                 -         50         45      63 (47.7%)
                 Any    (37.3%)    (33.6%)              
                 Grad                                   
                 e -                                    
                 Grad      50         45      63 (47.7%)
                 e      (37.3%)    (33.6%)              
                 1-2                                    
                 1         50         45      63 (47.7%)
                        (37.3%)    (33.6%)              
           —————————————————————————————————————————————
      
           {1} - Some notes
           —————————————————————————————————————————————
      
           main footer
      
      prov 
      footer that has a lot of 
      new 
      lines

---

    Code
      cat(toString(pg_tbl_no_clw[[3]], widths = clw))
    Output
      main title with some new 
      line
      sub
      -------
      titles
      
           —————————————————————————————————————————————
           SOME         A: Drug       B:      C: Combina
           TOPLEFT         X       Placebo       tion   
                        (N=134)    (N=134)     (N=132)  
           —————————————————————————————————————————————
           cl B.2                                       
             - Any         79         74      85 (64.4%)
             Grade -    (59.0%)    (55.2%)              
             Grade         30         30      33 (25.0%)
             1-2        (22.4%)    (22.4%)              
             1             30         30      33 (25.0%)
                        (22.4%)    (22.4%)              
             Grade         49         44      52 (39.4%)
             3-4        (36.6%)    (32.8%)              
             3             49         44      52 (39.4%)
                        (36.6%)    (32.8%)              
               dcd B.                                   
               2.1.2.                                   
               1                                        
                 -         49         44      52 (39.4%)
                 Any    (36.6%)    (32.8%)              
                 Grad                                   
                 e -                                    
                 Grad      49         44      52 (39.4%)
                 e      (36.6%)    (32.8%)              
                 3-4                                    
                 3         49         44      52 (39.4%)
                        (36.6%)    (32.8%)              
           cl D.1                                       
             - Any         79         67      80 (60.6%)
             Grade -    (59.0%)    (50.0%)              
             Grade         29         25      29 (22.0%)
             3-4        (21.6%)    (18.7%)              
             3             29         25      29 (22.0%)
                        (21.6%)    (18.7%)              
             Grade 5       50         42      51 (38.6%)
                        (37.3%)    (31.3%)              
               dcd D.                                   
               1.1.1.                                   
               1                                        
                 -         50         42      51 (38.6%)
                 Any    (37.3%)    (31.3%)              
                 Grad                                   
                 e -                                    
                 Grad      50         42      51 (38.6%)
                 e 5    (37.3%)    (31.3%)              
           —————————————————————————————————————————————
      
           main footer
      
      prov 
      footer that has a lot of 
      new 
      lines

---

    Code
      cat(toString(pg_tbl_w_clw[[3]], widths = clw))
    Output
      main title with some new 
      line
      sub
      -------
      titles
      
           —————————————————————————————————————————————
           SOME         A: Drug       B:      C: Combina
           TOPLEFT         X       Placebo       tion   
                        (N=134)    (N=134)     (N=132)  
           —————————————————————————————————————————————
           cl A.1                                       
             - Any         78         75      89 (67.4%)
             Grade -    (58.2%)    (56.0%)              
             Grade         78         75      89 (67.4%)
             1-2        (58.2%)    (56.0%)              
             1             30         27      39 (29.5%)
                        (22.4%)    (20.1%)              
             2             48         48      50 (37.9%)
                        (35.8%)    (35.8%)              
               dcd A.                                   
               1.1.1.                                   
               2                                        
                 -         48         48      50 (37.9%)
                 Any    (35.8%)    (35.8%)              
                 Grad                                   
                 e -                                    
                 Grad      48         48      50 (37.9%)
                 e      (35.8%)    (35.8%)              
                 1-2                                    
                 2         48         48      50 (37.9%)
                        (35.8%)    (35.8%)              
           —————————————————————————————————————————————
      
           main footer
      
      prov 
      footer that has a lot of 
      new 
      lines

# Pagination works for page types

    Code
      sapply(pag_res, nrow)
    Output
      [1] 35 35 42 42

---

    Code
      pag_res[2]
    Output
      [[1]]
      main title with some new 
      line
      sub
      -------
      titles
      
           ————————————————————————————————————
           SOME TOPLEFT          C: Combination
                                    (N=132)    
           ————————————————————————————————————
           - Any Grade -          120 (90.9%)  
           Grade 1-2               11 (8.3%)   
           1                        4 (3.0%)   
           2                        7 (5.3%)   
           Grade 3-4               34 (25.8%)  
           3                       16 (12.1%)  
           4                       18 (13.6%)  
           Grade 5                 75 (56.8%)  
           cl A.1                              
             - Any Grade -         89 (67.4%)  
             Grade 1-2             89 (67.4%)  
             1                     39 (29.5%)  
             2                     50 (37.9%)  
               dcd A.1.1.1.1                   
                 - Any Grade -     63 (47.7%)  
                 Grade 1-2         63 (47.7%)  
                 1                 63 (47.7%)  
               dcd A.1.1.1.2                   
                 - Any Grade -     50 (37.9%)  
                 Grade 1-2         50 (37.9%)  
                 2                 50 (37.9%)  
           cl B.2                              
             - Any Grade -         85 (64.4%)  
             Grade 1-2             33 (25.0%)  
             1                     33 (25.0%)  
             Grade 3-4             52 (39.4%)  
             3                     52 (39.4%)  
               dcd B.2.2.3.1                   
                 - Any Grade -     51 (38.6%)  
                 Grade 1-2         51 (38.6%)  
                 1                 51 (38.6%)  
               dcd B.2.1.2.1                   
                 - Any Grade -     52 (39.4%)  
                 Grade 3-4         52 (39.4%)  
                 3                 52 (39.4%)  
           ————————————————————————————————————
      
           main footer
      
      prov 
      footer that has a lot of 
      new 
      lines
      

---

    Code
      sapply(pag_res, nrow)
    Output
      [1] 43 43 39 39

---

    Code
      pag_res[4]
    Output
      [[1]]
      main title with some new 
      line
      sub
      -------
      titles
      
           ————————————————————————————————————
           SOME TOPLEFT          C: Combination
                                    (N=132)    
           ————————————————————————————————————
           cl D.1                              
             - Any Grade -         80 (60.6%)  
             Grade 3-4             29 (22.0%)  
             3                     29 (22.0%)  
             Grade 5               51 (38.6%)  
               dcd D.1.1.4.2                   
                 - Any Grade -     50 (37.9%)  
                 Grade 3-4         50 (37.9%)  
                 3                 50 (37.9%)  
           cl D.2                              
             - Any Grade -         57 (43.2%)  
             Grade 1-2             57 (43.2%)  
             1                     57 (43.2%)  
               dcd D.2.1.5.3                   
                 - Any Grade -     57 (43.2%)  
                 Grade 1-2         57 (43.2%)  
                 1                 57 (43.2%)  
           cl B.1                              
             - Any Grade -         43 (32.6%)  
             Grade 5               43 (32.6%)  
               dcd B.1.1.1.1                   
                 - Any Grade -     43 (32.6%)  
                 Grade 5           43 (32.6%)  
           cl C.2                              
             - Any Grade -         55 (41.7%)  
             Grade 1-2             55 (41.7%)  
             2                     55 (41.7%)  
               dcd C.2.1.2.1                   
                 - Any Grade -     55 (41.7%)  
                 Grade 1-2         55 (41.7%)  
                 2                 55 (41.7%)  
           cl C.1                              
             - Any Grade -         43 (32.6%)  
             Grade 3-4             43 (32.6%)  
             4                     43 (32.6%)  
               dcd C.1.1.1.3                   
                 - Any Grade -     43 (32.6%)  
                 Grade 3-4         43 (32.6%)  
                 4                 43 (32.6%)  
           ————————————————————————————————————
      
           main footer
      
      prov 
      footer that has a lot of 
      new 
      lines
      

---

    Code
      sapply(pag_res, nrow)
    Output
      [1] 55 55 22 22

---

    Code
      pag_res[3]
    Output
      [[1]]
      main title with some new 
      line
      sub
      -------
      titles
      
           —————————————————————————————————————————————
           SOME TOPLEFT          A: Drug X    B: Placebo
                                  (N=134)      (N=134)  
           —————————————————————————————————————————————
           cl B.1                                       
             - Any Grade -       47 (35.1%)   49 (36.6%)
             Grade 5             47 (35.1%)   49 (36.6%)
               dcd B.1.1.1.1                            
                 - Any Grade -   47 (35.1%)   49 (36.6%)
                 Grade 5         47 (35.1%)   49 (36.6%)
           cl C.2                                       
             - Any Grade -       35 (26.1%)   48 (35.8%)
             Grade 1-2           35 (26.1%)   48 (35.8%)
             2                   35 (26.1%)   48 (35.8%)
               dcd C.2.1.2.1                            
                 - Any Grade -   35 (26.1%)   48 (35.8%)
                 Grade 1-2       35 (26.1%)   48 (35.8%)
                 2               35 (26.1%)   48 (35.8%)
           cl C.1                                       
             - Any Grade -       43 (32.1%)   46 (34.3%)
             Grade 3-4           43 (32.1%)   46 (34.3%)
             4                   43 (32.1%)   46 (34.3%)
               dcd C.1.1.1.3                            
                 - Any Grade -   43 (32.1%)   46 (34.3%)
                 Grade 3-4       43 (32.1%)   46 (34.3%)
                 4               43 (32.1%)   46 (34.3%)
           —————————————————————————————————————————————
      
           main footer
      
      prov 
      footer that has a lot of 
      new 
      lines
      

---

    Code
      sapply(pag_res, nrow)
    Output
       [1]  5  5  3  3  9  9  9  9 10 10 10 10  8  8  9  9  8  8  6  6  8  8  8  8

---

    Code
      pag_res[5:10]
    Output
      [[1]]
      main title with some new 
      line
      sub
      -------
      titles
      
           —————————————————————————————————————————————
           SOME TOPLEFT          A: Drug X    B: Placebo
                                  (N=134)      (N=134)  
           —————————————————————————————————————————————
           cl A.1                                       
             - Any Grade -       78 (58.2%)   75 (56.0%)
             Grade 1-2           78 (58.2%)   75 (56.0%)
             1                   30 (22.4%)   27 (20.1%)
             2                   48 (35.8%)   48 (35.8%)
               dcd A.1.1.1.1                            
                 - Any Grade -   50 (37.3%)   45 (33.6%)
                 Grade 1-2       50 (37.3%)   45 (33.6%)
                 1               50 (37.3%)   45 (33.6%)
           —————————————————————————————————————————————
      
           main footer
      
      prov 
      footer that has a lot of 
      new 
      lines
      
      [[2]]
      main title with some new 
      line
      sub
      -------
      titles
      
           ————————————————————————————————————
           SOME TOPLEFT          C: Combination
                                    (N=132)    
           ————————————————————————————————————
           cl A.1                              
             - Any Grade -         89 (67.4%)  
             Grade 1-2             89 (67.4%)  
             1                     39 (29.5%)  
             2                     50 (37.9%)  
               dcd A.1.1.1.1                   
                 - Any Grade -     63 (47.7%)  
                 Grade 1-2         63 (47.7%)  
                 1                 63 (47.7%)  
           ————————————————————————————————————
      
           main footer
      
      prov 
      footer that has a lot of 
      new 
      lines
      
      [[3]]
      main title with some new 
      line
      sub
      -------
      titles
      
           —————————————————————————————————————————————
           SOME TOPLEFT          A: Drug X    B: Placebo
                                  (N=134)      (N=134)  
           —————————————————————————————————————————————
           cl A.1                                       
             - Any Grade -       78 (58.2%)   75 (56.0%)
             Grade 1-2           78 (58.2%)   75 (56.0%)
             1                   30 (22.4%)   27 (20.1%)
             2                   48 (35.8%)   48 (35.8%)
               dcd A.1.1.1.2                            
                 - Any Grade -   48 (35.8%)   48 (35.8%)
                 Grade 1-2       48 (35.8%)   48 (35.8%)
                 2               48 (35.8%)   48 (35.8%)
           —————————————————————————————————————————————
      
           main footer
      
      prov 
      footer that has a lot of 
      new 
      lines
      
      [[4]]
      main title with some new 
      line
      sub
      -------
      titles
      
           ————————————————————————————————————
           SOME TOPLEFT          C: Combination
                                    (N=132)    
           ————————————————————————————————————
           cl A.1                              
             - Any Grade -         89 (67.4%)  
             Grade 1-2             89 (67.4%)  
             1                     39 (29.5%)  
             2                     50 (37.9%)  
               dcd A.1.1.1.2                   
                 - Any Grade -     50 (37.9%)  
                 Grade 1-2         50 (37.9%)  
                 2                 50 (37.9%)  
           ————————————————————————————————————
      
           main footer
      
      prov 
      footer that has a lot of 
      new 
      lines
      
      [[5]]
      main title with some new 
      line
      sub
      -------
      titles
      
           —————————————————————————————————————————————
           SOME TOPLEFT          A: Drug X    B: Placebo
                                  (N=134)      (N=134)  
           —————————————————————————————————————————————
           cl B.2                                       
             - Any Grade -       79 (59.0%)   74 (55.2%)
             Grade 1-2           30 (22.4%)   30 (22.4%)
             1                   30 (22.4%)   30 (22.4%)
             Grade 3-4           49 (36.6%)   44 (32.8%)
             3                   49 (36.6%)   44 (32.8%)
               dcd B.2.2.3.1                            
                 - Any Grade -   48 (35.8%)   54 (40.3%)
                 Grade 1-2       48 (35.8%)   54 (40.3%)
                 1               48 (35.8%)   54 (40.3%)
           —————————————————————————————————————————————
      
           main footer
      
      prov 
      footer that has a lot of 
      new 
      lines
      
      [[6]]
      main title with some new 
      line
      sub
      -------
      titles
      
           ————————————————————————————————————
           SOME TOPLEFT          C: Combination
                                    (N=132)    
           ————————————————————————————————————
           cl B.2                              
             - Any Grade -         85 (64.4%)  
             Grade 1-2             33 (25.0%)  
             1                     33 (25.0%)  
             Grade 3-4             52 (39.4%)  
             3                     52 (39.4%)  
               dcd B.2.2.3.1                   
                 - Any Grade -     51 (38.6%)  
                 Grade 1-2         51 (38.6%)  
                 1                 51 (38.6%)  
           ————————————————————————————————————
      
           main footer
      
      prov 
      footer that has a lot of 
      new 
      lines
      

# Pagination works for page width and height

    Code
      sapply(pag_res, nrow)
    Output
      [1] 35 35 34 34  8  8

---

    Code
      pag_res[2]
    Output
      [[1]]
      main title with some new 
      line
      sub
      -------
      titles
      
           ————————————————————————————————————
           SOME TOPLEFT          C: Combination
                                    (N=132)    
           ————————————————————————————————————
           - Any Grade -          120 (90.9%)  
           Grade 1-2               11 (8.3%)   
           1                        4 (3.0%)   
           2                        7 (5.3%)   
           Grade 3-4               34 (25.8%)  
           3                       16 (12.1%)  
           4                       18 (13.6%)  
           Grade 5                 75 (56.8%)  
           cl A.1                              
             - Any Grade -         89 (67.4%)  
             Grade 1-2             89 (67.4%)  
             1                     39 (29.5%)  
             2                     50 (37.9%)  
               dcd A.1.1.1.1                   
                 - Any Grade -     63 (47.7%)  
                 Grade 1-2         63 (47.7%)  
                 1                 63 (47.7%)  
               dcd A.1.1.1.2                   
                 - Any Grade -     50 (37.9%)  
                 Grade 1-2         50 (37.9%)  
                 2                 50 (37.9%)  
           cl B.2                              
             - Any Grade -         85 (64.4%)  
             Grade 1-2             33 (25.0%)  
             1                     33 (25.0%)  
             Grade 3-4             52 (39.4%)  
             3                     52 (39.4%)  
               dcd B.2.2.3.1                   
                 - Any Grade -     51 (38.6%)  
                 Grade 1-2         51 (38.6%)  
                 1                 51 (38.6%)  
               dcd B.2.1.2.1                   
                 - Any Grade -     52 (39.4%)  
                 Grade 3-4         52 (39.4%)  
                 3                 52 (39.4%)  
           ————————————————————————————————————
      
           main footer
      
      prov 
      footer that has a lot of 
      new 
      lines
      

# AET04 variant 2 page_by pagination tests

    Code
      pag_res[c(2, 5)]
    Output
      [[1]]
      
      AEBODSYS: cl D.1
      
      ————————————————————————————————————————————————————————————
                          A: Drug X    B: Placebo   C: Combination
                           (N=134)      (N=134)        (N=132)    
      ————————————————————————————————————————————————————————————
      - Any Grade -       79 (59.0%)       0              0       
      Grade 3-4           29 (21.6%)       0              0       
      3                   29 (21.6%)       0              0       
      Grade 5             50 (37.3%)       0              0       
        dcd D.1.1.1.1                                             
          - Any Grade -   50 (37.3%)       0              0       
          Grade 5         50 (37.3%)       0              0       
        dcd D.1.1.4.2                                             
          - Any Grade -   48 (35.8%)       0              0       
          Grade 3-4       48 (35.8%)       0              0       
          3               48 (35.8%)       0              0       
      
      [[2]]
      
      AEBODSYS: cl D.2
      
      ————————————————————————————————————————————————————————————
                          A: Drug X    B: Placebo   C: Combination
                           (N=134)      (N=134)        (N=132)    
      ————————————————————————————————————————————————————————————
      - Any Grade -       47 (35.1%)       0              0       
      Grade 1-2           47 (35.1%)       0              0       
      1                   47 (35.1%)       0              0       
        dcd D.2.1.5.3                                             
          - Any Grade -   47 (35.1%)       0              0       
          Grade 1-2       47 (35.1%)       0              0       
          1               47 (35.1%)       0              0       
      
