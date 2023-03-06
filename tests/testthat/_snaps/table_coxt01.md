# 1. Cox Regression

    Code
      res
    Output
                                    n    Hazard Ratio      95% CI      p-value
      ————————————————————————————————————————————————————————————————————————
      Treatment:                                                              
        ARM A vs control (ARM B)   247       0.70       (0.51, 0.96)   0.0293 
      Covariate:                                                              
        Sex                        247       0.71       (0.52, 0.98)   0.0370 
        Race                       247       0.70       (0.51, 0.97)   0.0318 
        Age                        247       0.70       (0.51, 0.97)   0.0321 

# 2. Cox Regression (with Interaction Term)

    Code
      res
    Output
       [1] "                               n    Hazard Ratio      95% CI      p-value   Interaction p-value"
       [2] "———————————————————————————————————————————————————————————————————————————————————————————————"
       [3] "Treatment:                                                                                     "
       [4] "  ARM A vs control (ARM B)    247       0.70       (0.51, 0.96)   0.0293                       "
       [5] "Covariate:                                                                                     "
       [6] "  Sex                         247                                                 0.4635       "
       [7] "    F                                   0.64       (0.42, 0.98)                                "
       [8] "    M                                   0.82       (0.50, 1.35)                                "
       [9] "  Race                        247                                                 0.9197       "
      [10] "    ASIAN                               0.75       (0.48, 1.16)                                "
      [11] "    BLACK OR AFRICAN                    0.66       (0.34, 1.28)                                "
      [12] "    AMERICAN                                                                                   "
      [13] "    WHITE                               0.65       (0.33, 1.27)                                "
      [14] "  Age                         247                                                 0.8626       "
      [15] "    34                                  0.70       (0.51, 0.97)                                "

# 3. Cox Regression (specifying covariates)

    Code
      res
    Output
       [1] "                               n    Hazard Ratio      95% CI      p-value   Interaction p-value"
       [2] "———————————————————————————————————————————————————————————————————————————————————————————————"
       [3] "Treatment:                                                                                     "
       [4] "  ARM A vs control (ARM B)    247       0.70       (0.51, 0.96)   0.0293                       "
       [5] "Covariate:                                                                                     "
       [6] "  Sex                         247                                                 0.4635       "
       [7] "    F                                   0.64       (0.42, 0.98)                                "
       [8] "    M                                   0.82       (0.50, 1.35)                                "
       [9] "  Race                        247                                                 0.9197       "
      [10] "    ASIAN                               0.75       (0.48, 1.16)                                "
      [11] "    BLACK OR AFRICAN                    0.66       (0.34, 1.28)                                "
      [12] "    AMERICAN                                                                                   "
      [13] "    WHITE                               0.65       (0.33, 1.27)                                "
      [14] "  Age                         247                                                 0.8626       "
      [15] "    30                                  0.69       (0.48, 1.00)                                "
      [16] "    40                                  0.72       (0.48, 1.08)                                "
      [17] "    50                                  0.75       (0.35, 1.61)                                "

# 4. Cox Regression (setting strata, ties, and alpha level)

    Code
      res
    Output
       [1] "                               n    Hazard Ratio      90% CI      p-value   Interaction p-value"
       [2] "———————————————————————————————————————————————————————————————————————————————————————————————"
       [3] "Treatment:                                                                                     "
       [4] "  ARM A vs control (ARM B)    247       0.70       (0.53, 0.92)   0.0293                       "
       [5] "Covariate:                                                                                     "
       [6] "  Sex                         247                                                 0.4635       "
       [7] "    F                                   0.64       (0.42, 0.98)                                "
       [8] "    M                                   0.82       (0.50, 1.35)                                "
       [9] "  Race                        247                                                 0.9197       "
      [10] "    ASIAN                               0.75       (0.48, 1.16)                                "
      [11] "    BLACK OR AFRICAN                    0.66       (0.34, 1.28)                                "
      [12] "    AMERICAN                                                                                   "
      [13] "    WHITE                               0.65       (0.33, 1.27)                                "
      [14] "  Age                         247                                                 0.8626       "
      [15] "    30                                  0.69       (0.51, 0.94)                                "
      [16] "    40                                  0.72       (0.51, 1.02)                                "
      [17] "    50                                  0.75       (0.39, 1.42)                                "

