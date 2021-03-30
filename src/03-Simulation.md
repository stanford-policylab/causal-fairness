Simulation 03
================
Hamed Nilforoshan
2/22/2021

## Overview

We construct a stylized example illustrating the sub-optimal outcomes of
Wang and Blei’s fairness definitions (Affirmative Action and Equal
Opportunity) on utility.

## Setup

Prints out the results of a given admissions method

``` r
  print_results <- function(DF_TEST, DF_TEST_COL, N_ADMIT, RESULT_NAME){
    DF_TEST<-DF_TEST[order(DF_TEST_COL,decreasing=TRUE),]

    cat("Result Type: ", RESULT_NAME,'\n')
    cat("Lives Saved: ", sum(head(DF_TEST,n=N_ADMIT)$L_p),'\n')
    cat("Diverse Admits: ", sum(head(DF_TEST,n=N_ADMIT)$D_p),'\n')
   
  }
```

A “book of life” is a draw of each potential outcome for a population of
a given size. We’ll define a function that generates a book of life for
a given set of parameters. T

``` r
gen_book_of_life <- function(
  pop_size,
  frac_black,
  e_0,
  e_r, 
  e_noise,
  m_0,
  m_e,
  m_noise,
  t_0,
  t_m,
  t_e,
  t_noise,
  a_0,
  a_t,
  a_r,
  A_NORMALIZE_MIN, 
  A_NORMALIZE_MAX,
  ...
) {
  
  # Define the structural equations.
  f_E <- function(r_i, noise){
    return(round(e_0 + e_r*r_i + noise,digits=0))
  }
  
  f_M <- function(e_i, noise){
    return(round(m_0 + m_e*e_i + noise,digits=0))
  }

  f_T <- function(m_i, e_i, noise){
    return(round(t_0 + t_m*m_i + t_e*e_i + noise ,digits=0))
  }
  
  f_A_raw <- function(t_i, r_i){
    return(a_t*t_i + a_r*r_i + a_0)
    #return(normalize(a_t*t_i + a_r*r_i + a_0, method='range',range = c(A_NORMALIZE_MIN, A_NORMALIZE_MAX)))
    #return(round(sigmoid(a_t*t_i + a_r*r_i- a_0), digits = 0))
  }
  
  f_A <- function(t_i, r_i){
    return(sigmoid(a_t*t_i + a_r*r_i + a_0))
    #return(sigmoid(normalize(a_t*t_i + a_r*r_i + a_0, method='range',range = c(A_NORMALIZE_MIN, A_NORMALIZE_MAX))))
  }

  

  # Generate the book of life.
  book_of_life <- tibble(
      # Generate the exogenous variables.
      R = as.integer(rbernoulli(pop_size, frac_black)), # Race (Black=1, White=0)
      
      E_noise = rnorm(pop_size, mean = 0, sd = e_noise), #Educational opportunities
      E_black = f_E(1,noise = E_noise),
      E_white = f_E(0,noise = E_noise),
      
      M_noise = rnorm(pop_size, mean = 0, sd = m_noise), #Medical school preparation
      M_black = f_M(E_black, noise=M_noise),
      M_white = f_M(E_white, noise=M_noise),

      # Generate the endogenous variables.
      T_noise = rnorm(pop_size, mean = 0, sd = t_noise), #Medical school preparation
      T_black = f_T(M_black, E_black, noise=T_noise),
      T_white = f_T(M_white, E_white, noise=T_noise),
      
      E = if_else(R==1,E_black,E_white),
      M = if_else(R==1,M_black,M_white),
      T = if_else(R==1,T_black,T_white),
      
      
      # M -- isnt bimodal
      # M -- scale so bottom half is negative
      # Don't worry about population level diff being unrealistic #1 priority is qualitative the results make sense 

      A_raw = f_A_raw(T, R), # Admissions Committee Decision 
      A_raw_black = f_A_raw(T_black, 1), # Admissions Committee Decision 
      A_raw_white = f_A_raw(T_white, 0), # Admissions Committee Decision 

      A_prob = f_A(T,R),
      #A_prob_black = f_A(T_black, 1), # Admissions Committee Decision,
      #A_prob_white = f_A(T_white, 0), # Admissions Committee Decision 
      A = as.integer(runif(pop_size,min=0,max=1) < A_prob),
      
      # Generate potential outcomes
      L_p = M, # Lives saved if admitted
      D_p = R , # Added diversity if admitted  
      
      # Generate observed outcomes
      L_o = L_p * A, # Lives saved observed
      D_o = D_p * A  # Added diversity observed 
      
  )
}
```

## Initialize parameters and books of life

We initialize the training population size for the book of life, the
fraction of the population that is a minority (black), and the number of
counterfactual draws when calculating affirmative action decisions.

We then initial two books of life. One is for training our models, and
the second is for testing what happens when the models are applied to a
new, unseen dataset

``` r
POP_SIZE = 10000000
POP_SIZE_TEST = 10000
POP_SIZE_TEST_ADMIT = 2500
FRAC_BLACK = 0.2
COUNTER_FACTUAL_SAMPLE = 10000

E_0 = 50
E_R = -50
E_NOISE = 30
M_0 = 25
M_E = 0.5
M_NOISE = 20
T_0 = 0
T_M = 0.5
T_E = 0.5
T_NOISE = 5
A_0 = -2.0
A_T = 0.04
A_R = -1

df_train <-gen_book_of_life(pop_size=POP_SIZE,
                            frac_black=FRAC_BLACK,
                            e_0=E_0,
                            e_r=E_R,
                            e_noise=E_NOISE,
                            m_0=M_0,
                            m_e=M_E,
                            m_noise=M_NOISE,
                            t_0=T_0,
                            t_m=T_M,
                            t_e=T_E,
                            t_noise=T_NOISE,
                            a_0=A_0,
                            a_t=A_T,
                            a_r=A_R)

df_test <-gen_book_of_life(pop_size=POP_SIZE_TEST,
                            frac_black=FRAC_BLACK,
                            e_0=E_0,
                            e_r=E_R,
                            e_noise=E_NOISE,
                            m_0=M_0,
                            m_e=M_E,
                            m_noise=M_NOISE,
                            t_0=T_0,
                            t_m=T_M,
                            t_e=T_E,
                            t_noise=T_NOISE,
                            a_0=A_0,
                            a_t=A_T,
                            a_r=A_R)


POP_SIZE_TEST_ADMIT = sum(df_test$A)

df_train_ = filter(df_train, A == 1 )
BLACK_ADMITS = sum(df_train_$R)
```

## Use the biased admissions committee model as the baseline

``` r
ml_naive <- glm(A ~ T + R, data = df_train, family = "binomial", control = list(maxit = 1000))

df_test$ml_naive = plogis(predict(ml_naive,df_test))
print_results(df_test, df_test$ml_naive, POP_SIZE_TEST_ADMIT, 'ML_NAIVE')
```

    ## Result Type:  ML_NAIVE 
    ## Lives Saved:  276966 
    ## Diverse Admits:  16

``` r
summary(ml_naive)
```

    ## 
    ## Call:
    ## glm(formula = A ~ T + R, family = "binomial", data = df_train, 
    ##     control = list(maxit = 1000))
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.8885  -0.9053  -0.3503   0.9511   3.5998  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -1.999e+00  1.919e-03 -1041.8   <2e-16 ***
    ## T            3.997e-02  3.509e-05  1139.2   <2e-16 ***
    ## R           -1.001e+00  2.644e-03  -378.5   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 13612200  on 9999999  degrees of freedom
    ## Residual deviance: 10768260  on 9999997  degrees of freedom
    ## AIC: 10768266
    ## 
    ## Number of Fisher Scoring iterations: 5

## Maximize on lives saved

``` r
print_results(df_test, df_test$L_p, POP_SIZE_TEST_ADMIT, 'MAX_LIVES_SAVED')
```

    ## Result Type:  MAX_LIVES_SAVED 
    ## Lives Saved:  291902 
    ## Diverse Admits:  313

## Use the outcomes model

``` r
df_train_ = filter(df_train, A == 1 )

ml_outcomes <- lm(L_p ~ T+R+E, data = df_train_)
df_test$ml_outcomes = predict(ml_outcomes,df_test)

print_results(df_test, df_test$ml_outcomes, POP_SIZE_TEST_ADMIT, 'ML_Outcomes')
```

    ## Result Type:  ML_Outcomes 
    ## Lives Saved:  285775 
    ## Diverse Admits:  289

``` r
summary(ml_outcomes)
```

    ## 
    ## Call:
    ## lm(formula = L_p ~ T + R + E, data = df_train_)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -44.017  -6.025   0.000   6.019  46.208 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error   t value Pr(>|t|)    
    ## (Intercept)  5.0265106  0.0124090   405.069   <2e-16 ***
    ## T            1.5985757  0.0003981  4015.251   <2e-16 ***
    ## R           -0.0040476  0.0209242    -0.193    0.847    
    ## E           -0.6990400  0.0003269 -2138.252   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.959 on 4209910 degrees of freedom
    ## Multiple R-squared:  0.8568, Adjusted R-squared:  0.8568 
    ## F-statistic: 8.395e+06 on 3 and 4209910 DF,  p-value: < 2.2e-16

## Calculate the EO-Fair decision

``` r
df_test_black = tibble(df_test)
df_test_black$R=1
df_test_white = tibble(df_test)
df_test_white$R=0
df_test$ml_eo_fair = FRAC_BLACK*plogis(predict(ml_naive,df_test_black)) + (1-FRAC_BLACK)*plogis(predict(ml_naive,df_test_white))

print_results(df_test, df_test$ml_eo_fair, POP_SIZE_TEST_ADMIT, 'ML_EO_Fair')
```

    ## Result Type:  ML_EO_Fair 
    ## Lives Saved:  278930 
    ## Diverse Admits:  136

## Calculate the AA-Fair decision

``` r
#Setup counterfactual probability distributions
aa_probs_black <- df_train %>%
  count(R, T, T_black) %>%
  group_by(R, T) %>%
  mutate(p = n / sum(n)) %>%
  ungroup()
  
aa_probs_white <- df_train %>%
  count(R, T, T_white) %>%
  group_by(R, T) %>%
  mutate(p = n / sum(n)) %>%
  ungroup()


AA_FAIR_MODEL <- function(applicant_race, applicant_test){
  
# Get sampling space for black and white counterfactuals
aa_probs_black_sample <- aa_probs_black %>% 
filter(R == applicant_race, T==applicant_test)
aa_probs_white_sample <- aa_probs_white %>% 
filter(R == applicant_race, T==applicant_test)

#browser()

#sample test scores, first for black counterfactual, then for white
sampled_test_scores = aa_probs_black_sample[sample.int(nrow(aa_probs_black_sample), COUNTER_FACTUAL_SAMPLE*FRAC_BLACK, replace = TRUE, prob = aa_probs_black_sample$p),]$T_black
sampled_test_scores = c(sampled_test_scores,aa_probs_white_sample[sample.int(nrow(aa_probs_white_sample), COUNTER_FACTUAL_SAMPLE*(1-FRAC_BLACK), replace = TRUE, prob = aa_probs_white_sample$p),]$T_white)


aa_input_black <- tibble(R=1, T=sampled_test_scores)
aa_input_white <- tibble(R=0, T=sampled_test_scores)

AA_DECISION = mean(FRAC_BLACK*plogis(predict(ml_naive,aa_input_black)) + (1-FRAC_BLACK)*plogis(predict(ml_naive,aa_input_white)))

return(AA_DECISION)
  }
```

``` r
df_test <- df_test %>% 
  rowwise() %>% 
  mutate(ml_aa_fair = AA_FAIR_MODEL(R,T))

print_results(df_test, df_test$ml_aa_fair, POP_SIZE_TEST_ADMIT, 'ML_AA_Fair')
```

    ## Result Type:  ML_AA_Fair 
    ## Lives Saved:  271515 
    ## Diverse Admits:  860

## Use the outcomes model with diversity constraint

``` r
df_train_ = filter(df_train, A == 1 )

ml_outcomes <- lm(L_p ~ T+R+E, data = df_train_)
df_test$ml_outcomes = predict(ml_outcomes,df_test)

df_test_black<- df_test[order(df_test$ml_outcomes,decreasing=TRUE),] %>%
  filter( R == 1 )

df_test_white<- df_test[order(df_test$ml_outcomes,decreasing=TRUE),] %>%
  filter( R == 0 )


N=859

cat("Result Type: ", 'ML Outcomes Diverse','\n')
```

    ## Result Type:  ML Outcomes Diverse

``` r
cat("Lives Saved: ", sum(head(df_test_black,n=N)$L_p) + sum(head(df_test_white,n=POP_SIZE_TEST_ADMIT-N)$L_p) ,'\n')
```

    ## Lives Saved:  278347

``` r
cat("Diverse Admits: ", N,'\n')
```

    ## Diverse Admits:  859

``` r
print_results(df_test, df_test$ml_outcomes, POP_SIZE_TEST_ADMIT, 'ML_Outcomes')
```

    ## Result Type:  ML_Outcomes 
    ## Lives Saved:  285775 
    ## Diverse Admits:  289

``` r
summary(ml_outcomes)
```

    ## 
    ## Call:
    ## lm(formula = L_p ~ T + R + E, data = df_train_)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -44.017  -6.025   0.000   6.019  46.208 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error   t value Pr(>|t|)    
    ## (Intercept)  5.0265106  0.0124090   405.069   <2e-16 ***
    ## T            1.5985757  0.0003981  4015.251   <2e-16 ***
    ## R           -0.0040476  0.0209242    -0.193    0.847    
    ## E           -0.6990400  0.0003269 -2138.252   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.959 on 4209910 degrees of freedom
    ## Multiple R-squared:  0.8568, Adjusted R-squared:  0.8568 
    ## F-statistic: 8.395e+06 on 3 and 4209910 DF,  p-value: < 2.2e-16
