
mod.factor_parameterization.marginal_parameterization.free_threshold <- function(){}

mod.factor_parameterization.marginal_parameterization.free_threshold <- '

eta1_1=~l1_1*item1_1+l2_1*item2_1+l3_1*item3_1+l4_1*item4_1+l5_1*item5_1+l6_1*item6_1+l7_1*item7_1+l8_1*item8_1+l9_1*item9_1+l10_1*item10_1+l11_1*item11_1+l12_1*item12_1+l13_1*item13_1
#latent var means and var
eta1_1 ~ eta1_1_mean*1
eta1_1 ~~ eta1_1_var*eta1_1

eta1_1_mean==0

## Latent response variable (LRV) intercepts
item1_1  ~ int1_1 *1
item2_1  ~ int2_1 *1
item3_1  ~ int3_1 *1
item4_1  ~ int4_1 *1
item5_1  ~ int5_1 *1
item6_1  ~ int6_1 *1
item7_1  ~ int7_1 *1
item8_1  ~ int8_1 *1
item9_1  ~ int9_1 *1
item10_1 ~ int10_1*1
item11_1 ~ int11_1*1
item12_1 ~ int12_1*1
item13_1 ~ int13_1*1

int1_1 ==0
int2_1 ==0
int3_1 ==0
int4_1 ==0
int5_1 ==0
int6_1 ==0
int7_1 ==0
int8_1 ==0
int9_1 ==0
int10_1==0
int11_1==0
int12_1==0
int13_1==0


## thresholds link  LRVs to observed items
item1_1 | thr1_1*t1
item2_1 | thr2_1*t1
item3_1 | thr3_1*t1
item4_1 | thr4_1*t1
item5_1 | thr5_1*t1
item6_1 | thr6_1*t1
item7_1 | thr7_1*t1
item8_1 | thr8_1*t1
item9_1 | thr9_1*t1
item10_1 | thr10_1*t1
item11_1 | thr11_1*t1
item12_1 | thr12_1*t1
item13_1 | thr13_1*t1

## LRVs (co)variances
item1_1 ~~ var1_1*item1_1+ 0*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item2_1 ~~ var2_1*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item3_1 ~~ var3_1*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item4_1 ~~ var4_1*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item5_1 ~~ var5_1*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item6_1 ~~ var6_1*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item7_1 ~~ var7_1*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item8_1 ~~ var8_1*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item9_1 ~~ var9_1*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item10_1 ~~ var10_1*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item11_1 ~~ var11_1*item11_1+ 0*item12_1+ 0*item13_1
item12_1 ~~ var12_1*item12_1+ 0*item13_1
item13_1 ~~ var13_1*item13_1


## LRVs variances constraints
eta1_1_var*l1_1^2 + var1_1 == 1
eta1_1_var*l2_1^2 + var2_1 == 1
eta1_1_var*l3_1^2 + var3_1 == 1
eta1_1_var*l4_1^2 + var4_1 == 1
eta1_1_var*l5_1^2 + var5_1 == 1
eta1_1_var*l6_1^2 + var6_1 == 1
eta1_1_var*l7_1^2 + var7_1 == 1
eta1_1_var*l8_1^2 + var8_1 == 1
eta1_1_var*l9_1^2 + var9_1 == 1
eta1_1_var*l10_1^2 + var10_1 == 1
eta1_1_var*l11_1^2 + var11_1 == 1
eta1_1_var*l12_1^2 + var12_1 == 1
eta1_1_var*l13_1^2 + var13_1 == 1

'


mod.factor_parameterization.marginal_parameterization.free_intercept<- function(){}
mod.factor_parameterization.marginal_parameterization.free_intercept <- '

eta1_1=~l1_1*item1_1+l2_1*item2_1+l3_1*item3_1+l4_1*item4_1+l5_1*item5_1+l6_1*item6_1+l7_1*item7_1+l8_1*item8_1+l9_1*item9_1+l10_1*item10_1+l11_1*item11_1+l12_1*item12_1+l13_1*item13_1
#latent var means and var
eta1_1 ~ eta1_1_mean*1
eta1_1 ~~ eta1_1_var*eta1_1

eta1_1_mean==0

## Latent response variable (LRV) intercepts
item1_1 ~ int1_1*1
item2_1 ~ int2_1*1
item3_1 ~ int3_1*1
item4_1 ~ int4_1*1
item5_1 ~ int5_1*1
item6_1 ~ int6_1*1
item7_1 ~ int7_1*1
item8_1 ~ int8_1*1
item9_1 ~ int9_1*1
item10_1 ~ int10_1*1
item11_1 ~ int11_1*1
item12_1 ~ int12_1*1
item13_1 ~ int13_1*1


## thresholds link  LRVs to observed items
item1_1 | thr1_1*t1
item2_1 | thr2_1*t1
item3_1 | thr3_1*t1
item4_1 | thr4_1*t1
item5_1 | thr5_1*t1
item6_1 | thr6_1*t1
item7_1 | thr7_1*t1
item8_1 | thr8_1*t1
item9_1 | thr9_1*t1
item10_1 | thr10_1*t1
item11_1 | thr11_1*t1
item12_1 | thr12_1*t1
item13_1 | thr13_1*t1

thr1_1==0
thr2_1==0
thr3_1==0
thr4_1==0
thr5_1==0
thr6_1==0
thr7_1==0
thr8_1==0
thr9_1==0
thr10_1==0
thr11_1==0
thr12_1==0
thr13_1==0

## LRVs (co)variances
item1_1 ~~ var1_1*item1_1+ 0*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item2_1 ~~ var2_1*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item3_1 ~~ var3_1*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item4_1 ~~ var4_1*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item5_1 ~~ var5_1*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item6_1 ~~ var6_1*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item7_1 ~~ var7_1*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item8_1 ~~ var8_1*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item9_1 ~~ var9_1*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item10_1 ~~ var10_1*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item11_1 ~~ var11_1*item11_1+ 0*item12_1+ 0*item13_1
item12_1 ~~ var12_1*item12_1+ 0*item13_1
item13_1 ~~ var13_1*item13_1


## LRVs variances constraints
eta1_1_var*l1_1^2 + var1_1 == 1
eta1_1_var*l2_1^2 + var2_1 == 1
eta1_1_var*l3_1^2 + var3_1 == 1
eta1_1_var*l4_1^2 + var4_1 == 1
eta1_1_var*l5_1^2 + var5_1 == 1
eta1_1_var*l6_1^2 + var6_1 == 1
eta1_1_var*l7_1^2 + var7_1 == 1
eta1_1_var*l8_1^2 + var8_1 == 1
eta1_1_var*l9_1^2 + var9_1 == 1
eta1_1_var*l10_1^2 + var10_1 == 1
eta1_1_var*l11_1^2 + var11_1 == 1
eta1_1_var*l12_1^2 + var12_1 == 1
eta1_1_var*l13_1^2 + var13_1 == 1

'

mod.factor_parameterization.marginal_parameterization.free_mean<- function(){}
mod.factor_parameterization.marginal_parameterization.free_mean <- '

eta1_1=~l1_1*item1_1+l2_1*item2_1+l3_1*item3_1+l4_1*item4_1+l5_1*item5_1+l6_1*item6_1+l7_1*item7_1+l8_1*item8_1+l9_1*item9_1+l10_1*item10_1+l11_1*item11_1+l12_1*item12_1+l13_1*item13_1
#latent var means and var
eta1_1 ~ eta1_1_mean*1
eta1_1 ~~ eta1_1_var*eta1_1

## Latent response variable (LRV) intercepts
item1_1 ~  int1_1 *1
item2_1 ~  int2_1 *1
item3_1 ~  int3_1 *1
item4_1 ~  int4_1 *1
item5_1 ~  int5_1 *1
item6_1 ~  int6_1 *1
item7_1 ~  int7_1 *1
item8_1 ~  int8_1 *1
item9_1 ~  int9_1 *1
item10_1 ~ int10_1*1
item11_1 ~ int11_1*1
item12_1 ~ int12_1*1
item13_1 ~ int13_1*1

int1_1 ==0 
int2_1 ==0 
int3_1 ==0 
int4_1 ==0 
int5_1 ==0 
int6_1 ==0 
int7_1 ==0 
int8_1 ==0 
int9_1 ==0 
int10_1==0 
int11_1==0 
int12_1==0 
int13_1==0 

## thresholds link  LRVs to observed items
item1_1 | thr1_1*t1
item2_1 | thr2_1*t1
item3_1 | thr3_1*t1
item4_1 | thr4_1*t1
item5_1 | thr5_1*t1
item6_1 | thr6_1*t1
item7_1 | thr7_1*t1
item8_1 | thr8_1*t1
item9_1 | thr9_1*t1
item10_1 | thr10_1*t1
item11_1 | thr11_1*t1
item12_1 | thr12_1*t1
item13_1 | thr13_1*t1

thr7_1==0

# LRVs (co)variances
item1_1 ~~ var1_1*item1_1+ 0*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item2_1 ~~ var2_1*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item3_1 ~~ var3_1*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item4_1 ~~ var4_1*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item5_1 ~~ var5_1*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item6_1 ~~ var6_1*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item7_1 ~~ var7_1*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item8_1 ~~ var8_1*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item9_1 ~~ var9_1*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item10_1 ~~ var10_1*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item11_1 ~~ var11_1*item11_1+ 0*item12_1+ 0*item13_1
item12_1 ~~ var12_1*item12_1+ 0*item13_1
item13_1 ~~ var13_1*item13_1


## LRVs variances constraints
eta1_1_var*l1_1^2 + var1_1 == 1
eta1_1_var*l2_1^2 + var2_1 == 1
eta1_1_var*l3_1^2 + var3_1 == 1
eta1_1_var*l4_1^2 + var4_1 == 1
eta1_1_var*l5_1^2 + var5_1 == 1
eta1_1_var*l6_1^2 + var6_1 == 1
eta1_1_var*l7_1^2 + var7_1 == 1
eta1_1_var*l8_1^2 + var8_1 == 1
eta1_1_var*l9_1^2 + var9_1 == 1
eta1_1_var*l10_1^2 + var10_1 == 1
eta1_1_var*l11_1^2 + var11_1 == 1
eta1_1_var*l12_1^2 + var12_1 == 1
eta1_1_var*l13_1^2 + var13_1 == 1

'




mod.factor_parameterization.conditional_parameterization.free_threshold<- function(){}
mod.factor_parameterization.conditional_parameterization.free_threshold <- '
  eta1_1=~l1_1*item1_1+l2_1*item2_1+l3_1*item3_1+l4_1*item4_1+l5_1*item5_1+l6_1*item6_1+l7_1*item7_1+l8_1*item8_1+l9_1*item9_1+l10_1*item10_1+l11_1*item11_1+l12_1*item12_1+l13_1*item13_1
  #latent var means and var
  eta1_1 ~ eta1_1_mean*1
  eta1_1 ~~ eta1_1_var*eta1_1
  
  eta1_1_mean==0
  
  ## Latent response variable (LRV) intercepts
  item1_1 ~ int1_1*1
  item2_1 ~ int2_1*1
  item3_1 ~ int3_1*1
  item4_1 ~ int4_1*1
  item5_1 ~ int5_1*1
  item6_1 ~ int6_1*1
  item7_1 ~ int7_1*1
  item8_1 ~ int8_1*1
  item9_1 ~ int9_1*1
  item10_1 ~ int10_1*1
  item11_1 ~ int11_1*1
  item12_1 ~ int12_1*1
  item13_1 ~ int13_1*1

  int1_1 ==0
  int2_1 ==0
  int3_1 ==0
  int4_1 ==0
  int5_1 ==0
  int6_1 ==0
  int7_1 ==0
  int8_1 ==0
  int9_1 ==0
  int10_1==0
  int11_1==0
  int12_1==0
  int13_1==0

    ## thresholds link  LRVs to observed items
  item1_1 | thr1_1*t1
  item2_1 | thr2_1*t1
  item3_1 | thr3_1*t1
  item4_1 | thr4_1*t1
  item5_1 | thr5_1*t1
  item6_1 | thr6_1*t1
  item7_1 | thr7_1*t1
  item8_1 | thr8_1*t1
  item9_1 | thr9_1*t1
  item10_1 | thr10_1*t1
  item11_1 | thr11_1*t1
  item12_1 | thr12_1*t1
  item13_1 | thr13_1*t1
  
  ## LRVs (co)variances
  item1_1 ~~ var1_1*item1_1+ 0*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item2_1 ~~ var2_1*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item3_1 ~~ var3_1*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item4_1 ~~ var4_1*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item5_1 ~~ var5_1*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item6_1 ~~ var6_1*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item7_1 ~~ var7_1*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item8_1 ~~ var8_1*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item9_1 ~~ var9_1*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item10_1 ~~ var10_1*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item11_1 ~~ var11_1*item11_1+ 0*item12_1+ 0*item13_1
  item12_1 ~~ var12_1*item12_1+ 0*item13_1
  item13_1 ~~ var13_1*item13_1
  
  var1_1 ==1
  var2_1 ==1
  var3_1 ==1
  var4_1 ==1
  var5_1 ==1
  var6_1 ==1
  var7_1 ==1
  var8_1 ==1
  var9_1 ==1
  var10_1 ==1
  var11_1 ==1
  var12_1 ==1
  var13_1 ==1
'


mod.factor_parameterization.conditional_parameterization.free_intercept<- function(){}
mod.factor_parameterization.conditional_parameterization.free_intercept <- '
  eta1_1=~l1_1*item1_1+l2_1*item2_1+l3_1*item3_1+l4_1*item4_1+l5_1*item5_1+l6_1*item6_1+l7_1*item7_1+l8_1*item8_1+l9_1*item9_1+l10_1*item10_1+l11_1*item11_1+l12_1*item12_1+l13_1*item13_1
  #latent var means and var
  eta1_1 ~ eta1_1_mean*1
  eta1_1 ~~ eta1_1_var*eta1_1
  
  eta1_1_mean==0.5
  
  ## Latent response variable (LRV) intercepts
  item1_1 ~ int1_1*1
  item2_1 ~ int2_1*1
  item3_1 ~ int3_1*1
  item4_1 ~ int4_1*1
  item5_1 ~ int5_1*1
  item6_1 ~ int6_1*1
  item7_1 ~ int7_1*1
  item8_1 ~ int8_1*1
  item9_1 ~ int9_1*1
  item10_1 ~ int10_1*1
  item11_1 ~ int11_1*1
  item12_1 ~ int12_1*1
  item13_1 ~ int13_1*1

    ## thresholds link  LRVs to observed items
  item1_1 | thr1_1*t1
  item2_1 | thr2_1*t1
  item3_1 | thr3_1*t1
  item4_1 | thr4_1*t1
  item5_1 | thr5_1*t1
  item6_1 | thr6_1*t1
  item7_1 | thr7_1*t1
  item8_1 | thr8_1*t1
  item9_1 | thr9_1*t1
  item10_1 | thr10_1*t1
  item11_1 | thr11_1*t1
  item12_1 | thr12_1*t1
  item13_1 | thr13_1*t1
  
  thr1_1==0
  thr2_1==0
  thr3_1==0
  thr4_1==0
  thr5_1==0
  thr6_1==0
  thr7_1==0
  thr8_1==0
  thr9_1==0
  thr10_1==0
  thr11_1==0
  thr12_1==0
  thr13_1==0

  
  ## LRVs (co)variances
  item1_1 ~~ var1_1*item1_1+ 0*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item2_1 ~~ var2_1*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item3_1 ~~ var3_1*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item4_1 ~~ var4_1*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item5_1 ~~ var5_1*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item6_1 ~~ var6_1*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item7_1 ~~ var7_1*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item8_1 ~~ var8_1*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item9_1 ~~ var9_1*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item10_1 ~~ var10_1*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item11_1 ~~ var11_1*item11_1+ 0*item12_1+ 0*item13_1
  item12_1 ~~ var12_1*item12_1+ 0*item13_1
  item13_1 ~~ var13_1*item13_1
  
  var1_1 ==1
  var2_1 ==1
  var3_1 ==1
  var4_1 ==1
  var5_1 ==1
  var6_1 ==1
  var7_1 ==1
  var8_1 ==1
  var9_1 ==1
  var10_1 ==1
  var11_1 ==1
  var12_1 ==1
  var13_1 ==1
'
mod.factor_parameterization.conditional_parameterization.free_mean<- function(){}
mod.factor_parameterization.conditional_parameterization.free_mean <- '
  eta1_1=~l1_1*item1_1+l2_1*item2_1+l3_1*item3_1+l4_1*item4_1+l5_1*item5_1+l6_1*item6_1+l7_1*item7_1+l8_1*item8_1+l9_1*item9_1+l10_1*item10_1+l11_1*item11_1+l12_1*item12_1+l13_1*item13_1
  #latent var means and var
  eta1_1 ~ eta1_1_mean*1
  #eta1_1 ~~ eta1_1_var*eta1_1
  #eta1_1_var==1

  ## Latent response variable (LRV) intercepts
  item1_1 ~ int1_1*1
  item2_1 ~ int2_1*1
  item3_1 ~ int3_1*1
  item4_1 ~ int4_1*1
  item5_1 ~ int5_1*1
  item6_1 ~ int6_1*1
  item7_1 ~ int7_1*1
  item8_1 ~ int8_1*1
  item9_1 ~ int9_1*1
  item10_1 ~ int10_1*1
  item11_1 ~ int11_1*1
  item12_1 ~ int12_1*1
  item13_1 ~ int13_1*1

  int1_1 ==0 
  int2_1 ==0 
  int3_1 ==0 
  int4_1 ==0 
  int5_1 ==0 
  int6_1 ==0 
  int7_1 ==0 
  int8_1 ==0 
  int9_1 ==0 
  int10_1==0 
  int11_1==0 
  int12_1==0 
  int13_1==0 


    ## thresholds link  LRVs to observed items
  item1_1 | thr1_1*t1
  item2_1 | thr2_1*t1
  item3_1 | thr3_1*t1
  item4_1 | thr4_1*t1
  item5_1 | thr5_1*t1
  item6_1 | thr6_1*t1
  item7_1 | thr7_1*t1
  item8_1 | thr8_1*t1
  item9_1 | thr9_1*t1
  item10_1 | thr10_1*t1
  item11_1 | thr11_1*t1
  item12_1 | thr12_1*t1
  item13_1 | thr13_1*t1
  
  thr7_1==0

  
  ## LRVs (co)variances
  item1_1 ~~ var1_1*item1_1+ 0*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item2_1 ~~ var2_1*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item3_1 ~~ var3_1*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item4_1 ~~ var4_1*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item5_1 ~~ var5_1*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item6_1 ~~ var6_1*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item7_1 ~~ var7_1*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item8_1 ~~ var8_1*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item9_1 ~~ var9_1*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item10_1 ~~ var10_1*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item11_1 ~~ var11_1*item11_1+ 0*item12_1+ 0*item13_1
  item12_1 ~~ var12_1*item12_1+ 0*item13_1
  item13_1 ~~ var13_1*item13_1
  
  var1_1 ==1
  var2_1 ==1
  var3_1 ==1
  var4_1 ==1
  var5_1 ==1
  var6_1 ==1
  var7_1 ==1
  var8_1 ==1
  var9_1 ==1
  var10_1 ==1
  var11_1 ==1
  var12_1 ==1
  var13_1 ==1
'


mod.effects_coding.marginal_parameterization.free_threshold<- function(){}

mod.effects_coding.marginal_parameterization.free_threshold <- '

  eta1_1=~l1_1*item1_1+l2_1*item2_1+l3_1*item3_1+l4_1*item4_1+l5_1*item5_1+l6_1*item6_1+l7_1*item7_1+l8_1*item8_1+l9_1*item9_1+l10_1*item10_1+l11_1*item11_1+l12_1*item12_1+l13_1*item13_1
  13==l1_1+l2_1+l3_1+l4_1+l5_1+l6_1+l7_1+l8_1+l9_1+l10_1+l11_1+l12_1+l13_1

#latent var means and var
  eta1_1 ~ eta1_1_mean*1
  eta1_1 ~~ eta1_1_var*eta1_1
  eta1_1_mean==0

## Latent response variable (LRV) intercepts
  item1_1 ~ int1_1*1
  item2_1 ~ int2_1*1
  item3_1 ~ int3_1*1
  item4_1 ~ int4_1*1
  item5_1 ~ int5_1*1
  item6_1 ~ int6_1*1
  item7_1 ~ int7_1*1
  item8_1 ~ int8_1*1
  item9_1 ~ int9_1*1
  item10_1 ~ int10_1*1
  item11_1 ~ int11_1*1
  item12_1 ~ int12_1*1
  item13_1 ~ int13_1*1

  int1_1 ==0
  int2_1 ==0
  int3_1 ==0
  int4_1 ==0
  int5_1 ==0
  int6_1 ==0
  int7_1 ==0
  int8_1 ==0
  int9_1 ==0
  int10_1==0
  int11_1==0
  int12_1==0
  int13_1==0


## thresholds link  LRVs to observed items
  item1_1 | thr1_1*t1
  item2_1 | thr2_1*t1
  item3_1 | thr3_1*t1
  item4_1 | thr4_1*t1
  item5_1 | thr5_1*t1
  item6_1 | thr6_1*t1
  item7_1 | thr7_1*t1
  item8_1 | thr8_1*t1
  item9_1 | thr9_1*t1
  item10_1 | thr10_1*t1
  item11_1 | thr11_1*t1
  item12_1 | thr12_1*t1
  item13_1 | thr13_1*t1

## LRVs (co)variances
  item1_1 ~~ var1_1*item1_1+ 0*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item2_1 ~~ var2_1*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item3_1 ~~ var3_1*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item4_1 ~~ var4_1*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item5_1 ~~ var5_1*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item6_1 ~~ var6_1*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item7_1 ~~ var7_1*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item8_1 ~~ var8_1*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item9_1 ~~ var9_1*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item10_1 ~~ var10_1*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item11_1 ~~ var11_1*item11_1+ 0*item12_1+ 0*item13_1
  item12_1 ~~ var12_1*item12_1+ 0*item13_1
  item13_1 ~~ var13_1*item13_1


## LRVs variances constraints
  eta1_1_var*l1_1^2 + var1_1 == 1
  eta1_1_var*l2_1^2 + var2_1 == 1
  eta1_1_var*l3_1^2 + var3_1 == 1
  eta1_1_var*l4_1^2 + var4_1 == 1
  eta1_1_var*l5_1^2 + var5_1 == 1
  eta1_1_var*l6_1^2 + var6_1 == 1
  eta1_1_var*l7_1^2 + var7_1 == 1
  eta1_1_var*l8_1^2 + var8_1 == 1
  eta1_1_var*l9_1^2 + var9_1 == 1
  eta1_1_var*l10_1^2 + var10_1 == 1
  eta1_1_var*l11_1^2 + var11_1 == 1
  eta1_1_var*l12_1^2 + var12_1 == 1
  eta1_1_var*l13_1^2 + var13_1 == 1
'

mod.effects_coding.marginal_parameterization.free_intercept<- function(){}

mod.effects_coding.marginal_parameterization.free_intercept <- '

  eta1_1=~l1_1*item1_1+l2_1*item2_1+l3_1*item3_1+l4_1*item4_1+l5_1*item5_1+l6_1*item6_1+l7_1*item7_1+l8_1*item8_1+l9_1*item9_1+l10_1*item10_1+l11_1*item11_1+l12_1*item12_1+l13_1*item13_1
  13==l1_1+l2_1+l3_1+l4_1+l5_1+l6_1+l7_1+l8_1+l9_1+l10_1+l11_1+l12_1+l13_1

#latent var means and var
  eta1_1 ~ eta1_1_mean*1
  eta1_1 ~~ eta1_1_var*eta1_1

  eta1_1_mean==0

## Latent response variable (LRV) intercepts
  item1_1 ~ int1_1*1
  item2_1 ~ int2_1*1
  item3_1 ~ int3_1*1
  item4_1 ~ int4_1*1
  item5_1 ~ int5_1*1
  item6_1 ~ int6_1*1
  item7_1 ~ int7_1*1
  item8_1 ~ int8_1*1
  item9_1 ~ int9_1*1
  item10_1 ~ int10_1*1
  item11_1 ~ int11_1*1
  item12_1 ~ int12_1*1
  item13_1 ~ int13_1*1


## thresholds link  LRVs to observed items
  item1_1 | thr1_1*t1
  item2_1 | thr2_1*t1
  item3_1 | thr3_1*t1
  item4_1 | thr4_1*t1
  item5_1 | thr5_1*t1
  item6_1 | thr6_1*t1
  item7_1 | thr7_1*t1
  item8_1 | thr8_1*t1
  item9_1 | thr9_1*t1
  item10_1 | thr10_1*t1
  item11_1 | thr11_1*t1
  item12_1 | thr12_1*t1
  item13_1 | thr13_1*t1

  thr1_1==0
  thr2_1==0
  thr3_1==0
  thr4_1==0
  thr5_1==0
  thr6_1==0
  thr7_1==0
  thr8_1==0
  thr9_1==0
  thr10_1==0
  thr11_1==0
  thr12_1==0
  thr13_1==0


## LRVs (co)variances
  item1_1 ~~ var1_1*item1_1+ 0*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item2_1 ~~ var2_1*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item3_1 ~~ var3_1*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item4_1 ~~ var4_1*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item5_1 ~~ var5_1*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item6_1 ~~ var6_1*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item7_1 ~~ var7_1*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item8_1 ~~ var8_1*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item9_1 ~~ var9_1*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item10_1 ~~ var10_1*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item11_1 ~~ var11_1*item11_1+ 0*item12_1+ 0*item13_1
  item12_1 ~~ var12_1*item12_1+ 0*item13_1
  item13_1 ~~ var13_1*item13_1


## LRVs variances constraints
  eta1_1_var*l1_1^2 + var1_1 == 1
  eta1_1_var*l2_1^2 + var2_1 == 1
  eta1_1_var*l3_1^2 + var3_1 == 1
  eta1_1_var*l4_1^2 + var4_1 == 1
  eta1_1_var*l5_1^2 + var5_1 == 1
  eta1_1_var*l6_1^2 + var6_1 == 1
  eta1_1_var*l7_1^2 + var7_1 == 1
  eta1_1_var*l8_1^2 + var8_1 == 1
  eta1_1_var*l9_1^2 + var9_1 == 1
  eta1_1_var*l10_1^2 + var10_1 == 1
  eta1_1_var*l11_1^2 + var11_1 == 1
  eta1_1_var*l12_1^2 + var12_1 == 1
  eta1_1_var*l13_1^2 + var13_1 == 1
'

mod.effects_coding.marginal_parameterization.free_mean<- function(){}

mod.effects_coding.marginal_parameterization.free_mean <- '

  eta1_1=~l1_1*item1_1+l2_1*item2_1+l3_1*item3_1+l4_1*item4_1+l5_1*item5_1+l6_1*item6_1+l7_1*item7_1+l8_1*item8_1+l9_1*item9_1+l10_1*item10_1+l11_1*item11_1+l12_1*item12_1+l13_1*item13_1
  13==l1_1+l2_1+l3_1+l4_1+l5_1+l6_1+l7_1+l8_1+l9_1+l10_1+l11_1+l12_1+l13_1

#latent var means and var
  eta1_1 ~ eta1_1_mean*1
  eta1_1 ~~ eta1_1_var*eta1_1

## Latent response variable (LRV) intercepts
  item1_1 ~ int1_1*1
  item2_1 ~ int2_1*1
  item3_1 ~ int3_1*1
  item4_1 ~ int4_1*1
  item5_1 ~ int5_1*1
  item6_1 ~ int6_1*1
  item7_1 ~ int7_1*1
  item8_1 ~ int8_1*1
  item9_1 ~ int9_1*1
  item10_1 ~ int10_1*1
  item11_1 ~ int11_1*1
  item12_1 ~ int12_1*1
  item13_1 ~ int13_1*1

int1_1 ==0 
int2_1 ==0 
int3_1 ==0 
int4_1 ==0 
int5_1 ==0 
int6_1 ==0 
int7_1 ==0 
int8_1 ==0 
int9_1 ==0 
int10_1==0 
int11_1==0 
int12_1==0 
int13_1==0 


## thresholds link  LRVs to observed items
  item1_1 | thr1_1*t1
  item2_1 | thr2_1*t1
  item3_1 | thr3_1*t1
  item4_1 | thr4_1*t1
  item5_1 | thr5_1*t1
  item6_1 | thr6_1*t1
  item7_1 | thr7_1*t1
  item8_1 | thr8_1*t1
  item9_1 | thr9_1*t1
  item10_1 | thr10_1*t1
  item11_1 | thr11_1*t1
  item12_1 | thr12_1*t1
  item13_1 | thr13_1*t1

0==thr1_1+thr2_1+thr3_1+thr4_1+thr5_1+thr6_1+thr7_1+thr8_1+thr9_1+thr10_1+thr11_1+thr12_1+thr13_1


## LRVs (co)variances
  item1_1 ~~ var1_1*item1_1+ 0*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item2_1 ~~ var2_1*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item3_1 ~~ var3_1*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item4_1 ~~ var4_1*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item5_1 ~~ var5_1*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item6_1 ~~ var6_1*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item7_1 ~~ var7_1*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item8_1 ~~ var8_1*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item9_1 ~~ var9_1*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item10_1 ~~ var10_1*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item11_1 ~~ var11_1*item11_1+ 0*item12_1+ 0*item13_1
  item12_1 ~~ var12_1*item12_1+ 0*item13_1
  item13_1 ~~ var13_1*item13_1


## LRVs variances constraints
  eta1_1_var*l1_1^2 + var1_1 == 1
  eta1_1_var*l2_1^2 + var2_1 == 1
  eta1_1_var*l3_1^2 + var3_1 == 1
  eta1_1_var*l4_1^2 + var4_1 == 1
  eta1_1_var*l5_1^2 + var5_1 == 1
  eta1_1_var*l6_1^2 + var6_1 == 1
  eta1_1_var*l7_1^2 + var7_1 == 1
  eta1_1_var*l8_1^2 + var8_1 == 1
  eta1_1_var*l9_1^2 + var9_1 == 1
  eta1_1_var*l10_1^2 + var10_1 == 1
  eta1_1_var*l11_1^2 + var11_1 == 1
  eta1_1_var*l12_1^2 + var12_1 == 1
  eta1_1_var*l13_1^2 + var13_1 == 1
'


mod.effects_coding.conditional_parameterization.free_threshold<- function(){}
mod.effects_coding.conditional_parameterization.free_threshold <- '


  eta1_1=~l1_1*item1_1+l2_1*item2_1+l3_1*item3_1+l4_1*item4_1+l5_1*item5_1+l6_1*item6_1+l7_1*item7_1+l8_1*item8_1+l9_1*item9_1+l10_1*item10_1+l11_1*item11_1+l12_1*item12_1+l13_1*item13_1
  13==l1_1+l2_1+l3_1+l4_1+l5_1+l6_1+l7_1+l8_1+l9_1+l10_1+l11_1+l12_1+l13_1

#latent var means and var
  eta1_1 ~ eta1_1_mean*1
  eta1_1 ~~ eta1_1_var*eta1_1
  
  eta1_1_mean==0

## Latent response variable (LRV) intercepts
  item1_1 ~ int1_1*1
  item2_1 ~ int2_1*1
  item3_1 ~ int3_1*1
  item4_1 ~ int4_1*1
  item5_1 ~ int5_1*1
  item6_1 ~ int6_1*1
  item7_1 ~ int7_1*1
  item8_1 ~ int8_1*1
  item9_1 ~ int9_1*1
  item10_1 ~ int10_1*1
  item11_1 ~ int11_1*1
  item12_1 ~ int12_1*1
  item13_1 ~ int13_1*1

  int1_1 ==0
  int2_1 ==0
  int3_1 ==0
  int4_1 ==0
  int5_1 ==0
  int6_1 ==0
  int7_1 ==0
  int8_1 ==0
  int9_1 ==0
  int10_1==0
  int11_1==0
  int12_1==0
  int13_1==0


#Latent response variable (LRV) mean constraint

  int1_1 + l1_1*eta1_1_mean == 0
  int2_1 + l2_1*eta1_1_mean == 0
  int3_1 + l3_1*eta1_1_mean == 0
  int4_1 + l4_1*eta1_1_mean == 0
  int5_1 + l5_1*eta1_1_mean == 0
  int6_1 + l6_1*eta1_1_mean == 0
  int7_1 + l7_1*eta1_1_mean == 0
  int8_1 + l8_1*eta1_1_mean == 0
  int9_1 + l9_1*eta1_1_mean == 0
  int10_1 + l10_1*eta1_1_mean == 0
  int11_1 + l11_1*eta1_1_mean == 0
  int12_1 + l12_1*eta1_1_mean == 0
  int13_1 + l13_1*eta1_1_mean == 0

## thresholds link  LRVs to observed items
  item1_1 | thr1_1*t1
  item2_1 | thr2_1*t1
  item3_1 | thr3_1*t1
  item4_1 | thr4_1*t1
  item5_1 | thr5_1*t1
  item6_1 | thr6_1*t1
  item7_1 | thr7_1*t1
  item8_1 | thr8_1*t1
  item9_1 | thr9_1*t1
  item10_1 | thr10_1*t1
  item11_1 | thr11_1*t1
  item12_1 | thr12_1*t1
  item13_1 | thr13_1*t1

## LRVs (co)variances
  item1_1 ~~ var1_1*item1_1+ 0*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item2_1 ~~ var2_1*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item3_1 ~~ var3_1*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item4_1 ~~ var4_1*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item5_1 ~~ var5_1*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item6_1 ~~ var6_1*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item7_1 ~~ var7_1*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item8_1 ~~ var8_1*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item9_1 ~~ var9_1*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item10_1 ~~ var10_1*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item11_1 ~~ var11_1*item11_1+ 0*item12_1+ 0*item13_1
  item12_1 ~~ var12_1*item12_1+ 0*item13_1
  item13_1 ~~ var13_1*item13_1

  var1_1 ==1
  var2_1 ==1
  var3_1 ==1
  var4_1 ==1
  var5_1 ==1
  var6_1 ==1
  var7_1 ==1
  var8_1 ==1
  var9_1 ==1
  var10_1 ==1
  var11_1 ==1
  var12_1 ==1
  var13_1 ==1
'

mod.effects_coding.conditional_parameterization.free_intercept<- function(){}
mod.effects_coding.conditional_parameterization.free_intercept <- '


  eta1_1=~l1_1*item1_1+l2_1*item2_1+l3_1*item3_1+l4_1*item4_1+l5_1*item5_1+l6_1*item6_1+l7_1*item7_1+l8_1*item8_1+l9_1*item9_1+l10_1*item10_1+l11_1*item11_1+l12_1*item12_1+l13_1*item13_1
  13==l1_1+l2_1+l3_1+l4_1+l5_1+l6_1+l7_1+l8_1+l9_1+l10_1+l11_1+l12_1+l13_1

#latent var means and var
  eta1_1 ~ eta1_1_mean*1
  eta1_1 ~~ eta1_1_var*eta1_1
	eta1_1_mean==0
	
## Latent response variable (LRV) intercepts
  item1_1 ~ int1_1*1
  item2_1 ~ int2_1*1
  item3_1 ~ int3_1*1
  item4_1 ~ int4_1*1
  item5_1 ~ int5_1*1
  item6_1 ~ int6_1*1
  item7_1 ~ int7_1*1
  item8_1 ~ int8_1*1
  item9_1 ~ int9_1*1
  item10_1 ~ int10_1*1
  item11_1 ~ int11_1*1
  item12_1 ~ int12_1*1
  item13_1 ~ int13_1*1

## thresholds link  LRVs to observed items
  item1_1 | thr1_1*t1
  item2_1 | thr2_1*t1
  item3_1 | thr3_1*t1
  item4_1 | thr4_1*t1
  item5_1 | thr5_1*t1
  item6_1 | thr6_1*t1
  item7_1 | thr7_1*t1
  item8_1 | thr8_1*t1
  item9_1 | thr9_1*t1
  item10_1 | thr10_1*t1
  item11_1 | thr11_1*t1
  item12_1 | thr12_1*t1
  item13_1 | thr13_1*t1

  thr1_1==0
  thr2_1==0
  thr3_1==0
  thr4_1==0
  thr5_1==0
  thr6_1==0
  thr7_1==0
  thr8_1==0
  thr9_1==0
  thr10_1==0
  thr11_1==0
  thr12_1==0
  thr13_1==0


## LRVs (co)variances
  item1_1 ~~ var1_1*item1_1+ 0*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item2_1 ~~ var2_1*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item3_1 ~~ var3_1*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item4_1 ~~ var4_1*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item5_1 ~~ var5_1*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item6_1 ~~ var6_1*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item7_1 ~~ var7_1*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item8_1 ~~ var8_1*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item9_1 ~~ var9_1*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item10_1 ~~ var10_1*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item11_1 ~~ var11_1*item11_1+ 0*item12_1+ 0*item13_1
  item12_1 ~~ var12_1*item12_1+ 0*item13_1
  item13_1 ~~ var13_1*item13_1

  var1_1 ==1
  var2_1 ==1
  var3_1 ==1
  var4_1 ==1
  var5_1 ==1
  var6_1 ==1
  var7_1 ==1
  var8_1 ==1
  var9_1 ==1
  var10_1 ==1
  var11_1 ==1
  var12_1 ==1
  var13_1 ==1
'
mod.effects_coding.conditional_parameterization.free_mean<- function(){}
mod.effects_coding.conditional_parameterization.free_mean <- '


  eta1_1=~l1_1*item1_1+l2_1*item2_1+l3_1*item3_1+l4_1*item4_1+l5_1*item5_1+l6_1*item6_1+l7_1*item7_1+l8_1*item8_1+l9_1*item9_1+l10_1*item10_1+l11_1*item11_1+l12_1*item12_1+l13_1*item13_1
  13==l1_1+l2_1+l3_1+l4_1+l5_1+l6_1+l7_1+l8_1+l9_1+l10_1+l11_1+l12_1+l13_1

#latent var means and var
  eta1_1 ~ eta1_1_mean*1
  eta1_1 ~~ eta1_1_var*eta1_1

## Latent response variable (LRV) intercepts
  item1_1 ~ int1_1*1
  item2_1 ~ int2_1*1
  item3_1 ~ int3_1*1
  item4_1 ~ int4_1*1
  item5_1 ~ int5_1*1
  item6_1 ~ int6_1*1
  item7_1 ~ int7_1*1
  item8_1 ~ int8_1*1
  item9_1 ~ int9_1*1
  item10_1 ~ int10_1*1
  item11_1 ~ int11_1*1
  item12_1 ~ int12_1*1
  item13_1 ~ int13_1*1

  int1_1 ==0 
  int2_1 ==0 
  int3_1 ==0 
  int4_1 ==0 
  int5_1 ==0 
  int6_1 ==0 
  int7_1 ==0 
  int8_1 ==0 
  int9_1 ==0 
  int10_1==0 
  int11_1==0 
  int12_1==0 
  int13_1==0 


## thresholds link  LRVs to observed items
  item1_1 | thr1_1*t1
  item2_1 | thr2_1*t1
  item3_1 | thr3_1*t1
  item4_1 | thr4_1*t1
  item5_1 | thr5_1*t1
  item6_1 | thr6_1*t1
  item7_1 | thr7_1*t1
  item8_1 | thr8_1*t1
  item9_1 | thr9_1*t1
  item10_1 | thr10_1*t1
  item11_1 | thr11_1*t1
  item12_1 | thr12_1*t1
  item13_1 | thr13_1*t1

  0==thr1_1+thr2_1+thr3_1+thr4_1+thr5_1+thr6_1+thr7_1+thr8_1+thr9_1+thr10_1+thr11_1+thr12_1+thr13_1


## LRVs (co)variances
  item1_1 ~~ var1_1*item1_1+ 0*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item2_1 ~~ var2_1*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item3_1 ~~ var3_1*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item4_1 ~~ var4_1*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item5_1 ~~ var5_1*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item6_1 ~~ var6_1*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item7_1 ~~ var7_1*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item8_1 ~~ var8_1*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item9_1 ~~ var9_1*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item10_1 ~~ var10_1*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item11_1 ~~ var11_1*item11_1+ 0*item12_1+ 0*item13_1
  item12_1 ~~ var12_1*item12_1+ 0*item13_1
  item13_1 ~~ var13_1*item13_1

  var1_1 ==1
  var2_1 ==1
  var3_1 ==1
  var4_1 ==1
  var5_1 ==1
  var6_1 ==1
  var7_1 ==1
  var8_1 ==1
  var9_1 ==1
  var10_1 ==1
  var11_1 ==1
  var12_1 ==1
  var13_1 ==1
'


mod.reference_indicator.marginal_parameterization.free_threshold<- function(){}
mod.reference_indicator.marginal_parameterization.free_threshold <- '

  eta1_1=~l1_1*item1_1+l2_1*item2_1+l3_1*item3_1+l4_1*item4_1+l5_1*item5_1+l6_1*item6_1+l7_1*item7_1+l8_1*item8_1+l9_1*item9_1+l10_1*item10_1+l11_1*item11_1+l12_1*item12_1+l13_1*item13_1
  l4_1==1

  #latent var means and var
  eta1_1 ~ eta1_1_mean*1
  eta1_1 ~~ eta1_1_var*eta1_1
  eta1_1_mean==0
  
  ## Latent response variable (LRV) intercepts
  item1_1 ~ int1_1*1
  item2_1 ~ int2_1*1
  item3_1 ~ int3_1*1
  item4_1 ~ int4_1*1
  item5_1 ~ int5_1*1
  item6_1 ~ int6_1*1
  item7_1 ~ int7_1*1
  item8_1 ~ int8_1*1
  item9_1 ~ int9_1*1
  item10_1 ~ int10_1*1
  item11_1 ~ int11_1*1
  item12_1 ~ int12_1*1
  item13_1 ~ int13_1*1

  int1_1 ==0
  int2_1 ==0
  int3_1 ==0
  int4_1 ==0
  int5_1 ==0
  int6_1 ==0
  int7_1 ==0
  int8_1 ==0
  int9_1 ==0
  int10_1==0
  int11_1==0
  int12_1==0
  int13_1==0


  ## thresholds link  LRVs to observed items
  item1_1 | thr1_1*t1
  item2_1 | thr2_1*t1
  item3_1 | thr3_1*t1
  item4_1 | thr4_1*t1
  item5_1 | thr5_1*t1
  item6_1 | thr6_1*t1
  item7_1 | thr7_1*t1
  item8_1 | thr8_1*t1
  item9_1 | thr9_1*t1
  item10_1 | thr10_1*t1
  item11_1 | thr11_1*t1
  item12_1 | thr12_1*t1
  item13_1 | thr13_1*t1
  
  ## LRVs (co)variances
  item1_1 ~~ var1_1*item1_1+ 0*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item2_1 ~~ var2_1*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item3_1 ~~ var3_1*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item4_1 ~~ var4_1*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item5_1 ~~ var5_1*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item6_1 ~~ var6_1*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item7_1 ~~ var7_1*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item8_1 ~~ var8_1*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item9_1 ~~ var9_1*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item10_1 ~~ var10_1*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item11_1 ~~ var11_1*item11_1+ 0*item12_1+ 0*item13_1
  item12_1 ~~ var12_1*item12_1+ 0*item13_1
  item13_1 ~~ var13_1*item13_1
  
  eta1_1_var*l1_1^2 + var1_1 == 1
  eta1_1_var*l2_1^2 + var2_1 == 1
  eta1_1_var*l3_1^2 + var3_1 == 1
  eta1_1_var*l4_1^2 + var4_1 == 1
  eta1_1_var*l5_1^2 + var5_1 == 1
  eta1_1_var*l6_1^2 + var6_1 == 1
  eta1_1_var*l7_1^2 + var7_1 == 1
  eta1_1_var*l8_1^2 + var8_1 == 1
  eta1_1_var*l9_1^2 + var9_1 == 1
  eta1_1_var*l10_1^2 + var10_1 == 1
  eta1_1_var*l11_1^2 + var11_1 == 1
  eta1_1_var*l12_1^2 + var12_1 == 1
  eta1_1_var*l13_1^2 + var13_1 == 1
'

mod.reference_indicator.marginal_parameterization.free_intercept<- function(){}
mod.reference_indicator.marginal_parameterization.free_intercept <- '

  eta1_1=~l1_1*item1_1+l2_1*item2_1+l3_1*item3_1+l4_1*item4_1+l5_1*item5_1+l6_1*item6_1+l7_1*item7_1+l8_1*item8_1+l9_1*item9_1+l10_1*item10_1+l11_1*item11_1+l12_1*item12_1+l13_1*item13_1
  l4_1==1

  #latent var means and var
  eta1_1 ~ eta1_1_mean*1
  eta1_1 ~~ eta1_1_var*eta1_1
	eta1_1_mean==0
	
  ## Latent response variable (LRV) intercepts
  item1_1 ~ int1_1*1
  item2_1 ~ int2_1*1
  item3_1 ~ int3_1*1
  item4_1 ~ int4_1*1
  item5_1 ~ int5_1*1
  item6_1 ~ int6_1*1
  item7_1 ~ int7_1*1
  item8_1 ~ int8_1*1
  item9_1 ~ int9_1*1
  item10_1 ~ int10_1*1
  item11_1 ~ int11_1*1
  item12_1 ~ int12_1*1
  item13_1 ~ int13_1*1


    ## thresholds link  LRVs to observed items
  item1_1 | thr1_1*t1
  item2_1 | thr2_1*t1
  item3_1 | thr3_1*t1
  item4_1 | thr4_1*t1
  item5_1 | thr5_1*t1
  item6_1 | thr6_1*t1
  item7_1 | thr7_1*t1
  item8_1 | thr8_1*t1
  item9_1 | thr9_1*t1
  item10_1 | thr10_1*t1
  item11_1 | thr11_1*t1
  item12_1 | thr12_1*t1
  item13_1 | thr13_1*t1

  thr1_1==0
  thr2_1==0
  thr3_1==0
  thr4_1==0
  thr5_1==0
  thr6_1==0
  thr7_1==0
  thr8_1==0
  thr9_1==0
  thr10_1==0
  thr11_1==0
  thr12_1==0
  thr13_1==0

  
  ## LRVs (co)variances
  item1_1 ~~ var1_1*item1_1+ 0*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item2_1 ~~ var2_1*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item3_1 ~~ var3_1*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item4_1 ~~ var4_1*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item5_1 ~~ var5_1*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item6_1 ~~ var6_1*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item7_1 ~~ var7_1*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item8_1 ~~ var8_1*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item9_1 ~~ var9_1*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item10_1 ~~ var10_1*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item11_1 ~~ var11_1*item11_1+ 0*item12_1+ 0*item13_1
  item12_1 ~~ var12_1*item12_1+ 0*item13_1
  item13_1 ~~ var13_1*item13_1
  
  eta1_1_var*l1_1^2 + var1_1 == 1
  eta1_1_var*l2_1^2 + var2_1 == 1
  eta1_1_var*l3_1^2 + var3_1 == 1
  eta1_1_var*l4_1^2 + var4_1 == 1
  eta1_1_var*l5_1^2 + var5_1 == 1
  eta1_1_var*l6_1^2 + var6_1 == 1
  eta1_1_var*l7_1^2 + var7_1 == 1
  eta1_1_var*l8_1^2 + var8_1 == 1
  eta1_1_var*l9_1^2 + var9_1 == 1
  eta1_1_var*l10_1^2 + var10_1 == 1
  eta1_1_var*l11_1^2 + var11_1 == 1
  eta1_1_var*l12_1^2 + var12_1 == 1
  eta1_1_var*l13_1^2 + var13_1 == 1
'

mod.reference_indicator.marginal_parameterization.free_mean<- function(){}
mod.reference_indicator.marginal_parameterization.free_mean <- '

  eta1_1=~l1_1*item1_1+l2_1*item2_1+l3_1*item3_1+l4_1*item4_1+l5_1*item5_1+l6_1*item6_1+l7_1*item7_1+l8_1*item8_1+l9_1*item9_1+l10_1*item10_1+l11_1*item11_1+l12_1*item12_1+l13_1*item13_1
  l4_1==1

  #latent var means and var
  eta1_1 ~ eta1_1_mean*1
  eta1_1 ~~ eta1_1_var*eta1_1
  
  ## Latent response variable (LRV) intercepts
  item1_1 ~ int1_1*1
  item2_1 ~ int2_1*1
  item3_1 ~ int3_1*1
  item4_1 ~ int4_1*1
  item5_1 ~ int5_1*1
  item6_1 ~ int6_1*1
  item7_1 ~ int7_1*1
  item8_1 ~ int8_1*1
  item9_1 ~ int9_1*1
  item10_1 ~ int10_1*1
  item11_1 ~ int11_1*1
  item12_1 ~ int12_1*1
  item13_1 ~ int13_1*1

  int1_1 ==0 
  int2_1 ==0 
  int3_1 ==0 
  int4_1 ==0 
  int5_1 ==0 
  int6_1 ==0 
  int7_1 ==0 
  int8_1 ==0 
  int9_1 ==0 
  int10_1==0 
  int11_1==0 
  int12_1==0 
  int13_1==0 

  ## thresholds link  LRVs to observed items
  item1_1 | thr1_1*t1
  item2_1 | thr2_1*t1
  item3_1 | thr3_1*t1
  item4_1 | thr4_1*t1
  item5_1 | thr5_1*t1
  item6_1 | thr6_1*t1
  item7_1 | thr7_1*t1
  item8_1 | thr8_1*t1
  item9_1 | thr9_1*t1
  item10_1 | thr10_1*t1
  item11_1 | thr11_1*t1
  item12_1 | thr12_1*t1
  item13_1 | thr13_1*t1

  thr4_1==0

  
  ## LRVs (co)variances
  item1_1 ~~ var1_1*item1_1+ 0*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item2_1 ~~ var2_1*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item3_1 ~~ var3_1*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item4_1 ~~ var4_1*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item5_1 ~~ var5_1*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item6_1 ~~ var6_1*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item7_1 ~~ var7_1*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item8_1 ~~ var8_1*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item9_1 ~~ var9_1*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item10_1 ~~ var10_1*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item11_1 ~~ var11_1*item11_1+ 0*item12_1+ 0*item13_1
  item12_1 ~~ var12_1*item12_1+ 0*item13_1
  item13_1 ~~ var13_1*item13_1
  
  eta1_1_var*l1_1^2 + var1_1 == 1
  eta1_1_var*l2_1^2 + var2_1 == 1
  eta1_1_var*l3_1^2 + var3_1 == 1
  eta1_1_var*l4_1^2 + var4_1 == 1
  eta1_1_var*l5_1^2 + var5_1 == 1
  eta1_1_var*l6_1^2 + var6_1 == 1
  eta1_1_var*l7_1^2 + var7_1 == 1
  eta1_1_var*l8_1^2 + var8_1 == 1
  eta1_1_var*l9_1^2 + var9_1 == 1
  eta1_1_var*l10_1^2 + var10_1 == 1
  eta1_1_var*l11_1^2 + var11_1 == 1
  eta1_1_var*l12_1^2 + var12_1 == 1
  eta1_1_var*l13_1^2 + var13_1 == 1
'


mod.reference_indicator.conditional_parameterization.free_threshold<- function(){}
mod.reference_indicator.conditional_parameterization.free_threshold <- '

  eta1_1=~l1_1*item1_1+l2_1*item2_1+l3_1*item3_1+l4_1*item4_1+l5_1*item5_1+l6_1*item6_1+l7_1*item7_1+l8_1*item8_1+l9_1*item9_1+l10_1*item10_1+l11_1*item11_1+l12_1*item12_1+l13_1*item13_1
  l4_1==1

  #latent var means and var
  eta1_1 ~ eta1_1_mean*1
  eta1_1 ~~ eta1_1_var*eta1_1
  eta1_1_mean==0

  ## Latent response variable (LRV) intercepts
  item1_1 ~ int1_1*1
  item2_1 ~ int2_1*1
  item3_1 ~ int3_1*1
  item4_1 ~ int4_1*1
  item5_1 ~ int5_1*1
  item6_1 ~ int6_1*1
  item7_1 ~ int7_1*1
  item8_1 ~ int8_1*1
  item9_1 ~ int9_1*1
  item10_1 ~ int10_1*1
  item11_1 ~ int11_1*1
  item12_1 ~ int12_1*1
  item13_1 ~ int13_1*1

	int1_1 ==0
	int2_1 ==0
	int3_1 ==0
	int4_1 ==0
	int5_1 ==0
	int6_1 ==0
	int7_1 ==0
	int8_1 ==0
	int9_1 ==0
	int10_1==0
	int11_1==0
	int12_1==0
	int13_1==0


    ## thresholds link  LRVs to observed items
  item1_1 | thr1_1*t1
  item2_1 | thr2_1*t1
  item3_1 | thr3_1*t1
  item4_1 | thr4_1*t1
  item5_1 | thr5_1*t1
  item6_1 | thr6_1*t1
  item7_1 | thr7_1*t1
  item8_1 | thr8_1*t1
  item9_1 | thr9_1*t1
  item10_1 | thr10_1*t1
  item11_1 | thr11_1*t1
  item12_1 | thr12_1*t1
  item13_1 | thr13_1*t1
  
  ## LRVs (co)variances
  item1_1 ~~ var1_1*item1_1+ 0*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item2_1 ~~ var2_1*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item3_1 ~~ var3_1*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item4_1 ~~ var4_1*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item5_1 ~~ var5_1*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item6_1 ~~ var6_1*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item7_1 ~~ var7_1*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item8_1 ~~ var8_1*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item9_1 ~~ var9_1*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item10_1 ~~ var10_1*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item11_1 ~~ var11_1*item11_1+ 0*item12_1+ 0*item13_1
  item12_1 ~~ var12_1*item12_1+ 0*item13_1
  item13_1 ~~ var13_1*item13_1
  
  var1_1==1
  var2_1==1
  var3_1==1
  var4_1==1
  var5_1==1
  var6_1==1
  var7_1==1
  var8_1==1
  var9_1==1
  var10_1==1
  var11_1 ==1
  var12_1 ==1
  var13_1 ==1
'
mod.reference_indicator.conditional_parameterization.free_intercept<- function(){}
mod.reference_indicator.conditional_parameterization.free_intercept <- '

  eta1_1=~l1_1*item1_1+l2_1*item2_1+l3_1*item3_1+l4_1*item4_1+l5_1*item5_1+l6_1*item6_1+l7_1*item7_1+l8_1*item8_1+l9_1*item9_1+l10_1*item10_1+l11_1*item11_1+l12_1*item12_1+l13_1*item13_1
  l4_1==1

  #latent var means and var
  eta1_1 ~ eta1_1_mean*1
  eta1_1 ~~ eta1_1_var*eta1_1
  eta1_1_mean==0
  
  ## Latent response variable (LRV) intercepts
  item1_1 ~ int1_1*1
  item2_1 ~ int2_1*1
  item3_1 ~ int3_1*1
  item4_1 ~ int4_1*1
  item5_1 ~ int5_1*1
  item6_1 ~ int6_1*1
  item7_1 ~ int7_1*1
  item8_1 ~ int8_1*1
  item9_1 ~ int9_1*1
  item10_1 ~ int10_1*1
  item11_1 ~ int11_1*1
  item12_1 ~ int12_1*1
  item13_1 ~ int13_1*1


    ## thresholds link  LRVs to observed items
  item1_1 | thr1_1*t1
  item2_1 | thr2_1*t1
  item3_1 | thr3_1*t1
  item4_1 | thr4_1*t1
  item5_1 | thr5_1*t1
  item6_1 | thr6_1*t1
  item7_1 | thr7_1*t1
  item8_1 | thr8_1*t1
  item9_1 | thr9_1*t1
  item10_1 | thr10_1*t1
  item11_1 | thr11_1*t1
  item12_1 | thr12_1*t1
  item13_1 | thr13_1*t1

  thr1_1==0
  thr2_1==0
  thr3_1==0
  thr4_1==0
  thr5_1==0
  thr6_1==0
  thr7_1==0
  thr8_1==0
  thr9_1==0
  thr10_1==0
  thr11_1==0
  thr12_1==0
  thr13_1==0

  
  ## LRVs (co)variances
  item1_1 ~~ var1_1*item1_1+ 0*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item2_1 ~~ var2_1*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item3_1 ~~ var3_1*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item4_1 ~~ var4_1*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item5_1 ~~ var5_1*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item6_1 ~~ var6_1*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item7_1 ~~ var7_1*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item8_1 ~~ var8_1*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item9_1 ~~ var9_1*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item10_1 ~~ var10_1*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item11_1 ~~ var11_1*item11_1+ 0*item12_1+ 0*item13_1
  item12_1 ~~ var12_1*item12_1+ 0*item13_1
  item13_1 ~~ var13_1*item13_1
  
  var1_1==1
  var2_1==1
  var3_1==1
  var4_1==1
  var5_1==1
  var6_1==1
  var7_1==1
  var8_1==1
  var9_1==1
  var10_1==1
  var11_1 ==1
  var12_1 ==1
  var13_1 ==1
'

mod.reference_indicator.conditional_parameterization.free_mean<- function(){}
mod.reference_indicator.conditional_parameterization.free_mean <- '

  eta1_1=~l1_1*item1_1+l2_1*item2_1+l3_1*item3_1+l4_1*item4_1+l5_1*item5_1+l6_1*item6_1+l7_1*item7_1+l8_1*item8_1+l9_1*item9_1+l10_1*item10_1+l11_1*item11_1+l12_1*item12_1+l13_1*item13_1
  l4_1==1

  #latent var means and var
  eta1_1 ~ eta1_1_mean*1
  eta1_1 ~~ eta1_1_var*eta1_1
  
  ## Latent response variable (LRV) intercepts
  item1_1 ~ int1_1*1
  item2_1 ~ int2_1*1
  item3_1 ~ int3_1*1
  item4_1 ~ int4_1*1
  item5_1 ~ int5_1*1
  item6_1 ~ int6_1*1
  item7_1 ~ int7_1*1
  item8_1 ~ int8_1*1
  item9_1 ~ int9_1*1
  item10_1 ~ int10_1*1
  item11_1 ~ int11_1*1
  item12_1 ~ int12_1*1
  item13_1 ~ int13_1*1

  int1_1 ==0 
  int2_1 ==0 
  int3_1 ==0 
  int4_1 ==0 
  int5_1 ==0 
  int6_1 ==0 
  int7_1 ==0 
  int8_1 ==0 
  int9_1 ==0 
  int10_1==0 
  int11_1==0 
  int12_1==0 
  int13_1==0 

    ## thresholds link  LRVs to observed items
  item1_1 | thr1_1*t1
  item2_1 | thr2_1*t1
  item3_1 | thr3_1*t1
  item4_1 | thr4_1*t1
  item5_1 | thr5_1*t1
  item6_1 | thr6_1*t1
  item7_1 | thr7_1*t1
  item8_1 | thr8_1*t1
  item9_1 | thr9_1*t1
  item10_1 | thr10_1*t1
  item11_1 | thr11_1*t1
  item12_1 | thr12_1*t1
  item13_1 | thr13_1*t1

  thr4_1==0

  
  ## LRVs (co)variances
  item1_1 ~~ var1_1*item1_1+ 0*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item2_1 ~~ var2_1*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item3_1 ~~ var3_1*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item4_1 ~~ var4_1*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item5_1 ~~ var5_1*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item6_1 ~~ var6_1*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item7_1 ~~ var7_1*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item8_1 ~~ var8_1*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item9_1 ~~ var9_1*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item10_1 ~~ var10_1*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item11_1 ~~ var11_1*item11_1+ 0*item12_1+ 0*item13_1
  item12_1 ~~ var12_1*item12_1+ 0*item13_1
  item13_1 ~~ var13_1*item13_1
  
  var1_1==1
  var2_1==1
  var3_1==1
  var4_1==1
  var5_1==1
  var6_1==1
  var7_1==1
  var8_1==1
  var9_1==1
  var10_1==1
  var11_1 ==1
  var12_1 ==1
  var13_1 ==1
'

mod.factor_parameterization.marginal_parameterization.free_intercept.threqu0p5.old <- function(){}
mod.factor_parameterization.marginal_parameterization.free_intercept.threqu0p5.old <- '

eta1_1=~l1_1*item1_1+l2_1*item2_1+l3_1*item3_1+l4_1*item4_1+l5_1*item5_1+l6_1*item6_1+l7_1*item7_1+l8_1*item8_1+l9_1*item9_1+l10_1*item10_1+l11_1*item11_1+l12_1*item12_1+l13_1*item13_1
#latent var means and var
eta1_1 ~ eta1_1_mean*1
eta1_1 ~~ eta1_1_var*eta1_1
eta1_1_mean==0
	
## Latent response variable (LRV) intercepts
item1_1 ~ int1_1*1
item2_1 ~ int2_1*1
item3_1 ~ int3_1*1
item4_1 ~ int4_1*1
item5_1 ~ int5_1*1
item6_1 ~ int6_1*1
item7_1 ~ int7_1*1
item8_1 ~ int8_1*1
item9_1 ~ int9_1*1
item10_1 ~ int10_1*1
item11_1 ~ int11_1*1
item12_1 ~ int12_1*1
item13_1 ~ int13_1*1


## thresholds link  LRVs to observed items
item1_1 | thr1_1*t1
item2_1 | thr2_1*t1
item3_1 | thr3_1*t1
item4_1 | thr4_1*t1
item5_1 | thr5_1*t1
item6_1 | thr6_1*t1
item7_1 | thr7_1*t1
item8_1 | thr8_1*t1
item9_1 | thr9_1*t1
item10_1 | thr10_1*t1
item11_1 | thr11_1*t1
item12_1 | thr12_1*t1
item13_1 | thr13_1*t1

thr1_1==0.5
thr2_1==0.5
thr3_1==0.5
thr4_1==0.5
thr5_1==0.5
thr6_1==0.5
thr7_1==0.5
thr8_1==0.5
thr9_1==0.5
thr10_1==0.5
thr11_1==0.5
thr12_1==0.5
thr13_1==0.5

## LRVs (co)variances
item1_1 ~~ var1_1*item1_1+ 0*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item2_1 ~~ var2_1*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item3_1 ~~ var3_1*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item4_1 ~~ var4_1*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item5_1 ~~ var5_1*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item6_1 ~~ var6_1*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item7_1 ~~ var7_1*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item8_1 ~~ var8_1*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item9_1 ~~ var9_1*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item10_1 ~~ var10_1*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item11_1 ~~ var11_1*item11_1+ 0*item12_1+ 0*item13_1
item12_1 ~~ var12_1*item12_1+ 0*item13_1
item13_1 ~~ var13_1*item13_1


## LRVs variances constraints
eta1_1_var*l1_1^2 + var1_1 == 1
eta1_1_var*l2_1^2 + var2_1 == 1
eta1_1_var*l3_1^2 + var3_1 == 1
eta1_1_var*l4_1^2 + var4_1 == 1
eta1_1_var*l5_1^2 + var5_1 == 1
eta1_1_var*l6_1^2 + var6_1 == 1
eta1_1_var*l7_1^2 + var7_1 == 1
eta1_1_var*l8_1^2 + var8_1 == 1
eta1_1_var*l9_1^2 + var9_1 == 1
eta1_1_var*l10_1^2 + var10_1 == 1
eta1_1_var*l11_1^2 + var11_1 == 1
eta1_1_var*l12_1^2 + var12_1 == 1
eta1_1_var*l13_1^2 + var13_1 == 1

'

mod.factor_parameterization.conditional_parameterization.free_intercept.threqu0p5.old <- function(){}
mod.factor_parameterization.conditional_parameterization.free_intercept.threqu0p5.old <- '
  eta1_1=~l1_1*item1_1+l2_1*item2_1+l3_1*item3_1+l4_1*item4_1+l5_1*item5_1+l6_1*item6_1+l7_1*item7_1+l8_1*item8_1+l9_1*item9_1+l10_1*item10_1+l11_1*item11_1+l12_1*item12_1+l13_1*item13_1
  #latent var means and var
  eta1_1 ~ eta1_1_mean*1
  eta1_1 ~~ eta1_1_var*eta1_1
  eta1_1_mean==0
  	
  ## Latent response variable (LRV) intercepts
  item1_1 ~ int1_1*1
  item2_1 ~ int2_1*1
  item3_1 ~ int3_1*1
  item4_1 ~ int4_1*1
  item5_1 ~ int5_1*1
  item6_1 ~ int6_1*1
  item7_1 ~ int7_1*1
  item8_1 ~ int8_1*1
  item9_1 ~ int9_1*1
  item10_1 ~ int10_1*1
  item11_1 ~ int11_1*1
  item12_1 ~ int12_1*1
  item13_1 ~ int13_1*1

    ## thresholds link  LRVs to observed items
  item1_1 | thr1_1*t1
  item2_1 | thr2_1*t1
  item3_1 | thr3_1*t1
  item4_1 | thr4_1*t1
  item5_1 | thr5_1*t1
  item6_1 | thr6_1*t1
  item7_1 | thr7_1*t1
  item8_1 | thr8_1*t1
  item9_1 | thr9_1*t1
  item10_1 | thr10_1*t1
  item11_1 | thr11_1*t1
  item12_1 | thr12_1*t1
  item13_1 | thr13_1*t1
  
  thr1_1==0.5
  thr2_1==0.5
  thr3_1==0.5
  thr4_1==0.5
  thr5_1==0.5
  thr6_1==0.5
  thr7_1==0.5
  thr8_1==0.5
  thr9_1==0.5
  thr10_1==0.5
  thr11_1==0.5
  thr12_1==0.5
  thr13_1==0.5

  
  ## LRVs (co)variances
  item1_1 ~~ var1_1*item1_1+ 0*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item2_1 ~~ var2_1*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item3_1 ~~ var3_1*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item4_1 ~~ var4_1*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item5_1 ~~ var5_1*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item6_1 ~~ var6_1*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item7_1 ~~ var7_1*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item8_1 ~~ var8_1*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item9_1 ~~ var9_1*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item10_1 ~~ var10_1*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item11_1 ~~ var11_1*item11_1+ 0*item12_1+ 0*item13_1
  item12_1 ~~ var12_1*item12_1+ 0*item13_1
  item13_1 ~~ var13_1*item13_1
  
  var1_1 ==1
  var2_1 ==1
  var3_1 ==1
  var4_1 ==1
  var5_1 ==1
  var6_1 ==1
  var7_1 ==1
  var8_1 ==1
  var9_1 ==1
  var10_1 ==1
  var11_1 ==1
  var12_1 ==1
  var13_1 ==1
'

mod.factor_parameterization.marginal_parameterization.free_intercept.threqu0p5 <- function(){}
mod.factor_parameterization.marginal_parameterization.free_intercept.threqu0p5 <- '

eta1_1=~l1_1*item1_1+l2_1*item2_1+l3_1*item3_1+l4_1*item4_1+l5_1*item5_1+l6_1*item6_1+l7_1*item7_1+l8_1*item8_1+l9_1*item9_1+l10_1*item10_1+l11_1*item11_1+l12_1*item12_1+l13_1*item13_1
#latent var means and var
eta1_1 ~ eta1_1_mean*1
eta1_1 ~~ eta1_1_var*eta1_1

eta1_1_mean==0

## Latent response variable (LRV) intercepts
item1_1 ~ int1_1*1
item2_1 ~ int2_1*1
item3_1 ~ int3_1*1
item4_1 ~ int4_1*1
item5_1 ~ int5_1*1
item6_1 ~ int6_1*1
item7_1 ~ int7_1*1
item8_1 ~ int8_1*1
item9_1 ~ int9_1*1
item10_1 ~ int10_1*1
item11_1 ~ int11_1*1
item12_1 ~ int12_1*1
item13_1 ~ int13_1*1


## thresholds link  LRVs to observed items
item1_1 | thr1_1*t1
item2_1 | thr2_1*t1
item3_1 | thr3_1*t1
item4_1 | thr4_1*t1
item5_1 | thr5_1*t1
item6_1 | thr6_1*t1
item7_1 | thr7_1*t1
item8_1 | thr8_1*t1
item9_1 | thr9_1*t1
item10_1 | thr10_1*t1
item11_1 | thr11_1*t1
item12_1 | thr12_1*t1
item13_1 | thr13_1*t1

thr1_1==0.5
thr2_1==0.5
thr3_1==0.5
thr4_1==0.5
thr5_1==0.5
thr6_1==0.5
thr7_1==0.5
thr8_1==0.5
thr9_1==0.5
thr10_1==0.5
thr11_1==0.5
thr12_1==0.5
thr13_1==0.5

## LRVs (co)variances
item1_1 ~~ var1_1*item1_1+ 0*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item2_1 ~~ var2_1*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item3_1 ~~ var3_1*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item4_1 ~~ var4_1*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item5_1 ~~ var5_1*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item6_1 ~~ var6_1*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item7_1 ~~ var7_1*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item8_1 ~~ var8_1*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item9_1 ~~ var9_1*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item10_1 ~~ var10_1*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item11_1 ~~ var11_1*item11_1+ 0*item12_1+ 0*item13_1
item12_1 ~~ var12_1*item12_1+ 0*item13_1
item13_1 ~~ var13_1*item13_1


## LRVs variances constraints
eta1_1_var*l1_1^2 + var1_1 == 1
eta1_1_var*l2_1^2 + var2_1 == 1
eta1_1_var*l3_1^2 + var3_1 == 1
eta1_1_var*l4_1^2 + var4_1 == 1
eta1_1_var*l5_1^2 + var5_1 == 1
eta1_1_var*l6_1^2 + var6_1 == 1
eta1_1_var*l7_1^2 + var7_1 == 1
eta1_1_var*l8_1^2 + var8_1 == 1
eta1_1_var*l9_1^2 + var9_1 == 1
eta1_1_var*l10_1^2 + var10_1 == 1
eta1_1_var*l11_1^2 + var11_1 == 1
eta1_1_var*l12_1^2 + var12_1 == 1
eta1_1_var*l13_1^2 + var13_1 == 1

'

mod.factor_parameterization.conditional_parameterization.free_intercept.threqu0p5 <- function(){}
mod.factor_parameterization.conditional_parameterization.free_intercept.threqu0p5 <- '
  eta1_1=~l1_1*item1_1+l2_1*item2_1+l3_1*item3_1+l4_1*item4_1+l5_1*item5_1+l6_1*item6_1+l7_1*item7_1+l8_1*item8_1+l9_1*item9_1+l10_1*item10_1+l11_1*item11_1+l12_1*item12_1+l13_1*item13_1
  #latent var means and var
  eta1_1 ~ eta1_1_mean*1
  eta1_1 ~~ eta1_1_var*eta1_1

  eta1_1_mean==0
  
  ## Latent response variable (LRV) intercepts
  item1_1 ~ int1_1*1
  item2_1 ~ int2_1*1
  item3_1 ~ int3_1*1
  item4_1 ~ int4_1*1
  item5_1 ~ int5_1*1
  item6_1 ~ int6_1*1
  item7_1 ~ int7_1*1
  item8_1 ~ int8_1*1
  item9_1 ~ int9_1*1
  item10_1 ~ int10_1*1
  item11_1 ~ int11_1*1
  item12_1 ~ int12_1*1
  item13_1 ~ int13_1*1
    
    ## thresholds link  LRVs to observed items
  item1_1 | thr1_1*t1
  item2_1 | thr2_1*t1
  item3_1 | thr3_1*t1
  item4_1 | thr4_1*t1
  item5_1 | thr5_1*t1
  item6_1 | thr6_1*t1
  item7_1 | thr7_1*t1
  item8_1 | thr8_1*t1
  item9_1 | thr9_1*t1
  item10_1 | thr10_1*t1
  item11_1 | thr11_1*t1
  item12_1 | thr12_1*t1
  item13_1 | thr13_1*t1
  
  thr1_1==0.5
  thr2_1==0.5
  thr3_1==0.5
  thr4_1==0.5
  thr5_1==0.5
  thr6_1==0.5
  thr7_1==0.5
  thr8_1==0.5
  thr9_1==0.5
  thr10_1==0.5
  thr11_1==0.5
  thr12_1==0.5
  thr13_1==0.5

  
  ## LRVs (co)variances
  item1_1 ~~ var1_1*item1_1+ 0*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item2_1 ~~ var2_1*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item3_1 ~~ var3_1*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item4_1 ~~ var4_1*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item5_1 ~~ var5_1*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item6_1 ~~ var6_1*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item7_1 ~~ var7_1*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item8_1 ~~ var8_1*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item9_1 ~~ var9_1*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item10_1 ~~ var10_1*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item11_1 ~~ var11_1*item11_1+ 0*item12_1+ 0*item13_1
  item12_1 ~~ var12_1*item12_1+ 0*item13_1
  item13_1 ~~ var13_1*item13_1
  
  var1_1 ==1
  var2_1 ==1
  var3_1 ==1
  var4_1 ==1
  var5_1 ==1
  var6_1 ==1
  var7_1 ==1
  var8_1 ==1
  var9_1 ==1
  var10_1 ==1
  var11_1 ==1
  var12_1 ==1
  var13_1 ==1
'


mod.effects_coding.marginal_parameterization.free_intercept.threqu0p5 <- function(){}

mod.effects_coding.marginal_parameterization.free_intercept.threqu0p5 <- '

  eta1_1=~l1_1*item1_1+l2_1*item2_1+l3_1*item3_1+l4_1*item4_1+l5_1*item5_1+l6_1*item6_1+l7_1*item7_1+l8_1*item8_1+l9_1*item9_1+l10_1*item10_1+l11_1*item11_1+l12_1*item12_1+l13_1*item13_1
  13==l1_1+l2_1+l3_1+l4_1+l5_1+l6_1+l7_1+l8_1+l9_1+l10_1+l11_1+l12_1+l13_1

#latent var means and var
  eta1_1 ~ eta1_1_mean*1
  eta1_1 ~~ eta1_1_var*eta1_1
	eta1_1_mean==0
	
## Latent response variable (LRV) intercepts
  item1_1 ~ int1_1*1
  item2_1 ~ int2_1*1
  item3_1 ~ int3_1*1
  item4_1 ~ int4_1*1
  item5_1 ~ int5_1*1
  item6_1 ~ int6_1*1
  item7_1 ~ int7_1*1
  item8_1 ~ int8_1*1
  item9_1 ~ int9_1*1
  item10_1 ~ int10_1*1
  item11_1 ~ int11_1*1
  item12_1 ~ int12_1*1
  item13_1 ~ int13_1*1


## thresholds link  LRVs to observed items
  item1_1 | thr1_1*t1
  item2_1 | thr2_1*t1
  item3_1 | thr3_1*t1
  item4_1 | thr4_1*t1
  item5_1 | thr5_1*t1
  item6_1 | thr6_1*t1
  item7_1 | thr7_1*t1
  item8_1 | thr8_1*t1
  item9_1 | thr9_1*t1
  item10_1 | thr10_1*t1
  item11_1 | thr11_1*t1
  item12_1 | thr12_1*t1
  item13_1 | thr13_1*t1

  thr1_1==0.5
  thr2_1==0.5
  thr3_1==0.5
  thr4_1==0.5
  thr5_1==0.5
  thr6_1==0.5
  thr7_1==0.5
  thr8_1==0.5
  thr9_1==0.5
  thr10_1==0.5
  thr11_1==0.5
  thr12_1==0.5
  thr13_1==0.5


## LRVs (co)variances
  item1_1 ~~ var1_1*item1_1+ 0*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item2_1 ~~ var2_1*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item3_1 ~~ var3_1*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item4_1 ~~ var4_1*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item5_1 ~~ var5_1*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item6_1 ~~ var6_1*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item7_1 ~~ var7_1*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item8_1 ~~ var8_1*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item9_1 ~~ var9_1*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item10_1 ~~ var10_1*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item11_1 ~~ var11_1*item11_1+ 0*item12_1+ 0*item13_1
  item12_1 ~~ var12_1*item12_1+ 0*item13_1
  item13_1 ~~ var13_1*item13_1


## LRVs variances constraints
  eta1_1_var*l1_1^2 + var1_1 == 1
  eta1_1_var*l2_1^2 + var2_1 == 1
  eta1_1_var*l3_1^2 + var3_1 == 1
  eta1_1_var*l4_1^2 + var4_1 == 1
  eta1_1_var*l5_1^2 + var5_1 == 1
  eta1_1_var*l6_1^2 + var6_1 == 1
  eta1_1_var*l7_1^2 + var7_1 == 1
  eta1_1_var*l8_1^2 + var8_1 == 1
  eta1_1_var*l9_1^2 + var9_1 == 1
  eta1_1_var*l10_1^2 + var10_1 == 1
  eta1_1_var*l11_1^2 + var11_1 == 1
  eta1_1_var*l12_1^2 + var12_1 == 1
  eta1_1_var*l13_1^2 + var13_1 == 1
'

mod.effects_coding.conditional_parameterization.free_intercept.threqu0p5 <- function(){}
mod.effects_coding.conditional_parameterization.free_intercept.threqu0p5 <- '


  eta1_1=~l1_1*item1_1+l2_1*item2_1+l3_1*item3_1+l4_1*item4_1+l5_1*item5_1+l6_1*item6_1+l7_1*item7_1+l8_1*item8_1+l9_1*item9_1+l10_1*item10_1+l11_1*item11_1+l12_1*item12_1+l13_1*item13_1
  13==l1_1+l2_1+l3_1+l4_1+l5_1+l6_1+l7_1+l8_1+l9_1+l10_1+l11_1+l12_1+l13_1

#latent var means and var
  eta1_1 ~ eta1_1_mean*1
  eta1_1 ~~ eta1_1_var*eta1_1
	eta1_1_mean==0
	
## Latent response variable (LRV) intercepts
  item1_1 ~ int1_1*1
  item2_1 ~ int2_1*1
  item3_1 ~ int3_1*1
  item4_1 ~ int4_1*1
  item5_1 ~ int5_1*1
  item6_1 ~ int6_1*1
  item7_1 ~ int7_1*1
  item8_1 ~ int8_1*1
  item9_1 ~ int9_1*1
  item10_1 ~ int10_1*1
  item11_1 ~ int11_1*1
  item12_1 ~ int12_1*1
  item13_1 ~ int13_1*1

## thresholds link  LRVs to observed items
  item1_1 | thr1_1*t1
  item2_1 | thr2_1*t1
  item3_1 | thr3_1*t1
  item4_1 | thr4_1*t1
  item5_1 | thr5_1*t1
  item6_1 | thr6_1*t1
  item7_1 | thr7_1*t1
  item8_1 | thr8_1*t1
  item9_1 | thr9_1*t1
  item10_1 | thr10_1*t1
  item11_1 | thr11_1*t1
  item12_1 | thr12_1*t1
  item13_1 | thr13_1*t1

  thr1_1==0.5
  thr2_1==0.5
  thr3_1==0.5
  thr4_1==0.5
  thr5_1==0.5
  thr6_1==0.5
  thr7_1==0.5
  thr8_1==0.5
  thr9_1==0.5
  thr10_1==0.5
  thr11_1==0.5
  thr12_1==0.5
  thr13_1==0.5


## LRVs (co)variances
  item1_1 ~~ var1_1*item1_1+ 0*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item2_1 ~~ var2_1*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item3_1 ~~ var3_1*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item4_1 ~~ var4_1*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item5_1 ~~ var5_1*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item6_1 ~~ var6_1*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item7_1 ~~ var7_1*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item8_1 ~~ var8_1*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item9_1 ~~ var9_1*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item10_1 ~~ var10_1*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item11_1 ~~ var11_1*item11_1+ 0*item12_1+ 0*item13_1
  item12_1 ~~ var12_1*item12_1+ 0*item13_1
  item13_1 ~~ var13_1*item13_1

  var1_1 ==1
  var2_1 ==1
  var3_1 ==1
  var4_1 ==1
  var5_1 ==1
  var6_1 ==1
  var7_1 ==1
  var8_1 ==1
  var9_1 ==1
  var10_1 ==1
  var11_1 ==1
  var12_1 ==1
  var13_1 ==1
'

mod.reference_indicator.conditional_parameterization.free_intercept.threqu0p5<- function(){}
mod.reference_indicator.conditional_parameterization.free_intercept.threqu0p5 <- '

  eta1_1=~l1_1*item1_1+l2_1*item2_1+l3_1*item3_1+l4_1*item4_1+l5_1*item5_1+l6_1*item6_1+l7_1*item7_1+l8_1*item8_1+l9_1*item9_1+l10_1*item10_1+l11_1*item11_1+l12_1*item12_1+l13_1*item13_1
  l4_1==1

  #latent var means and var
  eta1_1 ~ eta1_1_mean*1
  eta1_1 ~~ eta1_1_var*eta1_1
	eta1_1_mean==0
	
  ## Latent response variable (LRV) intercepts
  item1_1 ~ int1_1*1
  item2_1 ~ int2_1*1
  item3_1 ~ int3_1*1
  item4_1 ~ int4_1*1
  item5_1 ~ int5_1*1
  item6_1 ~ int6_1*1
  item7_1 ~ int7_1*1
  item8_1 ~ int8_1*1
  item9_1 ~ int9_1*1
  item10_1 ~ int10_1*1
  item11_1 ~ int11_1*1
  item12_1 ~ int12_1*1
  item13_1 ~ int13_1*1


    ## thresholds link  LRVs to observed items
  item1_1 | thr1_1*t1
  item2_1 | thr2_1*t1
  item3_1 | thr3_1*t1
  item4_1 | thr4_1*t1
  item5_1 | thr5_1*t1
  item6_1 | thr6_1*t1
  item7_1 | thr7_1*t1
  item8_1 | thr8_1*t1
  item9_1 | thr9_1*t1
  item10_1 | thr10_1*t1
  item11_1 | thr11_1*t1
  item12_1 | thr12_1*t1
  item13_1 | thr13_1*t1

  thr1_1==0.5
  thr2_1==0.5
  thr3_1==0.5
  thr4_1==0.5
  thr5_1==0.5
  thr6_1==0.5
  thr7_1==0.5
  thr8_1==0.5
  thr9_1==0.5
  thr10_1==0.5
  thr11_1==0.5
  thr12_1==0.5
  thr13_1==0.5

  
  ## LRVs (co)variances
  item1_1 ~~ var1_1*item1_1+ 0*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item2_1 ~~ var2_1*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item3_1 ~~ var3_1*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item4_1 ~~ var4_1*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item5_1 ~~ var5_1*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item6_1 ~~ var6_1*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item7_1 ~~ var7_1*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item8_1 ~~ var8_1*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item9_1 ~~ var9_1*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item10_1 ~~ var10_1*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item11_1 ~~ var11_1*item11_1+ 0*item12_1+ 0*item13_1
  item12_1 ~~ var12_1*item12_1+ 0*item13_1
  item13_1 ~~ var13_1*item13_1
  
  var1_1==1
  var2_1==1
  var3_1==1
  var4_1==1
  var5_1==1
  var6_1==1
  var7_1==1
  var8_1==1
  var9_1==1
  var10_1==1
  var11_1 ==1
  var12_1 ==1
  var13_1 ==1
'

mod.reference_indicator.marginal_parameterization.free_intercept.threqu0p5 <- function(){}
mod.reference_indicator.marginal_parameterization.free_intercept.threqu0p5 <- '

  eta1_1=~l1_1*item1_1+l2_1*item2_1+l3_1*item3_1+l4_1*item4_1+l5_1*item5_1+l6_1*item6_1+l7_1*item7_1+l8_1*item8_1+l9_1*item9_1+l10_1*item10_1+l11_1*item11_1+l12_1*item12_1+l13_1*item13_1
  l4_1==1

  #latent var means and var
  eta1_1 ~ eta1_1_mean*1
  eta1_1 ~~ eta1_1_var*eta1_1
	eta1_1_mean==0
	
  ## Latent response variable (LRV) intercepts
  item1_1 ~ int1_1*1
  item2_1 ~ int2_1*1
  item3_1 ~ int3_1*1
  item4_1 ~ int4_1*1
  item5_1 ~ int5_1*1
  item6_1 ~ int6_1*1
  item7_1 ~ int7_1*1
  item8_1 ~ int8_1*1
  item9_1 ~ int9_1*1
  item10_1 ~ int10_1*1
  item11_1 ~ int11_1*1
  item12_1 ~ int12_1*1
  item13_1 ~ int13_1*1


    ## thresholds link  LRVs to observed items
  item1_1 | thr1_1*t1
  item2_1 | thr2_1*t1
  item3_1 | thr3_1*t1
  item4_1 | thr4_1*t1
  item5_1 | thr5_1*t1
  item6_1 | thr6_1*t1
  item7_1 | thr7_1*t1
  item8_1 | thr8_1*t1
  item9_1 | thr9_1*t1
  item10_1 | thr10_1*t1
  item11_1 | thr11_1*t1
  item12_1 | thr12_1*t1
  item13_1 | thr13_1*t1

  thr1_1==0.5
  thr2_1==0.5
  thr3_1==0.5
  thr4_1==0.5
  thr5_1==0.5
  thr6_1==0.5
  thr7_1==0.5
  thr8_1==0.5
  thr9_1==0.5
  thr10_1==0.5
  thr11_1==0.5
  thr12_1==0.5
  thr13_1==0.5

  
  ## LRVs (co)variances
  item1_1 ~~ var1_1*item1_1+ 0*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item2_1 ~~ var2_1*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item3_1 ~~ var3_1*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item4_1 ~~ var4_1*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item5_1 ~~ var5_1*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item6_1 ~~ var6_1*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item7_1 ~~ var7_1*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item8_1 ~~ var8_1*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item9_1 ~~ var9_1*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item10_1 ~~ var10_1*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
  item11_1 ~~ var11_1*item11_1+ 0*item12_1+ 0*item13_1
  item12_1 ~~ var12_1*item12_1+ 0*item13_1
  item13_1 ~~ var13_1*item13_1
  
  eta1_1_var*l1_1^2 + var1_1 == 1
  eta1_1_var*l2_1^2 + var2_1 == 1
  eta1_1_var*l3_1^2 + var3_1 == 1
  eta1_1_var*l4_1^2 + var4_1 == 1
  eta1_1_var*l5_1^2 + var5_1 == 1
  eta1_1_var*l6_1^2 + var6_1 == 1
  eta1_1_var*l7_1^2 + var7_1 == 1
  eta1_1_var*l8_1^2 + var8_1 == 1
  eta1_1_var*l9_1^2 + var9_1 == 1
  eta1_1_var*l10_1^2 + var10_1 == 1
  eta1_1_var*l11_1^2 + var11_1 == 1
  eta1_1_var*l12_1^2 + var12_1 == 1
  eta1_1_var*l13_1^2 + var13_1 == 1
'
