## TAENIA SOLIUM IN BURUNDI - DISABILITY ADJUSTED LIFE YEARS
### Last update:18/11/2021

###
### LOAD HELPER FUNCTIONS
###

source("residual-life-expectancy.R")
source("burden.R")

###
### SETTINGS
###

## set seed to allow for reproducibility
set.seed(264)

## number of iterations
n <- 1e5


###
### PARAMETER VALUES
###

## define population matrix
pop_mx <-
  matrix(c(893872, 1486390, 2459411, 470882, 218877,
           888554, 1514133, 2568012, 481213, 234234),
         ncol = 2)
## epilepsy prevalence, Beta parameters
e_prev_alpha <-42+43
e_prev_beta <-(4913+3218)-(43+42)


## proportion PWE with  based on Ag-ELISA, Uniform parameters 
prop_ncc_min <- 0.049
prop_ncc_max <- 0.383

## proportion treated, Beta parameters
ep_trt_alpha <- 59
ep_trt_beta <- 352-59

## epilepsy duration, Fixed
ep_dur <-
  matrix(c(1.4, 2.0, 3.6, 2.8, 1.6,
           1.6, 3.1, 5.9, 6.0, 2.8),
         ncol = 2)

## epilepsy onset, Fixed
ep_ons <- c(2.50, 9.95, 26.99, 51.94, 73.60)

## epilepsy treated disability weight, Uniform distribution 
ep_dsw_tr_min <- 0.211
ep_dsw_tr_max <- 0.445


## epilepsy non-treated disability weight, Uniform distribution 
ep_dsw_nt_min <- 0.279
ep_dsw_nt_max <- 0.572

## case fatality ratio, Beta distribution
ep_cfr_alpha <- 279
ep_cfr_beta <- 41575-279

## age at death, Fixed
ep_aad <- c(2.50, 10.0, 30.0, 53.50, 77.5)

## residual life expectancy, Fixed
ep_rle<- rle(ep_aad)


###
### SIMULATIONS
###

## epilepsy prevalence
e_prev <- rbeta(n, e_prev_alpha, e_prev_beta)

## proportion PWE with NCC
prop_ncc <- runif(n, prop_ncc_min, prop_ncc_max)

## prevalence NCC-associated epilepsy
ep_ncc <- e_prev * prop_ncc

## proportion treated
ep_trt <- rbeta(n, ep_trt_alpha, ep_trt_beta)

## epilepsy disability weights
ep_dsw_tr <- runif(n, ep_dsw_tr_min, ep_dsw_tr_max)
ep_dsw_nt <- runif(n, ep_dsw_nt_min, ep_dsw_nt_max)

## epilepsy case fatality ratio
ep_cfr <- rbeta(n, ep_cfr_alpha, ep_cfr_beta)


###
### CASES, DEATHS
###

## NCC-epilepsy incidence rate, age-sex specific [5*2 matrix]
ep_ncc_inc <- apply(ep_dur, 1:2, function(x) ep_ncc / x)

## NCC-epilepsy incident cases, age-sex specific [10 rows]
ep_ncc_N <- apply(ep_ncc_inc, 1, function(x) x * pop_mx)

## NCC-epilepsy incident cases - treated, age-sex specific [10 cols]
ep_ncc_tr_N <- apply(ep_ncc_N, 1, function(x) x * ep_trt)

## NCC-epilepsy incident cases - non-treated, age-sex specific [10 cols]
ep_ncc_nt_N <- apply(ep_ncc_N, 1, function(x) x * (1-ep_trt))


## NCC-epilepsy mortality rate, age-sex specific [5*2 matrix]
ep_ncc_mrt <- apply(ep_ncc_inc, 2:3, function(x) x * ep_cfr)

## NCC-epilepsy fatalities, age-sex specific [10 rows]
ep_ncc_M <- apply(ep_ncc_mrt, 1, function(x) x * pop_mx)


###
### DISABILITY ADJUSTED LIFE YEARS
###

## social weighting - select one of these
K <- 0; r <- 0     # DALY[0;0]
#K <- 0; r <- 0.03  # DALY[0;0.03]
#K <- 1; r <- 0     # DALY[1;0]
#K <- 1; r <- 0.03  # DALY[1;0.03]

## Years Lived with Disability, treated cases
yld_tr <- array(dim = c(5, 2, n))

yld_tr[1, 1, ] <-
  burden(N = ep_ncc_tr_N[, 1], DW = ep_dsw_tr,
         A = ep_ons[1], L = ep_dur[1, 1], K = K, r = r, a = ep_ons[1])
yld_tr[2, 1, ] <-
  burden(N = ep_ncc_tr_N[, 2], DW = ep_dsw_tr,
         A = ep_ons[2], L = ep_dur[2, 1], K = K, r = r, a = ep_ons[2])
yld_tr[3, 1, ] <-
  burden(N = ep_ncc_tr_N[, 3], DW = ep_dsw_tr,
         A = ep_ons[3], L = ep_dur[3, 1], K = K, r = r, a = ep_ons[3])
yld_tr[4, 1, ] <-
  burden(N = ep_ncc_tr_N[, 4], DW = ep_dsw_tr,
         A = ep_ons[4], L = ep_dur[4, 1], K = K, r = r, a = ep_ons[4])
yld_tr[5, 1, ] <-
  burden(N = ep_ncc_tr_N[, 5], DW = ep_dsw_tr,
         A = ep_ons[5], L = ep_dur[5, 1], K = K, r = r, a = ep_ons[5])

yld_tr[1, 2, ] <-
  burden(N = ep_ncc_tr_N[, 6], DW = ep_dsw_tr,
         A = ep_ons[1], L = ep_dur[1, 2], K = K, r = r, a = ep_ons[1])
yld_tr[2, 2, ] <-
  burden(N = ep_ncc_tr_N[, 7], DW = ep_dsw_tr,
         A = ep_ons[2], L = ep_dur[2, 2], K = K, r = r, a = ep_ons[2])
yld_tr[3, 2, ] <-
  burden(N = ep_ncc_tr_N[, 8], DW = ep_dsw_tr,
         A = ep_ons[3], L = ep_dur[3, 2], K = K, r = r, a = ep_ons[3])
yld_tr[4, 2, ] <-
  burden(N = ep_ncc_tr_N[, 9], DW = ep_dsw_tr,
         A = ep_ons[4], L = ep_dur[4, 2], K = K, r = r, a = ep_ons[4])
yld_tr[5, 2, ] <-
  burden(N = ep_ncc_tr_N[, 10], DW = ep_dsw_tr,
         A = ep_ons[5], L = ep_dur[5, 2], K = K, r = r, a = ep_ons[5])

yld_tr_all <- apply(yld_tr, 3, sum)

## Years Lived with Disability, non-treated cases
yld_nt <- array(dim = c(5, 2, n))

yld_nt[1, 1, ] <-
  burden(N = ep_ncc_nt_N[, 1], DW = ep_dsw_nt,
         A = ep_ons[1], L = ep_dur[1, 1], K = K, r = r, a = ep_ons[1])
yld_nt[2, 1, ] <-
  burden(N = ep_ncc_nt_N[, 2], DW = ep_dsw_nt,
         A = ep_ons[2], L = ep_dur[2, 1], K = K, r = r, a = ep_ons[2])
yld_nt[3, 1, ] <-
  burden(N = ep_ncc_nt_N[, 3], DW = ep_dsw_nt,
         A = ep_ons[3], L = ep_dur[3, 1], K = K, r = r, a = ep_ons[3])
yld_nt[4, 1, ] <-
  burden(N = ep_ncc_nt_N[, 4], DW = ep_dsw_nt,
         A = ep_ons[4], L = ep_dur[4, 1], K = K, r = r, a = ep_ons[4])
yld_nt[5, 1, ] <-
  burden(N = ep_ncc_nt_N[, 5], DW = ep_dsw_nt,
         A = ep_ons[5], L = ep_dur[5, 1], K = K, r = r, a = ep_ons[5])

yld_nt[1, 2, ] <-
  burden(N = ep_ncc_nt_N[, 6], DW = ep_dsw_nt,
         A = ep_ons[1], L = ep_dur[1, 2], K = K, r = r, a = ep_ons[1])
yld_nt[2, 2, ] <-
  burden(N = ep_ncc_nt_N[, 7], DW = ep_dsw_nt,
         A = ep_ons[2], L = ep_dur[2, 2], K = K, r = r, a = ep_ons[2])
yld_nt[3, 2, ] <-
  burden(N = ep_ncc_nt_N[, 8], DW = ep_dsw_nt,
         A = ep_ons[3], L = ep_dur[3, 2], K = K, r = r, a = ep_ons[3])
yld_nt[4, 2, ] <-
  burden(N = ep_ncc_nt_N[, 9], DW = ep_dsw_nt,
         A = ep_ons[4], L = ep_dur[4, 2], K = K, r = r, a = ep_ons[4])
yld_nt[5, 2, ] <-
  burden(N = ep_ncc_nt_N[, 10], DW = ep_dsw_nt,
         A = ep_ons[5], L = ep_dur[5, 2], K = K, r = r, a = ep_ons[5])

yld_nt_all <- apply(yld_nt, 3, sum)

## Years Lived with Disability, total
yld <- yld_tr + yld_nt
yld_all <- apply(yld, 3, sum)

## Years of Life Lost due to mortality
yll <- array(dim = c(5, 2, n))

yll[1, 1, ] <-
  burden(N = ep_ncc_M[1, ], DW = 1,
         A = ep_aad[1], L = ep_rle[1], K = K, r = r, a = ep_aad[1])
yll[2, 1, ] <-
  burden(N = ep_ncc_M[2, ], DW = 1,
         A = ep_aad[2], L = ep_rle[2], K = K, r = r, a = ep_aad[2])
yll[3, 1, ] <-
  burden(N = ep_ncc_M[3, ], DW = 1,
         A = ep_aad[3], L = ep_rle[3], K = K, r = r, a = ep_aad[3])
yll[4, 1, ] <-
  burden(N = ep_ncc_M[4, ], DW = 1,
         A = ep_aad[4], L = ep_rle[4], K = K, r = r, a = ep_aad[4])
yll[5, 1, ] <-
  burden(N = ep_ncc_M[5, ], DW = 1,
         A = ep_aad[5], L = ep_rle[5], K = K, r = r, a = ep_aad[5])

yll[1, 2, ] <-
  burden(N = ep_ncc_M[6, ], DW = 1,
         A = ep_aad[1], L = ep_rle[1], K = K, r = r, a = ep_aad[1])
yll[2, 2, ] <-
  burden(N = ep_ncc_M[7, ], DW = 1,
         A = ep_aad[2], L = ep_rle[2], K = K, r = r, a = ep_aad[2])
yll[3, 2, ] <-
  burden(N = ep_ncc_M[8, ], DW = 1,
         A = ep_aad[3], L = ep_rle[3], K = K, r = r, a = ep_aad[3])
yll[4, 2, ] <-
  burden(N = ep_ncc_M[9, ], DW = 1,
         A = ep_aad[4], L = ep_rle[4], K = K, r = r, a = ep_aad[4])
yll[5, 2, ] <-
  burden(N = ep_ncc_M[10, ], DW = 1,
         A = ep_aad[5], L = ep_rle[5], K = K, r = r, a = ep_aad[5])

yll_all <- apply(yll, 3, sum)

## Disability-Adjusted Life Years
daly <- yld + yll
daly_all <- apply(daly, 3, sum)

## zDALY calculation: ALE per 1000 persons year
ALE<-0.8017

###
### SUMMARIES
###

Summary <-
  function(x) {
    print(c(mean = mean(x),
            quantile(x, c(.025, .975))))
  }

## cases, deaths
Summary(colSums(ep_ncc_N))
Summary(colSums(ep_ncc_M))

## YLDs, YLLs, DALYs
Summary(yld_all)
Summary(yll_all)
Summary(daly_all)

## YLDs, YLLs, DALYs per 1000 population
Summary(1e3 * yld_all / sum(pop_mx))
Summary(1e3 * yll_all / sum(pop_mx))
Summary(1e3 * daly_all / sum(pop_mx))

## YLDs, YLLs, DALYs per incident case
Summary(yld_all / colSums(ep_ncc_N))
Summary(yll_all / colSums(ep_ncc_N))
Summary(daly_all / colSums(ep_ncc_N))

## YLD, YLL contribution
Summary(yll_all / daly_all)
Summary(yld_all / daly_all)

##contribution Incident cases,deaths and people without treatment in total population
Summary(colSums(ep_ncc_N)/sum(pop_mx))
Summary(colSums(ep_ncc_M)/sum(pop_mx))
Summary(rowSums(ep_ncc_tr_N))
Summary(rowSums(ep_ncc_nt_N))
Summary(rowSums(ep_ncc_nt_N)/sum(pop_mx))

##zDALY calculation per thousand person years
zDALY<-1e3 * daly_all / sum(pop_mx)+ALE
Summary(zDALY)
##zDALY calculation per 100 thousand person years
zDALY<-1e5 * daly_all / sum(pop_mx)+ALE*100
Summary(zDALY)

## contribution ALE and DALY per thousand person years in z-DALY
Summary(ALE/zDALY)
Summary((1e3 * daly_all / sum(pop_mx))/zDALY)



