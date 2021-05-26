#-----------------------------------------------------------------------------------------------------------------#
#                                                                                                                 #
# PROJECT: SDUD FEBUXOSTAT		                                                                                    #
# AUTHOR: MICHAEL MAGUIRE, MS, DMA II                                                                             #
# INSTITUTION: UNIVERSITY OF FLORIDA, COLLEGE OF PHARMACY                                                         #
# DEPARTMENT: PHARMACEUTICAL OUTCOMES AND POLICY                                                                  #
# SUPERVISORS: AMIE GOODIN, PHD, MPP | JUAN HINCAPIE-CASTILLO, PHARMD, PHD, MS                                    #
# SCRIPT: 02_import-and-clean.R                                                                            			  #
#                                                                                                                 #
#-----------------------------------------------------------------------------------------------------------------#

library(data.table)
library(dplyr)

## Read in the dataset. 
## Coerce NDC to character. Otherwise it is read as integer.

base <-
  fread(
    file_location,
    colClasses = c("proper_ndc" = "character")
  )

base

rxSeq <- c(
  1126,1399,1413,2474,2896,6557,6887,18980,20697,21674,23627,27085,46460,46504,1423,1885,1887,1981,2482,3657,3744,
  4069,4253,4330,4572,4587,5088,5312,5321,5410,5441,5452,5932,6555,6572,6898,6978,7086,7142,7581,8383,9169,9739,10459,
  10460,11264,12562,12819,14423,15203,15947,16251,16323,16464,18353,18769,19226,19686,21840,22841,23935,24731,26276,
  26534,26830,27015,27065,28246,28436,31725,32536,33051,36965,37598,38178,39238,39781,40060,40997,43723,46067,46209,
  46994,47588,48121,49053,49206,50918,51650,51997,52423,52490,52780,54027,54375,54391,54817,56091,56859,57219,57491,
  58538,59264,59485,59684,60750,60789,61226,61228,61925,62253,62345,64940,65899,68986,69389,69573,70314,71172,71974,
  72213,72688,73268,73771,74070,74631,75231,76196,76932,77088,78493
)

yrSt <-
  base[
    i = year %in% c(2017:2020) & seqidndc %in% rxSeq,
    j = .(year, proper_ndc, quarter, state, suppression, numberrx, prodnme, gennme)
  ]

yrSt %>%
  distinct(year, quarter) %>%
  arrange(year, desc(quarter))

drugsAggState <-
  yrSt[
  i  = ,
  j  = .(totalRX = sum(numberrx)),
  by = c("year", "state", "quarter", "suppression")
]

setorder(drugsAggState, year, state, quarter, suppression)

drugsAggStateGeneric <-
  yrSt[
    i  = ,
    j  = .(totalRX = sum(numberrx)),
    by = c("year", "gennme", "state", "quarter", "suppression")
  ]

setorder(drugsAggStateGeneric, year, state, quarter, gennme, suppression)

drugsAggStateProdnme <-
  yrSt[
    i  = ,
    j  = .(totalRX = sum(numberrx)),
    by = c("year", "gennme", "prodnme", "state", "quarter", "suppression")
  ]

setorder(drugsAggStateProdnme, year, state, quarter, gennme, prodnme, suppression)

drugsAggStateProdnme

rxIncluded <-
  unique(yrSt[, c("gennme", "prodnme")], by = c("gennme", "prodnme"))

rxIncluded

readr::write_csv(x = drugsAggState, file = "./data/clean/01_febuxostat-allopurinol-rx-aggregate-by-state.csv", na = "")
readr::write_csv(x = drugsAggStateGeneric, file = "./data/clean/02_febuxostat-allopurinol-rx-aggregate-by-state-and-generic.csv", na = "")
readr::write_csv(x = drugsAggStateProdnme, file = "./data/clean/03_febuxostat-allopurinol-rx-aggregate-by-state-generic-and-brand.csv", na = "")
readr::write_csv(x = drugsAggStateProdnme, file = "./data/clean/04_generic-brand-names-in-dataset.csv", na = "")
