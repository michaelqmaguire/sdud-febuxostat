#-----------------------------------------------------------------------------------------------------------------#
#                                                                                                                 #
# PROJECT: SDUD FEBUXOSTAT		                                                                                    #
# AUTHOR: MICHAEL MAGUIRE, MS, DMA II                                                                             #
# INSTITUTION: UNIVERSITY OF FLORIDA, COLLEGE OF PHARMACY                                                         #
# DEPARTMENT: PHARMACEUTICAL OUTCOMES AND POLICY                                                                  #
# SUPERVISORS: AMIE GOODIN, PHD, MPP | JUAN HINCAPIE-CASTILLO, PHARMD, PHD, MS                                    #
# SCRIPT: 03_impute-suppressed-values.R                                                                    			  #
#                                                                                                                 #
#-----------------------------------------------------------------------------------------------------------------#

library(data.table)
library(dplyr)

baseImputed <-
  yrSt[
    i = ,
    j = `:=`
      (imputedRx = fcase(
        suppression == "T", 10,
        suppression == "F", numberrx
        )
    )
  ][, check := fifelse(((numberrx != 0 & imputedRx == 10) | (numberrx == 0 & imputedRx != 10)), "Problem", "Good")]

janitor::tabyl(baseImputed$check)

drugsAggStateImputed <-
  baseImputed[
    i  = ,
    j  = .(totalRXImputed = sum(imputedRx)),
    by = c("year", "state", "quarter")
  ]

setorder(drugsAggStateImputed, year, state, quarter)

drugsAggStateGenericImputed <-
  baseImputed[
    i  = ,
    j  = .(totalRXImputed = sum(numberrx)),
    by = c("year", "gennme", "state", "quarter")
  ]

setorder(drugsAggStateGenericImputed, year, state, quarter, gennme)

drugsAggStateProdnmeImputed <-
  baseImputed[
    i  = ,
    j  = .(totalRXImputed = sum(imputedRx)),
    by = c("year", "gennme", "prodnme", "state", "quarter")
  ]

setorder(drugsAggStateProdnmeImputed, year, state, quarter, gennme, prodnme)

readr::write_csv(x = drugsAggStateImputed, file = "./data/clean/06_imputed-febuxostat-allopurinol-rx-aggregate-by-state.csv", na = "")
readr::write_csv(x = drugsAggStateGenericImputed, file = "./data/clean/07_imputed-febuxostat-allopurinol-rx-aggregate-by-state-and-generic.csv", na = "")
readr::write_csv(x = drugsAggStateProdnmeImputed, file = "./data/clean/08_imputed-febuxostat-allopurinol-rx-aggregate-by-state-generic-and-brand.csv", na = "")
