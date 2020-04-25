rm(list=ls())

# Define URL's
urlTests <- "https://epistat.sciensano.be/Data/COVID19BE_tests.csv"
urlMort <- "https://epistat.sciensano.be/Data/COVID19BE_MORT.csv"
urlHosp <- "https://epistat.sciensano.be/Data/COVID19BE_HOSP.csv"
urlMuniCumul <- "https://epistat.sciensano.be/Data/COVID19BE_CASES_MUNI_CUM.csv"
urlMuni <- "https://epistat.sciensano.be/Data/COVID19BE_CASES_MUNI.csv"
urlAgeSex <- "https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv"

# Import dependencies
library(tidyverse)

# Import URL's
dataTests <- as_tibble(read.csv(urlTests))
dataMort <- as_tibble(read.csv(urlMort))
dataHosp <- as_tibble(read.csv(urlHosp))
dataMuniCumul <- as_tibble(read.csv(urlMuniCumul))
dataMuni <- as_tibble(read.csv(urlMuni))
dataAgeSex <- as_tibble(read.csv(urlAgeSex))

# Get Excel that was provided on 31/03/2020 and filter February cases
urlAgeSexOld <- "https://epistat.sciensano.be/Data/20200331/COVID19BE_CASES_AGESEX_20200331.csv"
dataAgeSexOld <- as_tibble(read.csv(urlAgeSexOld)) %>%
  mutate(DATE = ymd(DATE)) %>%
  filter(DATE < dmy("01/03/2020"))

NrCasesFebruary <- sum(dataAgeSexOld$CASES)
cat("Found",NrCasesFebruary,"cases in COVID19BE_CASES_AGESEX_20200331 in February")
dataAgeSexOld %>% group_by(PROVINCE) %>% summarise(SUM = sum(CASES))

dataAgeSexOld$NR_NA <- rowSums(is.na(dataAgeSexOld))

# Combine the datasets
dataAgeSex$NR_NA <- rowSums(is.na(dataAgeSex))

dataAgeSex %>% 
  mutate(DATE = ymd(DATE)) %>%
  filter(is.na(DATE)) %>% 
  group_by(NR_NA) %>% 
  summarise(SUM = sum(CASES))

dataAgeSex %>%
  filter(is.na(DATE), !(NR_NA == 5), !(NR_NA == 2)) %>%
  group_by(PROVINCE) %>% summarise(SUM = sum(CASES))

NoBefore <- nrow(dataAgeSex)
dataAgeSex %>%
  filter(!is.na(DATE) | (is.na(DATE) & NR_NA == 2)) %>%
  mutate(DATE = ymd(DATE)) %>%
  union(dataAgeSexOld) %>%
  arrange(DATE) -> dataAgeSexCorrected
NoAfter <- nrow(dataAgeSexCorrected)
cat("Added", NoAfter - NoBefore, "cases")
rm(dataAgeSexOld)

# assume NA dates are on 1/3/2020
dataAgeSexCorrected %>% mutate(DATE = if_else(condition = is.na(DATE), true = dmy("01/03/2020"), false = DATE)) -> dataAgeSexCorrected

# import TESTS
as_tibble(read.csv2("D:/OneDrive/Documents/Corona/TESTS_FEBRUARY2.csv")) %>% mutate(DATE = dmy(DATE)) %>% select(DATE, TESTS) -> dataTestsOld
dataTests %>% mutate(DATE = ymd(DATE)) %>% union(dataTestsOld) %>% arrange(DATE) -> dataTestsCorrected
rm(dataTestsOld)
