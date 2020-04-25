# Plot percentage of tests that came up positive
dataAgeSexCorrected %>%
  group_by(DATE) %>%
  summarise(CASES = sum(CASES)) -> df

df %>% right_join(dataTestsCorrected, by = "DATE") %>% 
  arrange(DATE) -> df 

df %>% 
  mutate(CASES = ifelse(is.na(CASES), 0, CASES)) %>%
  mutate(TESTS = ifelse(is.na(TESTS), 0, TESTS)) %>%
  mutate(TESTS = if_else(condition = CASES > TESTS, true = CASES, false = TESTS)) %>%
  mutate(PERCENT_POS = CASES/TESTS) -> df

df %>% filter(DATE < dmy("12/03/2020")) %>% ggplot(mapping = aes(x = DATE, y = PERCENT_POS)) + geom_line()

df %>% filter(DATE < dmy("12/03/2020")) %>% ggplot(mapping = aes(x = DATE, y = TESTS)) + geom_line()

# Summary stats
dataAgeSexCorrected %>% group_by(AGEGROUP) %>% summarise(SUM = sum(CASES))


dataMort %>% mutate(DATE = ymd(DATE)) %>% group_by(AGEGROUP) %>% summarise(SUM = sum(DEATHS))
