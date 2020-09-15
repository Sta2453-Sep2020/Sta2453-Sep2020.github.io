
a <- 20
b <- 50

rate <- .5

hosp_a <- sum(rbinom(a, n = 365, p = rate))/(365*a)
hosp_b <- sum(rbinom(b, n = 365, p = rate))/(365*b)

n_sims <- 10000

hosp_a_sim <- c()
hosp_b_sim <- c()

for(i in 1:n_sims) {
  hosp_a_sim[i] <- sum(rbinom(a, n = 365, p = rate))/(365*a)
  hosp_b_sim[i] <- sum(rbinom(b, n = 365, p = rate))/(365*b)
}


ed <- readr::read_csv('lecture-notes-slides-2020/simulated_ed_data.csv')

ed <- ed %>% 
  select(-ctas_group,
         -mhesa)

ed <- ed %>% 
  mutate(los = as.numeric(difftime(ed_end_time, ed_start_time, units = 'hours')))

ed

ed_adm <- ed %>% 
  filter(admitted == 1)
ed_not_adm <- ed %>% 
  filter(admitted == 0)



low_probs <- c(.2, .001,
               .01, .00001, .0001,
               .1, .0001,
               .1, .0001,
               .05, .0001,
               .05, .0001, .00001,
               .019,
               .015,
               .2,
               .01,
               .05,
               .05,
               .001, .0001, .0001, .00001,
               .0001, .0001,
               .04,
               0.1,
               .00001,
               .001,
               .001,
               .001)
ed_not_adm <- ed_not_adm %>% 
  rowwise() %>% 
  mutate(presenting_complaint = sample(presenting_complaints, 1,p= low_probs, replace = T)) %>% 
  ungroup()

high_probs <- c(.05, .001,
               .18, .00001, .0001,
               .07, .0001,
               .01, .0001,
               .01, .0001,
               .01, .0001, .00001,
               .06,
               .05,
               .01,
               .01,
               .05,
               .05,
               .1, .0001, .0001, .00001,
               .22, .0001,
               .01,
               0.1, .00001,
               .001,
               .001,
               .001)

ed_adm <- ed_adm %>% 
  rowwise() %>% 
  mutate(presenting_complaint = sample(presenting_complaints, 1,p= high_probs, replace = T)) %>% 
  ungroup()

presenting_complaints <- c('Abdominal pain','Abdominal pain ',
                           'Chest pain',
                           'Chest  pain',
                           'Chest pian',
                           "Confusion",
                           " Confusion",
                           "Headache",
                           "Headach",
                           "Rash",
                           "rash",
                           "Back pain",
                           "Back  pain",
                           "Back pain ",
                           "Hallucinations",
                           "Bizarre behaviour",
                           "Sore Throat",
                           "Trouble Breathing",
                           "Lower extremity injury",
                           "Upper extremity injury",
                           "Burn",
                           " Burn ",
                           "Burns",
                           "burns",
                           "Traumatic injury",
                           "Traumatic injuries",
                           "General weakness",
                           "Loss of hearing",
                           "Lost hearing",
                           "Unknown",
                           "UNK",
                           "Missing"
                           )

ed <- ed_adm %>% 
  bind_rows(ed_not_adm)

ed <- ed %>% arrange(ed_start_time)

ed <- ed %>% 
  mutate(ENCOUNTER_NUM = 1:n())
ed

bad_encounters <- sample(1:nrow(ed), nrow(ed)*.02)

good <- ed %>% 
  filter(!ENCOUNTER_NUM %in% bad_encounters)
bad <- ed %>% 
  filter(ENCOUNTER_NUM %in% bad_encounters)

bad <- bad %>% 
  rowwise() %>% 
  mutate(ed_end_time = ed_start_time - dhours(sample(seq(1,15, .01), 1)))

ed <- good %>% 
  bind_rows(bad)

ed <- ed %>% 
  arrange(ENCOUNTER_NUM)

ed <- ed %>% 
  mutate(los = as.numeric(difftime(ed_end_time, ed_start_time, units = 'hours')))

summary(ed$los)

ed <- ed %>% 
  mutate(los = ifelse(los > 24, 24, los))

ed <- ed %>% 
  mutate(date = as_date(ed_start_time))


min(ed$date)
max(ed$date)

missing_dates <- c("2019-04-01", '2019-04-02',
                   "2019-04-03", "2019-04-04")

ed <- ed %>% 
  filter(!date %in% ymd(missing_dates))

extra_date <- c('2019-07-18')
good <- ed %>% 
  filter(date != ymd(extra_date))
bad <- ed %>% 
  filter(date == ymd(extra_date))

bad <- bad %>% 
  bind_rows(bad)

ed <- good %>% 
  bind_rows(bad)

ed <- ed %>% arrange(ENCOUNTER_NUM)


bad_encounters <- sample(1:nrow(ed), nrow(ed)*.02)

good <- ed %>% 
  filter(!ENCOUNTER_NUM %in% bad_encounters)
bad <- ed %>% 
  filter(ENCOUNTER_NUM %in% bad_encounters)


bad <- bad %>% 
  mutate(ed_pia_time = ymd_hms('2099-01-01 00:00:00'))

ed <- good %>% 
  bind_rows(bad) %>% 
  arrange(ENCOUNTER_NUM)
ed



bad_encounters <- sample(1:nrow(ed), nrow(ed)*.01)

good <- ed %>% 
  filter(!ENCOUNTER_NUM %in% bad_encounters)
bad <- ed %>% 
  filter(ENCOUNTER_NUM %in% bad_encounters)

bad <- bad %>% 
  mutate(ed_pia_time = ed_end_time + dhours(sample(1:5, nrow(bad), replace = T)))

ed <- good %>% 
  bind_rows(bad) %>% 
  arrange(ENCOUNTER_NUM)


ed %>% 
  filter(ed_pia_time > ed_end_time)

ed <- ed %>% 
  select(-date, -discharge)

ed
summary(ed)



bad_encounters <- sample(1:nrow(ed), nrow(ed)*.01)

good <- ed %>% 
  filter(!ENCOUNTER_NUM %in% bad_encounters)
bad <- ed %>% 
  filter(ENCOUNTER_NUM %in% bad_encounters)

bad <- bad %>% 
  mutate(ed_start_time = NA,
         los = NA)

ed <- good %>% 
  bind_rows(bad) %>% 
  arrange(ENCOUNTER_NUM)



bad_encounters <- sample(1:nrow(ed), nrow(ed)*.005)

good <- ed %>% 
  filter(!ENCOUNTER_NUM %in% bad_encounters)
bad <- ed %>% 
  filter(ENCOUNTER_NUM %in% bad_encounters)

bad <- bad %>% 
  mutate(ed_end_time = NA,
         los = NA)

ed <- good %>% 
  bind_rows(bad) %>% 
  arrange(ENCOUNTER_NUM)



readr::write_csv(ed, 'lecture-notes-slides-2020/raw_ed_data.csv')




```{r pressure, echo=FALSE}
leuk_dat <- read_csv("https://web.stanford.edu/~hastie/CASI_files/DATA/leukemia_big.csv")
leuk_dat %>% head()
g48 <- leuk_dat %>% 
  slice(48)
ALL <- g48 %>% 
  select(contains("ALL"))
AML <- g48 %>% 
  select(contains("AML"))

aml_mean <- mean(t(AML))
all_mean <- mean(t(ALL))
sd <- sd(t(g48))
se <- sd*sqrt((1/ncol(ALL)) + (1/ncol(AML)))




```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
