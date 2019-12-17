rm(list = ls())
load("judicial.RData")
result = readRDS("s610f19_final_result.RDS")

# Brown v. Mississippi (#18501)
c1 = judicial$caseid[judicial$year == 1936 & 
                       judicial$caseid %in% grep("^Brown\\s", judicial$parties)]
# Escobedo v. Illinois (#23115)
c2 = judicial$caseid[judicial$year == 1964 & 
                       judicial$caseid %in% grep("^Escobedo\\s", judicial$parties)]
# Miranda v. Arizona (#23601)
c3 = judicial$caseid[judicial$year == 1966 & 
                       judicial$caseid %in% grep("^Miranda\\s", judicial$parties)]
# R.I. v. Innis (#26918)
c4 = judicial$caseid[judicial$year == 1980 & 
                       judicial$caseid %in% grep("Innis$", judicial$parties)]

fifth_amendment_cases = c(c1, c2, c3, c4)
judicial$parties[judicial$caseid %in% fifth_amendment_cases]

fifth = result %>% 
  filter(caseid %in% fifth_amendment_cases) %>%
  mutate(year = step + 1799)

fifth$auth[fifth$caseid == 18501 & fifth$year < 1936] = NA
fifth$auth[fifth$caseid == 23115 & fifth$year < 1964] = NA
fifth$auth[fifth$caseid == 23601 & fifth$year < 1966] = NA
fifth$auth[fifth$caseid == 26918 & fifth$year < 1980] = NA

ggplot(subset(fifth, year >= 1930), 
       aes(x = year, y = auth, color = as.factor(caseid))) + 
  geom_point() + geom_line()


# graph using the public data ---------------------------------------------

fifth2 = authmat %>% filter(caseid %in% fifth_amendment_cases)
fifth2_lg = pivot_longer(fifth2, 2:length(fifth2), names_to = "year", values_to = "auth")
fifth2_lg = fifth2_lg %>% 
  mutate(year = as.integer(gsub("X", "", fifth2_lg$year))) %>%
  filter(year >= 1930)

ggplot(fifth2_lg, aes(x = year, y = auth, color = as.factor(caseid))) + 
  geom_point() + geom_line()
