# Set working directory
setwd('~/Documents/OneDrive - Elektroforeningen (EFO)/R/Scripts/ny-stat-modul')

# Load libraries
library(tidyverse)
library(lubridate)

# Read files


# Split files by source
last_l  <- last %>% filter(Statistikk == 'PI' | Statistikk == 'Leverandørstatistikk')
last_gk <- last %>% filter(Statistikk == 'G')
last_gm <- last %>% filter(Statistikk == 'GR')

all_l  <- all %>% filter(Statistikk == 'PI' | Statistikk == 'Leverandørstatistikk')
all_gk <- all %>% filter(Statistikk == 'G')
all_gm <- all %>% filter(Statistikk == 'GR')

# Delete unnecessary columns
all_l[,c('Statistikk', 'Periode.navn', 'Antall', 'Opprettet.dato', 'Opprettet.av', 'Sist.endret.dato', 'Sist.endret.av')] <- NULL

# Convert column names
colnames(all_l) <- c('year', 'quarter', 'member', 'segment', 'value')
all_l$segment <- as.character(all_l$segment)

# Set date formats to the first day of the last month in the quarter
Sys.setenv(TZ='Europe/Oslo')

all_l_d <- all_l %>% 
  select(year, quarter) %>% 
  mutate(quarter_date = make_date(year, month = quarter*3, day = 1))

all_l$date <- all_l_d$quarter_date
rm(all_l_d)

# Add a month and dynamically subtract one day to get to the last day of the quarter
all_l$date <- all_l$date %m+% months(1) %m-% days(1)
#table(all_l$date)

# Remove unnecessary integers as substitutes for dates
all_l[,c('year', 'quarter')] <- NULL

# Create subset of data from the latest quarter and add a column for the mean value of the last year
# and a column for flagging suspicious data
last_quarter <- subset(all_l, date %in% max(date))

# Create subset of data from the latest year, not including the latest quarter
last_year <- subset(all_l, date > max(date)-366 & date < max(date))

last_year_mean <- last_year %>%
                    group_by(member, segment) %>% 
                    summarize(v.mean = mean(value, na.rm = TRUE))

last_quarter <- left_join(last_quarter, last_year_mean, by = c('member', 'segment'))

# Replace "NaN"-values from v.mean with 0's
last_quarter[is.na(last_quarter$v.mean), 'v.mean'] <- 0

# Create multipliers and value limits to check unusually high changes in value by company, segment and quarter
pos_mult <- 4
neg_mult <- 4
pos_lim  <- 1000
neg_lim  <- 1000

last_quarter$eval <- 0
last_quarter$eval <- ifelse((last_quarter$value > last_quarter$v.mean * pos_mult) & (last_quarter$value - last_quarter$v.mean > pos_lim), 1, 0)
last_quarter$eval <- ifelse((last_quarter$value < last_quarter$v.mean / neg_mult) & (last_quarter$v.mean - last_quarter$value > neg_lim), -1, last_quarter$eval)


# Create total turnover per quarter per company for the last five quarters, not including last quarter
last_year_c <- all_l %>%
                    group_by(member, date) %>% 
                    summarize(value = sum(value, na.rm = TRUE)) %>% 
                    subset(date > max(date)-366 & date < max(date))

last_year_c_mean <- last_year_c %>%
                      group_by(member) %>% 
                      summarize(v.mean = round(mean(value), digits = 0))

# Create total turnover for last quarter per company
last_quarter_c <- all_l %>%
                    group_by(member, date) %>% 
                    summarize(value = sum(value, na.rm = TRUE)) %>% 
                    subset(date %in% max(date))

last_quarter_c <- full_join(last_quarter_c, last_year_c_mean, by = 'member')

# Replace "NaN"-values from v.mean with 0's
last_quarter_c[is.na(last_quarter_c$v.mean), 'v.mean'] <- 0

# Create multipliers and value limits to check unusually high changes in value by company and quarter
pos_mult_c <- 1.3
neg_mult_c <- 1.3
pos_lim_c  <- 5000
neg_lim_c  <- 5000

last_quarter_c$eval <- 0
last_quarter_c$eval <- ifelse((last_quarter_c$value > last_quarter_c$v.mean * pos_mult_c) & (last_quarter_c$value - last_quarter_c$v.mean > pos_lim_c), 1, 0)
last_quarter_c$eval <- ifelse((last_quarter_c$value < last_quarter_c$v.mean / neg_mult_c) & (last_quarter_c$v.mean - last_quarter_c$value > neg_lim_c), -1, last_quarter_c$eval)


# vei.q <- all_l %>% filter(Periode.år %in% c(2017, 2018), Segment == 330502) %>% group_by(Periode.år, Periode.kvartal) %>%
#                     summarize(q.sum = sum(Verdi, na.rm = T))
# vei.l <- all_l %>% filter(Periode.år %in% c(2017, 2018), Periode.kvartal == 3, Segment == 330502) %>% group_by(Periode.år, Periode.kvartal, Innmelder) %>%
#                     summarize(q.sum = sum(Verdi, na.rm = T))
# 
# dlight <- all_l %>% filter(Periode.år %in% c(2017, 2018), Periode.kvartal == 3, Segment == 330406) %>% group_by(Periode.år, Periode.kvartal, Innmelder) %>%
#                      summarize(q.sum = sum(Verdi, na.rm = T)) %>% spread(Periode.år, q.sum)
# dlight_sum <- sum(dlight[c(1,3:10,12,14),4])
# dlight_sum17 <- sum(dlight[c(1,3:10,12,14),3], na.rm=T)
# 
# tunell <- all_l %>% filter(Periode.år %in% c(2017, 2018), Periode.kvartal == 3, Segment == 330501) %>% group_by(Periode.år, Periode.kvartal, Innmelder) %>%
#                      summarize(q.sum = sum(Verdi, na.rm = T)) %>% spread(Periode.år, q.sum)
# 
# roadlight <- all_l %>% filter(Periode.år %in% c(2017, 2018), Periode.kvartal == 3, Segment == 330502) %>% group_by(Periode.år, Periode.kvartal, Innmelder) %>%
#                         summarize(q.sum = sum(Verdi, na.rm = T)) %>% spread(Periode.år, q.sum)
# roadlight_sum <- sum(roadlight[,4])
# 
# IP55 <- all_l %>% filter(Periode.år %in% c(2017, 2018), Periode.kvartal == 3, Segment == 330203) %>% group_by(Periode.år, Periode.kvartal, Innmelder) %>%
#                   summarize(q.sum = sum(Verdi, na.rm = T)) %>% spread(Periode.år, q.sum)
# IP55_sum <- sum(IP55[,4], na.rm = T)
# 
# int_ut <- all_l %>% filter(Periode.år %in% c(2017, 2018), Periode.kvartal == 3, Segment == 330105) %>% group_by(Periode.år, Periode.kvartal, Innmelder) %>%
#   summarize(q.sum = sum(Verdi, na.rm = T)) %>% spread(Periode.år, q.sum)
# int_ut_sum <- sum(int_ut[,4])
# int_ut_alle_sum <- sum(int_ut[c(1,4:10,12:13,16),4])
# 
# LED_s <- all_l %>% filter(Periode.år %in% c(2017, 2018), Periode.kvartal == 3, Segment == 330109) %>% group_by(Periode.år, Periode.kvartal, Innmelder) %>%
#   summarize(q.sum = sum(Verdi, na.rm = T)) %>% spread(Periode.år, q.sum)
# 
# DALI <- all_l %>% filter(Periode.år %in% c(2017, 2018), Periode.kvartal == 3, Segment == 370104) %>% group_by(Periode.år, Periode.kvartal, Innmelder) %>%
#   summarize(q.sum = sum(Verdi, na.rm = T)) %>% spread(Periode.år, q.sum)
# 
# mark <- all_l %>% filter(year(all_l$date) == 2018, quarter(all_l$date) == 4, segment == 330517)
#
# lysl <- all_l %>% filter(Periode.år %in% c(2017, 2018), Periode.kvartal == 3, Segment == 330101) %>% group_by(Periode.år, Periode.kvartal, Innmelder) %>%
#   summarize(q.sum = sum(Verdi, na.rm = T)) %>% spread(Periode.år, q.sum)
# lysl_sum <- sum(lysl[-3,4], na.rm=T)
# 
# gulvl <- all_l %>% filter(Periode.år %in% c(2017, 2018), Periode.kvartal == 3, Segment == 330117) %>% group_by(Periode.år, Periode.kvartal, Innmelder) %>%
#   summarize(q.sum = sum(Verdi, na.rm = T)) %>% spread(Periode.år, q.sum)
# gulvl_sum <- sum(gulvl[,4], na.rm=T)
#
# gulvovn <- all_l %>% filter(Periode.år %in% c(2017, 2018), Segment %in% c(500100, 500101, 500102, 500103, 500104, 500105)) %>% group_by(Periode.år, Periode.kvartal) %>%
#                       summarize(q.sum = sum(Verdi, na.rm = T))
# 
# veggovn <- all_l %>% filter(Periode.år %in% c(2017, 2018), Segment %in% c(500200, 500201, 500202, 500203, 500204)) %>% group_by(Periode.år, Periode.kvartal) %>%
#                       summarize(q.sum = sum(Verdi, na.rm = T))
# 
# utdatert_takl <- all_l %>% filter(Periode.år %in% c(2017, 2018), Segment == 330112)
# korrekt_takl  <- all_l %>% filter(Periode.år %in% c(2017, 2018), Segment == 330103)
# 
# kabelgr <- c('Elis Elektro AS', 'Ensto Nor AS', 'HF Danyko AS', 'LAPP Norway AS', 'Nexans Norway AS', 'NKT AS', 'Prysmian Group Norge AS', 'REKA Kabel AS', 'Tec Con Norge AS')
# 
# # Count number of unique companies within the cable group for each level 2 category
# k01 <- all_l %>% filter(year(all_l$date) == 2018, grepl("^0101", all_l$segment) == TRUE, member %in% kabelgr) %>% group_by(date) %>% summarize(nr = n_distinct(member))
# 
# dlight <- all_l %>% filter(Periode.år %in% c(2017, 2018), Periode.kvartal == 3, Segment == 330406) %>% group_by(Periode.år, Periode.kvartal, Innmelder) %>%
#                     summarize(q.sum = sum(Verdi, na.rm = T)) %>% spread(Periode.år, q.sum)
#
# v2n <- all_l %>% filter(year(all_l$date) == 2018, segment %in% c(500100, 500200, 500300, 500400, 500500, 500600, 500700, 500800, 500900, 501000,
#                    509600, 509800, 509900))
#
# garo <- all_l %>% filter(year(all_l$date) == 2018, member == 'Garo AS') %>%  group_by(member, segment, date) %>% summarize(q.sum = sum(value)) %>%
#   spread(date, q.sum)
# 
# lfs_last <- last %>% filter(Innmelder %in% c('Defa Lighting', 'Fagerhult Belysning AS', 'Glamox AS, BU Glamox Luxo Lighting Norge', 'IPAS AS', 'Ledvance AS',
#                                              'LUMINATOR AS', 'Osram AS', 'Sebra Light AS', 'Signify Norge', 'SML Lighting AS', 'ZG Lighting Norway AS')) %>%
#   filter(Segment %in% c(330101, 330102, 330103, 330104, 330105, 330106, 330107, 330108, 330109, 330113, 330117, 330118, 330201, 330202, 330203,
#                         330204, 330406, 330412, 330416, 330501, 330502, 330503, 330504, 330505, 330511, 330512, 330517, 370102, 370104)) %>%
#   summarize(sum=sum(Verdi, na.rm = TRUE))
# 
# cs_last <- last %>% filter(Innmelder %in% c('Elis Elektro AS', 'Ensto Nor AS', 'HF Danyko AS', 'LAPP Norway AS', 'Nexans Norway AS',
#                                             'NKT AS', 'Prysmian Group Norge AS', 'REKA Kabel AS', 'Tec Con Norge AS')) %>%
#                     filter(grepl("^01", Segment) == TRUE) %>% 
#                     summarize(sum=sum(Verdi, na.rm = TRUE))
#
# ssr <- last_l %>% filter(grepl("^1503", Segment) == TRUE)
#
# osram <- all_l %>% filter(member == 'Osram AS') %>%
#                    group_by(date) %>%
#                    summarize(sum=sum(value, na.rm = TRUE))
# 
# nvent <- all_l %>% filter(member == 'nVent Thermal Norway AS') %>%  group_by(member, segment, date) %>%  summarize(q.sum = sum(value)) %>% spread(date, q.sum)
# write.csv2(nvent, file = 'nVent.csv')
# 
# Varme_n2 <- all_l %>%
#                   filter(date > max(date)-366 & date < max(date) , grepl('^50', all_l$segment) == TRUE, grepl('00$', all_l$segment) == TRUE) %>% 
#                   group_by(member, segment, date) %>% 
#                   summarize(q.sum = sum(value, na.rm = TRUE)) %>% 
#                   spread(date, q.sum)
# 
# ipas <- all_l %>% filter(member == 'IPAS AS', date > max(date)-366) %>%  group_by(member, segment, date) %>%  summarize(q.sum = sum(value)) %>% spread(date, q.sum)
# colSums(ipas[,-c(1:2)], na.rm = TRUE)
# 
# nelfo <- all_l %>% 
#               mutate(segment_n2 = substring(segment, 1, 4), aar = year(all_l$date)) %>% 
#               filter(year(all_l$date) %in% c(2016, 2017, 2018), !(grepl('0000$', all_l$segment))) %>% 
#               group_by(segment_n2, aar) %>% 
#               summarize(gr_sum = sum(value, na.rm = TRUE)) %>% 
#               spread(aar, gr_sum)
#               
# colSums(nelfo[, -1], na.rm = TRUE)
# write.csv(nelfo, file = 'oppsummering_til_nelfo.csv', row.names = FALSE)

kabel <- all_l %>% filter(grepl('^01', all_l$segment) == TRUE, date > max(date)-366) %>%  group_by(member, segment, date) %>%  summarize(q.sum = sum(value)) %>% spread(date, q.sum)
colSums(kabel[,-c(1:2)], na.rm = TRUE)

kabel2 <- all_l %>% 
                filter(date %in% c(as.Date('2018-09-30'), as.Date('2019-09-30'))) %>% 
                mutate(segment_n2 = substring(segment, 1, 4)) %>% 
                group_by(segment_n2, date) %>% 
                summarize(gr_sum = sum(value, na.rm = TRUE)) %>% 
                spread(date, gr_sum) %>% 
                filter(grepl('^01', segment_n2) == TRUE) %>% 
                mutate(diff = `2019-09-30` - `2018-09-30`, prosent = diff/`2018-09-30`)

kabel2$prosent <- format(kabel2$prosent, digits=1)

kabel3 <- all_l %>%
                filter(date %in% c(as.Date('2018-09-30'), as.Date('2019-09-30'))) %>% 
                mutate(segment_n2 = substring(segment, 1, 4)) %>% 
                group_by(segment_n2, date, member) %>% 
                summarize(gr_sum = sum(value, na.rm = TRUE)) %>% 
                spread(date, gr_sum) %>% 
                filter(grepl('^01', segment_n2) == TRUE)          
kabel3[is.na(kabel3)] <- 0
kabel3 <- kabel3 %>% mutate(diff = `2019-09-30` - `2018-09-30`, prosent = diff/`2018-09-30`)
kabel3$prosent <- as.numeric(format(kabel3$prosent, digits=1))
colSums(kabel3[,c(3:4)], na.rm = TRUE)


analyse <- function(member_name){
  result <- filter(all_l, member == member_name, date > max(date)-366) %>% group_by(member, segment, date) %>% summarize(q.sum = sum(value)) %>% spread(date, q.sum) %>% print(n=Inf)
}

analyse('Prysmian Group Norge AS')

# Create a list of members who reported last quarter
l_last_q <- as.character(sort(unique(last_quarter$member)))   # Shows all levels (which includes G)

# Comparison with check-list
l_comp <- read.csv('innmeldere.csv')

# Ignore warnings for where there are no deviating values
oldw <- getOption('warn')
options(warn = -1)

  filter(last_quarter, eval == 1)
  filter(last_quarter, eval == -1)

  filter(last_quarter_c, eval == 1)
  filter(last_quarter_c, eval == -1)

options(warn = oldw)

# New members who have reported since last update
setdiff(l_last_q, l_comp[[1]])
write.csv(l_last_q, file = 'innmeldere.csv', row.names = FALSE)



