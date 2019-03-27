# Set working directory
setwd("~/Documents/R/_rydding råfil")

# Load libraries
library(data.table)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)

# Read file
df <- fread(input = 'Statistikk resultat.csv')

# Drop unneccesary columns
df <- df[,c(-2, -9:-12)]

# Rename columns
colnames(df)[1] <- 'stat'
colnames(df)[2] <- 'aar'
colnames(df)[3] <- 'kvartal'
colnames(df)[4] <- 'medlem'
colnames(df)[5] <- 'segment'
colnames(df)[6] <- 'verdi'
colnames(df)[7] <- 'antall'

# Divide data set into separate groups
g  <- df[df$stat=='G', ]
gr <- df[df$stat=='GR',]
pi <- df[df$stat=='PI',]
rm(df)

# Drop superflous factor-levels
g$segment  <- factor(g$segment)
gr$segment <- factor(gr$segment)
pi$segment <- factor(pi$segment)

g$medlem   <- factor(g$medlem)
gr$medlem  <- factor(gr$medlem)
pi$medlem  <- factor(pi$medlem)

# Clean-up old wholesaler-labels
g$segment  <- as.character(g$segment)

g[g$segment=='Bonuskabel', 'segment']                 <- 'Kabel og ledning A-F'
g[g$segment=='Svakstrøm/tele/data', 'segment']        <- 'Kabel og ledning A-F'
g[g$segment=='Varmekabel uten støpemasse', 'segment'] <- 'Kabel og ledning A-F'
g[g$segment=='Kabel 1 kV (EL) PEX', 'segment']        <- 'Kabel og ledning A-F'
g[g$segment=='Kabel 12 kV + 24 kV', 'segment']        <- 'Kabel og ledning A-F'
g[g$segment=='Skips-/Off-shore-kabel', 'segment']     <- 'Kabel og ledning A-F'
g[g$segment=='Kabel og ledning  A - F', 'segment']    <- 'Kabel og ledning A-F'

g[g$segment=='Kabelmuffer og kabelfordelingsskap', 'segment']                 <- 'Installasjonsmateriell'
g[g$segment=='Rør, tak- og veggbokser, koblingsmateriell og kana', 'segment'] <- 'Installasjonsmateriell'
g[g$segment=='Festemateriell, kabelstiger og kabelrenner', 'segment']         <- 'Installasjonsmateriell'
g[g$segment=='Brytere', 'segment']                                            <- 'Installasjonsmateriell'
g[g$segment=='Stikkontakter, støpsler m.m.', 'segment']                       <- 'Installasjonsmateriell'
g[g$segment=='Sikringsmateriell', 'segment']                                  <- 'Installasjonsmateriell'
g[g$segment=='Sikringsbokser -skap og sikringsstativer', 'segment']           <- 'Installasjonsmateriell'
g[g$segment=='Isolasjons- og loddemateriell, merkingsutstyr og k', 'segment'] <- 'Installasjonsmateriell'
g[g$segment=='Kontaktpressingsmateriell', 'segment']                          <- 'Installasjonsmateriell'
g[g$segment=='Plateskap med tilbehør i stål og aluminium', 'segment']         <- 'Installasjonsmateriell'
g[g$segment=='Skap med tilbehør i silumin, støpejern og isolerst', 'segment'] <- 'Installasjonsmateriell'

g[g$segment=='Blank line', 'segment']               <- 'Linjemateriell A-B'
g[g$segment=='Øvrig linjematriell', 'segment']      <- 'Linjemateriell A-B'
g[g$segment=='A-BLinjemateriell  A - B', 'segment'] <- 'Linjemateriell A-B'
          
g[g$segment=='Matriell for innendørs montasje, 12kV og høyere',
             'segment'] <- 'Materiell for innendørs montasje, 12kV og høyere'
g[g$segment=='Materiell for innendørs montasje, 12 kV og høyere',
             'segment'] <- 'Materiell for innendørs montasje, 12kV og høyere'
g[g$segment=='Matriell for innendørsmontasje, 12kV og høyere spe',
             'segment'] <- 'Materiell for innendørs montasje, 12kV og høyere'
g[g$segment=='Matriell for innendørsmontasje, 12kV og høyere spenninger',
             'segment'] <- 'Materiell for innendørs montasje, 12kV og høyere'

g[g$segment=='Glødelampearmaturer, innendørs', 'segment']             <- 'Lysutstyr'
g[g$segment=='Glødelampearmaturer, utendørs', 'segment']              <- 'Lysutstyr'
g[g$segment=='Lyskastere og effektbelysning', 'segment']              <- 'Lysutstyr'
g[g$segment=='Lysarmaturer og tilbehør', 'segment']                   <- 'Lysutstyr'
g[g$segment=='Armaturer for natrium- og metalldamplamper', 'segment'] <- 'Lysutstyr'
g[g$segment=='Stålrørmaster og stolpearmer.', 'segment']              <- 'Lysutstyr'
g[g$segment=='Gløde- og halogenlamper', 'segment']                    <- 'Lysutstyr'
g[g$segment=='Lysrør og damplamper', 'segment']                       <- 'Lysutstyr'

g[g$segment=='Spesialpakket (blisterpakket) elmateriell', 'segment']  <- 'Spesialpakket (blisterpakket) el-materiell'
g[g$segment=='Spesialpakket (blister pakket) elmateriell', 'segment'] <- 'Spesialpakket (blisterpakket) el-materiell'

g[g$segment=='Signalutstyr og alarmsystemer', 'segment']                   <- 'Svakstrømsmateriell'
g[g$segment=='Signaldistribusjonsanlegg', 'segment']                       <- 'Svakstrømsmateriell'
g[g$segment=='Kommunikasjonsapparater', 'segment']                         <- 'Svakstrømsmateriell'
g[g$segment=='Antennemateriell', 'segment']                                <- 'Svakstrømsmateriell'
g[g$segment=='Strømforsyning, nødlysanlegg og transformatorer', 'segment'] <- 'Svakstrømsmateriell'
g[g$segment=='Styresystemer', 'segment']                                   <- 'Svakstrømsmateriell'
g[g$segment=='Koblingsmateriell for tele og data', 'segment']              <- 'Svakstrømsmateriell'

g[g$segment=='Elektriske husholdningsapparater', 'segment']                               <- 'Annet'
g[g$segment=='Elektriske kokeapparater', 'segment']                                       <- 'Annet'
g[g$segment=='Elektroniske styringssystemer  (PLS) etc.', 'segment']                      <- 'Annet'
g[g$segment=='Instrumenter', 'segment']                                                   <- 'Annet'
g[g$segment=='Isolasjons- og loddemateriell, merkingsutstyr og krympeslanger', 'segment'] <- 'Annet'
g[g$segment=='Kjøle- og fryseskap og matbodekjølere med tilbehør', 'segment']             <- 'Annet'
g[g$segment=='Kondensatorer', 'segment']                                                  <- 'Annet'
g[g$segment=='Målere', 'segment']                                                         <- 'Annet'
g[g$segment=='Målere og måleromkoblere', 'segment']                                       <- 'Annet'
g[g$segment=='Motorer', 'segment']                                                        <- 'Annet'
g[g$segment=='Rør, tak- og veggbokser, koblingsmateriell og kanalsystemer', 'segment']    <- 'Annet'
g[g$segment=='Skap med tilbehør i silumin, støpejern og isolerstoff', 'segment']          <- 'Annet'
g[g$segment=='Startapparater', 'segment']                                                 <- 'Annet'
g[g$segment=='Styreorganer, motorvernbrytere og effektbrytere', 'segment']                <- 'Annet'
g[g$segment=='Vaske- og oppvaskmaskiner', 'segment']                                      <- 'Annet'
g[g$segment=='Verktøy', 'segment']                                                        <- 'Annet'
g[g$segment=='Vifter, varmevifter og pumper', 'segment']                                  <- 'Annet'

# Aggregating to the ten groups, and dropping 'antall' in the process
g <- aggregate(g[,6], by = g[,1:5], FUN = sum)

# Re-factoring the groups to remove unused factor-levles
g$segment <- as.factor(g$segment)

# Separating turnover from months
gm <- g[g$segment == 'Total omsetning',]
gk <- g[g$segment != 'Total omsetning',]
rm(g)

# Removing the segmen for months
gm$segment <- NULL

# Adding missing values from 2015 from external CSV-file for months and quarters respectively and
# set the correct statistics label for all entries
gm <- rbind(gm, read.csv2(file = 'G 2015 1-3.csv'))
gk <- rbind(gk, read.csv2(file = 'G 2015 1-3 kvartal.csv'))
gk$stat <- 'GK'

# Summing up quarterly turnover with the temporary data frame gq
gq <- aggregate(gk[,6], by = gk[,1:4], FUN = sum)
colnames(gq)[5] <- 'verdi'
gq$kvartal <- gq$kvartal * 3

# Combining the temporary gq data frame and the monthly data. Removing data before 2015
# as there is no monthly data from before 2015 in the data set (only in old Excel files)
gq <- data.table(rbind(gm, gq))
gq <- gq[aar >= 2015]

# Lining up monthly and quarterly data, then subtracting the two previous months from the
# quarterly data to calculate the monthly data for the 3rd, 6th, 9th and 12th months.
gq <- arrange(gq, medlem, aar, kvartal)
gm <- gq %>%
         mutate(verdi = ifelse(as.numeric(kvartal) %% 3 == 0, 
                               verdi - lag(verdi,1) - lag(verdi, 2),
                               verdi))
rm(gq)
colnames(gm)[3] <- 'mnd'


# Month: Set date formats to the first day of the month
gm <- gm %>%
  mutate(dato = make_date(aar, month = mnd, day = 1))

# Add a month and dynamically subtract one day to get to the last day of the month
gm$dato <- gm$dato %m+% months(1) %m-% days(1)
# table(gm$dato)

# Remove unnecessary integers as substitutes for dates, put date as the second column and
# set the correct statistics label for all entries
gm$aar <- NULL
gm$mnd <- NULL
gm <- gm[,c(1,4,2,3)]
gm$stat <- 'GM'


# Quarter: Set date formats to the first day of the last month in the quarter
gk <- gk %>%
  mutate(dato = make_date(aar, month = kvartal * 3, day = 1))

# Add a month and dynamically subtract one day to get to the last day of the quarter
gk$dato <- gk$dato %m+% months(1) %m-% days(1)
# table(gm$dato)

# Remove unnecessary integers as substitutes for dates, put date as the second column and
# set the correct statistics label for all entries
gk$aar <- NULL
gk$kvartal <- NULL
gk <- gk[,c(1,5,2,3,4)]
gk$stat <- 'GK'
gk <- arrange(gk, medlem, dato)


# Region: Set date formats to the first day of the last month in the quarter
gr <- gr %>%
  mutate(dato = make_date(aar, month = kvartal * 3, day = 1))

# Add a month and dynamically subtract one day to get to the last day of the quarter
gr$dato <- gr$dato %m+% months(1) %m-% days(1)
# table(gm$dato)

# Remove unnecessary integers as substitutes for dates, put date as the second column and
# set the correct statistics label for all entries
gr$aar <- NULL
gr$kvartal <- NULL
gr <- gr[,c(1,6,2,3,4)]
gr$stat <- 'GR'
gr <- arrange(gr, medlem, dato)

# Export files
write.csv(pi, file = 'pi.csv')
write.csv(gk, file = 'gk.csv')
write.csv(gm, file = 'gm.csv')
write.csv(gr, file = 'gr.csv')


# Finding potential new members for the statistics
liste.vk <- c(500501,500502,500503,500801,500802)
search.vk <- pi[pi$segment %in% liste.vk & pi$aar == 2017 & pi$kvartal == 4]

liste.vk.agg <- c(500000,500500,500800)
search.vk.agg <- pi[pi$segment %in% liste.vk.agg & pi$aar == 2017 & pi$kvartal == 4]

ggplot(search.vk, aes(x = segment)) +
  geom_bar(stat = 'count') +
  scale_y_continuous(breaks = seq(0, 13, 1)) +
  theme_bw()

ggplot(search.vk.agg, aes(x = segment)) +
  geom_bar(stat = 'count') +
  scale_y_continuous(breaks = seq(0, 13, 1)) +
  theme_bw()

arrange(search.vk, segment)
arrange(search.vk.agg, segment)

liste.vk.medlem <- factor(search.vk$medlem)
liste.vk.medlem <- unique(search.vk[,4])
liste.vk.medlem



liste.nf <- c(340101,340102,340103,340104,340111,340112,340113,
              340114,340121,340122,340123,340124,340201,340202,
              340203,340204,340301,340302,340303,340304,340401,
              340402,340501,350101,350102,350111,350112,350121,
              350122,350201,350202,350203,350301,350302,350303,
              350401)
search.nf <- pi[pi$segment %in% liste.nf & pi$aar == 2017 & pi$kvartal == 4]

liste.nf.agg <- c(340000,340100,340200,340300,340400,340500,350000,
                  350100,350200,350300,350400)
search.nf.agg <- pi[pi$segment %in% liste.nf.agg & pi$aar == 2017 & pi$kvartal == 4]

ggplot(search.nf, aes(x = segment)) +
  geom_bar(stat = 'count') +
  scale_y_continuous(breaks = seq(0, 35, 1)) +
  theme_bw()

ggplot(search.nf.agg, aes(x = segment)) +
  geom_bar(stat = 'count') +
  scale_y_continuous(breaks = seq(0, 35, 1)) +
  theme_bw()

arrange(search.nf, segment)
arrange(search.nf.agg, segment)


liste.lk <- c(360101,360102,360103,360104,360109,360201,360202,
              360203,360204,360205,360209,360301,360302,360303,
              360304,360305,360306,360309,360401,360402,360403,
              360404,360405,360406,360407,360409,360501,360502,
              360509,360701,369801,369901)
search.lk <- pi[pi$segment %in% liste.lk & pi$aar == 2017 & pi$kvartal == 4]

liste.lk.agg <- c(360000, 360100, 360200, 360300, 360400, 360500,
                  360700, 369800, 369900)
search.lk.agg <- pi[pi$segment %in% liste.lk.agg & pi$aar == 2017 & pi$kvartal == 4]

ggplot(search.lk, aes(x = segment)) +
  geom_bar(stat = 'count') +
  scale_y_continuous(breaks = seq(0, 35, 1)) +
  theme_bw()

ggplot(search.lk.agg, aes(x = segment)) +
  geom_bar(stat = 'count') +
  scale_y_continuous(breaks = seq(0, 35, 1)) +
  theme_bw()

arrange(search.lk, segment)
arrange(search.lk.agg, segment)


liste.ka <- c(010101,010102,010103,010106,010201,010202,010203,
              010206,010207,010208,040203,010300,010400,010600,
              010800,010900,019800,019900)
liste.ka <- paste0('0', liste.ka)
search.ka <- pi[pi$segment %in% liste.ka & pi$aar == 2017 & pi$kvartal == 4]

liste.ka.agg <- c(010000,010100,010200,040000,040200)
liste.ka.agg <- paste0('0', liste.ka.agg)
search.ka.agg <- pi[pi$segment %in% liste.ka.agg & pi$aar == 2017 & pi$kvartal == 4]

ggplot(search.ka, aes(x = segment)) +
  geom_bar(stat = 'count') +
  scale_y_continuous(breaks = seq(0, 35, 1)) +
  theme_bw()

ggplot(search.ka.agg, aes(x = segment)) +
  geom_bar(stat = 'count') +
  scale_y_continuous(breaks = seq(0, 35, 1)) +
  theme_bw()

arrange(search.ka, segment)
arrange(search.ka.agg, segment)



liste.xla <- c(330801,330802,330803)
search.xla <- pi[pi$segment %in% liste.xla & pi$aar == 2017 & pi$kvartal == 4]

ggplot(search.xla, aes(x = segment)) +
  geom_bar(stat = 'count') +
  scale_y_continuous(breaks = seq(0, 35, 1)) +
  theme_bw()

arrange(search.xla, medlem)


#Create a vector with six-digit segment IDs for the PI statistiic
liste.pi  <- c(010000,020000,030000,040000,050000,080000,090000)
liste.pi  <- paste0('0', liste.pi)
liste.pi2 <- c(100000,110000,120000,140000,150000,160000,170000,
               180000,190000,210000,220000,230000,240000,270000,
               310200,310300,310400,310600,310800,310900,311000,
               311100,312000,312100,312200,312300,319800,319900,
               330100,330200,330300,330400,330500,330700,339800,
               339900,340000,350000,360100,360200,360300,360400,
               360500,360600,360700,369800,369900,370100,370200,
               379800,379900,380000,400000,410000,420000,440000,
               450000,460000,480000,490000,500100,500200,500300,
               500400,500500,500600,500700,500800,500900,509600,
               509800,509900,540000,550000,560000,580000,590000,
               600000,630000,640000,650000,660000)
liste.pi2 <- as.character(liste.pi2)
liste.pi[8:89] <- liste.pi2
rm(liste.pi2)
#
# Create a vector with only the necessary information to create
# the PI statistic hierarchy
liste.pi.hier  <- c(01,02,03,04,05,06,08,09)
liste.pi.hier  <- paste0('0', liste.pi.hier)
liste.pi.hier2 <- c(10,11,12,14,15,16,17,18,19,21,22,23,24,27,
                    3102,3103,3104,3106,3108,3109,3110,3111,
                    3120,3121,3122,3123,3198,3199,3301,3302,
                    3303,3304,3305,3307,3398,3399,34,35,3601,
                    3602,3603,3604,3605,3606,3607,3698,3699,
                    3701,3702,3798,3799,38,40,41,42,44,45,46,
                    48,49,5001,5002,5003,5004,5005,5006,5007,
                    5008,5009,5096,5098,5099,54,55,56,58,59,60,
                    63,64,65,66)
liste.pi.hier2 <- as.character(liste.pi.hier2)
liste.pi.hier[8:89] <- liste.pi.hier2
rm(liste.pi.hier2)

# # Search for specific hierarchy groups
# b01 <- grepl('^01', pi$segment)
# pi <- cbind(pi, b01)
# search.pi.b01 <- pi[pi$b01 == TRUE & pi$aar == 2017 & pi$kvartal == 4]
# b01L <- length(unique(factor(search.pi.b01$medlem)))

# Loop the 89 conditions to get the amount of members for all
# hierarchy groups
i <- 1
while (i <= length(liste.pi.hier)){

  temp <- as.numeric(grepl(paste('^', liste.pi.hier[i], sep = ''), pi$segment))
  nam <- paste('b', i, sep = '')
  assign(nam, temp)
  pi <- cbind(pi, temp)
  i = i+1
}
rm(i, nam, temp)

# Name columns appropriately
colnames(pi)[8:96] <- liste.pi

# Remove copied vectors
rm(list = ls(pattern = '^b'))

# Create the length of the list of unique members who participate
# in a certain hierarchy group
# b01L <- length(unique(factor(pi[aar == 2017][kvartal == 4][`010000` == 1]$medlem)))
# b01L

# Find members per hierarchial group
innm <- pi[aar == 2017][kvartal == 4][, lapply(.SD, sum), by = medlem, .SDcols = 8:96]
innm.list <- t(data.frame(colSums(innm != 0)))
innm <- rbind(innm, innm.list)
innm[99,1] <- 'Sum ant. innmeldere'
rm(innm.list)
innm.kort <- innm[99,]
pi <- pi[,1:7]

# Create subset for those who are on the maximum level in the hierarchy
search.pi <- pi[pi$segment %in% liste.pi & pi$aar == 2017 & pi$kvartal == 4]

# Create subset for those who are above the maximum level in the hierarchy
liste.pi.agg <- c(310000,330000,360000,370000,500000)
search.pi.agg <- pi[pi$segment %in% liste.pi.agg & pi$aar == 2017 & pi$kvartal == 4]

ggplot(search.pi, aes(x = segment)) +
  geom_bar(stat = 'count') +
  scale_y_continuous(breaks = seq(0, 35, 1)) +
  theme_bw()

arrange(search.pi, segment)

# Number of members for each hierarcy top-level
n1 <- pi %>%
  filter(aar == 2017 & kvartal == 4) %>%
  mutate(blnrn1 = str_sub(segment, 1, 2)) %>%
  group_by(blnrn1, medlem) %>%
  summarise(Counts = n()) %>%
  group_by(blnrn1) %>%
  summarise(Counts = n())


# Create plots to check for the need for further adjustments in the statistics categories
ggplot(search.pi, aes(x = segment)) +
  geom_bar(stat = 'count') +
  scale_y_continuous(breaks = seq(0, 35, 1)) +
  theme_bw()


# # Sjekk for enkeltselskap
# selskap <- pi %>%
#   filter(aar == 2018 & kvartal == 2) %>%
#   mutate(blnrn2 = str_sub(segment, 1, 4)) %>%
#   group_by(blnrn2, medlem) %>%
#   summarise(Counts = n()) %>%
#   group_by(blnrn2) %>%
#   summarise(Counts = n())


## Overordnet PI-stat per kvartal
# Sette dato for aar og kvartal
pi <- pi %>%
        mutate(dato = make_date(aar, month = kvartal * 3, day = 1))
pi$dato <- pi$dato %m+% months(1) %m-% days(1)
table(pi$dato)

# Summere totalomsetning per kvartal
PI <- pi %>% 
        group_by(kvartal = dato) %>% 
        summarize(verdi = sum(verdi), innmeldere = length(unique(medlem)), innmeldinger = length(medlem))

write.csv(PI, file = "pi_aggregert.csv", row.names = FALSE)
                 
G <- gk %>% 
       group_by(kvartal = dato) %>% 
       summarize(verdi = sum(verdi), innmeldere = length(unique(medlem)), innmeldinger = length (medlem))

write.csv(G, file = "g_aggregert.csv", row.names = FALSE)




# # Antall innmeldere per blokknummer på nivå 3
# PI.ant <- pi %>% 
#             filter(dato == max(dato)) %>% 
#             group_by(blokknummer = segment) %>% 
#             summarize(verdi = sum(verdi), innmeldere = length(unique(medlem)), innmeldinger = length (medlem))
# 
# # Antall innmeldere per blokknummer på nivå 2
# n2.raa <- pi %>%
#             filter(grepl("00$", pi$segment) == TRUE & dato == max(dato))
# 
# n3.raa <- pi %>%
#   filter(dato == max(dato))
# 
# 
# # Antall innmeldinger på nivå 1
# n1.raa <- pi %>%
#             filter(grepl("0000", pi$segment) == TRUE & dato == max(dato))
# sum(n1.raa$verdi)


# # Antall innmeldinger innen varmegrupper
# varme5001 <- pi %>% filter(grepl("^5001", pi$segment) == TRUE & dato == max(dato))
# varme5002 <- pi %>% filter(grepl("^5002", pi$segment) == TRUE & dato == max(dato))
# varme5004 <- pi %>% filter(grepl("^5004", pi$segment) == TRUE & dato == max(dato))
# varme5005 <- pi %>% filter(grepl("^5005", pi$segment) == TRUE & dato == max(dato))
# varme5008 <- pi %>% filter(grepl("^5008", pi$segment) == TRUE & dato == max(dato))


# # Avlesninger til estimeringen av det profesjonelle lysarmaturmarkedet
# nrs <- c(48, 600, 195, 71, 1364, 1176, 939, 73, 84, 1251, 1229, 97, 51, 1345)
# sum <- pi %>% filter(medlem %in% nrs, year(pi$dato) == 2017) %>% group_by(medlem) %>% summarize(verdi = sum(verdi))
# sum
# sum %>% summarize(totalverdi = sum(verdi))

