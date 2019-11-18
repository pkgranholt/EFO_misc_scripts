
###############################################
# Beregning av ETIM-egenskaper og fakturering #
###############################################


## Filer som brukes i dette scriptet:
# (1) Egenskapsdata.csv fra FTP-server (dynamisk fil)
# (2) medlemmer og kunder.csv fra Dynamics 365 (dynamisk fil)
# (3) Statusrapport.csv med reservasjoner fra EFObasen (dynamisk fila)
# (4) satt til avvist.csv fra Harald (statistik fil fra 1. mars 2018)
# (5) unntak ETIMklasser.csv fra Knut (semi-statisk fil)
# (6) elnr. uten ETIMkl. som ikke skal dobbeltfaktureres fra Knut (dynamisk fil)
# (7) Unntaksliste (fra Harald) over alle produkter som Knut har sagt skal unntas (dynamisk fil)

# crontab-hjelp: https://stackoverflow.com/a/15456984/7365434


# Laster inn pakker
library(data.table)
library(tidyverse)
library(stringr)

## Yummy Pro må være satt til mappen 'R'
# Laster inn nyeste filer
setwd("~/Documents/OneDrive - Elektroforeningen (EFO)/filer fra FTP-server")
df <- fread(input = 'Egenskapsdata.csv')
harald.ok <- fread(input = 'Unntaksliste.txt')

# Endrer working directory og lagrer kopi av fil i samme mappe
setwd("~/Documents/OneDrive - Elektroforeningen (EFO)/R/Scripts/ETIM-andel og fakturering")
write.csv(df, file = 'Egenskapsdata.csv')

# Leser inn data og setter kolonnenavn
names(df) <- c("elnr", "status", "EFOkl", "levnr", "levnavn", "blokknr",
               "ETIMkl", "mangler.gdf", "ETIM.maks", "ETIM.utfylt")

# df$status  <- as.factor(df$status)
# df$levnavn <- as.factor(df$levnavn)
# df$ETIMkl  <- as.factor(df$ETIMkl)

# Fjerner Vivendis "elnr"
df <- df[levnr != 10000]

# Beregner ETIM-andel og de ulike gruppene
df <- mutate(df, ETIM.andel = (ETIM.utfylt / ETIM.maks)*100)

df[is.na.data.frame(df)] <- 0

E0   <- filter(df, ETIM.andel == 0)
E70  <- filter(df, ETIM.andel >= 70)
E100 <- filter(df, ETIM.andel == 100)

df$E70 <- ifelse(df$ETIM.andel >= 70, 1, 0)


# Skiller ut aktive elnr for ETIM-andelsberegningene
aktive <- filter(df, status == 'Aktiv')

# Plotter histogram over ETIM-andeler på produkter
ggplot(aktive, aes(aktive$ETIM.andel)) +
  geom_histogram(bins = 21,fill = rainbow(21)) +
  theme_minimal() +
  scale_y_continuous(limits=c(0,180000)) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  ylab('Antall aktive produkter i EFObasen') +
  xlab('Prosentandel utfylte ETIM-egenskaper')

# Setter df til data.table for å bruke [] til selektering
df <- as.data.table(df)

# Sjekk for enkeltselskaper, på aktive produkter, til godkjenning og avviste produkter
per.medlem <- df[status != 'Preregistrert' | status != 'Til godkjenning'] %>% 
  group_by(levnr, levnavn) %>% 
  summarise('Antall produkter som er aktive eller avviste' = n(),
            'Antall produkter med minst 70% ETIM-egenskaper' = sum(E70),
            'Antall produkter som ikke har nok ETIM-egenskaper' = sum(!E70),
            'Prosentandel med minst 70% ETIM-egenskaper' = sum(E70)/n()) %>% 
  arrange(levnavn)

#kontakt <- per.medlem[with(per.medlem, order(per.medlem$`Prosentandel med minst 70% ETIM-egenskaper`, decreasing = TRUE)),][11:20,]

write.csv2(per.medlem, file = 'Oversikt produkter med ETIM-andel per medlem (aktive + avviste).csv')


## Beregning av dobbeltfakturering av de relevante produktene med under 70% ETIM-egenskaper
# Koble 70%-produktene med fakturerte priser
# Skal dobbletfaktureres: Aktive, Avviste, Til godkjenning (="ikke godkjent")
# Skal ikke dobbeltfaktureres: Preregistrerte (="reserverte"), de som ble flyttet til avviste 1. mars

# Innlesing av produktene som ble avvist 1. mars
avvist <- fread(input = 'satt til avvist.csv')
avvist <- subset(avvist[,2])
colnames(avvist) <- 'elnr'

# Setter disse som unntak som ikke skal dobbeltfaktureres
df2 <- df
df2$unntak <- 0
avvist$avvist.i.mars <- 1
df2 <- left_join(df, avvist, by = 'elnr')
df2$elnr <- factor(df2$elnr)

# Markere alle elnr som er i kategorien preregistrert som dummy (som skal brukes
# til vurdering av hvilke produkter som skal unntas dobbeltfakturering)
df2$prereg.og.t.godkj <- ifelse(df2$status == 'Preregistrert' | df2$status == 'Til godkjenning', 1, 0)

# Sett 0 på tomme datafelter
df2[is.na(df2)] <- 0

# Legger til unntak som Knut har identifisert blant de produktene som ikke har ETIM-klasse
temp <- fread(input = 'uten ETIMkl som ikke skal dobbeltfaktureres.csv')
temp <- temp[,c(2,14)]
temp$`Faktureres dobbelt` <- factor(temp$`Faktureres dobbelt`)
temp <- temp[temp$`Faktureres dobbelt` == '0' | temp$`Faktureres dobbelt` == 'Ingen ETIMklasse']
liste.k <- temp$Elnummer
rm(temp)

temp <- fread(input = 'uten ETIMkl som ikke skal dobbeltfaktureres2.csv')
temp <- temp[,c(2,8,15,17)]
temp <- temp[`ETIM klasse` == '']
temp <- temp[`Fra ny medlemsfil 150918` == '']
temp$`Fra ny medlemsfil 150918` <-  ifelse(temp$Status == 'Til godkjenning' |
                                           temp$Status == 'Preregistrert'   |
                                           temp$Status == '#N/A', 'ok', '')
temp$`Fra ny medlemsfil 150918` <- factor(temp$`Fra ny medlemsfil 150918`)
temp <- temp[temp$`Fra ny medlemsfil 150918` == ''][,c(1,4)]
liste.m <- temp$Elnummer

# elnr som kommer løpende fra Knut (og via Harald)
liste.ekstra <- as.integer(c(8873955, 1317621, 1317622, 3101292, 3208613, 3208614, 3208630, 8839769, 8839770, 8839771,
                             8839772, 8839773, 8839774, 2810502, 8839769, 8839770, 8839771, 8839772, 8839773, 8839774,
                             2810501, 3200934)) 

liste.harald.ok <- harald.ok$V1

liste.comb.raw <- c(liste.k, liste.m, liste.ekstra, liste.harald.ok)
liste.comb <- unique(unlist(liste.comb.raw, use.names = FALSE))

df2$knut.ok <- 0
df2$knut.ok[which(df2$elnr %in% liste.comb & df2$ETIMkl == '')] <- 1

# elnr som har flagget "Ikke tilgjengelig" i EFObasen (lagt til oktober 2018)
df2$ikke.tilgjengelig <- 0
df2$ikke.tilgjengelig[which(df2$status == "Ikke tilgjengelig")] <- 1

# Sett inn indikator per produkt på om de skal unntas dobbeltfakturering eller ikke
# basert på om produktet har minst 70% ETIM-egenskaper, ble avvist pga. manglende
# grunndatafelter i mars eller er satt til preregistrert
df2$unntak <- ifelse(df2$E70 == 1 | df2$avvist.i.mars == 1 | df2$prereg.og.t.godkj == 1 | df2$knut.ok == 1 | df2$ikke.tilgjengelig == 1, 1, 0)


## Legger til antall ETIM-egenskaper som skal holdes unna i beregningen 
# Oversikt over ETIMklasser som er i bruk
ETIM.klasse.oversikt <- df2 %>%
  group_by(ETIMkl) %>%
  summarise('Antall produkter per ETIM-klasse' = n()) %>%
  arrange(desc(`Antall produkter per ETIM-klasse`))
#head(ETIM.klasse.oversikt)

# Henter ut hvor mange ETIM-egenskaper (Ee) det er per ETIM-klasse og kombinerer dette med oversikten over
# ETIM-klasser i bruk
temp <- df2 %>%
  group_by(ETIMkl) %>% 
  summarise('ant.Ee' = max(ETIM.maks))

ETIM.klasse.oversikt <- full_join(ETIM.klasse.oversikt, temp, by = 'ETIMkl')
rm(temp)

# Endrer wd slik at Knut direkte kan legge inn endringer i filen "unntak ETIM-klasser.csv"
setwd("~/Documents/Elektroforeningen (EFO)/Felles - Dokumenter/Statistikk og analyse/Unntaksliste")

temp <- fread(input = 'unntak ETIM-klasser.csv', sep = ",", fill = TRUE)

setwd("~/Documents/OneDrive - Elektroforeningen (EFO)/R/Scripts/ETIM-andel og fakturering")

temp <- temp %>%
  arrange(`ETIM-klasse`)

ETIM.klasse.oversikt <- left_join(ETIM.klasse.oversikt, temp, by = c('ETIMkl' = 'ETIM-klasse'))[,c(1,2,3,5)]

# Legger til en 10% fratrekk av egenskaper på de ETIM-klassene som ikke er gjennomgått (rundet opp til nærmeste hele egenskap)
ETIM.klasse.oversikt[1,4] <- 0
ETIM.klasse.oversikt$fratrekk <- ceiling(ETIM.klasse.oversikt$ant.Ee * 0.1)

ETIM.klasse.oversikt$`ant egenskaper som trekkes fra` <-
  ifelse(is.na(ETIM.klasse.oversikt$`ant egenskaper som trekkes fra`),
         ETIM.klasse.oversikt$fratrekk,
         ETIM.klasse.oversikt$`ant egenskaper som trekkes fra`)
ETIM.klasse.oversikt$fratrekk <- NULL

# Fjerner fratrekk fra de elnr som har kun en ETIM-egenskap, og som mister de under 10%-regelen (gjelder kun de som har 1 og 1 på antall
# egenskaper og fratrekk)
ETIM.klasse.oversikt$`ant egenskaper som trekkes fra` <- ifelse(ETIM.klasse.oversikt$ant.Ee == 1 & ETIM.klasse.oversikt$`ant egenskaper som trekkes fra` == 1, 0,
                                                                ETIM.klasse.oversikt$`ant egenskaper som trekkes fra`)

ETIM.klasse.oversikt <- ETIM.klasse.oversikt %>% arrange(desc(`Antall produkter per ETIM-klasse`))

# Endrer navn på et par kolonner
colnames(ETIM.klasse.oversikt)[2]  <- 'ant.prod'
colnames(ETIM.klasse.oversikt)[4]  <- 'fratrekk'

# Legger til siste kolonnen som viser hvor mange egenskaper som skal være med i beregningen av ETIM-%
ETIM.klasse.oversikt$ant.Ee.fakt <- ETIM.klasse.oversikt$ant.Ee - ETIM.klasse.oversikt$fratrekk

# Trekk ut informasjonen i Ee og legg til i df2
temp <- ETIM.klasse.oversikt %>% 
          select(ETIMkl, fratrekk, ant.Ee.fakt)
write.csv2(ETIM.klasse.oversikt, file = 'ETIM-klasseoversikt.csv')

df2 <- data.table(left_join(df2, temp, by = c('ETIMkl' = 'ETIMkl')))
rm(temp)
df2 <- df2[,c(1:9,18,19,10:17)]
colnames(df2)[11]  <- 'ETIM.maks.justert'

# Oppdaterer ETIM-andel og E70. Fjerner NaN og Inf-verdier. Legger til 'unntak' som er 1 hvis et eller flere av unntakene er 1
df2$ETIM.maks.justert <- ifelse(df2$ETIM.maks.justert == 0 & df2$ETIM.utfylt == 1, 1, df2$ETIM.maks.justert)
df2$ETIM.andel = ifelse(df2$ETIM.maks.justert == 0 & df2$ETIM.utfylt == 0 ,0 ,
                         (df2$ETIM.utfylt / df2$ETIM.maks.justert)*100)
df2$E70 <- ifelse(df2$ETIM.andel >= 70, 1, 0)
df2$unntak <- ifelse(df2$E70 == 1 | df2$avvist.i.mars == 1 | df2$prereg.og.t.godkj == 1 | df2$knut.ok == 1 | df2$ikke.tilgjengelig == 1, 1, 0)



## Fakturering
# Gruppere alle fakturerbare elnumre sammen per leverandør
fakt <- df2 %>% 
          group_by(levnr, levnavn) %>% 
          summarise('Antall fakturerbare elnummer' = n()) %>% 
          arrange(levnavn)

# Lage elnummeroversikt etter bolker
fakt$'0-50'    <- ifelse(fakt$`Antall fakturerbare elnummer` >= 50, 50, fakt$`Antall fakturerbare elnummer`)
fakt$'51-100'  <- ifelse(fakt$`Antall fakturerbare elnummer` >= 100, 50,
                    ifelse(fakt$`Antall fakturerbare elnummer` < 50, 0, (fakt$`Antall fakturerbare elnummer`) - 50))
fakt$'101-500' <- ifelse(fakt$`Antall fakturerbare elnummer` >= 500, 400,
                    ifelse(fakt$`Antall fakturerbare elnummer` < 100, 0, (fakt$`Antall fakturerbare elnummer`) - 100))
fakt$'500-'    <- ifelse(fakt$`Antall fakturerbare elnummer` >  500, (fakt$`Antall fakturerbare elnummer` - 500), 0)

# Les inn fil over medlemmer og kunder (hentet fra CRM-systemet)
medlemmer <- fread(input = 'medlemmer og kunder.csv')
#medlemmer <- medlemmer[,c(3,1,2)]
#medlemmer$levnr <- as.integer(medlemmer$levnr)
# Rekkefølgen er levnr, levnavn, medlemstype
names(medlemmer) <- c('levnr', 'levnavn', 'medlemstype')

# Fakturering av ABB - skal faktureres som medlemmer, selv om de står som kunder
# Dette betyr at avdelingene med få elnummer må betale minstepris
medlemmer$medlemstype <- ifelse(grepl('ABB ', medlemmer$levnavn), 'Medlem', medlemmer$medlemstype)

# Legge til informasjon om hvilke leverandører som er hhv. medlemmer eller kunder
fakt <- fakt %>% 
  right_join(medlemmer, by = c('levnr', 'levnavn')) %>% 
  arrange(levnavn)
fakt <- fakt[c(1,8,2:7)]

# Les inn fil med langitdsreservasjoner
langtid <- fread(input = 'Statusrapport.csv')
langtid <- langtid[,c(2,1,9)]
names(langtid) <- c('levnr', 'levnavn', 'reserverte')

# Fyll 0 inn der det mangler data, slå sammen med fakt, rydd og slett midlertidig fil
langtid[is.na(langtid)] <- 0
langtid <- langtid[langtid$reserverte != 0]
fakt <- full_join(fakt, langtid, by = c('levnr', 'levnavn'))
rm(langtid)
fakt <- arrange(fakt, levnavn)
fakt[is.na(fakt)] <- 0

# Fjern medlemmer/kunder uten produkter eller reservasjoner
fakt <- fakt[rowSums(fakt[, -1:-3])>0,]
fakt <- data.table(fakt)
fakt <- fakt[medlemstype != '0']
fakt <- data.frame(fakt, check.names = FALSE)

# Legge inn priser for medlemmer og kunder
priser.m <- c(48.5, 33, 19.5, 14.5, 11, 2500)
priser.k <- priser.m*2

# Beregne priser per bolk og summert per leverandør
fakt$`p.0-50`     <- fakt$`0-50`     * ifelse(fakt$medlemstype == 'Medlem', priser.m[1], priser.k[1])
fakt$`p.51-100`   <- fakt$`51-100`   * ifelse(fakt$medlemstype == 'Medlem', priser.m[2], priser.k[2])
fakt$`p.101-500`  <- fakt$`101-500`  * ifelse(fakt$medlemstype == 'Medlem', priser.m[3], priser.k[3])
fakt$`p.500-`     <- fakt$`500-`     * ifelse(fakt$medlemstype == 'Medlem', priser.m[4], priser.k[4])
fakt$p.reserverte <- fakt$reserverte * ifelse(fakt$medlemstype == 'Medlem', priser.m[5], priser.k[5])

# Medlemmer betaler sine priser, og minstepris hvis de er under grenseverdien. Samme for kunder.
prisgrunnlag                 <- fakt[c('p.0-50', 'p.51-100', 'p.101-500', 'p.500-', 'p.reserverte')]
fakt$sumpris.uten.minstepris <- ifelse(fakt$medlemstype == 'Medlem', rowSums(prisgrunnlag), rowSums(prisgrunnlag))
fakt$sumpris.med.minstepris  <- ifelse(fakt$medlemstype == 'Medlem',
                                       ifelse(rowSums(prisgrunnlag) < priser.m[6], priser.m[6],
                                              rowSums(prisgrunnlag)),
                                       ifelse(rowSums(prisgrunnlag) < priser.k[6], priser.k[6],
                                              rowSums(prisgrunnlag)))


# Informasjon om hvilke produkter som skal dobbeltfaktureres legges til i fakt, som
# skilles ut i egen data frame som fakt.db
# Gruppere alle dobbeltfakturerbare elnumre (der unntak = 0) per leverandør
temp <- df2[unntak == 0] %>%
  group_by(levnr, levnavn) %>% 
  summarise('Antall dobbeltfakturerbare elnummer' = n()) %>% 
  arrange(levnavn)

fakt.db <- fakt
fakt.db <- full_join(fakt.db, temp, by = c("levnr", "levnavn"))
rm(temp)
fakt.db <- arrange(fakt.db, levnavn)

# Lage elnummeroversikt etter bolker
fakt.db$'0-50.db'    <- ifelse(fakt.db$`Antall dobbeltfakturerbare elnummer` >= 50, 50,
                               fakt.db$`Antall dobbeltfakturerbare elnummer`)
fakt.db$'51-100.db'  <- ifelse(fakt.db$`Antall dobbeltfakturerbare elnummer` >= 100, 50,
                          ifelse(fakt.db$`Antall dobbeltfakturerbare elnummer` < 50, 0,
                                (fakt.db$`Antall dobbeltfakturerbare elnummer`) - 50))
fakt.db$'101-500.db' <- ifelse(fakt.db$`Antall dobbeltfakturerbare elnummer` >= 500, 400,
                          ifelse(fakt.db$`Antall dobbeltfakturerbare elnummer` < 100, 0,
                                (fakt.db$`Antall dobbeltfakturerbare elnummer`) - 100))
fakt.db$'500-.db'    <- ifelse(fakt.db$`Antall dobbeltfakturerbare elnummer` >  500,
                              (fakt.db$`Antall dobbeltfakturerbare elnummer` - 500), 0)

# Beregne priser for produkter som skal dobbeltfaktureres per bolk og summert per leverandør
fakt.db$`p.0-50.db`     <- fakt.db$`0-50.db`     * ifelse(fakt.db$medlemstype == 'Medlem', priser.m[1], priser.k[1])
fakt.db$`p.51-100.db`   <- fakt.db$`51-100.db`   * ifelse(fakt.db$medlemstype == 'Medlem', priser.m[2], priser.k[2])
fakt.db$`p.101-500.db`  <- fakt.db$`101-500.db`  * ifelse(fakt.db$medlemstype == 'Medlem', priser.m[3], priser.k[3])
fakt.db$`p.500-.db`     <- fakt.db$`500-.db`     * ifelse(fakt.db$medlemstype == 'Medlem', priser.m[4], priser.k[4])

# Beregning av dobbeltfaktureringsprisene (eksl. de vanlige faktureringsprisene)
prisgrunnlag.db   <- fakt.db[c('p.0-50.db', 'p.51-100.db', 'p.101-500.db', 'p.500-.db')]

fakt.db$sumpris.uten.minstepris.db <- ifelse(fakt.db$medlemstype == 'Medlem', rowSums(prisgrunnlag.db),
                                             rowSums(prisgrunnlag.db))
fakt.db$sumpris.med.minstepris.db  <- ifelse(fakt.db$medlemstype == 'Medlem',
                                             ifelse(rowSums(prisgrunnlag.db) < priser.m[6], priser.m[6],
                                                    rowSums(prisgrunnlag.db)),
                                             ifelse(rowSums(prisgrunnlag.db) < priser.k[6], priser.k[6],
                                                    rowSums(prisgrunnlag.db)))

# Legge sammen de vanlige faktureringsprisene og prisene for det som skal dobbelt faktureres, med unntakene trukket fra
fakt.db$sumpris.uten.minstepris.tot <- fakt.db$sumpris.uten.minstepris + fakt.db$sumpris.uten.minstepris.db
fakt.db$sumpris.med.minstepris.tot  <- ifelse(fakt.db$medlemstype == 'Medlem',
                                              ifelse(fakt.db$sumpris.uten.minstepris.tot < priser.m[6], priser.m[6],
                                                     fakt.db$sumpris.uten.minstepris.tot),
                                              ifelse(fakt.db$sumpris.uten.minstepris.tot < priser.k[6], priser.k[6],
                                                     fakt.db$sumpris.uten.minstepris.tot))

# Slå sammen filen for ordinær fakturering og dobbeltfakturering
fakt.tot <- left_join(fakt, fakt.db, by = c("levnr", "levnavn", "medlemstype", "Antall fakturerbare elnummer", "0-50", "51-100",
                                            "101-500", "500-", "reserverte", "p.0-50", "p.51-100", "p.101-500", "p.500-",
                                            "p.reserverte", "sumpris.uten.minstepris", "sumpris.med.minstepris"))
fakt.tot <- arrange(fakt.tot, levnr)

# Legge til "faktureringssum" der det ikke er dobbeltfakturering
fakt.tot$sumpris.med.minstepris.tot <- ifelse(is.na(fakt.tot$sumpris.med.minstepris.tot) == TRUE,
                                              fakt.tot$sumpris.med.minstepris, fakt.tot$sumpris.med.minstepris.tot)

# Differansen mellom ordinær fakturering og dobbeltfakturering
sum(fakt.tot$sumpris.med.minstepris.tot) - sum(fakt.tot$sumpris.med.minstepris)

# Ordinær fakturering
sum(fakt.tot$sumpris.med.minstepris)

# Print av total-tabellen
write.csv2(fakt.tot, file = 'Totaloversikt fakturering og dobbeltfakturering.csv')


## Div. spørringer
# Hente ut elnr som ikke oppfyller grensekravet for et medlem
#df2[levnr == ][status == 'Aktiv' | status == 'Avvist'][unntak == 0][,c(1:2,4:5,7,12,11,13)] som fakturert
#df[levnr == ][E70 == 0][,c(1:2,4:5,7,10,9,11)][status %in% c('Aktiv', 'Avvist')] totalt sett
uttrekk <- filter(df2, levnr == 65)
uttrekk

write.csv2(uttrekk, file = 'uttrekk.csv', row.names = F)


# # Hvor mange produkter har minst én ETIM-egenskap? Regner ut på to ulike måter og sjekker at svarene er identiske
# length(df2[ETIM.andel > 0][status != 'Preregistrert'][status != 'Avvist']$ETIM.andel)
# length(df2[ETIM.utfylt >= 1][status != 'Preregistrert'][status != 'Avvist']$ETIM.utfylt)
# 
# length(df2[ETIM.andel > 0][status != 'Preregistrert'][status != 'Avvist']$ETIM.andel) ==
#   length(df2[ETIM.utfylt >= 1][status != 'Preregistrert'][status != 'Avvist']$ETIM.utfylt)

write.csv2(df2, file = 'oversikt per elnummer.csv')
write.csv2(ETIM.klasse.oversikt, file = 'ETIM fratrekk.csv')

# 
# # Hvilke blokknr.gr har produktene som Knut holder utenfor?
# knuts.liste <- df2[which(df2$knut.ok == 1)]
# knuts.liste$blokknr <- as.character(knuts.liste$blokknr)
# #table(nchar(knuts.liste$blokknr))
# 
# knuts.liste$blokknr <- str_pad(knuts.liste$blokknr, 6, side = "left", pad = "0")
# #table(nchar(knuts.liste$blokknr))
# 
# knuts.liste$blokknr.gr <- substr(knuts.liste$blokknr, start = 1, stop = 2)
# #table(knuts.liste$blokknr.gr)
# knuts.liste$blokknr.gr <- as.numeric(knuts.liste$blokknr.gr)
# hist(knuts.liste$blokknr.gr, breaks = 99)
# 
# 
# ## Liste for kunder/medlemmer som lurer på hvilke produkter som blir dobbeltfakturert
# #navn <- df2[levnr == XXX & unntak == 0]
# #write.csv(navn, file = "navn.csv")
# 
# 
# ## Til shiny-app'en
# # Oversiktsvisningen - forskjellen i 'a' og 'b' ligger i om de henter data fra df eller df2,
# # dvs. ujustert eller justert for at ikke alle ETIM-egenskaper nødvendigvis er relevante
# meta1a <- df %>%
#   group_by(levnr, levnavn) %>% 
#   summarise('akt.el.avv' = n(),
#             'E70' = sum(E70),
#             'ikke.nok' = n() - sum(E70),
#             'andel.ok' = (sum(E70/n())*100)) %>%
#   mutate(andel.ok = round(andel.ok, 1)) %>% 
#   arrange(levnavn) %>% 
#   mutate(type = 'Kvalitetssjekk')
# 
# meta1b <- df2 %>% 
#   filter(status == 'Aktiv' | status == 'Avvist') %>% 
#   group_by(levnr, levnavn) %>% 
#   summarise('akt.el.avv' = n(),
#             'E70' = sum(E70),
#             'ikke.nok' = n() - sum(E70),
#             'andel.ok' = (sum(E70/n())*100)) %>%
#   mutate(andel.ok = round(andel.ok, 1)) %>% 
#   arrange(levnavn) %>% 
#   mutate(type = 'Dobbeltfakturering')
# 
# meta1ab <- rbind(meta1a, meta1b)
# 
# # Detaljert visning - forskjellen i 'a' og 'b' ligger i om de henter data fra df eller df2,
# # dvs. ujustert eller justert for at ikke alle ETIM-egenskaper nødvendigvis er relevante
# meta2a <- df %>%
#   select(elnr, levnr, levnavn, ETIMkl, ETIM.utfylt, ETIM.maks = ETIM.maks, ETIM.andel) %>% 
#   mutate(ETIM.andel = round(ETIM.andel, 1)) %>% 
#   arrange(elnr) %>% 
#   mutate(type = 'Kvalitetssjekk')
# 
# meta2b <- df2 %>% 
#   filter(status == 'Aktiv' | status == 'Avvist') %>% 
#   select(elnr, levnr, levnavn, ETIMkl, ETIM.utfylt, ETIM.maks = ETIM.maks.justert, ETIM.andel) %>% 
#   mutate(ETIM.andel = round(ETIM.andel, 1)) %>% 
#   arrange(elnr) %>% 
#   mutate(type = 'Dobbeltfakturering')
# 
# meta2ab <- rbind(meta2a, meta2b)
# 
# write.csv(meta1ab, file = './shiny_efobasen/oversikt.csv', row.names = FALSE)
# write.csv(meta2ab, file = './shiny_efobasen/detaljer.csv', row.names = FALSE)
# 
# 
# # Finne "Ikke tilgjenglig"-produkter for enkeltbedrift
# hager <- df[status == 'Ikke tilgjengelig' & levnr == 242]
# write.csv(hager, file = 'Hager elnummer ikke tilgjengelig.csv', row.names = FALSE)

# # Finne produkter med maksimalt 0 ETIM-egenskaper etter justering
# nuller <- df2[df2$ETIM.maks.justert == 0 & ETIMkl != '']
# write.csv(nuller, file = 'nuller.csvc', row.names = FALSE)
# 
# fakt26nov <- fakt.tot %>% filter(levnr %in% c(21, 207, 278, 727, 947, 1308, 1408))
# write.csv(fakt26nov, file = 'fakt26nov.csv', row.names = F)
#
# # Finne produkter med ETIM-klasse
# ETIMkl <- df2 %>% filter(ETIMkl != '')
# 
# til.knut <- fakt.tot %>% filter(levnr %in% c(209, 239, 1042, 1053))
# write.csv(til.knut, file = 'Fakturering 190109.csv', row.names = FALSE)





##############################
#   Dobbeltfaktuering 2019   #
##############################

df3 <- df2

df3$unntak <- 0
df3$unntak <- ifelse(df3$E70 == 1 | df3$status == 'Preregistrert' | df3$knut.ok == 1, 1, 0)

sum(df3$E70)
sum(df3$status == 'Preregistrert')
sum(df3$ikke.tilgjengelig == 1)

## Fakturering 2019
# Gruppere alle fakturerbare elnumre sammen per leverandør
fakt3 <- df3 %>% 
  group_by(levnr, levnavn) %>% 
  summarise('Antall fakturerbare elnummer' = n()) %>% 
  arrange(levnavn)

# Lage elnummeroversikt etter bolker
fakt3$'0-50'    <- ifelse(fakt3$`Antall fakturerbare elnummer` >= 50, 50, fakt3$`Antall fakturerbare elnummer`)
fakt3$'51-100'  <- ifelse(fakt3$`Antall fakturerbare elnummer` >= 100, 50,
                         ifelse(fakt3$`Antall fakturerbare elnummer` < 50, 0, (fakt3$`Antall fakturerbare elnummer`) - 50))
fakt3$'101-500' <- ifelse(fakt3$`Antall fakturerbare elnummer` >= 500, 400,
                         ifelse(fakt3$`Antall fakturerbare elnummer` < 100, 0, (fakt3$`Antall fakturerbare elnummer`) - 100))
fakt3$'500-'    <- ifelse(fakt3$`Antall fakturerbare elnummer` >  500, (fakt3$`Antall fakturerbare elnummer` - 500), 0)

# Les inn fil over medlemmer og kunder (hentet fra CRM-systemet)
medlemmer3 <- fread(input = 'medlemmer og kunder.csv')
#medlemmer3 <- medlemmer3[,c(3,1,2)]
#medlemmer3$levnr <- as.integer(medlemmer3$levnr)
# Rekkefølgen er levnr, levnavn, medlemstype
names(medlemmer3) <- c('levnr', 'levnavn', 'medlemstype')

# fakt3urering av ABB - skal fakt3ureres som medlemmer3, selv om de står som kunder
# Dette betyr at avdelingene med få elnummer må betale minstepris
medlemmer3$medlemstype <- ifelse(grepl('ABB ', medlemmer3$levnavn), 'Medlem', medlemmer3$medlemstype)

# Legge til informasjon om hvilke leverandører som er hhv. medlemmer3 eller kunder
fakt3 <- fakt3 %>% 
  right_join(medlemmer3, by = c('levnr', 'levnavn')) %>% 
  arrange(levnavn)
fakt3 <- fakt3[c(1,8,2:7)]

# Les inn fil med langitdsreservasjoner
langtid3 <- fread(input = 'Statusrapport.csv')
langtid3 <- langtid3[,c(2,1,9)]
names(langtid3) <- c('levnr', 'levnavn', 'reserverte')

# Fyll 0 inn der det mangler data, slå sammen med fakt3, rydd og slett midlertidig fil
langtid3[is.na(langtid3)] <- 0
langtid3 <- langtid3[langtid3$reserverte != 0]
fakt3 <- full_join(fakt3, langtid3, by = c('levnr', 'levnavn'))
rm(langtid3)
fakt3 <- arrange(fakt3, levnavn)
fakt3[is.na(fakt3)] <- 0

# Fjern medlemmer/kunder uten produkter eller reservasjoner
fakt3 <- fakt3[rowSums(fakt3[, -1:-3])>0,]
fakt3 <- data.table(fakt3)
fakt3 <- fakt3[medlemstype != '0']
fakt3 <- data.frame(fakt3, check.names = FALSE)

# Legge inn priser for medlemmer og kunder
priser.m <- c(48.5, 33, 19.5, 14.5, 11, 2500)
priser.k <- priser.m*2

# Beregne priser per bolk og summert per leverandør
fakt3$`p.0-50`     <- fakt3$`0-50`     * ifelse(fakt3$medlemstype == 'Medlem', priser.m[1], priser.k[1])
fakt3$`p.51-100`   <- fakt3$`51-100`   * ifelse(fakt3$medlemstype == 'Medlem', priser.m[2], priser.k[2])
fakt3$`p.101-500`  <- fakt3$`101-500`  * ifelse(fakt3$medlemstype == 'Medlem', priser.m[3], priser.k[3])
fakt3$`p.500-`     <- fakt3$`500-`     * ifelse(fakt3$medlemstype == 'Medlem', priser.m[4], priser.k[4])
fakt3$p.reserverte <- fakt3$reserverte * ifelse(fakt3$medlemstype == 'Medlem', priser.m[5], priser.k[5])

# Medlemmer betaler sine priser, og minstepris hvis de er under grenseverdien. Samme for kunder.
prisgrunnlag3                 <- fakt3[c('p.0-50', 'p.51-100', 'p.101-500', 'p.500-', 'p.reserverte')]
fakt3$sumpris.uten.minstepris <- ifelse(fakt3$medlemstype == 'Medlem', rowSums(prisgrunnlag3), rowSums(prisgrunnlag3))
fakt3$sumpris.med.minstepris  <- ifelse(fakt3$medlemstype == 'Medlem',
                                       ifelse(rowSums(prisgrunnlag3) < priser.m[6], priser.m[6],
                                              rowSums(prisgrunnlag3)),
                                       ifelse(rowSums(prisgrunnlag3) < priser.k[6], priser.k[6],
                                              rowSums(prisgrunnlag3)))


# Informasjon om hvilke produkter som skal dobbeltfaktureres legges til i fakt3, som
# skilles ut i egen data frame som fakt3.db
# Gruppere alle dobbeltfakt3urerbare elnumre (der unntak = 0) per leverandør
temp <- df3[unntak == 0] %>%
  group_by(levnr, levnavn) %>% 
  summarise('Antall dobbeltfakturerbare elnummer' = n()) %>% 
  arrange(levnavn)

fakt3.db <- fakt3
fakt3.db <- full_join(fakt3.db, temp, by = c("levnr", "levnavn"))
rm(temp)
fakt3.db <- arrange(fakt3.db, levnavn)

# Lage elnummeroversikt etter bolker
fakt3.db$'0-50.db'    <- ifelse(fakt3.db$`Antall dobbeltfakturerbare elnummer` >= 50, 50,
                                fakt3.db$`Antall dobbeltfakturerbare elnummer`)
fakt3.db$'51-100.db'  <- ifelse(fakt3.db$`Antall dobbeltfakturerbare elnummer` >= 100, 50,
                               ifelse(fakt3.db$`Antall dobbeltfakturerbare elnummer` < 50, 0,
                                     (fakt3.db$`Antall dobbeltfakturerbare elnummer`) - 50))
fakt3.db$'101-500.db' <- ifelse(fakt3.db$`Antall dobbeltfakturerbare elnummer` >= 500, 400,
                               ifelse(fakt3.db$`Antall dobbeltfakturerbare elnummer` < 100, 0,
                                     (fakt3.db$`Antall dobbeltfakturerbare elnummer`) - 100))
fakt3.db$'500-.db'    <- ifelse(fakt3.db$`Antall dobbeltfakturerbare elnummer` >  500,
                               (fakt3.db$`Antall dobbeltfakturerbare elnummer` - 500), 0)

# Beregne priser for produkter som skal dobbeltfaktureres per bolk og summert per leverandør
fakt3.db$`p.0-50.db`     <- fakt3.db$`0-50.db`     * ifelse(fakt3.db$medlemstype == 'Medlem', priser.m[1], priser.k[1])
fakt3.db$`p.51-100.db`   <- fakt3.db$`51-100.db`   * ifelse(fakt3.db$medlemstype == 'Medlem', priser.m[2], priser.k[2])
fakt3.db$`p.101-500.db`  <- fakt3.db$`101-500.db`  * ifelse(fakt3.db$medlemstype == 'Medlem', priser.m[3], priser.k[3])
fakt3.db$`p.500-.db`     <- fakt3.db$`500-.db`     * ifelse(fakt3.db$medlemstype == 'Medlem', priser.m[4], priser.k[4])

# Beregning av dobbeltfaktureringsprisene (eksl. de vanlige faktureringsprisene)
prisgrunnlag3.db   <- fakt3.db[c('p.0-50.db', 'p.51-100.db', 'p.101-500.db', 'p.500-.db')]

fakt3.db$sumpris.uten.minstepris.db <- ifelse(fakt3.db$medlemstype == 'Medlem', rowSums(prisgrunnlag3.db),
                                             rowSums(prisgrunnlag3.db))
fakt3.db$sumpris.med.minstepris.db  <- ifelse(fakt3.db$medlemstype == 'Medlem',
                                             ifelse(rowSums(prisgrunnlag3.db) < priser.m[6], priser.m[6],
                                                    rowSums(prisgrunnlag3.db)),
                                             ifelse(rowSums(prisgrunnlag3.db) < priser.k[6], priser.k[6],
                                                    rowSums(prisgrunnlag3.db)))

# Legge sammen de vanlige faktureringsprisene og prisene for det som skal dobbelt faktureres, med unntakene trukket fra
fakt3.db$sumpris.uten.minstepris.tot <- fakt3.db$sumpris.uten.minstepris + fakt3.db$sumpris.uten.minstepris.db
fakt3.db$sumpris.med.minstepris.tot  <- ifelse(fakt3.db$medlemstype == 'Medlem',
                                              ifelse(fakt3.db$sumpris.uten.minstepris.tot < priser.m[6], priser.m[6],
                                                     fakt3.db$sumpris.uten.minstepris.tot),
                                              ifelse(fakt3.db$sumpris.uten.minstepris.tot < priser.k[6], priser.k[6],
                                                     fakt3.db$sumpris.uten.minstepris.tot))

# Slå sammen filen for ordinær fakturering og dobbeltfakturering
fakt3.tot <- left_join(fakt3, fakt3.db, by = c("levnr", "levnavn", "medlemstype", "Antall fakturerbare elnummer", "0-50", "51-100",
                                            "101-500", "500-", "reserverte", "p.0-50", "p.51-100", "p.101-500", "p.500-",
                                            "p.reserverte", "sumpris.uten.minstepris", "sumpris.med.minstepris"))
fakt3.tot <- arrange(fakt3.tot, levnr)

# Legge til "faktureringssum" der det ikke er dobbeltfakturering
fakt3.tot$sumpris.med.minstepris.tot <- ifelse(is.na(fakt3.tot$sumpris.med.minstepris.tot) == TRUE,
                                              fakt3.tot$sumpris.med.minstepris, fakt3.tot$sumpris.med.minstepris.tot)

# Differansen mellom ordinær fakturering og dobbeltfakturering
sum(fakt3.tot$sumpris.med.minstepris.tot) - sum(fakt3.tot$sumpris.med.minstepris)

# Ordinær fakturering
sum(fakt3.tot$sumpris.med.minstepris)

# Sjekk for enkeltselskaper, på aktive produkter, til godkjenning og avviste produkter
per.medlem3 <- df3[status != 'Preregistrert' | status != 'Til godkjenning'] %>% 
  group_by(levnr, levnavn) %>% 
  summarise('Antall produkter som er aktive eller avviste' = n(),
            'Antall produkter med minst 70% ETIM-egenskaper' = sum(E70),
            'Antall produkter som ikke har nok ETIM-egenskaper' = sum(!E70),
            'Prosentandel med minst 70% ETIM-egenskaper' = sum(E70)/n()) %>% 
  arrange(levnavn)





# Dele unntakene fra Knut på bedrift
unntaksprodukter <- df2 %>% filter(knut.ok == 1)

unntaksoversikt <- unntaksprodukter %>% 
                   group_by(bedr = unntaksprodukter$levnavn) %>% 
                   summarise(unntatte.prod = n()) %>% 
                   arrange(desc(unntatte.prod))

unntaksoversikt


# Se nøyere på den rå unntakslisten
raa.unntaksliste         <- df2[df2$elnr %in% liste.comb]
raa.unntaksliste$levnavn <- as.factor(raa.unntaksliste$levnavn)
raa.unntaksliste$status  <- as.factor(raa.unntaksliste$status)
raa.unntaksliste$ETIMkl  <- as.factor(raa.unntaksliste$ETIMkl)
str(raa.unntaksliste)
summary(raa.unntaksliste)


#write.csv(unntaksprodukter, file = 'Unntaksproukter.csv', row.names = FALSE)
write.csv(fakt3.tot, file = 'Faktureringsliste 2019.csv', row.names = FALSE) 
write.csv(df3, file = 'Faktureringsstatus per produkt.csv', row.names = FALSE)




df %>% 
  filter(status == 'Aktiv', elnr >= 3000000, elnr <= 3899999) %>% 
  summarize(snitt = mean(ETIM.andel))

df %>% 
  filter(status == 'Aktiv', elnr >= 3300000, elnr <= 3399999) %>% 
  summarize(snitt = mean(ETIM.andel))

df %>% 
  filter(status == 'Aktiv') %>% 
  summarize(snitt = mean(ETIM.andel))


df %>% 
  filter(status == 'Aktiv') %>% 
  summarize(ant = mean(ETIM.maks))

df %>% 
  filter(status == 'Aktiv', elnr >= 3000000, elnr <= 3899999) %>% 
  summarize(ant = mean(ETIM.maks))

df %>% 
  filter(status == 'Aktiv', elnr >= 3300000, elnr <= 3399999) %>% 
  summarize(ant = mean(ETIM.maks))

