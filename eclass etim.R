# Sette arbeidsomr?de
# Windows
#setwd('C:/Users/paal/Desktop')
# Mac
setwd("~/Documents/OneDrive - Elektroforeningen (EFO)/R/Scripts/eClass")

# Laste biblioteker
library(tidyverse)
library(VennDiagram)

# Lese filer
etim7  <- readxl::read_xlsx('export_ETIM_7_electric_NO.xlsx')
eclass <- readxl::read_xlsx('Mapping ETIM 7-0 Electro to eClass 11-0 V1.xlsx') 

# Gir nytt navn for ? forenkle sammensl?ing senere
names(eclass)[1] <- 'class_id'
names(etim7)[1]  <- 'class_id'



### Deskriptiv statistikk
# Hvor mange eClasser?
ant_eclass <- length(unique(eclass$`eClass Class-ID`))
print(paste0('Ant. eClasser er ', ant_eclass))

# Hvor mange ETIM-klasser p? elektro?
ant_etim7e <- length(unique(etim7$class_id))
print(paste0('Ant. ETIMklasser er ', ant_etim7e))

# Isolere distinkte ETIMklasser for ? kunne telle 1-til-n overganger
eclass_mod <- eclass[!duplicated(eclass$class_id),]
plot(table(eclass_mod$`ETIM to eClass`))
table(eclass_mod$`ETIM to eClass`)

# Hvor mange klasser er 1 til x?
ant_etim_fra_eclass <- length(eclass_mod$class_id)
print(paste0('Ant. ETIMklasser i eClass-oversikten er ', ant_etim_fra_eclass))

en_til_n <- ant_etim_fra_eclass - table(eclass_mod$`ETIM to eClass`)[[1]] 
print(paste0('Ant. ETIMklasser som *ikke* har 1-til-1 forhold til eClasser er ', en_til_n))

# Hvor mange %?
print(paste0('Andelen ETIM til eClass som *ikke* er 1-til-1 er ', as.numeric(format((en_til_n / ant_etim_fra_eclass) * 100, digits = 2)), '%'))



### Sammenligning mellom de to oversiktene
# Overlappet mellom de to filene p? ETIM-klasser
overlapp <- inner_join(eclass, etim7, by = 'class_id')
overlapp_etim <- length(unique(overlapp$class_id))

# Ant. ETIM-klasser som kun er i ETIM-filen
join_etim <- left_join(etim7, eclass, by = 'class_id')
join_etim_lengde <- length(unique(join_etim$class_id))

# Ant. ETIM-klasser som kun er i eClass-filen
join_eclass <- left_join(eclass, etim7, by = 'class_id')
join_eclass_lengde <- length(unique(join_eclass$class_id))

# Er begge filene identiske p? ETIMklasser?
a <- assertthat::are_equal(overlapp_etim, join_etim_lengde, join_eclass_lengde)
if (a == TRUE){
  print("Begge filene har identiske ETIMklasser")
} else {
  print("Filene har ulike ETIMklasser")
}

# Venn-diagram overlapp
grid.newpage()
draw.pairwise.venn(ant_etim7e, ant_etim_fra_eclass, overlapp_etim, category = c("ETIM7", "ETIM7 fra eClass-oversikt"), lty = rep("blank", 2),
                   fill = c("light blue", "pink"), alpha = rep(0.5, 2), cat.pos = c(0,0), cat.dist = rep(0.025, 2))

