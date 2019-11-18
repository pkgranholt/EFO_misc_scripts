# # Transformation from all members
# Set wd
setwd("~/Documents/R/Google Analytics")

# Read csv-file
ga <- read.csv(file='Egenskapsdata.csv', sep = ';')

# Choose only selected members' data
ga_norsk_wavin <- ga[ga$Lev..nummer == 632, ][, -c(2:10)]
ga_ipas        <- ga[ga$Lev..nummer == 1229,][, -c(2:10)]
ga_hellermann  <- ga[ga$Lev..nummer == 53,  ][, -c(2:10)]

# Create strings with elnumbers
ga_norsk_wavin_string <- paste(ga_norsk_wavin, collapse = '|')
ga_ipas_string        <- paste(ga_ipas       , collapse = '|')
ga_hellermann_string  <- paste(ga_hellermann , collapse = '|')

# Export to text files
write(ga_norsk_wavin_string, 'norsk_wavin_string.txt')
write(ga_ipas_string,               'ipas_string.txt')
write(ga_hellermann_string,   'hellermann_string.txt')