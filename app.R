#setwd("~/Documents/R/ETIM-andel og fakturering/shiny_efobasen")
library(shiny)
library(ggplot2)
library(dplyr)
library(rsconnect)
library(DT)
library(xlsx)

oversikt <- read.csv("oversikt.csv", stringsAsFactors = FALSE)
detaljer <- read.csv("detaljer.csv", stringsAsFactors = FALSE)

names(oversikt) <- c("Medlemsnummer", "Navn på bedriften", "Aktive og avviste elnummer",
                     "Elnummer med minst 70% ETIM-egenskaper", "Elnummer med for få
                     ETIM-egenskaper", "Andel av produktene som har nok ETIM-egenskaper",
                     "Utvalg")
names(detaljer) <- c("Elnummer", "Medlemsnummer", "Navn på bedriften", "ETIM-klasse",
                     "Utfylte ETIM-egenskaper", "Antall ETIM-egenskapsfelter",
                     "ETIM-andel", "Utvalg")


ui <- fluidPage(
  titlePanel("Datakvalitet i EFObasen"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("levnavnOutput"),
      br(),
      radioButtons("typeInput", "Jeg vil se ifm.",
                   choices = c("Dobbeltfakturering", "Kvalitetssjekk"),
                   selected = "Dobbeltfakturering"),
      
      'Du kan velge mellom "Dobbeltfakturering" og "Kvalitetssjekk" i visningen. Hvis du ønsker å øke datakvaliteten slik at du ikke blir
      dobbeltfakturert til neste år, velger du "Dobbeltfakturering". Hvis du ønsker å nå 100% dekning av produkter som har 
      minst 70% ETIM-egenskaper, velger du "Kvalitetssjekk". Forskjellen mellom de to kommer av fritak for dobbeltfakturering, som gjør
      at noen produkter ikke blir dobbeltfakturert, selv om datakvaliteten ikke er god nok. Vurderingen har vært som
      følger; hvis produktet oppfyller et av de følgende kriteriene blir det ikke dobbeltfakturert:',
      br(), br(),
      '1) Produktet har tilstrekkelig med ETIM-egenskaper (minst 70%).',
      br(), br(),
      '2) Produktet står som "Til godkjenning" eller "Preregistrert".',
      br(), br(),
      '3) Produktet ble fjernet som aktivt i mars 2018 da vi satt alle grunndatafelter til obligatoriske. De produktene som
      ikke hadde grunndatafelter ble satt til "Preregistrert", og blir fremdeles fakturert ordinært, men de er ikke lengre
      synlige og søkbare i EFObasen.',
      br(), br(),
      '4) Knut Vibstad har gjennomgått alle produkter uten ETIM-klasse, og der det vurderes at det ikke er 
      mulig å sette en ETIM-klasse, har elnummeret blitt fritatt for dobbeltfakturering.',
      br(), br(), br(), br(),
      'Hvis du ønsker å komme i kontakt med oss kan du gjøre det her:',
      br(),
      'tlf. 22 62 63 50',
      br(),
      'e-post: elektroforeningen@efo.no',
      br(), br(), br(), br(),
      'Hvis tabellene til høyre ikke vises, kan du prøve å bytte nettleser.'
      
      
    ),
    mainPanel(
      h3('Status på produkter i EFObasen'),
      br(),
      tableOutput("oversiktsvisning"),
      br(), br(),
      h3('Produkter som ikke har tilstrekkelig med ETIM-egenskaper (70%):'),
      br(),
      DT::dataTableOutput("detaljert_visning"),
      br(), br(),
      downloadButton("downloadData", label = "Last ned listen"),
      br(), br(), br(), br(),
      paste("Denne siden ble sist oppdatert fra EFObasen 20. mars kl. 11:00")
    )
  )
  )


server <- function(input, output) {
  
  output$levnavnOutput <- renderUI({
    selectInput("levnavnInput", "Velg bedrift",
                sort(unique(oversikt$`Navn på bedriften`)),
                selected = "nVent Thermal Norway AS")
  })
  
  output$oversiktsvisning <- renderTable({
    if(is.null(input$levnavnInput)){
    return(NULL)
  }
    
      oversikt %>% 
      filter(`Navn på bedriften` == input$levnavnInput,
             Utvalg == input$typeInput
      )

  })
  
  output$detaljert_visning <- renderDataTable({
    
    if(is.null(input$levnavnInput)){
      return(NULL)
    }
    
    detaljer_filtrert <<-
      detaljer %>% 
        filter(`Navn på bedriften` == input$levnavnInput,
               `ETIM-andel` < 70,
               Utvalg == input$typeInput
        )
    detaljer_filtrert
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-" ,Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(detaljer_filtrert, file, row.names = FALSE)
    }
  )
}


shinyApp(ui = ui, server = server) # siste linje med kode
