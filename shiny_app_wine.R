#install.packages("shiny")
#install.packages("ggplot2")
#install.packages("shinyWidgets")
#install.packages("rstudioapi")

#Workspace leeren
rm(list = ls())
gc()

#Bibliotheken laden
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(rstudioapi)

#Den Pfad auf das aktuelle Verzeichnis des R-Skrits setzen
setwd(dirname(getActiveDocumentContext()$path))

# Den bereinigten Datensatz einlesen
df <- read.csv("winequality_cleaned.csv")

# Die Spaltennamen umbenennen und dem Vektor "spalten" zuordnen 
spalten <- c(
  "fixierte.säure" = "Fixierte Säure",
  "flüchtige.säure" = "Flüchtige Säure",
  "zitronensäure" = "Zitronensäure",
  "restzucker" = "Restzucker",
  "chloride" = "Chloride",
  "freies.schwefeldioxid" = "Freies Schwefeldioxid",
  "gesamtschwefeldioxid" = "Gesamtschwefeldioxid",
  "dichte" = "Dichte",
  "pHWert" = "pH-Wert",
  "sulfate" = "Sulfate",
  "alkoholgehalt" = "Alkoholgehalt"
)

# User-Interface der Shiny-App erstellen
ui <- fluidPage(                                          #Hautcontainer-Layout (für versch. Bildschirmgrößen) 
  titlePanel("Shiny Dashboard: Weinqualität"),            #Dashboard Titel
  sidebarLayout(                                          #Seitenleiste (über alle Tabs) & Hauptbereich
    sidebarPanel(                                         #Benutzersteuerung & Eingabeelemente
      tags$div(style = "margin-top: 20px;"),              #Abstände festlegen
      selectInput("variable", "Kontinuierliche Variable auswählen:", choices = setNames(names(spalten), spalten)),          #Dropdown-Menü für kont. Variablen
      verbatimTextOutput("statValues"),
      tags$div(style = "margin-top: 50px;"),
      radioButtons("options", "Klasse auswählen:",        #Auswahlmöglichkeiten der Klassen
                   choices = list(
                     "Qualität" = "qualität", 
                     "Weintyp" = "typ",
                     "Qualität und Weintyp" = "qualität_typ"
                   ),
                   selected = "qualität"),               #Defaultauswahl Qualität
      tags$div(style = "margin-top: 50px;"),
      selectInput("alpha", "Signifikanzniveau auswählen:", choices = c(0.1, 0.05, 0.01, 0.005, 0.001), selected = 0.05),    #Dropdown-Menü zur Auswahl Signifikanzniveau
      
    ),
    mainPanel(                                                           #Hauptbereich definieren (Anzeige der Ergebnisse: Tests & Plot)
      tabsetPanel(                                                       #Registerkarten/Tabs zur Navigation erstellen
        id = "results",                                                  #Dem Panel eine ID zuweisen für den Server-Code
        tabPanel("Daten explorieren",                                   #Registerkartenpanel Datenexploration  
                  fluidRow(                                              #erstellt Zeile  
                    column(12,                                           #Spalte für Histogramm und Data Table über gesamte Seite 
                           plotOutput("histPlot"),                       #Plot des Histograms 
                           tags$div(style = "margin-top: 50px;"),        #Erhöhter Abstand vor dem Plot
                           dataTableOutput("dataTable")                  #Anzeige der Daten in Tabellenformat 
                    ),
                    column(12, 
                           dataTableOutput('dttbl')
                           )
                  )
        ),
        tabPanel("erweiterte Datenanalyse",                              #Registerkartenpanel Datenanalyse
                 fluidRow(                                               #erstellt Zeile
                   column(12,                                            #Spalte für Test-Output (über gesamte Breite) als Text mit dynamischem UI Inhalt
                          uiOutput("testHeader")),
                   verbatimTextOutput("testOutput")
                 ),
                 column(12,                                              #Spalte für Violin-Plot (über gesamte Breite) mit dynamischem UI Inhalt
                        tags$div(style = "margin-top: 50px;"),           #Erhöhter Abstand vor dem Plot
                        plotOutput("violinPlot", height = "400px")
                 ),
        )
      ),
    )
  )
)
server <- function(input, output) {                                   #Server-Funktion (enthält Serverlogik)
  output$violinPlot <- renderPlot({                                   #Violin-Plot als Ausgabeelement definieren
    data <- df
    plot_title <- paste("Violinplot von", spalten[input$variable])    #Titel, basierend auf ausgwählte Variable
    
    # 3 verschiedene Violin-Plots in Abhängigkeit der ausgewählten Klasse
    if (input$options == "qualität_typ") {                            #Plot 1: Auswahl Qualität und Weintyp
      ggplot(data, aes(x = as.factor(qualität), y = .data[[input$variable]], fill = typ)) +
        geom_violin(trim = FALSE, position = position_dodge(width = 0.5), color = "black", alpha = 0.7) +
        geom_boxplot(width = 0.1, position = position_dodge(width = 0.5), color = "black", alpha = 0.5) +
        scale_fill_manual(values = c("Rotwein" = "#E74C3C", "Weißwein" = "#F5CBA7")) +         # Farben für Rot- und Weißwein definieren
        labs(                                                         #Titel und Beschriftungen festlegen
          title = paste(plot_title, "nach Weintyp und Qualität"), 
          x = "Qualität", 
          y = spalten[input$variable],
          fill = "Weintyp"
        ) +
        theme_minimal() +                                             #Schriftgrößen und Legendenposition anpassen
        theme(
          plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          legend.position = "right"
        )
      
    } else if (input$options == "qualität") {                         #Plot 2: Bei Auswahl "Qualität"
      ggplot(data, aes(x = as.factor(qualität), y = .data[[input$variable]], fill = as.factor(qualität))) +
        geom_violin(trim = FALSE, color = "black", alpha = 0.7) +
        geom_boxplot(width = 0.1, position = position_dodge(width = 0.9), color = "black", alpha = 0.5) +
        scale_fill_grey(start = 0.5, end = 1) +
        labs(
          title = paste(plot_title, "nach Qualität"), 
          x = "Qualität", 
          y = spalten[input$variable]
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          legend.position = "none"
        )
      
    } else if (input$options == "typ") {                               #Plot 3: Bei Auswahl Weintyp
      ggplot(data, aes(x = typ, y = .data[[input$variable]], fill = typ)) +
        geom_violin(trim = FALSE, color = "black", alpha = 0.7) +
        geom_boxplot(width = 0.1, position = position_dodge(width = 0.9), color = "black", alpha = 0.5) +
        scale_fill_manual(values = c("Rotwein" = "#E74C3C", "Weißwein" = "#F5CBA7")) +
        labs(
          title = paste(plot_title, "nach Weintyp"), 
          x = "Weintyp", 
          y = spalten[input$variable],
          fill = "Weintyp"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          legend.position = "none"
        )
    }
  })
  
  output$statValues <- renderPrint({         #Berechnung und Anzeige (in der Sidebar) statistischer Werte
    variable <- input$variable               #ruft die vom Benutzer ausgwählte Variable auf
    data <- df[[variable]]
    cat("Mittelwert:", mean(data), "\n")          #Ausgabe Mittelwert
    cat("Median:", median(data), "\n")            #Ausgabe Median
    cat("Minimum:", min(data), "\n")              #Ausgabe Minimum
    cat("Maximum:", max(data), "\n")              #Ausgabe Maximum
    cat("Standardabweichung:", sd(data), "\n")    #Ausgabe Standardabweichung
  })
  
  output$testHeader <- renderPrint({              #Überschrift für entsprechenden Test (auf Basis der Benutzerauswahl)
    variable <- input$variable
    
    if (input$options == "typ") {
      h3(paste("Mann-Whitney-U-Test für", spalten[variable], "in Abhängigkeit des Weintyps:"))
    } else if (input$options == "qualität") {
      h3(paste("Spearman’s Rangkorrelationskoeffizient für", spalten[variable], "und Qualität:"))
    }
  })
  
  output$testOutput <- renderPrint({              #Ergebnisse des entsprechenden Tests (auf Basis der Benutzerauswahl)
    variable <- input$variable
    alpha <- as.numeric(input$alpha)              #Konvertiert ausgewähltes Signifikanzniveau in numerische Variable
    
  #Durchführung der Tests (in Abhängigkeit der ausgwählten Klasse)  
    if (input$options == "typ") {
      wilcox_result <- wilcox.test(df[[variable]] ~ df$typ)                                      #Test 1 (bei Auswahl Weintyp): Mann-Whitney-U-Test mit ausgewählter Variable
      p_wert_w <- wilcox_result$p.value                                                          #P-Wert übergeben
      med_rot <- median(df[df$typ =="Rotwein", variable])                                        #Statistische Werte und Stichprobengröße für die Effektstärke berechnen
      med_weiß <- median(df[df$typ =="Weißwein", variable])
      mean_rot <- mean(df[df$typ == "Rotwein", variable])
      mean_weiß <- mean(df[df$typ == "Weißwein", variable])
      sd_rot <- sd(df[df$typ == "Rotwein", variable])
      sd_weiß <- sd(df[df$typ == "Weißwein", variable])
      n_rot <- sum(df$typ == "Rotwein")
      n_weiß <- sum(df$typ == "Weißwein")
      SDp <- sqrt(((n_rot - 1) * sd_rot^2 + (n_weiß - 1) * sd_weiß^2) / (n_rot + n_weiß - 2))
      cohen_d <- (mean_rot - mean_weiß) / SDp                                                   #Cohen´s d berechnen und Beurteilung der Effektstärke festlegen
      cohen_d_interpretation <- ifelse(cohen_d >= 0.5, "stark", 
                                       ifelse(cohen_d >= 0.3, "mittel", 
                                              ifelse(cohen_d >= 0.1, "schwach", "sehr schwach")))
      cat("W-Statistik:", wilcox_result$statistic, "\n")                                        #Ausgabe der Ergebnisse in Abhängigkeit der Signifikanz 
      cat("p-Wert:", p_wert_w, "\n")
      cat("Median Rotwein:",med_rot, "| Median Weißwein:",med_weiß,"\n")
      cat("Effektstärke (Cohen´s d):", cohen_d, "\n")
      
      if (p_wert_w < alpha) {
        cat("Ergebnis: Es gibt einen statistisch signifikanten Unterschied (bei einem alpha =", alpha, ")\nzwischen der Variable", spalten[variable], "in Abhängigkeit des Weintyps. Nach Cohen (1992) ist dieser Unterschied" ,cohen_d_interpretation,".\n")
      } else {
        cat("Ergebnis: Der Unterschied zwischen der Variable", spalten[variable], "in Abhängigkeit des Weintyps ist nicht signifikant (bei einem alpha =", alpha, ").Nach Cohen (1992) ist dieser Unterschied" ,cohen_d_interpretation,".\n")
      }
      
      
    } else if (input$options == "qualität") {                                                           #Test 2 (bei Auswahl der Qualität): Spearman´s Rangkorrelationskoeffizient
      suppressWarnings({
        spearman_result <- cor.test(df[[variable]], df$qualität, method = "spearman")                   #Warnmeldung ("Kann exakten p-Wert bei Bindungen nicht berechnen") unterdrücken
      })                    
      spearman_koeffizient <- spearman_result$estimate
      staerke <- ifelse(abs(spearman_koeffizient) >= 0.5, "stark",                                       #Festlegen der Wertebereiche für die Interpretation der Stärke und der Richtung
                        ifelse(abs(spearman_koeffizient) >= 0.2, "mittleren", "schwach"))
      richtung <- ifelse(spearman_koeffizient > 0, "positiven", "negativen")
      cat("\u03C1:", spearman_koeffizient, "\n")
      cat("p-Wert:", spearman_result$p.value, "\n")
      
      if (spearman_result$p.value < alpha) {
        cat("Ergebnis: Es gibt einen", staerke, richtung,"Zusammenhang zwischen Qualität und", spalten[variable], ".Der Zusammenhang ist signifikant (bei einem alpha =", alpha,").\n")
      } else {
        cat("Ergebnis: Es gibt einen", staerke, richtung,"Zusammenhang zwischen Qualität und", spalten[variable],".Der Zusammenhang ist nicht signifikant (bei einem alpha =", alpha,").\n")
      }
    } else {
      cat("Bitte wählen Sie Qualität oder Weintyp um die statistischen Tests durchzuführen.")            #Gibt eine Nachricht aus, wenn weder "typ" noch "qualität" als Option ausgewählt ist.
    }
  })
  output$histPlot <- renderPlot({                                                                        #
    variable <- input$variable
    data <- df[[variable]]
    ggplot(df, aes(x = .data[[variable]])) +
      geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
      labs(
        title = paste("Histogramm von", spalten[variable]),
        x = spalten[variable],
        y = "Häufigkeit"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)
      )
  })
  
  output$dttbl<- renderDataTable(df, options = list(pageLenght=5))
}

#App starten
shinyApp(ui = ui, server = server)