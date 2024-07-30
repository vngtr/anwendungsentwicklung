# Installationskommentare entfernen, wenn nicht notwendig
# #install.packages("shiny")
# #install.packages("ggplot2")
# #install.packages("shinyWidgets")
# #install.packages("rstudioapi")
# #install.packages("DT")
# #install.packages("ggcorrplot")

# Workspace leeren
rm(list = ls())
gc()

# Bibliotheken laden
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(rstudioapi)
library(DT) # Für die tabellarische Darstellung der Daten
library(ggcorrplot)

# Den Pfad auf das aktuelle Verzeichnis des R-Skripts setzen
setwd(dirname(getActiveDocumentContext()$path))

# Den bereinigten Datensatz einlesen
df <- read.csv("winequality_cleaned.csv")

# Die Spaltennamen umbenennen und dem Vektor "spalten" zuordnen
spalten <- c(
  "fixierte.säure" = "Fixierte Säure",
  "flüchtige.säure" = "Flüchtige Säure",
  "zitronensäure" = "Zitronensäure",
  "restzucker" = "Restzucker",
  "chloride" = "Chlorid",
  "freies.schwefeldioxid" = "Freies Schwefeldioxid",
  "gesamtschwefeldioxid" = "Gesamtschwefeldioxid",
  "dichte" = "Dichte",
  "pHWert" = "pH-Wert",
  "sulfate" = "Sulfat",
  "alkoholgehalt" = "Alkoholgehalt"
)

# Spalteneinheiten für die Anzeige im Histogramm
einheiten <- c(
  "fixierte.säure" = "(g/dm³)",
  "flüchtige.säure" = "(g/dm³)",
  "zitronensäure" = "(g/dm³)",
  "restzucker" = "(g/dm³)",
  "chloride" = "(g/dm³)",
  "freies.schwefeldioxid" = "(mg/dm³)",
  "gesamtschwefeldioxid" = "(mg/dm³)",
  "dichte" = "(g/cm³)",
  "pHWert" = "",
  "sulfate" = "(g/dm³)",
  "alkoholgehalt" = "(%)"
)

# User-Interface der Shiny-App erstellen
ui <- fluidPage(
  titlePanel("Shiny Dashboard: Weinqualität"),
  sidebarLayout(
    sidebarPanel(
      tags$div(style = "margin-top: 20px;"),
      selectInput("variable", "Kontinuierliche Variable auswählen:", choices = setNames(names(spalten), spalten)),
      selectInput("variable1", "Variable 1 für Scatterplot:", choices = setNames(names(spalten), spalten)),
      selectInput("variable2", "Variable 2 für Scatterplot:", choices = setNames(names(spalten), spalten)),
      verbatimTextOutput("statValues"),
      tags$div(style = "margin-top: 50px;"),
      radioButtons("options", "Klasse auswählen:",
                   choices = list(
                     "Qualität" = "qualität",
                     "Weintyp" = "typ",
                     "Qualität und Weintyp" = "qualität_typ"
                   ),
                   selected = "qualität"),
      tags$div(style = "margin-top: 50px;"),
      selectInput("alpha", "Signifikanzniveau auswählen:", choices = c(0.1, 0.05, 0.01, 0.005, 0.001), selected = 0.05)
    ),
    mainPanel(
      tabsetPanel(
        id = "results",
        tabPanel("Histogramm",
                 fluidRow(
                   column(12,
                          plotOutput("histPlot"),
                          tags$div(style = "margin-top: 50px;"),
                          plotOutput("qqPlot")
                   )
                 )
        ),
        tabPanel("Scatterplot",
                 fluidRow(
                   column(12,
                          plotOutput("scatterPlot"),
                          dataTableOutput("dataTable")
                   )
                 )
        ),
        tabPanel("Korrelationsmatrix",
                 fluidRow(
                   column(12,
                          plotOutput("correlationMatrix")
                   )
                 )
        ),
        tabPanel("Erweiterte Datenanalyse",
                 fluidRow(
                   column(12,
                          uiOutput("testHeader")),
                   verbatimTextOutput("testOutput")
                 ),
                 column(12,
                        tags$div(style = "margin-top: 50px;"),
                        plotOutput("violinPlot", height = "400px")
                 )
        )
      )
    )
  )
)

server <- function(input, output) {
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
  
  # Erweiterte Statistiken berechnen und anzeigen
  output$statValues <- renderPrint({
    variable <- input$variable
    data <- df[[variable]]
    cat("Mittelwert:", mean(data), "\n")
    cat("Median:", median(data), "\n")
    cat("Minimum:", min(data), "\n")
    cat("Maximum:", max(data), "\n")
    cat("Standardabweichung:", sd(data), "\n")
    cat("1. Quartil:", quantile(data, 0.25), "\n")
    cat("3. Quartil:", quantile(data, 0.75), "\n")
    cat("Interquartilsabstand (IQR):", IQR(data), "\n")
    cat("Schiefe:", e1071::skewness(data), "\n")
    cat("Kurtosis:", e1071::kurtosis(data), "\n")
  })
  
  output$testHeader <- renderUI({
    variable <- input$variable
    
    if (input$options == "typ") {
      h3(paste("Mann-Whitney-U-Test für", spalten[variable], "in Abhängigkeit des Weintyps:"))
    } else if (input$options == "qualität") {
      h3(paste("Spearman’s Rangkorrelationskoeffizient für", spalten[variable], "und Qualität:"))
    }
  })
  
  output$testOutput <- renderPrint({
    variable <- input$variable
    alpha <- as.numeric(input$alpha)
    
    if (input$options == "typ") {
      wilcox_result <- wilcox.test(df[[variable]] ~ df$typ)
      p_wert_w <- wilcox_result$p.value
      med_rot <- median(df[df$typ == "Rotwein", variable])
      med_weiß <- median(df[df$typ == "Weißwein", variable])
      mean_rot <- mean(df[df$typ == "Rotwein", variable])
      mean_weiß <- mean(df[df$typ == "Weißwein", variable])
      sd_rot <- sd(df[df$typ == "Rotwein", variable])
      sd_weiß <- sd(df[df$typ == "Weißwein", variable])
      n_rot <- sum(df$typ == "Rotwein")
      n_weiß <- sum(df$typ == "Weißwein")
      SDp <- sqrt(((n_rot - 1) * sd_rot^2 + (n_weiß - 1) * sd_weiß^2) / (n_rot + n_weiß - 2))
      cohen_d <- (mean_rot - mean_weiß) / SDp
      cohen_d_interpretation <- ifelse(cohen_d >= 0.5, "stark",
                                       ifelse(cohen_d >= 0.3, "mittel",
                                              ifelse(cohen_d >= 0.1, "schwach", "sehr schwach")))
      cat("W-Statistik:", wilcox_result$statistic, "\n")
      cat("p-Wert:", p_wert_w, "\n")
      cat("Median Rotwein:", med_rot, "| Median Weißwein:", med_weiß, "\n")
      cat("Effektstärke (Cohen’s d):", cohen_d, "\n")
      
      if (p_wert_w < alpha) {
        cat("Ergebnis: Es gibt einen statistisch signifikanten Unterschied (bei einem alpha =", alpha, ")\nzwischen der Variable", spalten[variable], "in Abhängigkeit des Weintyps. Nach Cohen (1992) ist dieser Unterschied", cohen_d_interpretation,".\n")
      } else {
        cat("Ergebnis: Der Unterschied zwischen der Variable", spalten[variable], "in Abhängigkeit des Weintyps ist nicht signifikant (bei einem alpha =", alpha, "). Nach Cohen (1992) ist dieser Unterschied", cohen_d_interpretation,".\n")
      }
      
    } else if (input$options == "qualität") {
      suppressWarnings({
        spearman_result <- cor.test(as.numeric(df[[variable]]), as.numeric(df$qualität), method = "spearman")
      })
      spearman_koeffizient <- spearman_result$estimate
      staerke <- ifelse(abs(spearman_koeffizient) >= 0.5, "stark",
                        ifelse(abs(spearman_koeffizient) >= 0.2, "mittleren", "schwach"))
      richtung <- ifelse(spearman_koeffizient > 0, "positiven", "negativen")
      cat("\u03C1:", spearman_koeffizient, "\n")
      cat("p-Wert:", spearman_result$p.value, "\n")
      
      if (spearman_result$p.value < alpha) {
        cat("Ergebnis: Es gibt einen", staerke, richtung, "Zusammenhang zwischen Qualität und", spalten[variable], ". Der Zusammenhang ist signifikant (bei einem alpha =", alpha, ").\n")
      } else {
        cat("Ergebnis: Es gibt einen", staerke, richtung, "Zusammenhang zwischen Qualität und", spalten[variable], ". Der Zusammenhang ist nicht signifikant (bei einem alpha =", alpha, ").\n")
      }
    } else {
      cat("Bitte wählen Sie Qualität oder Weintyp, um die statistischen Tests durchzuführen.")
    }
  })
  
  output$histPlot <- renderPlot({
    variable <- input$variable
    data <- df[[variable]]
    
    ggplot(df, aes(x = .data[[variable]])) +
      geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "black", color = "black", alpha = 0.5) +
      stat_function(fun = dnorm, args = list(mean = mean(data), sd = sd(data)), color = "red", linewidth = 1) +
      labs(
        title = paste("Histogramm von", spalten[variable], einheiten[variable]),
        x = paste(spalten[variable], einheiten[variable]),
        y = "Dichte"
      ) +
      theme_minimal()
  })
  
  output$dataTable <- renderDataTable({
    datatable(df, options = list(pageLength = 10), colnames = unname(spalten))
  })
  
  output$scatterPlot <- renderPlot({
    variable1 <- input$variable1
    variable2 <- input$variable2
    ggplot(df, aes(x = .data[[variable1]], y = .data[[variable2]])) +
      geom_point() +
      labs(
        title = paste("Scatterplot von", spalten[variable1], "und", spalten[variable2]),
        x = spalten[variable1],
        y = spalten[variable2]
      ) +
      theme_minimal()
  })
  
  output$correlationMatrix <- renderPlot({
    corr_df <- df[, !(names(df) %in% c("typ", "qualität"))]
    colnames(corr_df) <- unname(spalten[names(corr_df)])
    corr <- round(cor(corr_df), 2)
    ggcorrplot::ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE)
    
  })
  
  output$qqPlot <- renderPlot({
    variable <- input$variable
    data <- df[[variable]]
    ggplot(data.frame(sample = data), aes(sample = sample)) +
      stat_qq() +
      stat_qq_line() +
      labs(
        title = paste("QQ-Plot von", spalten[variable]),
        x = "Theoretische Quantile",
        y = "Beobachtete Quantile"
      ) +
      theme_minimal()
  })
}
# App starten
shinyApp(ui = ui, server = server)
