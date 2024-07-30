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
                   column(6,
                          plotOutput("histPlot")
                   ),
                   column(6,
                          plotOutput("qqPlot")
                   )
                 ),
                 fluidRow(
                   column(12,
                          radioButtons("wineType", "Weintyp auswählen:",
                                       choices = list("Alle" = "all", "Rotwein" = "Rotwein", "Weißwein" = "Weißwein"),
                                       selected = "all"),
                          selectInput("Vergleichsvariable", "Zweite Variable auswählen:", choices = setNames(names(spalten), spalten)),
                          actionButton("scatterButton", "Scatter"),
                          actionButton("correlationButton", "Korrelation"),
                          plotOutput("dynamicPlot"),
                          verbatimTextOutput("correlationResult")
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

server <- function(input, output, session) {
  filteredData <- reactive({
    if (input$wineType == "all") {
      df
    } else {
      subset(df, typ == input$wineType)
    }
  })
  
  output$violinPlot <- renderPlot({
    data <- filteredData()
    plot_title <- paste("Violinplot von", spalten[input$variable])
    
    if (input$options == "qualität_typ") {
      ggplot(data, aes(x = as.factor(qualität), y = .data[[input$variable]], fill = typ)) +
        geom_violin(trim = FALSE, position = position_dodge(width = 0.5), color = "black", alpha = 0.7) +
        geom_boxplot(width = 0.1, position = position_dodge(width = 0.5), color = "black", alpha = 0.5) +
        scale_fill_manual(values = c("Rotwein" = "#E74C3C", "Weißwein" = "#F5CBA7")) +
        labs(
          title = paste(plot_title, "nach Weintyp und Qualität"),
          x = "Qualität",
          y = spalten[input$variable],
          fill = "Weintyp"
        ) +
        theme_minimal() +
        theme(legend.position = "none")
      
    } else if (input$options == "qualität") {
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
        theme(legend.position = "none")
      
    } else if (input$options == "typ") {
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
        theme(legend.position = "none")
    }
  })
  
  output$statValues <- renderPrint({
    data <- filteredData()
    variable <- input$variable
    cat("Mittelwert:", mean(data[[variable]]), "\n")
    cat("Median:", median(data[[variable]]), "\n")
    cat("Minimum:", min(data[[variable]]), "\n")
    cat("Maximum:", max(data[[variable]]), "\n")
    cat("Standardabweichung:", sd(data[[variable]]), "\n")
    cat("1. Quartil:", quantile(data[[variable]], 0.25), "\n")
    cat("3. Quartil:", quantile(data[[variable]], 0.75), "\n")
    cat("Interquartilsabstand (IQR):", IQR(data[[variable]]), "\n")
    cat("Schiefe:", e1071::skewness(data[[variable]]), "\n")
    cat("Kurtosis:", e1071::kurtosis(data[[variable]]), "\n")
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
    data <- filteredData()
    variable <- input$variable
    alpha <- as.numeric(input$alpha)
    
    if (input$options == "typ") {
      wilcox_result <- wilcox.test(data[[variable]] ~ data$typ)
      p_wert_w <- wilcox_result$p.value
      med_rot <- median(data[data$typ == "Rotwein", variable])
      med_weiß <- median(data[data$typ == "Weißwein", variable])
      mean_rot <- mean(data[data$typ == "Rotwein", variable])
      mean_weiß <- mean(data[data$typ == "Weißwein", variable])
      sd_rot <- sd(data[data$typ == "Rotwein", variable])
      sd_weiß <- sd(data[data$typ == "Weißwein", variable])
      n_rot <- sum(data$typ == "Rotwein")
      n_weiß <- sum(data$typ == "Weißwein")
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
        spearman_result <- cor.test(as.numeric(data[[variable]]), as.numeric(data$qualität), method = "spearman")
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
    data <- filteredData()
    variable <- input$variable
    plot_color <- if (input$wineType == "Rotwein") "red" else if (input$wineType == "Weißwein") "#F5CBA7" else "black"
    
    ggplot(data, aes(x = .data[[variable]])) +
      geom_histogram(aes(y = after_stat(density)), bins = 30, fill = plot_color, color = "black", alpha = 0.5) +
      stat_function(fun = dnorm, args = list(mean = mean(data[[variable]]), sd = sd(data[[variable]])), color = "red", linewidth = 1) +
      labs(
        title = paste("Histogramm von", spalten[variable], einheiten[variable]),
        x = paste(spalten[variable], einheiten[variable]),
        y = "Dichte"
      ) +
      theme_minimal()
  })
  
  output$dataTable <- renderDataTable({
    datatable(filteredData(), options = list(pageLength = 10), colnames = unname(spalten))
  })
  
  output$scatterPlot <- renderPlot({
    data <- filteredData()
    req(input$Vergleichsvariable)
    variable1 <- input$variable
    variable2 <- input$Vergleichsvariable
    point_color <- if (input$wineType == "Rotwein") "red" else if (input$wineType == "Weißwein") "#F5CBA7" else "black"
    
    ggplot(data, aes(x = .data[[variable1]], y = .data[[variable2]])) +
      geom_point(color = point_color) +
      labs(
        title = paste("Scatterplot von", spalten[variable1], "und", spalten[variable2]),
        x = spalten[variable1],
        y = spalten[variable2]
      ) +
      theme_minimal()
  })
  
  observeEvent(input$scatterButton, {
    output$dynamicPlot <- renderPlot({
      data <- filteredData()
      variable1 <- input$variable
      variable2 <- input$Vergleichsvariable
      point_color <- if (input$wineType == "Rotwein") "red" else if (input$wineType == "Weißwein") "#F5CBA7" else "black"
      
      ggplot(data, aes(x = .data[[variable1]], y = .data[[variable2]])) +
        geom_point(color = point_color) +
        labs(
          title = paste("Scatterplot von", spalten[variable1], "und", spalten[variable2]),
          x = spalten[variable1],
          y = spalten[variable2]
        ) +
        theme_minimal()
    })
    
    output$correlationResult <- renderPrint(NULL) # Clear correlation result when scatter plot is shown
  })
  
  observeEvent(input$correlationButton, {
    output$correlationResult <- renderPrint({
      data <- filteredData()
      variable1 <- input$variable
      variable2 <- input$Vergleichsvariable
      corr <- cor(data[[variable1]], data[[variable2]], use = "complete.obs")
      cat("Korrelation zwischen", spalten[variable1], "und", spalten[variable2], ":", round(corr, 2))
    })
    
    output$dynamicPlot <- renderPlot(NULL) # Clear scatter plot when correlation result is shown
  })
  
  output$qqPlot <- renderPlot({
    data <- filteredData()
    variable <- input$variable
    point_color <- if (input$wineType == "Rotwein") "red" else if (input$wineType == "Weißwein") "#F5CBA7" else "black"
    
    ggplot(data.frame(sample = data[[variable]]), aes(sample = sample)) +
      stat_qq(color = point_color) +
      stat_qq_line() +
      labs(
        title = paste("QQ-Plot von", spalten[variable]),
        x = "Theoretische Quantile",
        y = "Beobachtete Quantile"
      ) +
      theme_minimal()
  })
  
  observeEvent(input$results, {
    if (input$results == "Erweiterte Datenanalyse") {
      updateRadioButtons(session, "wineType", selected = "all")
    }
  })
}

# App starten
shinyApp(ui = ui, server = server)
