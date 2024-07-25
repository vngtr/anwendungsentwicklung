#install.packages("shiny")
#install.packages("ggplot2")
#install.packages("shinyWidgets")

rm(list = ls())
gc()

library(shiny)
library(shinyWidgets)
library(ggplot2)

setwd("C:/Users/jojoh/OneDrive/Dokumente/Studium/Data Science & Business Analytics/Term 6/Anwendungsentwicklung/Prüfungsleistung")

df <- read.csv("winequality_cleaned.csv")

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

ui <- fluidPage(
  titlePanel("Shiny App: Weinqualität"),
  fluidRow(
    column(4,
           selectInput("variable", "Interessierendes Attribut auswählen:", choices = setNames(names(spalten), spalten)),
           radioButtons("options", "Klasse auswählen:", 
                        choices = list(
                          "Qualität" = "qualität", 
                          "Weintyp" = "typ",
                          "Qualität und Weintyp" = "qualität_typ"
                        ),
                        selected = "qualität")
    ),
    column(8,
           selectInput("alpha", "Signifikanzniveau:", choices = c(0.5,0.1, 0.05, 0.01), selected = 0.05),
           verbatimTextOutput("testOutput")
    )
  ),
  fluidRow(
    column(12,
           tags$div(style = "margin-top: 50px;"),  # Erhöhter Abstand vor dem Plot
           plotOutput("violinPlot", height = "400px")
    )
  )
)

server <- function(input, output) {
  output$violinPlot <- renderPlot({
    data <- df
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
        theme(
          plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          legend.position = "right"
        )
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
        theme(
          plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          legend.position = "none"
        )
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
        theme(
          plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          legend.position = "none"
        )
    }
  })
  
  output$testOutput <- renderPrint({
    variable <- input$variable
    alpha <- input$alpha
    
    if (input$options == "typ") {
      t_test_result <- t.test(df[[variable]] ~ df$typ)
      cat("T-Test für", spalten[variable], "zwischen Rotwein und Weißwein\n")
      cat("t-Wert:", t_test_result$statistic, "\n")
      cat("p-Wert:", t_test_result$p.value, "\n")
      if (t_test_result$p.value < alpha) {
        cat("Ergebnis: Es gibt einen statistisch signifikanten Unterschied (bei einem alpha =", alpha, ")\nzwischen der Variable", spalten[variable], "in Abhängigkeit des Weintyps\n")
      } else {
        cat("Ergebnis: Der Unterschied zwischen der Variable", spalten[variable], "in Abhängigkeit des Weintyps\nist nicht signifikant (bei einem alpha =", alpha, ")\n")
      }
    } else if (input$options == "qualität") {
      kruskal_test_result <- kruskal.test(df[[variable]] ~ as.factor(df$qualität))
      cat("Kruskal-Wallis-Test für", spalten[variable], "nach Qualitätsstufen\n")
      cat("Chi-Quadrat-Wert:", kruskal_test_result$statistic, "\n")
      cat("p-Wert:", kruskal_test_result$p.value, "\n")
      if (kruskal_test_result$p.value < alpha) {
        cat("Ergebnis: Es gibt einen statistisch signifikanten Unterschied (bei einem alpha =", alpha, ")\nzwischen der Variable", spalten[variable], "in Abhängigkeit der Qualitätsstufen\n")
      } else {
        cat("Ergebnis: Der Unterschied zwischen der Variable", spalten[variable], "in Abhängigkeit der Qualitätsstufen\nist nicht signifikant (bei einem alpha =", alpha, ")\n")
      }
    } else {
      cat("Bitte eine gültige Option auswählen, um den Test durchzuführen.")
    }
  })
}
###
# Run the Shiny app
shinyApp(ui = ui, server = server)