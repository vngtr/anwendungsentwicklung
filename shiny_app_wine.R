#install.packages("shiny")
#install.packages("ggplot2")
#install.packages("shinyWidgets")

rm(list = ls())
gc()

library(shiny)
library(shinyWidgets)
library(ggplot2)

#setwd("C:/Users/jojoh/OneDrive/Dokumente/Studium/Data Science & Business Analytics/Term 6/Anwendungsentwicklung/Prüfungsleistung")

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
  titlePanel("Shiny Dashboard: Weinqualität"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Kontinuierliche Variable auswählen:", choices = setNames(names(spalten), spalten)),
      radioButtons("options", "Klasse auswählen:", 
                   choices = list(
                     "Qualität" = "qualität", 
                     "Weintyp" = "typ",
                     "Qualität und Weintyp" = "qualität_typ"
                   ),
                   selected = "qualität")
    ),
  mainPanel(
    tabsetPanel(
      id = "results",
      tabPanel("Daten explorieren"),
      tabPanel("erweiterte Datenanalyse", 
                fluidRow(
                  column(12,
                         selectInput("alpha", "Signifikanzniveau:", choices = c(0.1, 0.05, 0.01, 0.005, 0.001), selected = 0.05),
                         verbatimTextOutput("testOutput")
                  ),
                  column(12,
                        tags$div(style = "margin-top: 100px;"),  # Erhöhter Abstand vor dem Plot
                        plotOutput("violinPlot", height = "400px")
                  ),
                )
              ),
        )
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
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
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
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
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
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          legend.position = "none"
        )
    }
  })
  
  output$testOutput <- renderPrint({
    variable <- input$variable
    alpha <- as.numeric(input$alpha)
    
    if (input$options == "typ") {
      wilcox_result <- wilcox.test(df[[variable]] ~ df$typ)
      p_wert_w <- wilcox_result$p.value
      med_rot <- median(df[df$typ =="Rotwein", variable])
      med_weiß <- median(df[df$typ =="Weißwein", variable])
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
      cat("Mann-Whitney-U-Test für die Variable", spalten[variable], "zwischen Rotwein und Weißwein:\n")
      cat("W-Statistik:", wilcox_result$statistic, "\n")
      cat("p-Wert:", p_wert_w, "\n")
      cat("Median Rotwein:",med_rot, "| Median Weißwein:",med_weiß,"\n")
      cat("Effektstärke (Cohen´s d):", cohen_d, "\n")
      
      if (p_wert_w < alpha) {
        cat("Ergebnis: Es gibt einen statistisch signifikanten Unterschied (bei einem alpha =", alpha, ")\nzwischen der Variable", spalten[variable], "in Abhängigkeit des Weintyps. Nach Cohen (1992) ist dieser Unterschied" ,cohen_d_interpretation, ".\n")
      } else {
        cat("Ergebnis: Der Unterschied zwischen der Variable", spalten[variable], "in Abhängigkeit des Weintyps ist nicht signifikant (bei einem alpha =", alpha, "). Nach Cohen (1992) ist dieser Unterschied" ,cohen_d_interpretation, ".\n")
      }
    } else if (input$options == "qualität") {
      spearman_result <- suppressWarnings(cor.test(df[[variable]], df$qualität, method="spearman"))
      spearman_koeffizient <- spearman_result$estimate
      staerke <- ifelse(abs(spearman_koeffizient) >= 0.5, "stark",
                        ifelse(abs(spearman_koeffizient) >= 0.2, "mittleren", "schwach"))
      richtung <- ifelse(spearman_koeffizient > 0, "positiven", "negativen")
      cat("Rangkorrelationskoeffizient nach Spearman für", spalten[variable], "und der Qualität:\n")
      cat("\u03C1:", spearman_koeffizient, "\n")
      cat("p-Wert:", spearman_result$p.value, "\n")
      
      if (spearman_result$p.value < alpha) {
        cat("Ergebnis: Es gibt einen", staerke, richtung, "Zusammenhang zwischen Qualität und", spalten[variable],".Der Zusammenhang ist signifikant (bei einem alpha =", alpha,").\n")
      } else {
        cat("Ergebnis: Es gibt einen", staerke, richtung, "Zusammenhang zwischen Qualität und", spalten[variable],".Der Zusammenhang ist nicht signifikant (bei einem alpha =", alpha,").\n")
      }
    } else {
      cat("Bitte wählen Sie Qualität oder Weintyp um die statistischen Tests durchzuführen.")
    }
  })
}
###
# Run the Shiny app
shinyApp(ui = ui, server = server)