#install.packages("psych")
#install.packages("e1071")

# Workspace leeren
rm(list = ls())
gc()

library(psych)
library(dplyr)
library(e1071)

# Pfad ändern
# Den Pfad auf das aktuelle Verzeichnis des R-Skripts setzen
setwd(dirname(getActiveDocumentContext()$path))

# Datensatz einlesen
df<-read.csv("winequalityN.csv")

head(df)

# Missing Values
colSums(is.na(df))


# Fehlende Werte durch den Mittelwert der Spalte ersetzen. Da Missing Values nur in den numerischen Spalten vorkommen, prüft dies Schleife zuvor
for (col in names(df)) {
  if (col != "type") {
    if (is.numeric(df[[col]])) {
      df[[col]][is.na(df[[col]])] <- mean(df[[col]], na.rm = TRUE)
    }
  }
}

# erneute Prüfung zeigt keine NV mehr
any(is.na(df))

# Deskriptive Statistik
table(df$type) #1599 Rotweine, 4898 Weißweine
describe(df)

# Spaltennamen ändern
names(df) <- c("typ", "fixierte.säure","flüchtige.säure","zitronensäure", "restzucker", "chloride", "freies.schwefeldioxid", "gesamtschwefeldioxid","dichte","pHWert", "sulfate","alkoholgehalt","qualität")
df$typ <- recode(df$typ, "white" = "Weißwein", "red" = "Rotwein")

# Skala von Qualität ändern, so dass sie von 1 bis 7 reicht
unique(df$qualität)
df$qualität <- df$qualität - 2

#bereinigter Datensatz im aktuellen Pfad speichern´
write.csv(df, "winequality_cleaned.csv", row.names = FALSE)


dataset<-read.csv("winequality_cleaned.csv")

#Zusammenfassende Statistiken
summary(dataset)

# Erweiterte beschreibende Statistiken
library(psych)
describe(dataset)
pairs.panels(dataset)

#QQ-Plots zur Untersuchung der Verteilung
create_qq_plots <- function(dataset) {
  numeric_cols <- sapply(dataset, is.numeric)
  par(mfrow = c(3, 4))
  
  for (colname in names(dataset)[numeric_cols]) {
    qqnorm(dataset[[colname]], main = paste("QQ Plot of", colname))
    qqline(dataset[[colname]], col = "red")
  }
  
  par(mfrow = c(1, 1)) 
}

# QQ-Plots erstellen
create_qq_plots(df)


create_plots <- function(dataset) {
  numeric_cols <- sapply(dataset, is.numeric)
  num_plots <- sum(numeric_cols)
  
  par(mfrow = c(3, 4)) # Layout für 4x6 Plots (22 Plots auf einer Seite)
  
  for (colname in names(dataset)[numeric_cols]) {
    hist(dataset[[colname]], main = paste("Histogram of", colname), 
         xlab = colname, col = "lightblue", border = "black", probability = TRUE)
    skewness_value <- round(e1071::skewness(dataset[[colname]]), 2)
    legend("topright", legend = paste("Skewness:", skewness_value), bty = "n")
  }
  
  par(mfrow = c(1, 1)) # Layout zurücksetzen
}
create_plots(df)
