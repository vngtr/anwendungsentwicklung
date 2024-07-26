#install.packages("psych")

# Workspace leeren
rm(list = ls())
gc()

library(psych)
library(dplyr)

# Pfad ändern
setwd("C:/Users/jojoh/OneDrive/Dokumente/Studium/Data Science & Business Analytics/Term 6/Anwendungsentwicklung/Prüfungsleistung")

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


