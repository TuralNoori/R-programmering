library(readxl)
library(car)
library(ggplot2)

#Datan
df <- read_excel("C:/Users/Admin/Desktop/Data.xlsx")

#Rensar och konverterar numeriska kolumner
df$Försäljningspris <- gsub("[^0-9]", "", df$Försäljningspris)
df$Försäljningspris[df$Försäljningspris == ""] <- NA
df$Försäljningspris <- as.numeric(df$Försäljningspris)

df$Miltal <- as.numeric(gsub("[^0-9]", "", df$Miltal))
df$Hästkrafter <- as.numeric(gsub("[^0-9]", "", df$Hästkrafter))
df$Motorstorlek <- as.numeric(gsub("[^0-9]", "", df$Motorstorlek))

#Tar bort rader med NA
df <- df[!is.na(df$Försäljningspris) &
           !is.na(df$Miltal) &
           !is.na(df$Hästkrafter) &
           !is.na(df$Motorstorlek) &
           !is.na(df$Modellår), ]

#Skapar 'Ålder' från Modellår
df$Ålder <- 2025 - df$Modellår

#Omvandlar kategorier till faktorer
df$Säljare <- as.factor(df$Säljare)
df$Bränsle <- as.factor(df$Bränsle)
df$Växellåda <- as.factor(df$Växellåda)
df$Drivning <- as.factor(df$Drivning)
df$Biltyp <- as.factor(df$Biltyp)
df$Modell <- as.factor(df$Modell)

#Rensar Färg
df$Färg <- gsub("\\(.*\\)", "", df$Färg)
df$Färg <- trimws(df$Färg)

#Första modellen
model <- lm(Försäljningspris ~ Ålder + Miltal + Hästkrafter + Motorstorlek +
              Säljare + Bränsle + Växellåda + Biltyp + Drivning + Modell, data = df)

print("Aliased coefficients:")
print(alias(model))

#Modell2
model2 <- lm(Försäljningspris ~ Ålder + Miltal + Hästkrafter + Motorstorlek +
               Säljare + Bränsle + Växellåda + Biltyp + Drivning,
             data = df, na.action = na.exclude)

#Sammanfattning
summary(model2)

#Residualdiagnostik
hist(residuals(model2), main = "Residualer", xlab = "Residual", breaks = 30)
qqnorm(residuals(model2))
qqline(residuals(model2))

resids <- residuals(model2)
fits <- fitted(model2)
if (length(resids) == length(fits)) {
  plot(fits, resids,
       xlab = "Predikterat pris", ylab = "Residualer", main = "Residualplot")
  abline(h = 0, col = "red")
}

#VIF
vif(model2)

#Konfidensintervall
confint(model2)

#Lägger till predikterade värden i datan
df$Predikterat <- fitted(model2)

#Beräknar skillnaden och absoluta skillnaden
df$Skillnad <- df$Försäljningspris - df$Predikterat
df$AbsolutSkillnad <- abs(df$Skillnad)

#Sorterar och visar de 10 största avvikelserna
top_diff <- df[order(-df$AbsolutSkillnad), ][1:10, ]
print("Topp 10 största avvikelser mellan faktiskt och predikterat pris:")
print(top_diff[, c("Försäljningspris", "Predikterat", "Skillnad", "AbsolutSkillnad")])

#Faktiskt vs. Predikterat
ggplot(df, aes(x = Predikterat, y = Försäljningspris)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, col = "blue") +
  labs(title = "Linjär regression: Faktiskt vs. Predikterat pris (modell2)",
       x = "Predikterat pris", y = "Faktiskt pris")

#Predikterar ny bil med 95% prediktionsintervall
ny_bil <- data.frame(
  Ålder = 3,
  Miltal = 2000,
  Hästkrafter = 190,
  Motorstorlek = 2000,
  Säljare = factor("Företag", levels = levels(df$Säljare)),
  Bränsle = factor("Bensin", levels = levels(df$Bränsle)),
  Växellåda = factor("Automat", levels = levels(df$Växellåda)),
  Biltyp = factor("SUV", levels = levels(df$Biltyp)),
  Drivning = factor("Fyrhjulsdriven", levels = levels(df$Drivning))
)
pred_ny_bil <- predict(model2, newdata = ny_bil, interval = "prediction", level = 0.95)
ny_bil$Predikterat <- pred_ny_bil[1]
ny_bil$Lower <- pred_ny_bil[2]
ny_bil$Upper <- pred_ny_bil[3]

print("Prediktion för ny_bil med 95% intervall:")
print(ny_bil)

ggplot(df, aes(x = Predikterat, y = Försäljningspris)) +
  geom_point(alpha = 0.3, color = "gray") +
  geom_point(data = ny_bil, aes(x = Predikterat, y = Predikterat), 
             color = "green", size = 4, shape = 17, inherit.aes = FALSE) +
  geom_errorbar(data = ny_bil, aes(x = Predikterat, ymin = Lower, ymax = Upper), 
                width = 0, color = "green", linetype = "dashed", inherit.aes = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Förutsagt pris för ny bil (ny_bil)",
       x = "Predikterat pris", y = "Faktiskt pris") +
  annotate("text", x = ny_bil$Predikterat, y = ny_bil$Upper + 10000,
           label = "ny_bil", color = "green") +
  theme_minimal()

#Predikterar ny_bil2 med 95% prediktionsintervall
ny_bil2 <- data.frame(
  Ålder = 1,
  Miltal = 1344,
  Hästkrafter = 253,
  Motorstorlek = 1969,
  Säljare = factor("Företag", levels = levels(df$Säljare)),
  Bränsle = factor("Bensin", levels = levels(df$Bränsle)),
  Växellåda = factor("Automat", levels = levels(df$Växellåda)),
  Biltyp = factor("SUV", levels = levels(df$Biltyp)),
  Drivning = factor("Fyrhjulsdriven", levels = levels(df$Drivning))
)
pred_ny_bil2 <- predict(model2, newdata = ny_bil2, interval = "prediction", level = 0.95)
ny_bil2$Predikterat <- pred_ny_bil2[1]
ny_bil2$Lower <- pred_ny_bil2[2]
ny_bil2$Upper <- pred_ny_bil2[3]

print("Prediktion för ny_bil2 med 95% intervall:")
print(ny_bil2)

ggplot(df, aes(x = Predikterat, y = Försäljningspris)) +
  geom_point(alpha = 0.3, color = "gray") +
  geom_point(data = ny_bil2, aes(x = Predikterat, y = Predikterat), 
             color = "red", size = 4, shape = 17, inherit.aes = FALSE) +
  geom_errorbar(data = ny_bil2, aes(x = Predikterat, ymin = Lower, ymax = Upper), 
                width = 0, color = "red", linetype = "dashed", inherit.aes = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Förutsagt pris för ny bil (ny_bil2)",
       x = "Predikterat pris", y = "Faktiskt pris") +
  annotate("text", x = ny_bil2$Predikterat, y = ny_bil2$Upper + 10000,
           label = "ny_bil2", color = "red") +
  theme_minimal()

