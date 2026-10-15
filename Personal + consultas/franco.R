df<- readr::read_csv("franco.csv")
plot(df)
abline(lm(df$concentracion ~ df$mv), col = "red")
model <- lm(df$concentracion ~ df$mv)
loglog_model<- lm(log(df$concentracion) ~ log(df$mv))
linlog_model <- lm(log(df$concentracion) ~ df$mv)
loglin_model <- lm(df$concentracion ~ log(df$mv))

plot(df$mv,log(df$concentracion))


summary(model)
summary(loglog_model)
summary(linlog_model)


df <- read.csv("franco.csv")

model <- lm(concentracion ~ mv, data = df)
loglog_model <- lm(log(concentracion) ~ log(mv), data = df)
linlog_model <- lm(log(concentracion) ~ mv, data = df)
loglin_model <- lm(concentracion ~ log(mv), data = df)

par(mfrow = c(2, 2))

plot(df$mv, df$concentracion, main = "Original")
abline(model, col = "red")

plot(log(df$mv), log(df$concentracion), main = "Log-Log")
abline(loglog_model, col = "red")

plot(df$mv, log(df$concentracion), main = "Lin-Log")
abline(linlog_model, col = "red")

plot(log(df$mv), df$concentracion, main = "Log-Lin")
abline(loglin_model, col = "red")
summary(loglin_model)
