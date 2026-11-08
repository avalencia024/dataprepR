# =============================================================================
# Pruebas del paquete dataprepR
# -----------------------------------------------------------------------------
# Se ejecutan automáticamente con R CMD check (botón Check de RStudio).
# Cada prueba calcula un resultado y lo compara con el valor esperado,
# calculado a mano o conocido de antemano. Si algún resultado no coincide,
# stopifnot() para la ejecución y el check da error.
# =============================================================================

library(dataprepR)


# Discretización ---------------------------------------------------------------

# Igual anchura: los valores van de 0.5 a 20.5 y, con 4 intervalos, los
# cortes están en 5.5, 10.5 y 15.5
x <- c(11.5, 10.2, 1.2, 0.5, 5.3, 20.5, 8.4)
ew <- discretizeEW(x, num.bins = 4)
stopifnot(all.equal(ew$cut.points, c(5.5, 10.5, 15.5)))
stopifnot(as.integer(ew$discretized) == c(3, 2, 1, 1, 1, 4, 2))
stopifnot(levels(ew$discretized)[1] == "(-Inf, 5.5]")

# Igual frecuencia: ordenados, los valores son 0.5, 1.2, 5.3, 8.4, 10.2,
# 11.5 y 20.5. Con 4 intervalos, los cortes quedan a mitad de camino entre
# las posiciones 2 y 3, 4 y 5, y 5 y 6, y los intervalos tienen 2, 2, 1 y 2
# valores
ef <- discretizeEF(x, num.bins = 4)
stopifnot(all.equal(ef$cut.points, c(3.25, 9.3, 10.85)))
stopifnot(as.vector(table(ef$discretized)) == c(2, 2, 1, 2))

# Igual frecuencia con valores repetidos: los cuatro 1 van siempre juntos, los
# cortes se ponen entre valores distintos (1.5 y 3.5) y no queda ningún
# intervalo vacío. Salen 3 intervalos en vez de 4 (suppressWarnings oculta el
# aviso)
ties <- suppressWarnings(discretizeEF(c(1, 1, 1, 1, 2, 3, 4, 5), num.bins = 4))
stopifnot(all.equal(ties$cut.points, c(1.5, 3.5)))
stopifnot(as.vector(table(ties$discretized)) == c(4, 2, 2))

# Variable constante: un único intervalo, con los dos métodos
stopifnot(nlevels(suppressWarnings(discretizeEW(rep(3, 5), 3))$discretized) == 1)
stopifnot(nlevels(suppressWarnings(discretizeEF(rep(3, 5), 3))$discretized) == 1)

# discretize: un valor igual a un corte (5.5) va al intervalo de la izquierda
# y los valores fuera del rango original van al primer o al último intervalo
stopifnot(as.integer(discretize(c(5.5, -100, 100), ew$cut.points)) == c(1, 1, 4))

# Dataset completo, con los dos métodos: las variables numéricas pasan a ser
# factores y Species, que ya era un factor, no cambia
for (method in c("EW", "EF")) {
  result <- discretizeData(iris, num.bins = 3, method = method)
  stopifnot(all(sapply(result$discretized, is.factor)))
  stopifnot(identical(result$discretized$Species, iris$Species))
  stopifnot(length(result$cut.points) == 4)
}

# En un objeto dataset la variable clase no se discretiza
ds <- createDataset(mtcars, class.name = "am")
ds.disc <- discretizeData(ds, num.bins = 3)
stopifnot(identical(ds.disc$discretized$data$am, mtcars$am))


# Entropía ---------------------------------------------------------------------

# Entropía con los niveles a, b y c y los datos (a, a, c, c, c):
# -0.4 * log2(0.4) - 0.6 * log2(0.6) = 0.971
f <- factor(c("a", "a", "c", "c", "c"), levels = c("a", "b", "c"))
stopifnot(abs(entropy(f) - 0.971) < 0.001)

# Si todos los valores son iguales la entropía es 0, y si los 4 valores
# posibles son igual de frecuentes es la máxima, log2(4) = 2
stopifnot(entropy(factor(rep("a", 10))) == 0)
stopifnot(entropy(factor(c("a", "b", "c", "d"))) == 2)


# AUC ------------------------------------------------------------------------

# Ejemplo de comprobación, calculado a mano: AUC esperado = 0.75
auc.data <- data.frame(value = c(0.1, 0.4, 0.35, 0.8),
                       class = c(FALSE, FALSE, TRUE, TRUE))
result <- calculateAUC(auc.data)
stopifnot(all.equal(result$auc, 0.75))
stopifnot(result$roc.points$FPR == c(0, 0, 0.5, 0.5, 1))
stopifnot(result$roc.points$TPR == c(0, 0.5, 0.5, 1, 1))

# Separación perfecta (AUC = 1), separación al revés (AUC = 0) y ninguna
# separación, con todos los valores iguales (AUC = 0.5)
labels <- c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE)
stopifnot(calculateAUC(data.frame(1:6, labels))$auc == 1)
stopifnot(calculateAUC(data.frame(6:1, labels))$auc == 0)
stopifnot(calculateAUC(data.frame(rep(1, 6), labels))$auc == 0.5)


# Métricas -------------------------------------------------------------------

datos <- mtcars
datos$cyl <- factor(datos$cyl)
metrics <- computeMetrics(datos, class.name = "am")

# Varianza y AUC para las numéricas, entropía para las categóricas. La
# variable clase no aparece en el resultado
stopifnot(all.equal(metrics["mpg", "variance"], var(mtcars$mpg)))
stopifnot(all.equal(metrics["wt", "auc"], calculateAUC(mtcars[, c("wt", "am")])$auc))
stopifnot(is.na(metrics["cyl", "variance"]), !is.na(metrics["cyl", "entropy"]))
stopifnot(!("am" %in% metrics$variable))

# Si la clase no es binaria (Species tiene 3 valores) no se calcula el AUC,
# pero sí el resto de métricas (suppressWarnings oculta el aviso)
iris.metrics <- suppressWarnings(computeMetrics(iris, class.name = "Species"))
stopifnot(all(is.na(iris.metrics$auc)))
stopifnot(all.equal(iris.metrics["Sepal.Length", "variance"], var(iris$Sepal.Length)))

# No se admiten valores NA: con un NA en una variable numérica hay un error
# (tryCatch recoge el error)
result <- tryCatch(computeMetrics(data.frame(a = c(1, NA, 3))),
                   error = function(e) "error")
stopifnot(identical(result, "error"))

# Tampoco se admiten valores infinitos en la variable clase
result <- tryCatch(computeMetrics(data.frame(x = 1:4, y = c(0, Inf, 0, Inf)),
                                  class.name = "y"),
                   error = function(e) "error")
stopifnot(identical(result, "error"))


# Normalización y estandarización ----------------------------------------------

# Al normalizar, mínimo 0 y máximo 1; al estandarizar, media 0 y desviación
# estándar 1
v <- c(-0.29, 2.11, -3.84, 10.69, 5.30)
stopifnot(min(normalize(v)) == 0, max(normalize(v)) == 1)
stopifnot(abs(mean(standardize(v))) < 1e-12, all.equal(sd(standardize(v)), 1))

# En un dataset solo se transforman las variables numéricas, y en un objeto
# dataset tampoco se transforma la variable clase
iris.norm <- normalizeData(iris)
stopifnot(identical(iris.norm$Species, iris$Species))
stopifnot(all(sapply(iris.norm[1:4], min) == 0), all(sapply(iris.norm[1:4], max) == 1))
stopifnot(identical(standardizeData(ds)$data$am, mtcars$am))

# Variable constante: no se puede dividir entre el rango (0), así que se
# devuelven ceros
stopifnot(suppressWarnings(normalize(rep(2, 4))) == 0)


# Filtrado -----------------------------------------------------------------------

# Varianza mayor que 10: solo quedan mpg, disp y hp
filtered <- filterData(datos, metric = "variance", threshold = 10)
stopifnot(identical(names(filtered), c("mpg", "disp", "hp")))

# Con keep.others = TRUE se mantiene también el factor cyl, porque la
# varianza no se aplica a las variables categóricas
filtered <- filterData(datos, metric = "variance", threshold = 10, keep.others = TRUE)
stopifnot(identical(names(filtered), c("mpg", "cyl", "disp", "hp")))

# AUC mayor que 0.8 respecto a la clase am: mpg (0.83), drat (0.90) y gear
# (0.94). La clase se mantiene siempre
filtered <- filterData(datos, metric = "auc", threshold = 0.8, class.name = "am")
stopifnot(identical(names(filtered), c("mpg", "drat", "am", "gear")))

# Si ninguna variable cumple el requisito solo queda la clase, y sus métricas
# son una tabla vacía
only.class <- filterData(ds, metric = "variance", threshold = 1e6)
stopifnot(identical(names(only.class$data), "am"))
stopifnot(nrow(computeMetrics(only.class)) == 0)


# Correlación e información mutua ---------------------------------------------

# La correlación implementada a mano da lo mismo que la función cor de R
stopifnot(all.equal(correlation(mtcars$mpg, mtcars$wt), cor(mtcars$mpg, mtcars$wt)))
numeric.data <- mtcars[, c("mpg", "disp", "hp", "wt")]
stopifnot(all.equal(computeCorrelations(numeric.data)$correlation, cor(numeric.data)))

# La información mutua de una variable consigo misma es su entropía, y la de
# dos variables independientes es 0
cyl <- factor(mtcars$cyl)
stopifnot(all.equal(mutualInformation(cyl, cyl), entropy(cyl)))
a <- factor(c("x", "x", "y", "y"))
b <- factor(c("u", "v", "u", "v"))
stopifnot(mutualInformation(a, b) == 0)


# Gráficos ---------------------------------------------------------------------

# Todos los gráficos se dibujan sin errores. Se usa pdf(NULL), un dispositivo
# gráfico que no guarda nada
pdf(NULL)
plotROC(auc.data)
plotAUC(ds)
plotCorrelation(numeric.data)
plotCorrelation(data.frame(cyl = cyl, gear = factor(mtcars$gear)),
                type = "mutual.information")
plotEntropy(ds.disc$discretized)
invisible(dev.off())


# Lectura y escritura -----------------------------------------------------------

# readCSV detecta el tipo de cada columna
file.csv <- tempfile(fileext = ".csv")
write.csv(mtcars, file.csv, row.names = FALSE)
leido <- readCSV(file.csv, max.categories = 5)
stopifnot(is.factor(leido$cyl), is.integer(leido$carb), is.numeric(leido$mpg))

# Al guardar y leer un dataset con el formato propio no se pierde nada
file.dts <- tempfile(fileext = ".dts")
ds.leido <- createDataset(leido, class.name = "am", name = "Coches")
writeDataset(ds.leido, file.dts)
ds2 <- readDataset(file.dts)
stopifnot(ds2$class.name == "am", ds2$name == "Coches")
stopifnot(all.equal(ds.leido$data, ds2$data))

# Los textos con comas, con comillas o con el texto "NA" también se guardan y
# se leen bien (el texto "NA" no se convierte en un valor perdido)
textos <- data.frame(texto = c("con, coma", "con \"comillas\"", "NA"),
                     stringsAsFactors = FALSE)
writeDataset(textos, file.dts)
stopifnot(identical(readDataset(file.dts)$data$texto, textos$texto))


# Clase dataset -----------------------------------------------------------------

stopifnot(isDataset(ds), !isDataset(mtcars))
stopifnot(identical(summary(ds), computeMetrics(ds)))

cat("Todas las pruebas se han superado correctamente\n")
