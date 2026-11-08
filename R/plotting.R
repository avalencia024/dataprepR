# =============================================================================
# plotting.R
# -----------------------------------------------------------------------------
# Visualización:
#   - plotROC:         curva ROC de una variable (con su AUC).
#   - plotAUC:         diagrama de barras con el AUC de cada variable numérica.
#   - plotCorrelation: matriz de correlaciones o de información mutua.
#   - plotEntropy:     diagrama de barras con la entropía normalizada.
#
# Se usan solo las funciones gráficas básicas de R (paquetes graphics y
# grDevices), así el paquete no depende de ningún paquete externo.
#
# Nota: en los textos de los gráficos las tildes se escriben con códigos
# \u00xx (por ejemplo "\u00ed" es una í). Es la forma recomendada en los
# paquetes de R, porque el código solo debe tener caracteres ASCII.
# =============================================================================


#' Dibujar la curva ROC de una variable
#'
#' @description Dibuja la curva ROC de una variable numérica respecto a una
#' clase binaria, indicando su AUC en el título.
#'
#' @param data \code{data.frame} (o matriz) de dos columnas: la variable
#' numérica y la clase (ver \code{\link{calculateAUC}}).
#' @param main Título del gráfico. Al final se le añade el valor del AUC.
#'
#' @details Los puntos de la curva se calculan con \code{\link{calculateAUC}}.
#' La diagonal discontinua corresponde a un AUC de 0.5: ordenar por la
#' variable no separa las clases (ver \code{\link{calculateAUC}}).
#'
#' @return De forma invisible, la lista que devuelve
#' \code{\link{calculateAUC}} (puntos de la curva y AUC).
#'
#' @seealso \code{\link{calculateAUC}}, \code{\link{plotAUC}}
#'
#' @examples
#' data <- data.frame(value = c(0.1, 0.4, 0.35, 0.8),
#'                    class = c(FALSE, FALSE, TRUE, TRUE))
#' plotROC(data)
#'
#' plotROC(mtcars[, c("wt", "am")], main = "Curva ROC de wt")
#' @export
plotROC <- function(data, main = "Curva ROC") {

  # Calculamos la curva ROC y el AUC
  result <- calculateAUC(data)
  roc.points <- result$roc.points

  # FPR en el eje X y TPR en el eje Y; los dos son proporciones, así que
  # fijamos los límites de los ejes entre 0 y 1. Con type = "o" se dibujan
  # los puntos y las líneas que los unen.
  plot(roc.points$FPR, roc.points$TPR,
       type = "o", pch = 19, cex = 0.7, lwd = 2, col = "steelblue",
       xlim = c(0, 1), ylim = c(0, 1),
       xlab = "FPR (tasa de falsos positivos)",
       ylab = "TPR (tasa de verdaderos positivos)",
       main = paste(main, "- AUC =", round(result$auc, 3)))

  # Diagonal de referencia: AUC = 0.5 (ordenar por la variable no separa las
  # clases)
  abline(a = 0, b = 1, lty = 2, col = "grey50")

  invisible(result)
}


#' Diagrama de barras con el AUC de las variables numéricas
#'
#' @description Calcula el AUC de todas las variables numéricas de un dataset
#' respecto a la variable clase y lo muestra en un diagrama de barras ordenado
#' de mayor a menor.
#'
#' @param data \code{data.frame}, matriz u objeto de clase \code{dataset}.
#' @param class.name Nombre de la variable clase (binaria). En un objeto
#' \code{dataset} se usa por defecto su propia variable clase.
#' @param main Título del gráfico.
#'
#' @details Las barras de las variables con AUC mayor o igual que 0.5 se
#' dibujan en azul y las de AUC menor que 0.5 en rojo. La línea discontinua
#' marca el valor 0.5 (ordenar por la variable no separa las clases).
#'
#' @return De forma invisible, vector con el AUC de cada variable numérica,
#' ordenado de mayor a menor.
#'
#' @seealso \code{\link{computeMetrics}}, \code{\link{plotROC}}
#'
#' @examples
#' plotAUC(mtcars, class.name = "am")
#' @export
plotAUC <- function(data, class.name = NULL,
                    main = "AUC de las variables num\u00e9ricas") {

  # En un dataset, si no se indica otra cosa, usamos su variable clase
  if (isDataset(data) && is.null(class.name)) {
    class.name <- data$class.name
  }

  # El AUC necesita una variable clase binaria (en un dataset, las columnas
  # están en data$data)
  if (isDataset(data)) {
    checkBinaryClass(data$data, class.name)
  } else {
    checkBinaryClass(toDataFrame(data), class.name)
  }

  metrics <- computeMetrics(data, class.name = class.name)

  # Nos quedamos con las variables que tienen AUC (las numéricas)
  metrics <- metrics[!is.na(metrics$auc), ]

  if (nrow(metrics) == 0) {
    stop("No hay variables numericas a las que calcular el AUC")
  }

  auc.values <- metrics$auc
  names(auc.values) <- metrics$variable
  auc.values <- sort(auc.values, decreasing = TRUE)

  # Azul si AUC >= 0.5 y rojo si es menor
  colors <- ifelse(auc.values >= 0.5, "steelblue", "indianred")

  # Más margen abajo para los nombres de las variables (verticales). par()
  # devuelve los márgenes que había antes, y on.exit() hace que se vuelvan a
  # poner al terminar la función, para no cambiar los gráficos siguientes.
  old.par <- par(mar = c(8, 4, 4, 2))
  on.exit(par(old.par))

  barplot(auc.values, ylim = c(0, 1), col = colors, las = 2,
          ylab = "AUC", main = main)

  abline(h = 0.5, lty = 2, col = "grey30")

  invisible(auc.values)
}


#' Visualizar la matriz de correlaciones o de información mutua
#'
#' @description Dibuja como un mapa de calor (heatmap) la matriz de
#' correlaciones entre las variables numéricas o la matriz de información mutua
#' entre las variables categóricas de un dataset. En cada casilla se escribe
#' además su valor.
#'
#' @param data \code{data.frame}, matriz u objeto de clase \code{dataset}.
#' @param type Matriz a dibujar: \code{"correlation"} (valor por defecto) o
#' \code{"mutual.information"}.
#' @param main Título del gráfico. Si es \code{NULL} se pone uno según el tipo.
#'
#' @details Las matrices se calculan con \code{\link{computeCorrelations}}. En
#' la matriz de correlaciones los colores van del azul (correlación -1) al rojo
#' (correlación 1), pasando por el blanco (0). En la de información mutua van
#' del blanco (0) al verde (valor máximo de la matriz).
#'
#' @return De forma invisible, la matriz dibujada.
#'
#' @seealso \code{\link{computeCorrelations}}
#'
#' @examples
#' plotCorrelation(mtcars[, c("mpg", "disp", "hp", "drat", "wt", "qsec")])
#'
#' datos <- data.frame(cyl = factor(mtcars$cyl), gear = factor(mtcars$gear),
#'                     am = factor(mtcars$am), vs = factor(mtcars$vs))
#' plotCorrelation(datos, type = "mutual.information")
#' @export
plotCorrelation <- function(data, type = "correlation", main = NULL) {

  correlations <- computeCorrelations(data)

  # Elegimos la matriz según el tipo pedido
  values <- switch(type,
    correlation = correlations$correlation,
    mutual.information = correlations$mutual.information,
    stop("'type' debe ser 'correlation' o 'mutual.information'")
  )

  # Comprobamos que exista la matriz antes de usarla (por ejemplo, no hay
  # matriz de información mutua si no hay variables categóricas)
  if (is.null(values) || nrow(values) < 2) {
    stop("Se necesitan al menos dos variables del tipo adecuado para dibujar la matriz")
  }

  # Colores, rango de la escala y título según el tipo de matriz.
  # colorRampPalette(c(...)) crea una función que genera la cantidad de
  # colores que le pidamos pasando gradualmente por los colores indicados;
  # por ejemplo, 101 colores del azul al rojo pasando por el blanco.
  if (type == "correlation") {
    colors <- colorRampPalette(c("steelblue", "white", "firebrick"))(101)
    z.limits <- c(-1, 1)
    default.title <- "Matriz de correlaciones"
  } else {
    colors <- colorRampPalette(c("white", "darkgreen"))(100)
    z.limits <- c(0, max(values))
    default.title <- "Matriz de informaci\u00f3n mutua"
  }

  if (is.null(main)) {
    main <- default.title
  }

  plotHeatmap(values, colors = colors, z.limits = z.limits, main = main)

  invisible(values)
}


#' Diagrama de barras con la entropía normalizada
#'
#' @description Dado un dataset discretizado (con variables categóricas),
#' muestra en un diagrama de barras la entropía normalizada de cada variable
#' categórica.
#'
#' @param data \code{data.frame} u objeto de clase \code{dataset}.
#' @param main Título del gráfico.
#'
#' @details La entropía se calcula con \code{\link{entropy}} y se normaliza
#' dividiéndola entre su valor máximo, que es el logaritmo (en base 2) del
#' número de posibles valores de la variable. Así todas las variables quedan
#' en el rango [0, 1] y se pueden comparar aunque tengan distinto número de
#' valores. Las variables no categóricas se ignoran.
#'
#' @return De forma invisible, vector con la entropía normalizada de cada
#' variable categórica.
#'
#' @seealso \code{\link{entropy}}, \code{\link{discretizeData}}
#'
#' @examples
#' iris.disc <- discretizeData(iris, num.bins = 4, method = "EW")$discretized
#' plotEntropy(iris.disc)
#' @export
plotEntropy <- function(data, main = "Entrop\u00eda normalizada") {

  if (isDataset(data)) {
    data <- data$data
  }
  data <- toDataFrame(data)

  categorical.columns <- names(data)[sapply(data, isCategorical)]

  if (length(categorical.columns) == 0) {
    stop("No hay variables categoricas: discretiza antes los datos con discretizeData")
  }

  normalized.entropy <- sapply(data[categorical.columns], FUN = function(x) {

    if (is.logical(x)) {
      x <- factor(x)
    }

    # Número de posibles valores de la variable
    num.values <- length(levels(x))

    # Con un solo valor posible la entropía máxima es log2(1) = 0, así que
    # no se puede dividir: la entropía normalizada es 0.
    if (num.values <= 1) {
      return(0)
    }

    return(entropy(x) / log2(num.values))
  })

  # Más margen abajo para los nombres de las variables (verticales). Con
  # on.exit() se recuperan los márgenes originales al terminar la función.
  old.par <- par(mar = c(8, 4, 4, 2))
  on.exit(par(old.par))

  barplot(normalized.entropy, ylim = c(0, 1), col = "darkseagreen", las = 2,
          ylab = "Entrop\u00eda normalizada", main = main)

  invisible(normalized.entropy)
}


# -----------------------------------------------------------------------------
# FUNCIÓN INTERNA: plotHeatmap
# -----------------------------------------------------------------------------
# Dibuja una matriz cuadrada como un mapa de calor con la función image() y
# escribe el valor de cada casilla.
#
# Entradas:
#   values    Matriz cuadrada con nombres de filas y columnas.
#   colors    Vector de colores de la escala.
#   z.limits  Valores mínimo y máximo de la escala de colores.
#   main      Título.
# -----------------------------------------------------------------------------

plotHeatmap <- function(values, colors, z.limits, main) {

  n <- nrow(values)

  # Dejamos más margen abajo y a la izquierda para los nombres de las
  # variables, y recuperamos los márgenes originales al terminar (on.exit).
  old.par <- par(mar = c(7, 7, 4, 2))
  on.exit(par(old.par))

  # image() dibuja la primera fila de la matriz abajo del todo. Para que se
  # vea como al imprimir la matriz (primera variable arriba) invertimos el
  # orden de las filas. Además image() pone las filas de 'z' en el eje X,
  # por eso trasponemos la matriz.
  image(x = 1:n, y = 1:n, z = t(values[n:1, , drop = FALSE]),
        col = colors, zlim = z.limits, axes = FALSE,
        xlab = "", ylab = "", main = main)

  axis(1, at = 1:n, labels = colnames(values), las = 2, tick = FALSE)
  axis(2, at = 1:n, labels = rev(rownames(values)), las = 1, tick = FALSE)

  # Escribimos el valor de cada casilla. La casilla [i, j] de la matriz (fila
  # i, columna j) está en x = j, y = n - i + 1, porque la primera fila se
  # dibuja arriba del todo.
  for (i in 1:n) {
    for (j in 1:n) {
      text(x = j, y = n - i + 1, labels = round(values[i, j], 2), cex = 0.8)
    }
  }

  # Líneas para separar las casillas
  abline(h = (0:n) + 0.5, v = (0:n) + 0.5, col = "grey80")
}
