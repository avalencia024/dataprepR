# =============================================================================
# discretization.R
# -----------------------------------------------------------------------------
# Discretización por igual anchura e igual frecuencia:
#   - discretize:     discretiza con unos puntos de corte dados.
#   - discretizeEW:   igual anchura (equal width) para un vector.
#   - discretizeEF:   igual frecuencia (equal frequency) para un vector.
#   - discretizeData: cualquiera de los dos métodos para un dataset completo.
#
# Los intervalos son cerrados por la derecha, (a, b], y el primero y el último
# llegan a -Inf e Inf para poder discretizar valores nuevos que caigan fuera
# del rango de los datos originales.
# =============================================================================


#' Discretizar un vector con unos puntos de corte dados
#'
#' @description Convierte un vector numérico en un factor, asignando cada valor
#' al intervalo que le corresponde según los puntos de corte indicados.
#'
#' @param x Vector numérico a discretizar.
#' @param cut.points Vector numérico con los puntos de corte. No es necesario
#' que esté ordenado. Puede estar vacío: en ese caso hay un único intervalo.
#'
#' @details Con \code{k} puntos de corte se obtienen \code{k + 1} intervalos:
#' \code{(-Inf, c1], (c1, c2], ..., (ck, Inf)}. Un valor que coincide
#' exactamente con un punto de corte pertenece al intervalo de su izquierda.
#' Sin puntos de corte, todos los valores quedan en el intervalo
#' \code{(-Inf, Inf)}.
#'
#' Para que se lean bien, los nombres de los intervalos se redondean a 4
#' cifras significativas; los puntos de corte exactos son los de
#' \code{cut.points}.
#'
#' Esta función es la que usan internamente \code{\link{discretizeEW}} y
#' \code{\link{discretizeEF}}, y permite aplicar los puntos de corte que
#' devuelven esas funciones a valores nuevos.
#'
#' @return Un factor de la misma longitud que \code{x}. Los niveles del factor
#' son los intervalos (en orden) y su nombre indica los límites de cada uno.
#' Se incluyen todos los intervalos, aunque alguno quede vacío.
#'
#' @seealso \code{\link{discretizeEW}}, \code{\link{discretizeEF}}
#'
#' @examples
#' x <- c(11.5, 10.2, 1.2, 0.5, 5.3, 20.5, 8.4)
#' discretize(x, cut.points = c(5.5, 10.5, 15.5))
#'
#' # Valores fuera del rango original
#' discretize(c(-3, 100), cut.points = c(5.5, 10.5, 15.5))
#' @export
discretize <- function(x, cut.points) {

  checkNumeric(x, "x")

  # Los puntos de corte pueden estar vacíos (un único intervalo), pero si hay
  # alguno tiene que ser un número finito
  if (!is.numeric(cut.points) || any(!is.finite(cut.points))) {
    stop("'cut.points' debe ser un vector numerico sin valores NA ni infinitos")
  }

  # Ordenamos los puntos de corte y quitamos posibles repetidos
  cut.points <- sort(unique(cut.points))

  # Siempre hay un intervalo más que puntos de corte
  num.intervals <- length(cut.points) + 1

  # El intervalo de cada valor es 1 + el número de puntos de corte que quedan
  # estrictamente por debajo de él. Usamos ">" para que un valor igual a un
  # punto de corte quede en el intervalo de la izquierda: (a, b].
  interval.number <- sapply(x, FUN = function(value) {
    return(1 + sum(value > cut.points))
  })

  # Etiquetas de los intervalos, del tipo "(5.5, 10.5]"
  limits <- c(-Inf, cut.points, Inf)
  interval.names <- getIntervalNames(limits)

  # Convertimos los números de intervalo en un factor. Al fijar 'levels'
  # aparecen todos los intervalos, aunque alguno no tenga valores.
  result <- factor(interval.number,
                   levels = 1:num.intervals,
                   labels = interval.names)

  return(result)
}


#' Discretización por igual anchura (equal width)
#'
#' @description Discretiza un vector numérico dividiendo su rango en
#' \code{num.bins} intervalos de la misma anchura.
#'
#' @param x Vector numérico a discretizar.
#' @param num.bins Número de intervalos (entero mayor o igual que 2).
#'
#' @details La anchura de cada intervalo es \code{(max(x) - min(x)) / num.bins}
#' y los \code{num.bins - 1} puntos de corte se sitúan a esa distancia, empezando
#' en el mínimo. Por ejemplo, para los valores
#' \code{(11.5, 10.2, 1.2, 0.5, 5.3, 20.5, 8.4)} y 4 intervalos, el rango va de
#' 0.5 a 20.5, la anchura es 5 y los puntos de corte son 5.5, 10.5 y 15.5.
#'
#' Si todos los valores de \code{x} son iguales la anchura es 0 y no se puede
#' dividir el rango: no hay puntos de corte y todos los valores quedan en un
#' único intervalo (se avisa con un \code{warning}).
#'
#' @return Una lista con dos elementos:
#' \item{discretized}{Factor con los valores discretizados (ver
#' \code{\link{discretize}}).}
#' \item{cut.points}{Vector con los puntos de corte, para poder discretizar
#' nuevos valores con \code{\link{discretize}}.}
#'
#' @seealso \code{\link{discretizeEF}}, \code{\link{discretize}},
#' \code{\link{discretizeData}}
#'
#' @examples
#' x <- c(11.5, 10.2, 1.2, 0.5, 5.3, 20.5, 8.4)
#' result <- discretizeEW(x, num.bins = 4)
#' result$discretized
#' result$cut.points
#'
#' # Aplicamos los mismos puntos de corte a valores nuevos
#' discretize(c(3, 12, 30), result$cut.points)
#' @export
discretizeEW <- function(x, num.bins) {

  checkNumeric(x, "x")
  checkNumBins(num.bins)

  # Extremos del rango de los datos
  min.value <- min(x)
  max.value <- max(x)

  # Anchura común de todos los intervalos
  bin.width <- (max.value - min.value) / num.bins

  if (bin.width == 0) {
    # Variable constante: no se puede dividir el rango, así que no hay puntos
    # de corte y todos los valores quedan en un único intervalo
    warning("Todos los valores de 'x' son iguales: se crea un unico intervalo")
    cut.points <- numeric(0)
  } else {
    # Para num.bins intervalos necesitamos num.bins - 1 puntos de corte
    cut.points <- min.value + bin.width * (1:(num.bins - 1))
  }

  discretized <- discretize(x, cut.points)

  # Devolvemos también los puntos de corte para poder aplicarlos más adelante
  # a nuevos datos.
  result <- list(discretized = discretized, cut.points = cut.points)

  return(result)
}


#' Discretización por igual frecuencia (equal frequency)
#'
#' @description Discretiza un vector numérico buscando los puntos de corte que
#' hacen que en cada intervalo caiga (aproximadamente) el mismo número de
#' valores.
#'
#' @param x Vector numérico a discretizar.
#' @param num.bins Número de intervalos (entero entre 2 y la longitud de
#' \code{x}).
#'
#' @details Se ordenan los valores y se toma como final del intervalo
#' \code{i} la posición \code{round(i * n / num.bins)}, donde \code{n} es el
#' número de valores. El punto de corte se coloca en el punto medio entre el
#' último valor de un intervalo y el primero del siguiente. Así, cada intervalo
#' tiene el mismo número de valores, más o menos uno.
#'
#' \strong{Empates:} los valores repetidos siempre quedan en el mismo intervalo,
#' porque un punto de corte solo se puede poner entre dos valores distintos. Si
#' la posición calculada cae entre dos valores iguales, el corte se mueve al
#' hueco entre valores distintos más cercano a la posición ideal
#' (\code{i * n / num.bins}). Por eso, con valores repetidos las frecuencias
#' pueden quedar algo descompensadas, y si dos cortes acaban en el mismo hueco
#' se obtienen menos intervalos de los pedidos (se avisa con un
#' \code{warning}). Nunca queda ningún intervalo vacío. Si todos los valores
#' son iguales se crea un único intervalo.
#'
#' @return Una lista con dos elementos:
#' \item{discretized}{Factor con los valores discretizados (ver
#' \code{\link{discretize}}).}
#' \item{cut.points}{Vector con los puntos de corte, para poder discretizar
#' nuevos valores con \code{\link{discretize}}.}
#'
#' @seealso \code{\link{discretizeEW}}, \code{\link{discretize}},
#' \code{\link{discretizeData}}
#'
#' @examples
#' x <- c(11.5, 10.2, 1.2, 0.5, 5.3, 20.5, 8.4)
#' result <- discretizeEF(x, num.bins = 4)
#' result$discretized
#' result$cut.points
#' table(result$discretized)
#' @export
discretizeEF <- function(x, num.bins) {

  checkNumeric(x, "x")
  checkNumBins(num.bins)

  n <- length(x)

  if (num.bins > n) {
    stop("'num.bins' no puede ser mayor que el numero de valores de 'x'")
  }

  # Ordenamos los valores de menor a mayor
  x.sorted <- sort(x)

  # Posición ideal (en el vector ordenado) del último valor de cada intervalo,
  # excepto el último intervalo, que llega hasta el final. Como las posiciones
  # tienen que ser números enteros, las redondeamos.
  ideal.positions <- (1:(num.bins - 1)) * n / num.bins
  positions <- round(ideal.positions)

  # Un punto de corte solo se puede poner entre dos valores distintos; si no,
  # separaría valores iguales. 'gaps' son las posiciones i en las que el valor
  # siguiente es distinto (x.sorted[i] != x.sorted[i + 1]).
  gaps <- which(x.sorted[-n] != x.sorted[-1])

  if (length(gaps) == 0) {

    # Variable constante: no se puede separar ningún valor
    warning("Todos los valores de 'x' son iguales: se crea un unico intervalo")
    cut.points <- numeric(0)

  } else {

    # Si en una posición hay dos valores iguales, movemos el corte al hueco
    # entre valores distintos más cercano a la posición ideal
    for (i in 1:length(positions)) {
      if (!(positions[i] %in% gaps)) {
        positions[i] <- gaps[which.min(abs(gaps - ideal.positions[i]))]
      }
    }

    # Dos cortes pueden acabar en el mismo hueco. En ese caso nos quedamos con
    # uno solo (habrá menos intervalos que los pedidos).
    if (any(duplicated(positions))) {
      warning("Por los valores repetidos de 'x' se obtienen menos de ",
              "'num.bins' intervalos")
      positions <- unique(positions)
    }

    # Cada punto de corte va a mitad de camino entre el último valor de un
    # intervalo y el primero del siguiente
    cut.points <- (x.sorted[positions] + x.sorted[positions + 1]) / 2
  }

  discretized <- discretize(x, cut.points)

  result <- list(discretized = discretized, cut.points = cut.points)

  return(result)
}


#' Discretizar todas las variables numéricas de un dataset
#'
#' @description Aplica la discretización por igual anchura
#' (\code{\link{discretizeEW}}) o por igual frecuencia
#' (\code{\link{discretizeEF}}) a todas las variables numéricas de un dataset.
#' Las variables que no son numéricas se dejan como están.
#'
#' @param data \code{data.frame}, matriz u objeto de clase \code{dataset}.
#' @param num.bins Número de intervalos para cada variable.
#' @param method Método de discretización: \code{"EW"} (igual anchura, valor
#' por defecto) o \code{"EF"} (igual frecuencia).
#'
#' @details Es una función genérica. Si \code{data} es un objeto de clase
#' \code{dataset}, la variable clase no se discretiza aunque sea numérica.
#'
#' @return Una lista con dos elementos:
#' \item{discretized}{Los datos discretizados, del mismo tipo que \code{data}
#' (\code{data.frame} u objeto \code{dataset}).}
#' \item{cut.points}{Lista con los puntos de corte de cada variable
#' discretizada (un elemento por variable, con su nombre).}
#'
#' @seealso \code{\link{discretizeEW}}, \code{\link{discretizeEF}}
#'
#' @examples
#' result <- discretizeData(iris, num.bins = 3, method = "EF")
#' head(result$discretized)
#' result$cut.points$Sepal.Length
#'
#' ds <- createDataset(mtcars, class.name = "am")
#' result <- discretizeData(ds, num.bins = 3)
#' head(result$discretized$data)
#' @export
discretizeData <- function(data, num.bins, method = "EW") {
  UseMethod("discretizeData")
}


#' @rdname discretizeData
#' @export
discretizeData.default <- function(data, num.bins, method = "EW") {

  data <- toDataFrame(data)

  # Elegimos la función de discretización según el método
  discretize.function <- switch(method,
    EW = discretizeEW,
    EF = discretizeEF,
    stop("'method' debe ser 'EW' (igual anchura) o 'EF' (igual frecuencia)")
  )

  numeric.columns <- names(data)[sapply(data, is.numeric)]

  if (length(numeric.columns) == 0) {
    warning("No hay variables numericas que discretizar")
    return(list(discretized = data, cut.points = list()))
  }

  # Discretizamos cada columna numérica. El resultado es una lista con, para
  # cada variable, lo que devuelve discretizeEW / discretizeEF.
  results <- lapply(data[numeric.columns],
                    FUN = discretize.function,
                    num.bins = num.bins)

  # Sustituimos las columnas por su versión discretizada y guardamos aparte
  # los puntos de corte de cada variable.
  data[numeric.columns] <- lapply(results, FUN = function(result) {
    return(result$discretized)
  })

  cut.points <- lapply(results, FUN = function(result) {
    return(result$cut.points)
  })

  return(list(discretized = data, cut.points = cut.points))
}


#' @rdname discretizeData
#' @export
discretizeData.dataset <- function(data, num.bins, method = "EW") {

  # Discretizamos solo las variables que no son la clase
  predictors <- getPredictorNames(data)

  result <- discretizeData.default(data$data[predictors],
                                   num.bins = num.bins,
                                   method = method)

  # Volvemos a meter las variables discretizadas en el dataset
  data$data[predictors] <- result$discretized

  return(list(discretized = data, cut.points = result$cut.points))
}


# -----------------------------------------------------------------------------
# FUNCIÓN INTERNA: checkNumBins
# -----------------------------------------------------------------------------
# Comprueba que el número de intervalos sea un entero (finito) mayor o igual
# que 2.
# -----------------------------------------------------------------------------

checkNumBins <- function(num.bins) {

  if (!is.numeric(num.bins) || length(num.bins) != 1 || !is.finite(num.bins) ||
      num.bins < 2 || num.bins != round(num.bins)) {
    stop("'num.bins' debe ser un numero entero mayor o igual que 2")
  }

  invisible(TRUE)
}


# -----------------------------------------------------------------------------
# FUNCIÓN INTERNA: getIntervalNames
# -----------------------------------------------------------------------------
# Crea los nombres de los intervalos a partir de sus límites.
#
# Entrada:
#   limits  Vector ordenado: -Inf, puntos de corte, Inf.
#
# Salida:
#   Vector de textos del tipo "(-Inf, 5.5]", "(5.5, 10.5]", ..., "(15.5, Inf)".
# -----------------------------------------------------------------------------

getIntervalNames <- function(limits) {

  # Redondeamos a 4 cifras significativas para que los nombres sean legibles
  # (con igual frecuencia salen puntos de corte con muchos decimales).
  values <- signif(limits, 4)

  # Si al redondear dos límites quedan iguales, los nombres se repetirían, así
  # que en ese caso usamos los valores sin redondear.
  if (any(duplicated(values))) {
    values <- limits
  }

  num.intervals <- length(limits) - 1

  interval.names <- paste("(", values[1:num.intervals], ", ",
                          values[2:(num.intervals + 1)], "]", sep = "")

  # El último intervalo llega a infinito, así que es abierto: (a, Inf)
  interval.names[num.intervals] <- paste("(", values[num.intervals],
                                         ", Inf)", sep = "")

  return(interval.names)
}
