# =============================================================================
# normalization.R
# -----------------------------------------------------------------------------
# Normalización y estandarización, tanto para una
# variable como para todas las variables numéricas de un dataset:
#   - normalize / normalizeData:     lleva los valores al rango [0, 1].
#   - standardize / standardizeData: media 0 y desviación estándar 1.
# =============================================================================


#' Normalizar un vector numérico
#'
#' @description Desplaza y escala los valores de un vector para que el mínimo
#' pase a ser 0 y el máximo 1.
#'
#' @param x Vector numérico.
#'
#' @details Se aplica \code{(x - min(x)) / (max(x) - min(x))}. Si todos los
#' valores son iguales no se puede dividir entre el rango (que es 0), así que
#' se devuelve un vector de ceros y se avisa con un \code{warning}.
#'
#' @return Vector numérico normalizado, de la misma longitud que \code{x}.
#'
#' @seealso \code{\link{standardize}}, \code{\link{normalizeData}}
#'
#' @examples
#' x <- c(-0.29, 2.11, -3.84, 10.69, 5.30)
#' x.norm <- normalize(x)
#' x.norm
#' range(x.norm)
#' @export
normalize <- function(x) {

  checkNumeric(x, "x")

  min.value <- min(x)
  max.value <- max(x)

  # Variable constante: el rango es 0 y no se puede dividir
  if (max.value == min.value) {
    warning("La variable es constante: se devuelve un vector de ceros")
    return(rep(0, length(x)))
  }

  result <- (x - min.value) / (max.value - min.value)

  return(result)
}


#' Estandarizar un vector numérico
#'
#' @description Transforma los valores de un vector para que tengan media 0 y
#' desviación estándar 1.
#'
#' @param x Vector numérico.
#'
#' @details Se resta la media y se divide entre la desviación estándar:
#' \code{(x - mean(x)) / sd(x)}. Si la desviación estándar es 0 (todos los
#' valores iguales) se devuelve un vector de ceros y se avisa con un
#' \code{warning}.
#'
#' @return Vector numérico estandarizado, de la misma longitud que \code{x}.
#'
#' @seealso \code{\link{normalize}}, \code{\link{standardizeData}}
#'
#' @examples
#' x <- c(-0.29, 2.11, -3.84, 10.69, 5.30)
#' x.std <- standardize(x)
#' x.std
#' mean(x.std)
#' sd(x.std)
#' @export
standardize <- function(x) {

  checkNumeric(x, "x")

  mean.value <- mean(x)
  sd.value <- sd(x)

  # Con un solo valor sd() da NA, y con valores iguales da 0
  if (is.na(sd.value) || sd.value == 0) {
    warning("La desviacion estandar es 0: se devuelve un vector de ceros")
    return(rep(0, length(x)))
  }

  result <- (x - mean.value) / sd.value

  return(result)
}


#' Normalizar las variables numéricas de un dataset
#'
#' @description Aplica \code{\link{normalize}} a todas las variables numéricas
#' de un dataset, de modo que cada una quede en el rango [0, 1]. Las variables
#' no numéricas se dejan como están.
#'
#' @param data \code{data.frame}, matriz u objeto de clase \code{dataset}.
#'
#' @details Es una función genérica. Si \code{data} es un objeto de clase
#' \code{dataset}, la variable clase no se modifica aunque sea numérica.
#'
#' @return Los datos con las variables numéricas normalizadas, del mismo tipo
#' que \code{data} (\code{data.frame} u objeto \code{dataset}).
#'
#' @seealso \code{\link{normalize}}, \code{\link{standardizeData}}
#'
#' @examples
#' iris.norm <- normalizeData(iris)
#' summary(iris.norm)
#' @export
normalizeData <- function(data) {
  UseMethod("normalizeData")
}


#' @rdname normalizeData
#' @export
normalizeData.default <- function(data) {
  data <- toDataFrame(data)
  return(transformNumericColumns(data, FUN = normalize))
}


#' @rdname normalizeData
#' @export
normalizeData.dataset <- function(data) {

  # Transformamos solo las variables que no son la clase
  predictors <- getPredictorNames(data)
  data$data[predictors] <- normalizeData.default(data$data[predictors])

  return(data)
}


#' Estandarizar las variables numéricas de un dataset
#'
#' @description Aplica \code{\link{standardize}} a todas las variables
#' numéricas de un dataset, de modo que cada una tenga media 0 y desviación
#' estándar 1. Las variables no numéricas se dejan como están.
#'
#' @param data \code{data.frame}, matriz u objeto de clase \code{dataset}.
#'
#' @details Es una función genérica. Si \code{data} es un objeto de clase
#' \code{dataset}, la variable clase no se modifica aunque sea numérica.
#'
#' @return Los datos con las variables numéricas estandarizadas, del mismo tipo
#' que \code{data} (\code{data.frame} u objeto \code{dataset}).
#'
#' @seealso \code{\link{standardize}}, \code{\link{normalizeData}}
#'
#' @examples
#' iris.std <- standardizeData(iris)
#' sapply(iris.std[1:4], mean)
#' sapply(iris.std[1:4], sd)
#' @export
standardizeData <- function(data) {
  UseMethod("standardizeData")
}


#' @rdname standardizeData
#' @export
standardizeData.default <- function(data) {
  data <- toDataFrame(data)
  return(transformNumericColumns(data, FUN = standardize))
}


#' @rdname standardizeData
#' @export
standardizeData.dataset <- function(data) {

  predictors <- getPredictorNames(data)
  data$data[predictors] <- standardizeData.default(data$data[predictors])

  return(data)
}
