# =============================================================================
# correlation.R
# -----------------------------------------------------------------------------
# Relación por pares entre las variables de un dataset:
#   - correlation:         correlación de Pearson entre dos variables numéricas.
#   - mutualInformation:   información mutua entre dos variables categóricas.
#   - computeCorrelations: matrices de correlación (variables numéricas) e
#                          información mutua (variables categóricas) de un
#                          dataset, teniendo en cuenta el tipo de cada variable.
# =============================================================================


#' Correlación de Pearson entre dos variables numéricas
#'
#' @description Calcula el coeficiente de correlación lineal de Pearson entre
#' dos vectores numéricos de la misma longitud.
#'
#' @param x Vector numérico.
#' @param y Vector numérico de la misma longitud que \code{x}.
#'
#' @details Se calcula como
#' \deqn{r = \frac{\sum (x_i - \bar{x})(y_i - \bar{y})}{\sqrt{\sum (x_i - \bar{x})^2 \sum (y_i - \bar{y})^2}}}{r = sum((x - mean(x)) * (y - mean(y))) / sqrt(sum((x - mean(x))^2) * sum((y - mean(y))^2))}
#' El resultado es el mismo que el de la función \code{cor} de R, pero
#' implementado a mano.
#'
#' @return Valor entre -1 y 1. Si alguna de las variables es constante la
#' correlación no está definida y se devuelve \code{NA}.
#'
#' @seealso \code{\link{computeCorrelations}}
#'
#' @examples
#' correlation(mtcars$mpg, mtcars$wt)
#' @export
correlation <- function(x, y) {

  checkNumeric(x, "x")
  checkNumeric(y, "y")

  if (length(x) != length(y)) {
    stop("'x' e 'y' deben tener la misma longitud")
  }

  # Centramos las dos variables restando su media
  x.centered <- x - mean(x)
  y.centered <- y - mean(y)

  denominator <- sqrt(sum(x.centered^2) * sum(y.centered^2))

  # Si una variable es constante el denominador es 0
  if (denominator == 0) {
    return(NA)
  }

  return(sum(x.centered * y.centered) / denominator)
}


#' Información mutua entre dos variables categóricas
#'
#' @description Calcula la información mutua (en bits) entre dos variables
#' discretas de tipo factor. Mide cuánta información aporta conocer una de las
#' variables sobre la otra: es 0 si son independientes.
#'
#' @param x Factor.
#' @param y Factor de la misma longitud que \code{x}.
#'
#' @details Se usa la relación entre la información mutua y la entropía:
#' \deqn{I(X; Y) = H(X) + H(Y) - H(X, Y)}{I(X; Y) = H(X) + H(Y) - H(X, Y)}
#' donde \eqn{H(X, Y)} es la entropía conjunta, que se calcula con
#' \code{\link{entropy}} sobre la variable que combina los valores de ambas
#' (cada par de valores observado es un valor posible de la variable conjunta).
#' La información mutua de una variable consigo misma es su entropía.
#'
#' @return Valor numérico mayor o igual que 0.
#'
#' @seealso \code{\link{entropy}}, \code{\link{computeCorrelations}}
#'
#' @examples
#' cyl <- factor(mtcars$cyl)
#' gear <- factor(mtcars$gear)
#' mutualInformation(cyl, gear)
#' mutualInformation(cyl, cyl)
#' entropy(cyl)
#' @export
mutualInformation <- function(x, y) {

  if (!is.factor(x) || !is.factor(y)) {
    stop("'x' e 'y' deben ser factores")
  }

  if (length(x) != length(y)) {
    stop("'x' e 'y' deben tener la misma longitud")
  }

  # Variable conjunta: cada observación pasa a ser el par (valor de x, valor
  # de y). Usamos los códigos numéricos de los niveles para que dos pares
  # distintos nunca den el mismo texto.
  joint <- factor(paste(as.integer(x), as.integer(y), sep = "_"))

  mi.value <- entropy(x) + entropy(y) - entropy(joint)

  # Por errores de redondeo puede salir algo como -1e-16 en vez de 0
  return(max(0, mi.value))
}


#' Correlación e información mutua por pares entre las variables de un dataset
#'
#' @description Calcula, para cada par de variables de un dataset, la
#' correlación de Pearson si ambas son numéricas o la información mutua si
#' ambas son categóricas. La función tiene en cuenta el tipo de cada variable.
#'
#' @param data \code{data.frame}, matriz u objeto de clase \code{dataset}.
#'
#' @details Las variables numéricas (\code{numeric} e \code{integer}) se
#' comparan entre sí con \code{\link{correlation}} y las categóricas
#' (\code{factor} y \code{logical}) con \code{\link{mutualInformation}}. Como
#' la correlación y la información mutua miden cosas distintas y tienen escalas
#' diferentes, se devuelven en dos matrices separadas. Los pares formados por
#' una variable numérica y otra categórica no se comparan; si se quiere
#' estudiar su relación se puede discretizar antes la variable numérica con
#' \code{\link{discretizeData}}.
#'
#' En un objeto \code{dataset} la variable clase también se incluye, ya que es
#' interesante ver su relación con el resto de variables.
#'
#' @return Una lista con dos elementos:
#' \item{correlation}{Matriz de correlaciones entre las variables numéricas
#' (\code{NULL} si no hay ninguna).}
#' \item{mutual.information}{Matriz de información mutua entre las variables
#' categóricas (\code{NULL} si no hay ninguna).}
#'
#' @seealso \code{\link{plotCorrelation}}
#'
#' @examples
#' datos <- mtcars
#' datos$cyl <- factor(datos$cyl)
#' datos$gear <- factor(datos$gear)
#' datos$am <- factor(datos$am)
#' result <- computeCorrelations(datos)
#' round(result$correlation, 2)
#' round(result$mutual.information, 3)
#' @export
computeCorrelations <- function(data) {
  UseMethod("computeCorrelations")
}


#' @rdname computeCorrelations
#' @export
computeCorrelations.default <- function(data) {

  data <- toDataFrame(data)

  # Separamos las variables según su tipo
  numeric.columns <- names(data)[sapply(data, is.numeric)]
  categorical.columns <- names(data)[sapply(data, isCategorical)]

  result <- list(correlation = NULL, mutual.information = NULL)

  # Correlación entre todos los pares de variables numéricas
  if (length(numeric.columns) > 0) {
    result$correlation <- pairwiseMatrix(data[numeric.columns],
                                         FUN = correlation)
  }

  # Información mutua entre todos los pares de variables categóricas
  if (length(categorical.columns) > 0) {

    categorical.data <- data[categorical.columns]

    # Las variables lógicas se pasan a factor para poder usar entropy()
    categorical.data[] <- lapply(categorical.data, FUN = function(x) {
      if (is.logical(x)) {
        x <- factor(x)
      }
      return(x)
    })

    result$mutual.information <- pairwiseMatrix(categorical.data,
                                                FUN = mutualInformation)
  }

  return(result)
}


#' @rdname computeCorrelations
#' @export
computeCorrelations.dataset <- function(data) {
  return(computeCorrelations.default(data$data))
}
