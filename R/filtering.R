# =============================================================================
# filtering.R
# -----------------------------------------------------------------------------
# Filtrado de variables en función de las métricas de computeMetrics:
# a partir de un dataset se obtiene otro con solo las variables que cumplen
# el requisito indicado (por ejemplo, una entropía mayor que un umbral).
# =============================================================================


#' Filtrar las variables de un dataset según una métrica
#'
#' @description A partir de un dataset obtiene uno nuevo que solo contiene las
#' variables que cumplen un requisito sobre una de las métricas implementadas
#' (varianza, AUC o entropía). Por ejemplo, quedarse con las variables cuya
#' entropía sea mayor que un cierto umbral.
#'
#' @param data \code{data.frame}, matriz u objeto de clase \code{dataset}.
#' @param metric Métrica a usar: \code{"variance"}, \code{"auc"} o
#' \code{"entropy"}.
#' @param threshold Valor umbral.
#' @param greater Si es \code{TRUE} (valor por defecto) se mantienen las
#' variables con un valor de la métrica estrictamente mayor que
#' \code{threshold}; si es \code{FALSE}, las que tengan un valor estrictamente
#' menor.
#' @param keep.others Qué hacer con las variables a las que no se les aplica la
#' métrica (por ejemplo, las variables numéricas al filtrar por entropía). Si
#' es \code{FALSE} (valor por defecto) se eliminan, de modo que en el resultado
#' solo quedan las variables que cumplen el requisito, además de la clase. Si
#' es \code{TRUE} se mantienen sin cambios, lo que permite encadenar filtros
#' sobre métricas distintas.
#' @param class.name Nombre de la variable clase. Es necesaria para filtrar
#' por AUC, y en ese caso tiene que ser binaria. La variable clase nunca se
#' elimina. En un objeto \code{dataset} se usa por defecto su propia variable
#' clase.
#'
#' @details Las métricas se calculan con \code{\link{computeMetrics}}, así que
#' la varianza y el AUC se aplican a las variables numéricas y la entropía a
#' las categóricas. Para aplicar varios requisitos a la vez basta con
#' encadenar varias llamadas a la función; si los requisitos son sobre
#' métricas de tipos distintos (por ejemplo, AUC y entropía), hay que usar
#' \code{keep.others = TRUE} para que el primer filtro no elimine las
#' variables del otro tipo.
#'
#' @return Los datos filtrados, del mismo tipo que \code{data}
#' (\code{data.frame} u objeto \code{dataset}). Las variables mantienen su
#' orden original.
#'
#' @seealso \code{\link{computeMetrics}}
#'
#' @examples
#' datos <- mtcars
#' datos$cyl <- factor(datos$cyl)
#' datos$gear <- factor(datos$gear)
#'
#' # Variables con varianza mayor que 10
#' names(filterData(datos, metric = "variance", threshold = 10))
#'
#' # Variables con AUC mayor que 0.7 respecto a la clase "am" (la clase se
#' # mantiene siempre)
#' names(filterData(datos, metric = "auc", threshold = 0.7, class.name = "am"))
#'
#' # Variables con entropía mayor que 1.5
#' names(filterData(datos, metric = "entropy", threshold = 1.5))
#'
#' # Lo mismo, pero manteniendo las variables a las que no se aplica la
#' # entropía (las numéricas)
#' names(filterData(datos, metric = "entropy", threshold = 1.5,
#'                  keep.others = TRUE))
#' @export
filterData <- function(data, metric, threshold, greater = TRUE,
                       keep.others = FALSE, class.name = NULL) {
  UseMethod("filterData")
}


#' @rdname filterData
#' @export
filterData.default <- function(data, metric, threshold, greater = TRUE,
                               keep.others = FALSE, class.name = NULL) {

  data <- toDataFrame(data)

  # Control de los argumentos
  if (!(metric %in% c("variance", "auc", "entropy"))) {
    stop("'metric' debe ser 'variance', 'auc' o 'entropy'")
  }

  # Para filtrar por AUC hace falta una clase binaria. Si no lo comprobamos
  # aquí, computeMetrics dejaría el AUC como NA y no se filtraría nada.
  if (metric == "auc") {
    checkBinaryClass(data, class.name)
  }

  if (!is.numeric(threshold) || length(threshold) != 1 || !is.finite(threshold)) {
    stop("'threshold' debe ser un unico valor numerico finito")
  }

  # Calculamos las métricas de todas las variables y nos quedamos con la
  # columna de la métrica elegida
  metrics <- computeMetrics(data, class.name = class.name)
  values <- metrics[[metric]]

  # ¿Qué variables cumplen el requisito? (NA si la métrica no se aplica)
  if (greater) {
    satisfies <- values > threshold
  } else {
    satisfies <- values < threshold
  }

  not.applicable <- is.na(values)

  # Variables que se mantienen
  if (keep.others) {
    keep <- not.applicable | satisfies
  } else {
    keep <- !not.applicable & satisfies
  }

  # Usamos which() para que los NA cuenten como FALSE
  selected <- metrics$variable[which(keep)]

  # Mantenemos el orden original de las columnas y siempre la clase
  columns <- names(data)[names(data) %in% c(selected, class.name)]

  return(data[, columns, drop = FALSE])
}


#' @rdname filterData
#' @export
filterData.dataset <- function(data, metric, threshold, greater = TRUE,
                               keep.others = FALSE,
                               class.name = data$class.name) {

  data$data <- filterData.default(data$data,
                                  metric = metric,
                                  threshold = threshold,
                                  greater = greater,
                                  keep.others = keep.others,
                                  class.name = class.name)

  return(data)
}
