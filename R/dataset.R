# =============================================================================
# dataset.R
# -----------------------------------------------------------------------------
# Gestión de datasets: definición de la clase S3 'dataset'.
#
# Se ha elegido S3 en lugar de S4 porque es suficiente para lo que se necesita
# (agrupar los datos con alguna información extra) y es más sencillo de usar.
# Un objeto 'dataset' es una lista con tres elementos:
#   - data:       data.frame con todas las variables (incluida la clase).
#   - class.name: nombre de la variable clase o NULL si no hay.
#   - name:       nombre descriptivo del dataset o NULL.
#
# El tipo de cada variable es el tipo de su columna en 'data': numeric o
# integer para las numéricas, factor o logical para las categóricas y
# character para los textos (a los que no se les calcula ninguna métrica).
#
# Las funciones de preprocesado del paquete son funciones genéricas con un
# método para data.frames/matrices y otro para objetos 'dataset'. El método
# para 'dataset' sabe cuál es la variable clase, de modo que no la transforma
# (no la normaliza, no la discretiza...) y la usa automáticamente para el AUC.
# =============================================================================


#' Crear un objeto de clase dataset
#'
#' @description Crea un objeto S3 de clase \code{dataset}, que agrupa un
#' \code{data.frame} con el nombre de su variable clase (opcional) y un nombre
#' descriptivo (opcional).
#'
#' @param data \code{data.frame} (o matriz, que se convierte en
#' \code{data.frame}) con los datos. Cada columna es una variable.
#' @param class.name Nombre de la columna que actúa como variable clase. Por
#' defecto \code{NULL}, es decir, un dataset no supervisado.
#' @param name Texto con un nombre descriptivo para el dataset. Por defecto
#' \code{NULL}.
#'
#' @details Internamente el objeto es una lista con tres elementos:
#' \code{data}, \code{class.name} y \code{name}, a los que se puede acceder con
#' el operador \code{$}.
#'
#' El tipo de cada variable es el tipo de su columna en \code{data}:
#' \code{numeric} o \code{integer} para las variables numéricas,
#' \code{factor} o \code{logical} para las categóricas y \code{character} para
#' los textos.
#'
#' Las funciones de preprocesado del paquete (\code{\link{discretizeData}},
#' \code{\link{normalizeData}}, \code{\link{standardizeData}},
#' \code{\link{computeMetrics}}, \code{\link{filterData}} y
#' \code{\link{computeCorrelations}}) tienen un método específico para esta
#' clase que tiene en cuenta la variable clase: no la modifica y la usa
#' automáticamente para calcular el AUC.
#'
#' @return Un objeto de clase \code{dataset}.
#'
#' @seealso \code{\link{isDataset}}, \code{\link{print.dataset}},
#' \code{\link{summary.dataset}}, \code{\link{readDataset}}
#'
#' @examples
#' ds <- createDataset(mtcars, class.name = "am", name = "Coches")
#' ds
#' ds$class.name
#' @export
createDataset <- function(data, class.name = NULL, name = NULL) {

  # Aceptamos también matrices, que convertimos en data.frame
  data <- toDataFrame(data)

  # La variable clase, si se indica, tiene que ser una de las columnas
  if (!is.null(class.name)) {
    if (length(class.name) != 1 || !(class.name %in% names(data))) {
      stop("'class.name' debe ser el nombre de una de las columnas de 'data'")
    }
  }

  if (!is.null(name) && (!is.character(name) || length(name) != 1)) {
    stop("'name' debe ser un texto")
  }

  # El objeto es una lista a la que añadimos el atributo class
  dataset <- list(data = data, class.name = class.name, name = name)
  class(dataset) <- "dataset"

  return(dataset)
}


#' Comprobar si un objeto es un dataset válido
#'
#' @description Comprueba que un objeto sea de clase \code{dataset} y que su
#' contenido sea coherente.
#'
#' @param x Objeto a comprobar.
#'
#' @return \code{TRUE} si \code{x} es un objeto de clase \code{dataset} válido
#' y \code{FALSE} en caso contrario.
#'
#' @examples
#' ds <- createDataset(mtcars, class.name = "am")
#' isDataset(ds)
#' isDataset(mtcars)
#' @export
isDataset <- function(x) {

  # inherits() indica si el objeto tiene la clase 'dataset'
  if (!inherits(x, "dataset")) {
    return(FALSE)
  }

  # Al ser S3, nada impide modificar el objeto a mano, así que comprobamos
  # también que su contenido tenga sentido.
  valid.data <- is.data.frame(x$data)
  valid.class <- is.null(x$class.name) || x$class.name %in% names(x$data)

  return(valid.data && valid.class)
}


#' Mostrar un objeto dataset
#'
#' @description Método \code{print} para objetos de clase \code{dataset}.
#' Muestra el nombre, las dimensiones, la variable clase, el tipo de cada
#' variable y las primeras filas de los datos.
#'
#' @param x Objeto de clase \code{dataset}.
#' @param ... No se usa (necesario por compatibilidad con la función
#' genérica \code{print}).
#'
#' @return Devuelve \code{x} de forma invisible.
#'
#' @examples
#' ds <- createDataset(iris, class.name = "Species", name = "Iris")
#' print(ds)
#' @export
print.dataset <- function(x, ...) {

  cat("Objeto de clase 'dataset'\n")

  if (!is.null(x$name)) {
    cat("Nombre:", x$name, "\n")
  }

  cat("Dimensiones:", nrow(x$data), "filas x", ncol(x$data), "variables\n")

  if (is.null(x$class.name)) {
    cat("Variable clase: ninguna (dataset no supervisado)\n")
  } else {
    cat("Variable clase:", x$class.name, "\n")
  }

  # Tipo de cada variable (nos quedamos con la primera clase de cada columna)
  types <- sapply(x$data, FUN = function(column) {
    return(class(column)[1])
  })

  cat("\nTipos de las variables:\n")
  print(types, quote = FALSE)

  cat("\nPrimeras filas:\n")
  print(head(x$data))

  invisible(x)
}


#' Resumen de un objeto dataset
#'
#' @description Método \code{summary} para objetos de clase \code{dataset}.
#' Devuelve las métricas de todas las variables calculadas con
#' \code{\link{computeMetrics}}, usando la variable clase del dataset para el
#' AUC. Si la variable clase no es binaria, el AUC queda como \code{NA}.
#'
#' @param object Objeto de clase \code{dataset}.
#' @param ... No se usa (necesario por compatibilidad con la función
#' genérica \code{summary}).
#'
#' @return El \code{data.frame} de métricas que devuelve
#' \code{\link{computeMetrics}}.
#'
#' @examples
#' datos <- mtcars
#' datos$am <- factor(datos$am)
#' ds <- createDataset(datos, class.name = "am")
#' summary(ds)
#' @export
summary.dataset <- function(object, ...) {
  return(computeMetrics(object))
}


# -----------------------------------------------------------------------------
# FUNCIÓN INTERNA: getPredictorNames
# -----------------------------------------------------------------------------
# Devuelve los nombres de las variables de un dataset que no son la clase.
# La usan los métodos para 'dataset' de las funciones de preprocesado.
# -----------------------------------------------------------------------------

getPredictorNames <- function(dataset) {
  return(setdiff(names(dataset$data), dataset$class.name))
}
