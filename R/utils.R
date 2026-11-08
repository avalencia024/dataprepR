# =============================================================================
# utils.R
# -----------------------------------------------------------------------------
# Funciones auxiliares (internas) que usan el resto de ficheros del paquete.
# No se exportan, por lo que no aparecen en la ayuda del paquete, pero se
# documentan con comentarios normales para que el código se entienda.
# =============================================================================


# -----------------------------------------------------------------------------
# FUNCIÓN INTERNA: checkNumeric
# -----------------------------------------------------------------------------
# Comprueba que 'x' sea un vector numérico sin valores NA ni infinitos. Si no
# lo es, para la ejecución con un mensaje de error.
#
# Entradas:
#   x         Objeto a comprobar.
#   arg.name  Nombre del argumento, para que el mensaje de error sea claro.
#
# Salida:
#   Nada (TRUE de forma invisible). Solo sirve para validar.
# -----------------------------------------------------------------------------

checkNumeric <- function(x, arg.name = "x") {

  if (!is.numeric(x)) {
    stop("'", arg.name, "' debe ser un vector numerico")
  }

  if (length(x) == 0) {
    stop("'", arg.name, "' no puede ser un vector vacio")
  }

  # Para simplificar, en todo el paquete asumimos datos sin valores perdidos
  # ni infinitos. is.finite() es FALSE tanto para NA como para Inf y -Inf.
  if (any(!is.finite(x))) {
    stop("'", arg.name, "' contiene valores NA o infinitos")
  }

  invisible(TRUE)
}


# -----------------------------------------------------------------------------
# FUNCIÓN INTERNA: toDataFrame
# -----------------------------------------------------------------------------
# Convierte la entrada en un data.frame. Permite que las funciones que
# trabajan con datasets completos acepten tanto data.frames como matrices.
#
# Entrada:
#   data  data.frame o matriz.
#
# Salida:
#   data.frame con los mismos datos.
# -----------------------------------------------------------------------------

toDataFrame <- function(data) {

  if (is.matrix(data)) {
    data <- as.data.frame(data)
  }

  if (!is.data.frame(data)) {
    stop("Los datos deben ser un data.frame, una matriz o un objeto 'dataset'")
  }

  return(data)
}


# -----------------------------------------------------------------------------
# FUNCIÓN INTERNA: isCategorical
# -----------------------------------------------------------------------------
# Indica si una variable se considera categórica (factor o lógica).
# Los vectores de tipo character NO se consideran categóricos: siguiendo el
# criterio de readCSV, un texto con muchos valores distintos (nombres, ids...)
# no es una variable categórica.
# -----------------------------------------------------------------------------

isCategorical <- function(x) {
  return(is.factor(x) || is.logical(x))
}


# -----------------------------------------------------------------------------
# FUNCIÓN INTERNA: toBinaryClass
# -----------------------------------------------------------------------------
# Convierte una variable clase binaria en un vector lógico (TRUE = positivo).
#
# Entrada:
#   labels  Vector lógico, factor con dos niveles o vector (numérico o de
#           texto) con exactamente dos valores distintos.
#
# Salida:
#   Vector lógico. Si 'labels' ya es lógico se devuelve tal cual. En otro caso
#   se considera positivo el segundo valor posible: el segundo nivel en un
#   factor (de los que aparecen en los datos) o el mayor de los dos valores en
#   un vector (por ejemplo, el 1 en una variable 0/1).
# -----------------------------------------------------------------------------

toBinaryClass <- function(labels) {

  if (is.logical(labels)) {
    return(labels)
  }

  # Posibles valores de la clase
  possible.values <- getClassValues(labels)

  if (length(possible.values) != 2) {
    stop("La variable clase debe ser binaria (exactamente dos valores posibles)")
  }

  return(labels == possible.values[2])
}


# -----------------------------------------------------------------------------
# FUNCIÓN INTERNA: getClassValues
# -----------------------------------------------------------------------------
# Devuelve, ordenados, los valores distintos que toma una variable clase. En
# un factor se usan sus niveles, pero solo los que aparecen en los datos
# (droplevels): un nivel sin ningún valor, por ejemplo después de filtrar
# filas, no cuenta como un valor más de la clase.
# -----------------------------------------------------------------------------

getClassValues <- function(labels) {

  if (is.factor(labels)) {
    return(levels(droplevels(labels)))
  }

  return(sort(unique(labels)))
}


# -----------------------------------------------------------------------------
# FUNCIÓN INTERNA: isBinaryClass
# -----------------------------------------------------------------------------
# Indica si una variable clase es binaria (toma exactamente dos valores), que
# es lo necesario para poder calcular el AUC.
# -----------------------------------------------------------------------------

isBinaryClass <- function(labels) {
  return(length(getClassValues(labels)) == 2)
}


# -----------------------------------------------------------------------------
# FUNCIÓN INTERNA: checkBinaryClass
# -----------------------------------------------------------------------------
# Para la ejecución con un mensaje de error si 'class.name' no es el nombre de
# una variable clase binaria de 'data'. La usan las funciones que solo tienen
# sentido con el AUC (filterData con metric = "auc" y plotAUC).
#
# Entradas:
#   data        data.frame con los datos.
#   class.name  Nombre de la variable clase (puede ser NULL).
# -----------------------------------------------------------------------------

checkBinaryClass <- function(data, class.name) {

  if (is.null(class.name) || !(class.name %in% names(data)) ||
      !isBinaryClass(data[[class.name]])) {
    stop("Para calcular el AUC hay que indicar una variable clase binaria ",
         "('class.name')")
  }

  invisible(TRUE)
}


# -----------------------------------------------------------------------------
# FUNCIÓN INTERNA: transformNumericColumns
# -----------------------------------------------------------------------------
# Aplica una función (por ejemplo normalize o standardize) a todas las
# columnas numéricas de un data.frame, dejando el resto sin cambios.
# Se usa en normalizeData y standardizeData para no repetir código.
#
# Entradas:
#   data  data.frame.
#   FUN   Función que recibe un vector numérico y devuelve otro igual de largo.
#
# Salida:
#   data.frame con las columnas numéricas transformadas.
# -----------------------------------------------------------------------------

transformNumericColumns <- function(data, FUN) {

  numeric.columns <- sapply(data, is.numeric)

  if (!any(numeric.columns)) {
    warning("No hay variables numericas: los datos se devuelven sin cambios")
    return(data)
  }

  # Usamos data[...] <- lapply(...) para mantener la estructura de data.frame
  # y los nombres de las columnas.
  data[numeric.columns] <- lapply(data[numeric.columns], FUN = FUN)

  return(data)
}


# -----------------------------------------------------------------------------
# FUNCIÓN INTERNA: pairwiseMatrix
# -----------------------------------------------------------------------------
# Calcula una matriz simétrica aplicando una función a cada par de columnas.
# Se usa tanto para la correlación como para la información mutua.
#
# Entradas:
#   data  data.frame con las variables.
#   FUN   Función de dos argumentos (x, y) que devuelve un número.
#
# Salida:
#   Matriz cuadrada con los resultados; filas y columnas tienen el nombre de
#   las variables.
# -----------------------------------------------------------------------------

pairwiseMatrix <- function(data, FUN) {

  variables <- names(data)
  n <- length(variables)

  result <- matrix(NA, nrow = n, ncol = n,
                   dimnames = list(variables, variables))

  # Como la medida es simétrica, solo calculamos la mitad superior de la
  # matriz (j >= i) y copiamos el valor en la posición simétrica.
  for (i in 1:n) {
    for (j in i:n) {
      value <- FUN(data[[i]], data[[j]])
      result[i, j] <- value
      result[j, i] <- value
    }
  }

  return(result)
}
