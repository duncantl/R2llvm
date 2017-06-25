
#' Translate a typesys Type to an Rllvm Type
#'
#' This function converts a typesys Type to an equivalent Rllvm Type.
#'
#' @param type (typesys::Type) A type to convert.
#'
#' @export
translate_type = function(type) {
  UseMethod("translate_type")
}

#' @export
`translate_type.typesys::RealType` = function(type) {
  return (Rllvm::DoubleType)
}

#' @export
`translate_type.typesys::IntegerType` = function(type) {
  return (Rllvm::Int32Type)
}


`translate_type.typesys::ListType` = function(type) {
  return (Rllvm::VECSXPType)
}


#' @export
translate_type.default = function(type) {
  browser()
}
