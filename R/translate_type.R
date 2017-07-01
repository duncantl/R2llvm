
#' Translate a typesys Type to an Rllvm Type
#'
#' This function converts a typesys Type to an equivalent Rllvm Type.
#'
#' @param type (typesys::Type) A type to convert.
#'
#' @export
translate_type = function(type, ...) {
  UseMethod("translate_type")
}

#' @export
`translate_type.typesys::RealType` = function(type, ...) {
  return (Rllvm::DoubleType)
}

`translate_type.typesys::BooleanType` = function(type, ...) {
  return (Rllvm::Int32Type) 
}

#' @export
`translate_type.typesys::IntegerType` = function(type, ...) {
  return (Rllvm::Int32Type)
}


`translate_type.typesys::ListType` = function(type, ...) {
  return (Rllvm::VECSXPType)
}

`translate_type.typesys::ArrayType` = function(type, ...)
{
  elt = type@types
  if(length(elt) == 1) {
    if(is(elt[[1]], "typesys::RealType") )
        return(Rllvm::REALSXPType)
    else if(is(elt[[1]], "typesys::IntegerType") )
        return(Rllvm::INTSXPType)
    else if(is(elt[[1]], "typesys::BooleanType") )
        return(Rllvm::LGLSXPType)
  }
      
  Rllvm::VECSXPType
}



#' @export
translate_type.default = function(type, ...) {
  browser()
}


`translate_type.typesys::StringType` = function(type, ...) {
  return (Rllvm::STRSXPType)
}

`translate_type.typesys::SEXPType` = function(type, ...) {
  return (Rllvm::SEXPType)
}

`translate_type.typesys::LISTSEXPType` = function(type, ...) {
  return (Rllvm::VECSXPType)
}
