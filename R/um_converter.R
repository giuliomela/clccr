#' Perform measurement unit conversion
#'
#' A function that wraps upon `units::set_units` to perform measurement unit conversions,
#'
#' @param value A numeric value. It can also be a vector. Values to convert.
#' @param from A string. The measurement units from which the conversion must be performed.
#' @param to A strong. The measurement units to which the conversion must be performed.
#' @return A numeric value (dimensionless): the converted value.
#'
um_converter <- function (value, from, to) {


  if (from == to) {

    value

  } else {

    units::units_options(set_units_mode = "standard") # allowing the use of sgtrings to set measurement units

    if (isFALSE(units::ud_are_convertible(from, to)))
      stop("Measurement units are not convertible. Review query or define new units with units::install_unit")

    first_step <- units::set_units(value, from)

    second_step <- units::set_units(first_step, to)

    as.numeric(second_step)

  }

}
