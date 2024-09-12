#' The OMOP `bundle` class
#' @export
bundle <- function(x, id, domain, version = "latest") {
  x <- as.character(x)
  validate_bundle(new_bundle(x, id = id, domain = domain, version = version))
}

#' Test if an object is an OMOP bundle
#' ...
#' @export
is_bundle <- function(x) {
  inherits(x, "omop_bundle")
}

new_bundle <- function(x = character(), id = "", domain = "", version = "latest") {
  stopifnot(is.character(x))
  stopifnot(is.character(id))
  stopifnot(is.character(domain))

  vctrs::new_vctr(x,
    class = "omop_bundle",
    id = id,
    domain = domain,
    version = version
  )
}

validate_bundle <- function(x) {
  concept_ids <- unclass(x)
  id <- attr(x, "id")
  domain <- attr(x, "domain")
  version <- attr(x, "version")
  
  if (any(is.na(concept_ids)) || any(concept_ids == "")) {
    rlang::abort("All concept IDs must be non-missing and non-empty strings")
  }
  if (length(id) != 1 || id == "") {
    rlang::abort("The bundle ID must be a single non-empty string") 
  }
  if (length(domain) != 1 || domain == "") {
    rlang::abort("The bundle domain must be a single non-empty string") 
  }
  if (length(version) != 1 || version == "") {
    rlang::abort("The bundle version must be a single non-empty string") 
  }
  
  ## Return the input so we can reuse the validator in the helper
  x
}