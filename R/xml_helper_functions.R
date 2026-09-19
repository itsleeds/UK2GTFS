# TransXchange import functions

#' Import Simple
#' ????
#' @param xml1 XML object
#' @param nm name to find
#' @noRd
import_simple <- function(xml1, nm) {
  xml2::xml_text(xml2::xml_find_all(xml1, nm))
}

#' Import Via Loop
#' Loops over children
#' @param xml1 XML object
#' @param nm name to find
#' @noRd
import_vialoop <- function(xml1, nm) {
  res <- list()
  for (i in seq(1, xml2::xml_length(xml1))) {
    chld <- xml2::xml_child(xml1, i)
    chld <- xml2::xml_text(xml2::xml_child(chld, nm))
    if (length(chld) == 0) {
      chld <- NA
    }
    res[[i]] <- chld
  }
  res <- unlist(res)
  return(res)
}


#' Import all
#' Modified version of xml2::xml_find_all combined with import_simple
#' Shoudl handel missing values at higher speeds
#' @param xml1 XML object
#' @param nm name to find
#' @noRd
import_simple_xml <- function(xml1, nm) {
  if (length(xml1) == 0) {
    return(xml1)
  }

  nodes <- xml2::xml_find_first(xml1, nm)
  nodes <- xml2::xml_text(nodes)
  return(nodes)
}
# import_simple_xml <- function(xml1, nm) {
#   if (length(xml1) == 0) {
#     return(xml1)
#   }
#   # return(xml_nodeset())
#
#   nodes <- lapply(xml1, function(x) {
#     res <- xml2:::xpath_search(x$node,
#                                x$doc,
#                                xpath = nm,
#                                nsMap = xml2::xml_ns(x),
#                                num_results = Inf
#     )
#
#     # Alt method see https://stackoverflow.com/questions/35103804/what-is-the-preferred-method-for-sharing-compiled-c-code-in-an-r-package-and-run
#     # res <- .Call("xpath_search",
#     #   x$node,
#     #   x$doc,
#     #   xpath = nm,
#     #   nsMap = xml2::xml_ns(x),
#     #   num_results = Inf,
#     #   PACKAGE="xml2"
#     # )
#
#     if (length(res) == 0) {
#       return(NA)
#     } else if (length(res) == 1) {
#       res <- xml2::xml_text(res[[1]])
#       return(res)
#     } else {
#       stop("res is not of length 0 or 1")
#     }
#   })
#   nodes <- unlist(nodes, recursive = FALSE)
#   return(nodes)
# }



#' Import When some rows are missing
#' Checks lengths of obejct against lgth
#' @param xml1 XML object
#' @param nm character name to find
#' @param lgth numeric length check
#' @noRd
import_withmissing <- function(xml1, nm, lgth) {
  xml2 <- import_simple(xml1, nm)
  ids <- xml2::xml_length(xml2::xml_children(xml1))
  ids <- ids == lgth
  ids <- cumsum(ids)
  ids[duplicated(ids)] <- NA
  xml2 <- xml2[ids]
  return(xml2)
}

#' Import When some rows are missing
#' Goes down mulitple layers and returns a value with NA for missing
#' @param xml1 XML object
#' @param nm character name to find
#' @param layers how many layers down
#' @param idvar the id variaible in the higher tree
#' @noRd
import_withmissing2 <- function(xml1, nm, layers, idvar) {
  xml_2 <- xml2::xml_find_all(xml1, nm)
  xml2_parent <- xml2::xml_parent(xml_2)
  if (layers > 1) {
    for (i in seq(2, layers)) {
      xml2_parent <- xml2::xml_parent(xml2_parent)
    }
  }
  xml2_parent_id <- xml2::xml_text(xml2::xml_find_all(xml2_parent, idvar))
  xml1_id <- xml2::xml_text(xml2::xml_find_all(xml1, idvar))

  res <- rep(NA, length(xml1_id))
  res[match(xml2_parent_id, xml1_id)] <- xml2::xml_text(xml_2)
  return(res)
}

#' Pull several child elements from a nodeset in one pass
#'
#' For each node in `parents`, returns the text of its direct child named by
#' each entry of `wanted`, NA where that child is absent, aligned to `parents`.
#' This is what repeated `import_simple_xml()` calls produce, but without the
#' per-node XPath.
#'
#' xml2 evaluates `xml_find_all()` / `xml_find_first()` on a nodeset by looping
#' over it in R, one `.Call` per node, while `xml_children()`, `xml_name()`,
#' `xml_text()` and `xml_length()` are vectorised at C level. On an 8MB
#' TransXchange file with 11,000 journey pattern timing links that is the
#' difference between about 0.19s and about 0.02s per column, and the importer
#' pulls a dozen such columns. `import_OperatingProfile()` already works this
#' way - see `paste_child_names()` there.
#'
#' Where a parent has more than one child of the same name the first wins, as
#' `xml_find_first()` did.
#'
#' @param parents an xml_nodeset
#' @param wanted character vector of child element names, without the `d1:`
#'   namespace prefix
#' @return a named list of character vectors, each `length(parents)` long
#' @noRd
kid_cols <- function(parents, wanted) {
  n <- length(parents)
  counts <- xml2::xml_length(parents)
  kids <- xml2::xml_children(parents)
  grp <- rep(seq_len(n), times = counts)
  nm <- xml2::xml_name(kids)
  tx <- xml2::xml_text(kids)

  out <- lapply(wanted, function(w) {
    v <- rep(NA_character_, n)
    # reversed, so that if a parent repeats a child the earliest one is
    # assigned last and therefore survives
    sel <- rev(which(nm == w))
    v[grp[sel]] <- tx[sel]
    v
  })
  names(out) <- wanted
  out
}
