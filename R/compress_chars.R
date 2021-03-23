#' Recursively Call \code{\link{char_map}}/\code{\link{map2char}} on a List
#'
#' These originally internal functions are exported because they are also useful
#' for reducing the size of e.g. a data frame before storing it to disk using
#' \code{\link{saveRDS}}. This also improves the (de)serialization speed.
#'
#' @param l an object, typically a list
#' @param limit the minimum length of a character vector for
#'   \code{\link{char_map}} to be applied
#' @param compress_altreps should a character vector be compressed if it is an
#'   ALTREP? The default "if_allocated" only does so if the regular
#'   representation was already created. This was chosen as the default because
#'   in this case is is the regular representation which would be serialized.
#' @param class additional classes to set on the \code{\link{char_map}}-objects
#'   created by \code{compress_chars}. For \code{uncompress_chars}, only call
#'   \code{\link{map2char}} on those \code{\link{char_map}}-objects which
#'   additionally inherit from all these classes.
#'
#' @note The object returned by \code{compress_chars} might be an invalid S3
#'   object, e.g. if \code{l} is a data frame. These functions are intended to
#'   be called immediately before and after (de)serializing the object, i.e.
#'   compress -> serialize -> store/transfer -> de-serialize -> uncompress.
#'
#' @section Lifecycle:
#'   \ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options:
#'    alt='[Experimental]'}}}{\strong{[Experimental]}}
#'
#' @return For \code{compress_chars}, \code{l}, but with character vectors
#'   replaced by objects of class \code{\link{char_map}}. For
#'   \code{uncompress_chars}, \code{l}, but with all
#'   \code{\link{char_map}}-objects, which also inherit from all classes given
#'   in \code{class}, replaced by the original character vectors.
#'
#' @export
compress_chars <- function(l, limit = 0L,
                           compress_altreps = c("if_allocated", "yes", "no"),
                           class = character()) {
  compress_altreps <- match.arg(compress_altreps)

  compress_chars_core <- function(l) {
    if (is.environment(l)) return(l)
    if (inherits(l, c("shm_obj", "char_map"))) return(l)
    if (isS4(l)) return(l)

    if (is.environment(l)) {
      # nocov start
      if (isTRUE(attr(l, "bettermc_recurse_protect"))) return(l)
      attr(l, "bettermc_recurse_protect") <- TRUE
      # nocov end
    }
    cls <- attr(l, "class")
    class(l) <- NULL

    idx <- if (is.list(l)) {
      which(vapply(l, function(e) !identical(e, quote(expr = )), logical(1)))
    } else {
      # nocov start
      all_names <- names(l)
      ok_names <-
        !vapply(all_names, is.missing.arg, logical(1), env = l) &
        !vapply(all_names, is.uneval.promise, logical(1), env = l) &
        !vapply(all_names, bindingIsActive, logical(1), env = l) &
        !vapply(all_names, bindingIsLocked, logical(1), env = l)
      all_names[ok_names]
      # nocov end
    }

    for (i in idx) {
      e <- l[[i]]

      if (is.list(e) || is.environment(e)) {
        l[[i]] <- compress_chars_core(e)
      } else if (is.character(e) && !isS4(e) && length(e) >= limit && (
        !is_altrep(e) ||
        compress_altreps == "yes" ||
        (compress_altreps == "if_allocated" && is_allocated(e))
      )) {
        l[[i]] <- char_map(e)
        class(l[[i]]) <- c(class, class(l[[i]]))
      }
    }

    attr(l, "bettermc_recurse_protect") <- NULL
    class(l) <- cls
    l
  }

  if (!is.list(l) && !is.environment(l)) {
    compress_chars_core(list(l))[[1L]]
  } else {
    compress_chars_core(l)
  }
}

#' @rdname compress_chars
#'
#' @export
uncompress_chars <- function(l, class = character()) {
  uncompress_chars_core <- function(l) {
    if (is.environment(l)) return(l)
    if (isS4(l)) return(l)

    if (is.environment(l)) {
      # nocov start
      if (isTRUE(attr(l, "bettermc_recurse_protect"))) return(l)
      attr(l, "bettermc_recurse_protect") <- TRUE
      # nocov end
    }

    cls <- attr(l, "class")
    class(l) <- NULL

    idx <- if (is.list(l)) {
      which(vapply(l, function(e) !identical(e, quote(expr = )), logical(1)))
    } else {
      # nocov start
      all_names <- names(l)
      ok_names <-
        !vapply(all_names, is.missing.arg, logical(1), env = l) &
        !vapply(all_names, is.uneval.promise, logical(1), env = l) &
        !vapply(all_names, bindingIsActive, logical(1), env = l) &
        !vapply(all_names, bindingIsLocked, logical(1), env = l)
      all_names[ok_names]
      # nocov end
    }

    for (i in idx) {
      e <- l[[i]]
      if (all(inherits(e, c(class, "char_map"), which = TRUE))) {
        l[[i]] <- map2char(e)
      } else if (is.list(e) || is.environment(e)) {
        l[[i]] <- uncompress_chars_core(e)
      }
    }

    attr(l, "bettermc_recurse_protect") <- NULL
    class(l) <- cls
    l
  }

  if ((!is.list(l) && !is.environment(l)) ||
      all(inherits(l, c(class, "char_map"), which = TRUE))) {
    uncompress_chars_core(list(l))[[1L]]
  } else {
    uncompress_chars_core(l)
  }
}

#' Split a Character Vector into its Unique Elements and a Mapping on These
#'
#' This is implemented using a radix sort on the CHARSXPs directly, i.e. on the
#' addresses of the strings in the global string cache. Hence, in contrast to
#' \code{\link[base]{unique}}, this function does not consider two strings equal
#' which differ only in their encoding. Also, the order of the unique elements
#' is undefined.
#'
#' @param x a character vector. Long vectors are supported.
#'
#' @return \code{char_map} returns an S3 object of class "char_map", which is a
#'   list with the following elements: (chars) the unique set of strings in
#'   \code{x} in undefined order, (idx) an integer (or - for long vectors -
#'   double) vector such that \code{map$chars[map$idx]} is identical to \code{x}
#'   (except maybe for attributes), (attributes) the attributes of x as a
#'   shallow copy of the corresponding pairlist.
#'
#' @examples
#' x <- sample(letters, 100, replace = TRUE)
#' map <- char_map(x)
#' stopifnot(identical(x, map$chars[map$idx]))
#'
#' names(x) <- 1:100
#' stopifnot(identical(x, map2char(char_map(x))))
#'
#' @section Lifecycle:
#'   \ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options:
#'   alt='[Stable]'}}}{\strong{[Stable]}}
#'
#' @export
char_map <- function(x) {
  checkmate::assert_character(x)
  if (isS4(x)) stop("'x' must not be an S4 object")
  if (is.integer(length(x))) {
    structure(.Call(C_char_map, x), names = c("chars", "idx", "attributes"),
              class = "char_map")
  } else {
    structure(.Call(C_char_map_long, x), names = c("chars", "idx", "attributes"),
              class = "char_map")
  }

}

#' @rdname char_map
#'
#' @param map an object as returned by \code{char_map}.
#'
#' @return \code{map2char} returns a character vector identical to \code{x},
#'   including attributes.
#'
#' @export
map2char <- function(map) {
  checkmate::assert_class(map, "char_map")
  res <- map$chars[map$idx]
  .Call(C_set_attr, res, map$attributes)
}
