vectors2shm <- function(l, limit = 2L,
                        share_altreps = c("no", "yes", "if_allocated"),
                        copy = TRUE,
                        name_prefix,
                        class = character()) {
  share_altreps <- match.arg(share_altreps)

  cntr <- 1L
  OK <- TRUE

  on.exit(unlink_all_shm(name_prefix, 1L))

  vectors2shm_core <- function(l) {
    if (is.environment(l)) return(l)
    if (inherits(l, "shm_obj")) return(l)
    if (inherits(l, "char_map")) is_char_map <- TRUE else is_char_map <- FALSE
    if (isS4(l)) return(l)
    if (!OK) return(l)
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
        l[[i]] <- vectors2shm_core(e)
      } else if ((is.logical(e) || is.numeric(e) || is.complex(e) || is.raw(e)) &&
                 !isS4(e) && length(e) >= limit &&
                 (
                   !is_altrep(e) ||
                   share_altreps == "yes" ||
                   (share_altreps == "if_allocated" && is_allocated(e))
                 )) {
        name <- paste0(name_prefix, cntr)
        shm_obj <- copy2shm(e, name, copy = ifelse(is_char_map, FALSE, copy))
        if (inherits(shm_obj, "shm_obj")) {
          class(shm_obj) <- c(class, class(shm_obj))
          cntr <<- cntr + 1L
          l[[i]] <- shm_obj
        } else {
          OK <<- FALSE
          message(shm_obj)
          break
        }
      }
    }

    attr(l, "bettermc_recurse_protect") <- NULL
    class(l) <- cls
    l
  }

  if (!is.list(l) && !is.environment(l)) {
    res <- vectors2shm_core(list(l))[[1L]]
  } else {
    res <- vectors2shm_core(l)
  }

  on.exit(NULL)
  res
}


shm2vectors <- function(l, class = character()) {
  shm2vectors_core <- function(l) {
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

    # we loop over idx in reverse order and hence also allocate from the shared
    # memory objects in reverse order; this is because the cleanup function
    # unlink_all_shm() in mclapply() iterates through them in regular order and
    # stops on the first missing object (an object is deleted on successful
    # allocation); example:
    # - assume there are five shm objects: 1, 2, 3, 4, 5
    # - we successfully allocated from objects 5 and 4
    # - we are interrupted before allocation from object 3
    # - cleanup will unlink objects 1, 2 and 3
    # - cleanup will no longer find object 4 and hence stop
    for (i in rev(idx)) {
      e <- l[[i]]
      if (all(inherits(e, c(class, "shm_obj"), which = TRUE))) {
        l[[i]] <- allocate_from_shm(e)
      } else if (is.list(e) || is.environment(e)) {
        l[[i]] <- shm2vectors_core(e)
      }
    }

    attr(l, "bettermc_recurse_protect") <- NULL
    class(l) <- cls
    l
  }

  if ((!is.list(l) && !is.environment(l)) ||
      all(inherits(l, c(class, "shm_obj"), which = TRUE))) {
    shm2vectors_core(list(l))[[1L]]
  } else {
    shm2vectors_core(l)
  }
}


#' Copy to and Allocate from POSIX Shared Memory
#'
#' Copy the data of a vector to a POSIX shared memory object and allocate from
#' such.
#'
#' @param x a logical, integer, double, complex or raw vector, an S3 object
#'   based hereon or a factor. Long vectors are supported.
#' @param name the name of the shared memory object to create. A portable name
#'   starts with a "/", followed by one or more (up to 253) characters, none of
#'   which are slashes. \bold{Note:} on macOS the total length of the name must
#'   not exceed 31 characters.
#' @param overwrite should an already existing shared memory object with the
#'   given name be overwritten? If \code{FALSE}, the copy fails if such an
#'   object already exists. \bold{Note:} Due to bugs in the macOS implementation
#'   of POSIX shared memory, (as of now) only \code{FALSE} is supported.
#' @param copy should the vector placed in shared memory be used directly
#'   (\code{FALSE}) by \code{allocate_from_shm} or rather a copy of it
#'   (\code{TRUE})? \code{FALSE} is apparently faster (initially), but might
#'   require more memory in the long run (up to double what is normally required
#'   by such a vector): if we modify elements of such a vector, new memory
#'   (pages) will be allocated to hold these changed values. The original memory
#'   (pages) of the shared memory object will only be freed when the vector is
#'   garbage collected. If we initially copy the whole vector from shared memory
#'   to "regular" one, the former can be freed directly and the latter can be
#'   modified in place. \bold{Note:} The value passed to \code{copy2shm} has no
#'   direct effect. It only sets the default value for \code{allocate_from_shm},
#'   which can safely be changed. \bold{Note 2:} \code{FALSE} is silently
#'   ignored on macOS.
#'
#' @note See also the general notes on POSIX shared memory under
#'   \code{\link{mclapply}}.
#'
#' @return \code{copy2shm} returns an S3 object of class "shm_obj", which is a
#'   list with the following elements: (name) the name of the shared memory
#'   object as given, (type) an integer specifying the type of \code{x},
#'   (length) the number of elements in \code{x} as a double, (size) the size of
#'   the shared memory object in bytes as a double, (attributes) the attributes
#'   of \code{x} as a shallow copy of the corresponding pairlist, (copy) the
#'   default value for the \code{copy} argument passed to
#'   \code{allocate_from_shm}. \bold{Note:} this function will not produce an
#'   error if an operation related directly to the copy to shared memory fails.
#'   In this case a character vector of length 1 containing the error message
#'   will be returned.
#'
#' @examples
#' if (tolower(Sys.info()[["sysname"]]) != "windows") {
#'   x <- runif(100)
#'   obj <- copy2shm(x, "/random")
#'   if (inherits(obj, "shm_obj")) {
#'     # copy2shm succeeded
#'     y <- allocate_from_shm(obj)
#'     stopifnot(identical(x, y))
#'   } else {
#'     # copy2shm failed -> print the error message
#'     print(obj)
#'   }
#' }
#'
#' @section Windows Support: Not supported on Windows.
#'
#' @section Lifecycle:
#'   \ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options:
#'   alt='[Stable]'}}}{\strong{[Stable]}}
#'
#' @useDynLib bettermc, .registration = TRUE, .fixes = "C_"
#' @export
copy2shm <- function(x, name, overwrite = FALSE, copy = TRUE) {
  if (isS4(x)) stop("'x' must not be an S4 object")

  if (!isFALSE(overwrite)) {
    if (OSTYPE == "macos") {
      stop("On macOS, 'overwrite' must be FALSE.")
    } else if (!OSTYPE %in% c("linux", "solaris", "windows")) {
      stop("invalid value for OSTYPE: ", OSTYPE)
    }
  }

  ret <- .Call(C_copy2shm, x, name, overwrite,
               getOption("bettermc.hugepage_limit", 104857600))  # 100 MiB
  if (is.character(ret)) return(ret)

  structure(c(ret, copy), names = c("name", "type", "length", "size",
                                    "attributes", "copy"),
            class = "shm_obj")
}

#' @rdname copy2shm
#' @param obj an object as returned by \code{copy2shm}, which was typically
#'   called in another process.
#'
#' @return \code{allocate_from_shm} returns a vector. \bold{Note:} this function
#'   cannot be called more than once on any \code{obj}, since it unlinks the
#'   shared memory object immediately after \emph{trying} to open it. If
#'   \code{copy = TRUE}, the vector will be allocated using a custom allocator,
#'   but this is not guaranteed. As of now, vectors with less than two elements
#'   are allocated using R's default allocator. This implementational detail
#'   must not be relied on. If \code{copy = FALSE}, the custom allocator
#'   \emph{privately} maps the shared memory object into the address space of
#'   the current process. In particular this means that changes made to this
#'   memory region by subsequently forked child processes are private to them:
#'   neither the parent nor a sibling process will see these changes. This is
#'   most probably what we want and expect.
#'
#' @export
allocate_from_shm <- function(obj, copy = obj$copy) {
  checkmate::assertClass(obj, "shm_obj")

  # macOS does not support privately mapping a POSIX shared memory object;
  # if copy = TRUE we can (and do) create a shared mapping, hence:
  if (OSTYPE == "macos") {
    copy <- TRUE
  } else if (!OSTYPE %in% c("linux", "solaris", "windows")) {
    stop("invalid value for OSTYPE: ", OSTYPE)
  }

  .Call(C_allocate_from_shm, obj$name, obj$type, obj$length, obj$size,
        obj$attributes, copy)
}

unlink_all_shm <- function(prefix, start) {
  invisible(.Call(C_unlink_all_shm, prefix, start))
}

is_altrep <- function(x) {
  .Call(C_is_altrep, x)
}

is_allocated <- function(x) {
  .Call(C_is_allocated, x)
}
