% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sem.R
\name{sem}
\alias{sem}
\alias{sem_open}
\alias{sem_post}
\alias{sem_wait}
\alias{sem_close}
\alias{sem_unlink}
\title{Named POSIX Semaphores}
\usage{
sem_open(name, create = FALSE, overwrite = FALSE, value = 0)

sem_post(sem)

sem_wait(sem)

sem_close(sem)

sem_unlink(name)
}
\arguments{
\item{name}{the name of the semaphore. Consult \code{man sem_overview} for
what makes a valid name.}

\item{create}{should the semaphore be created if it currently does not exist?}

\item{overwrite}{if \code{create == TRUE}, should we overwrite an already
existing semaphore with the name (\code{TRUE}) or rather fail
(\code{FALSE}).}

\item{value}{the initial value of the semaphore (\code{>= 0}).}

\item{sem}{an object as returned by \code{sem_open}.}
}
\value{
For \code{sem_open}, an object of class "sem", which is an external
  pointer to the POSIX semaphore. All other functions return \code{NULL}
  invisibly and are called for their side effects.
}
\description{
Named POSIX Semaphores
}
\section{Windows Support}{
 Not supported on Windows.
}

\section{Lifecycle}{

  \ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options:
  alt='[Experimental]'}}}{\strong{[Experimental]}}
}

