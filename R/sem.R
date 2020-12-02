sem_open <- function(name, create = FALSE, overwrite = FALSE, value = 0) {
  .Call(C_semaphore_open, name, create, overwrite, value)
}

sem_post <- function(sem) {
  .Call(C_semaphore_post, sem)
}

sem_wait <- function(sem) {
  .Call(C_semaphore_wait, sem)
}

sem_close <- function(sem) {
  .Call(C_semaphore_close, sem)
}

sem_unlink <- function(name) {
  .Call(C_semaphore_unlink, name)
}
