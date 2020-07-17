

makeCacheMatrix <- function(my_matrix = matrix()) {
   
   # Is it a matrix?
   if (!is.matrix(my_matrix)) {
      stop("This is not a matrix")
   }
   
   matrix_inversa <- NULL
   
   set <- function(y) {
      
      
      my_matrix <<- y
      
      matrix_inversa <<- NULL
      
      
   }
   
   # Ajustar Caché de la MAtrix
   get <- function() my_matrix
   # Inversing the matrix using build in solve() function in R
   set_inversa <- function(solve) matrix_inversa <<- solve
   get.inversa <- function() matrix_inversa
   
   list(
      set = set, 
      get = get,
      set_inversa = set_inversa,
      get.inversa = get.inversa)
   
}


## Cache Solve function. It calculates the inverse of the matrix
## If the matrix has already been created, and has no changes, then the cached info is pasted
## If data has changed, then the new value is calculated

cacheSolve <- function(cache_matrix, ...) {
   matrix_inversa <- cache_matrix$get.inversa()
   
   if(!is.null(matrix_inversa)) {
      message("Getting cache of inversed matrix")
      return(matrix_inversa)
   }
   # Let's create inverted matrix in case
   # there's no cached matrix available.
   matrix.to.inverse <- cache_matrix$get()
   matrix_inversa <- solve(matrix.to.inverse)
   cache_matrix$set_inversa(matrix_inversa)
   matrix_inversa
   
}
