# This function essentially takes in a matrix, gets the value and the inverse
# if it exists and then lists all the outputs

makeCacheMatrix <- function(mat=matrix()) {
    
    inverse <- NULL
    set_value <- function(mat2) {
        
        mat <<- mat2
        inverse <<- NULL
    }
    
    get_val <- function() {mat}
    set_inv <- function(inv) {inverse <<- inv} 
    get_inv <- function() {inverse}
    list(set_value=set_value, get_val=get_val,
         set_inv=set_inv, get_inv=get_inv)
    
}

# This function will compute the inverse if not computed already and then 
# cache it and then once called will return the stored value

cacheSolve <- function(mat, ...) {
    
    inverse <- mat$get_inv() #check to see if the inv has already been calculated
    if (!is.null(inverse)) {
        
        message("Getting cached data")
        return (inverse)
        
    }
    
    data <- mat$get_val()
    inverse <- solve(data, ...) # solve function computes the inverse
    mat$set_inv(inverse)
    inverse
}
