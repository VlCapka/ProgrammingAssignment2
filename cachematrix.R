# makeCacheMatrix is a function that creates a list of 4 objects that are also functions. It also stores X , the matrix we want to invert, and 
# inv, the inverted matrix result.
# Objects in the list are given names in a list, that are the same as functions so we can later use the the $ subsetting argument to call upon those functions
# No matrix inversion calculation takes place in makeCacherMatrix. That is done in cacheSolve function later, but only if not already cached.
# Functions created in makeCacheMatrix are created but not used to do anything here. They are just defined and stored for future use by cacheSolve or 
# directly in console.

makeCacheMatrix<-function(X = matrix()) {  # this initializes X as a matrix and sets inv value to NULL. inv value will be the inverted matrix later
        inv=NULL
        
        set<-function(Y) {  # when set(Y) is used, it takes function argument Y (also a matrix) and assigns it to X in parent environment and 
                # replaces the previous X value. Also it resets the inv value to NULL in parent environment, erasing the previous inv value
                X<<-Y
                inv<<-NULL
        }
       
        get<-function() X  # get() function retrieves the X matrix from the parent environment
        
        setSolve<-function(i) inv<<-i  # setSolve(i) takes the argument i and assigns it to inv value in parent environment, replacing the 
        # NULL or the previously stored inv value
        
        getSolve<-function() inv  # getSolve() function retrieves the inv value from the parent environment
        
        list(set=set, get=get,setSolve=setSolve, getSolve=getSolve)
}


# In cacheSolve I renamed the function argument to z as to not confuse it with X matrix in makeCacheMatrix as it is not the same thing.
# The z argument is a makeCacheMatrix-tupe of argument local to this function, not a matrix specified in makeCacheMatrx.
# cacheSolve function first retrieves the inv value from parent environment and checks if it is NULL. If it is not NULL, it means the value contains a 
# cached inverted matrix from previous calculations so the function displays the message and shows the cached value.
# If it is NULL, then the function proceeds to retrieve the X matrix from makeCacheMatrix parent environment and calculate the inverted value.
# The inverted value is then displayed and also returned to makeCacheMatrix parent environment, overwriting the previous value.

cacheSolve<-function(z,...) {  
        inv.local<-z$getSolve()  # retrieves inv value from makeCacheMatrix parent environment and assigns to inv.local in local environment
        if(!is.null(inv.local)) {  # if inv.local is NOT NULL, display message and print value
                message("getting chached inverted matrix")
                return(inv.local) }
        data<-z$get()  # retrieves matrix X from makeCacheMatrix parent environment and assigns to data
        inv.local<-solve(data,...)   # calculates inverted matrix of "data" an stores in inv.local
        z$setSolve(inv.local)  # sets inv.local as a new inv value in the makeCacheMatrix parent environment
        inv.local  # displays inv.local value, which now contains the inverse of the matrix, it is also in inv value in parent environment ready for retrieval
}

