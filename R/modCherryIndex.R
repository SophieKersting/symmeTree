#' Modified cherry index of a rooted binary tree
#'
#' \code{modCherryIndex} - This function calculates the modified cherry index
#' value, i.e. the number of leaves that are not in a cherry, for rooted
#' binary trees.
#'
#' @author Sophie Kersting
#' @param tree Rooted binary tree of class "phylo"
#'                  (no specific node enumeration order required)
#'
#' @return the number of leaves not in a cherry (numeric)
#' @export
#' @examples
#' mat <- cbind(c(7,7,6,5,5,6),c(1,2,3,4,6,7))
#' tree <- list(edge=mat, tip.label=c("","","",""), Nnode=3)
#' attr(tree, "class") <- "phylo"
#' modCherryIndex(tree)
modCherryIndex <- function(tree){
    if (!inherits(tree,"phylo")) stop("The input tree must be in phylo-format.")
    n <- length(tree$tip.label)
    if (n == 1) return(1)
    n_interior <- tree$Nnode
    if (n_interior != n - 1) stop('The tree is not a rooted binary tree.')
    #--------------------------
    # get descendants matrix (leaves have (NA,NA), inner nodes (desc1, desc2))
    Descs <- getDescMatrix(tree)
    numb_cherries <- 0
    for(row in 1:nrow(Descs)){
        if(!is.na(Descs[row,1])){ #if not leaf, check if descendants are leaves
            if(is.na(Descs[Descs[row,1],1]) && is.na(Descs[Descs[row,2],1])){
                numb_cherries <- numb_cherries + 1 #found a cherry
            }
        }
    }
    return(n - 2*numb_cherries) # return number of leaves not in a cherry
}
