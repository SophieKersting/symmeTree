#' Rogers J Index of a rooted binary tree
#'
#' \code{rogersJ} - This function calculates the Rogers J index value, the
#' number of interior nodes that are not balanced (i.e. their two pending
#' subtrees differ in size), for rooted binary trees.
#'
#' @author Sophie Kersting
#' @param tree Rooted binary tree of class "phylo"
#'                  (no specific node enumeration order required)
#' @return the number of unbalanced nodes (numeric)
#' @export
#' @examples
#' mat <- cbind(c(7,7,6,5,5,6),c(1,2,3,4,6,7))
#' tree <- list(edge=mat, tip.label=c("","","",""), Nnode=3)
#' attr(tree, "class") <- "phylo"
#' rogersJ(tree)
rogersJ <- function(tree){
    if (!inherits(tree,"phylo")) stop("The input tree must be in phylo-format.")
    n <- length(tree$tip.label)
    if (n == 1) return(0)
    n_interior <- tree$Nnode
    if (n_interior != n - 1) stop('The tree is not a rooted binary tree.')
    #--------------------------
    Descs <- getDescMatrix(tree)
    Ancs <- getAncVec(tree)
    depthResults <- getNodesOfDepth(mat=Descs,root=which(is.na(Ancs)),
                                    n=tree$Nnode+1)
    #--------------------------
    nv <- rep(NA,length(n+n_interior)) # at position i: n_v of node with number i
    unbalanced_count <- 0 # number of unbalanced nodes
    for(d in depthResults$maxdepth:0){
        current_Nodes <- na.omit(as.vector(depthResults$nodesOfDepth[d+1,]))
        for(v in current_Nodes){
            if(is.na(Descs[v,1])){
                nv[v] <- 1 #if leaf, then nv=1
            }else{
                desc_nvs <- nv[Descs[v,]]
                nv[v] <- sum(desc_nvs)
                if(desc_nvs[1]!=desc_nvs[2]){
                    unbalanced_count <- unbalanced_count+1
                }
            }
        }
    }
    return(unbalanced_count)
}
