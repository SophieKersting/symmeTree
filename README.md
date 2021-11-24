# symmeTree
Calculate the symmetry nodes, the modified cherry and the Rogers J Index of rooted binary trees.

Short tutorial:

When using the package devtools the symmeTree package can be installed directly from Github using the
following command:

      devtools::install_github("SophieKersting/symmeTree")

As an example run the following commands that calculate the index values for the caterpillar tree with 4 leaves.

      mat <- cbind(c(7,7,6,5,5,6),c(1,2,3,4,6,7))
      tree <- list(edge=mat, tip.label=c("","","",""), Nnode=3)
      attr(tree, "class") <- "phylo"

      symmeTree::symNodesIndex(tree)
      symmeTree::modCherryIndex(tree)
      symmeTree::rogersJ(tree)

To cite the symmeTree package in publications please use (Bibtex formatted citation):

    @misc{
      title={Measuring tree balance using symmetry nodes -- A new balance index and its extremal properties}, 
      author={Sophie J. Kersting and Mareike Fischer},
      journal = {Mathematical Biosciences},
      volume = {341},
      pages = {108690},
      year = {2021},
      issn = {0025-5564},
      doi = {https://doi.org/10.1016/j.mbs.2021.108690},
      url = {https://www.sciencedirect.com/science/article/pii/S0025556421001127},
    }
