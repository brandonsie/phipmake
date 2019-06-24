# phipmake <img src="https://brandonsie.github.io/docs/phipmake.png" align="right" width="140">

![License](https://img.shields.io/github/license/brandonsie/phipmake.svg) 
[![Lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/) 
![Code Size](https://img.shields.io/github/languages/code-size/brandonsie/phipmake.svg) 
![Last Commit](https://img.shields.io/github/last-commit/brandonsie/epitopefinder.svg)

R package of [Drake](https://github.com/ropensci/drake)-based tools for the Larman Lab's [PhIP-Seq](https://www.nature.com/articles/s41596-018-0025-6) data analysis pipeline.

``` r  
if(!requireNamespace("remotes")) install.packages("remotes")
remotes::install_github("ropensci/drake") # drake is on CRAN, but phipmake depends on the more recently updated Github version.
remotes::install_github("brandonsie/phipmake")
```  

``` r
drake::expose_imports(phipmake) # allow drake to keep track of nested function dependencies.
plan <- phipmake::define_plan() # initialize pipeline. looks for mpath.txt and ppath.txt in working directory
drake::make(plan) # execute pipeline
v <- drake::vis_drake_graph(drake::drake_config(plan)) # visualize pipeline dependencies
v
visNetwork::visSave(v, "dependency_graph.html") # save dependency graph
```


### Dependency Graph
A simplified version of the dependency graph is below. For a more complete & interactive representation, please click [here](https://brandonsie.github.io/phipmake/dependency_graph.html).

![simple dependency](https://raw.githubusercontent.com/brandonsie/phipmake/master/docs/simple_dependency.png)

More documentation will come soon to [phipmake's website](https://brandonsie.github.io/phipmake/).

### Acknowledgements
* Daniel Monaco wrote the network filter used for polyclonal scoring, and AVARDA, used for deconvoluting viral proteome antibody specificitity data.  
