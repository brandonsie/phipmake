# phipmake

R package of [Drake](https://github.com/ropensci/drake)-based tools for the Larman Lab's [PhIP-Seq](https://www.nature.com/articles/s41596-018-0025-6) data analysis pipeline.

``` r  
if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("ropensci/drake") # drake is on CRAN, but we use the more recently updated Github version.
devtools::install_github("brandonsie/phipmake")
```  

``` r
plan <- phipdrake::define_plan() # initialize pipeline. looks for mpath.txt and ppath.txt in working directory
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
* Daniel Monaco wrote the network filter used for polyclonal scoring.  
