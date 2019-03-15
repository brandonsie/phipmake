# phipdrake

R package of [Drake](https://github.com/ropensci/drake)-based tools for the Larman Lab's [PhIP-seq](https://www.nature.com/articles/s41596-018-0025-6) data analysis pipeline.

``` r  
if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("ropensci/drake") # drake is on CRAN, but we use the more recent Github version
devtools::install_github("brandonsie/phipmake")
```  

``` r
plan <- phipdrake::define_plan() # initialize pipeline. looks for mpath.txt and ppath.txt in working directory
drake::make(plan) # execute pipeline
v <- drake::vis_drake_graph(drake::drake_config(plan)) # visualize pipeline dependencies
v
visNetwork::visSave(v, "dependency_graph.html") # save dependency graph
```
![dependency graph](https://raw.githubusercontent.com/brandonsie/brandonsie.github.io/master/docs/phipdrake_dependency_2.PNG)
