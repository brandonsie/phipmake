# phipdrake

R package of Drake-based tools for the Larman Lab's PhIP-seq data analysis pipeline.

``` r  
if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("PhIPdbDrake")
```  

``` r
plan <- phipdrake::define_plan() # initialize pipeline. looks for mpath.txt and ppath.txt in working directory
drake::make(plan) # execute pipeline
drake::vis_drake_graph(drake::drake_config(plan)) # visualize pipeline
```
