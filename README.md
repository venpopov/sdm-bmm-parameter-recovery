# sdm-bmm-parameter-recovery

Contains parameter recovery simulations for the Signal Discrimination Model as implemented in the bmm package (https://venpopov.github.io/bmm/articles/bmm_sdm_simple.html)


To use, first clone the repository:

```bash
git clone https://github.com/venpopov/sdm-bmm-parameter-recovery.git
```

Before running the code, you might need to install some packages. I use `renv` for package management. `renv` should install itself due to code in the .Rprofile file when you open the project in Rstudio or open an R console in the main directory. Afterwards run this line in the R console to install any missing dependencies:

```R
renv::restore()
```

The models fits are saved in `output/`. The generating scripts are in `R`, but since the outputs are available, only rerun them if necessary.

The results are extracted and analyzed in the corresponding `R/analyze_*.R` files. 
