
[![Travis-CI Build Status](https://travis-ci.org/reconhub/branchr.svg?branch=master)](https://travis-ci.org/reconhub/branchr)
[![Coverage Status](https://codecov.io/github/reconhub/branchr/coverage.svg?branch=master)](https://codecov.io/github/reconhub/branchr?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/branchr)](https://cran.r-project.org/package=branchr)



*branchr*: R Estimation from Cluster Sizes
---------------------------------------------------------------

*branchr* implements estimations of reproduction numbers from cluster size distribution.

<br>

This project is currently in testing stage. Please contact [the
authors](mailto:p.nouvellet@imperial.ac.uk) if you plan on using it.




<br>

Installation
-------------

To install the development version from github (requires Rtools on windows and
GSL headers on all platforms):


```r
devtools::install_github("reconhub/branchr")
```

To add local copies of the vignettes, you will need to specify:

```r
devtools::install_github("reconhub/branchr", build_vignettes = TRUE)
```

Then, to load the package, use:


```r
library("branchr")
```



<br>

Documentation
-------------

*branchr* is fully documented on a [dedicated
 website](http://www.repidemicsconsortium.org/branchr/). 

It also comes with the following vignettes:

* vignette 1: ...
* vignette 2: ...


<br>

Contributors
------------

- [Pierre Nouvellet](https://github.com/pnouvellet)
- [Thibaut Jombart](https://github.com/thibautjombart)


See details of contributions on: <br>
https://github.com/reconhub/branchr/graphs/contributors



Contributions are welcome via **pull requests**.

Please note that this project is released with a [Contributor Code of
Conduct](CONDUCT.md). By participating in this project you agree to abide by its
terms.

**Maintainer:** Pierre Nouvellet (p.nouvellet@imperial.ac.uk)
