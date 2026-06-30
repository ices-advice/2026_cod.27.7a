# 2026_cod.27.7a_assessment
2026 - Cod (Gadus morhua) in Division 7.a (Irish Sea)

The assessment uses Stock Synthesis in the latest version.

The stockobject cod7a_sOSS3.Rdata created directly from the stock is in the folders "model" and "output". Forecast stock object cod7a_forecast.Rdata is stored in the "Report" folder.

To run in R install the following packages necessary:

## R packages

``` r
icesTAF
data.table
FLCore
FLAssess
FLasher
r4ss
ss3diags
ss3om
ss3sim
```
They can be installed (along with thier dependencies) with:

``` r
install.packages("icesTAF")

### list with required packages
library(icesTAF)
install.deps(repos = c('https://flr.r-universe.dev', 'https://ices-tools-prod.r-universe.dev','https://cloud.r-project.org'))
```

## Running the assessment

The easiest way to run the assessment is to clone or download this
repository and run:

``` r
### load the icesTAF package
library(icesTAF)
### load data and install R packages
taf.bootstrap()
### run all scripts
sourceAll()
```

This code snippet runs the entire data compilation and assessment and
creates the tables and figures presented in the WG report.
