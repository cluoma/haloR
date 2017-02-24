HALO 5 API R Package
====================

Provides easy access to the Halo 5 and Halo Wars 2 web APIs provided by 343i.

A developer API is required to use this package. You can get one from [here](https://developer.haloapi.com/)

Use Microsoft's official [API documentation](https://developer.haloapi.com/docs/services/) as many of variables will not make sense without their explanations.

Installation:
<pre>
install.packages(c("devtools","httr","jsonlite"))
library(devtools)
install_github("cluoma/haloR")
</pre>

Example:
<pre>
library(dplyr)
library(haloR)

key <- "XXXXXXXXXXXXXXXXXXXXX"
</pre>
