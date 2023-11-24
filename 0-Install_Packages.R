# Install Packages -------
# run the code below to install the packages
pack <- c("latex2exp","tidyr","extraDistr","devtools","Formula","dplyr","ggplot2")

package.check <- lapply(
  pack,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

