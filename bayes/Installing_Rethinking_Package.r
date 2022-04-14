# Install the Statistical Rethinking library
# Following the instructions from https://github.com/rmcelreath/rethinking

# Install RStan
# Via https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started

install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

# Configuring C Toolchain for Windows
# Via https://github.com/stan-dev/rstan/wiki/Configuring-C---Toolchain-for-Windows
install.packages("pkgbuild")

rt_path = gsub("\\","/",pkgbuild::rtools_path(),fixed=T)
rt_bin = paste0(substr(rt_path,1,nchar(rt_path)-4),"/mingw_$(WIN)/bin/")
writeLines(paste0('PATH="',rt_path,';${PATH}"'), con = "~/.Renviron")
writeLines(paste0('Sys.setenv(BINPREF = "',rt_bin,'")'), con = "~/.Rprofile")


install.packages("jsonlite",type="source")

# Install CmdStanR
# Via https://mc-stan.org/cmdstanr/

install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

# Install Rethinking package
# Via https://github.com/rmcelreath/rethinking

install.packages(c("coda","mvtnorm","devtools","loo","dagitty","shape"))
devtools::install_github("rmcelreath/rethinking")