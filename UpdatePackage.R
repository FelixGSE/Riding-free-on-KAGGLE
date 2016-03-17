# ----------------------------------------------------------------------
#  UPDATE PRAVETZ DOCUMENTAION
# ----------------------------------------------------------------------

if (!require("devtools")) install.packages("devtools"); library(devtools)
if (!require("roxygen2")) install.packages("roxygen2"); library(roxygen2)

setwd('/Users/felix/Documents/GSE/Term 2/15D012 Advanced Computational Methods/Competition/Riding-free-on-KAGGLE/Package/pravetz/')
devtools::document()

# ----------------------------------------------------------------------
#  END END END END END END END END END END END END END END END END END 
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
#  INSTALL PRAVETZ 
# ----------------------------------------------------------------------

setwd('/Users/felix/Documents/GSE/Term 2/15D012 Advanced Computational Methods/Competition/Riding-free-on-KAGGLE/Package/')

devtools::install('pravetz')
library(pravetz)

# ----------------------------------------------------------------------
#  END END END END END END END END END END END END END END END END END 
# ----------------------------------------------------------------------


