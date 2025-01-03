# test script to see if we can install using "Rscript <this>"

install.packages("RadData", lib=Sys.getenv("R_LIBS_USER"))
print("Successfully installed")
remove.packages("RadData", lib = Sys.getenv("R_LIBS_USER"))