@echo off
FOR /L %%A IN (1,1,25) DO (
    start "Thread%%A" cmd /k C:\Users\azalesa1\AppData\Local\Programs\R\R-4.4.2\bin\Rscript.exe generate_sixty_five_plus_estimates_multithreaded.R %%A
)