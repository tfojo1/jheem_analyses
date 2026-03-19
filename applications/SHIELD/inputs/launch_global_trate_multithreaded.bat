@echo off
REM set "groupings=1-3 4-6 7-9 10-12 13-15 16-18 19-21 22-24 25-27 28-30 31-33"
set "groupings=7-9 13-15 16-18 19-21"
FOR %%X in (%groupings%) DO (
    start "Running global transmission rate sim for group %%X" cmd /k ""C:\Program Files\R\R-4.5.2\bin\Rscript.exe"" find_global_trate_sims_thread.R %%X
)