@echo off
FOR %%X IN (C.12580,
            C.12060,
            C.35620,
            C.33100,
            C.31080,
            C.26420,
            C.38060,
            C.16980) DO (
    start "Location %%X" cmd /k ""C:\Program Files\R\R-4.5.2\bin\Rscript.exe"" shield_calib_setup_and_run_multiprocess.R %%X
)