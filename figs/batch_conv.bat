ECHO

REM this creates a local variable for the convert function in IM
REM otherwise convert is confused with a window command
REM files in the directory are converted from pdf to tif

SETLOCAL EnableDelayedExpansion
SET IMCONV="C:\Program Files\ImageMagick-7.0.4-Q8\Convert"

FOR %%f IN (Fig1.pdf) do %IMCONV% -units PixelsPerInch -density 300 -compress LZW "%%~f" "%%~dpnf.tif"
