set R_Script="C:\Program Files\R\R-4.2.2\bin\RScript.exe"

%R_Script% exmpl.R 5 6 -3 > exmpl-1.batch 2>&1
%R_Script% exmpl.R h 2 100 > exmpl-2.batch 2>&1
%R_Script% exmpl.R 3 2 100 > exmpl-3.batch 2>&1
%R_Script% exmpl.R -34 21 112 > exmpl-4.batch 2>&1








