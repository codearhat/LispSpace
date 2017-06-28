@echo off
set HOME=%~dp0
%~d0
cd %HOME%
set PATH=%PATH%;%~dp0lib;%~dp0gnuplot\bin
set CL_TYPE=ABCL
emacs\bin\runemacs.exe
