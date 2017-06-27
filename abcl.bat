@echo off
set HOME=%~dp0
%~d0
cd %HOME%
set PATH=%PATH%;%~dp0lib
set CL_TYPE=ABCL
emacs\bin\runemacs.exe
