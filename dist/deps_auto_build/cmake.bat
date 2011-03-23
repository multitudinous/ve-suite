
@echo off

REM set Windows CMake path first
set Path=%3\bin;%Path%

%comspec% /c "%1 %2 && cmake %4 %5 %6 %7"
