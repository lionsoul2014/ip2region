::ip2region golang maker makefile in windows
@echo off

if [%1] == [] goto:build

if %1==clean (
	call:clean
) else if %1==build (
	call:build
)
exit /b 0

:build
go build -o xdb_maker.exe
exit /b 0

:clean
del/f/s/q xdb_maker.exe
