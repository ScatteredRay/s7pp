@echo off
IF "%1"=="" GOTO Usage
mkdir %1

cp *.scm %1
cp s7.exe %1
cp s7pp.exe %1

echo Installiation complete

GOTO End

:Usage
echo "install <destination path>"

:End