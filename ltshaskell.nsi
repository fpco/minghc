!Include MUI2.nsh
!Include EnvVarUpdate.nsh
Name "LTSHaskell"
OutFile "LTSHaskell.exe"
InstallDir "c:\Program Files\LTSHaskell1"
RequestExecutionLevel User
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_LANGUAGE "English"
Section "" _sec1
  SetOutPath "$INSTDIR"
  WriteUninstaller "uninstall.exe"

  File /r "ghc-7.8.3"
  File /r "cabal-1.20"
  File /r "msys-1.0"


  ${EnvVarUpdate} $0 "PATH" "A" "HKLM" "%APPDATA%\cabal\bin"
  ${EnvVarUpdate} $0 "PATH" "A" "HKLM" "$INSTDIR\ghc-7.8.3\bin"
  ${EnvVarUpdate} $0 "PATH" "A" "HKLM" "$INSTDIR\cabal-1.20\bin"
  ${EnvVarUpdate} $0 "PATH" "A" "HKLM" "$INSTDIR\msys-1.0\bin"
SectionEnd
Section "Uninstall" _sec2
  RMDir /r "$INSTDIR"
  ${un.EnvVarUpdate} $0 "PATH" "R" "HKLM" "$INSTDIR\msys-1.0\bin"
  ${un.EnvVarUpdate} $0 "PATH" "R" "HKLM" "$INSTDIR\cabal-1.20\bin"
  ${un.EnvVarUpdate} $0 "PATH" "R" "HKLM" "$INSTDIR\ghc-7.8.3\bin"
  ${un.EnvVarUpdate} $0 "PATH" "R" "HKLM" "%APPDATA%\cabal\bin"
SectionEnd
