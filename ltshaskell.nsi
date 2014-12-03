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

  ; Should use HKLM instead of HKCU for all but APPDATA.
  ; However, we need to ensure that the APPDATA path comes first.
  ; And this is the only way I could make that happen.

  ${EnvVarUpdate} $0 "PATH" "A" "HKCU" "$APPDATA\cabal\bin"
  ${EnvVarUpdate} $0 "PATH" "A" "HKCU" "$INSTDIR\ghc-7.8.3\bin"
  ${EnvVarUpdate} $0 "PATH" "A" "HKCU" "$INSTDIR\ghc-7.8.3\mingw\bin"
  ${EnvVarUpdate} $0 "PATH" "A" "HKCU" "$INSTDIR\cabal-1.20\bin"
  ${EnvVarUpdate} $0 "PATH" "A" "HKCU" "$INSTDIR\msys-1.0\bin"
SectionEnd
Section "Uninstall" _sec2
  RMDir /r "$INSTDIR"
  ${un.EnvVarUpdate} $0 "PATH" "R" "HKCU" "$INSTDIR\msys-1.0\bin"
  ${un.EnvVarUpdate} $0 "PATH" "R" "HKCU" "$INSTDIR\cabal-1.20\bin"
  ${un.EnvVarUpdate} $0 "PATH" "R" "HKCU" "$INSTDIR\ghc-7.8.3\mingw\bin"
  ${un.EnvVarUpdate} $0 "PATH" "R" "HKCU" "$INSTDIR\ghc-7.8.3\bin"
  ${un.EnvVarUpdate} $0 "PATH" "R" "HKCU" "$APPDATA\cabal\bin"
SectionEnd
