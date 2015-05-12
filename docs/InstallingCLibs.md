# Installing C Libraries with MinGHC

Some cabal packages, including some in the Haskell Platform, need C librarires to build and install.  Currently there is no automatic tool for this, so you will have to do it manually.

## MinGHC Default Directories
Note: %LOCALAPPDATA% is `C:\Users\<user name>\AppData\Local\` in Windows 7 and above
- 32bit Install
  - `%LOCALAPPDATA%\Programs\minghc-x.y.z-i386\ghc.x.y.z\mingw\include\`
  - `%LOCALAPPDATA%\Programs\minghc-x.y.z-i386\ghc.x.y.z\mingw\lib\`
  - `C:\Windows\System32\` (DLLs, if not in program directory)
- 64bit Install
  - `%LOCALAPPDATA%\Programs\minghc-x.y.z-x86_64\ghc.x.y.z\mingw\x86_64-w64-mingw32\include\`
  - `%LOCALAPPDATA%\Programs\minghc-x.y.z-x86_64\ghc.x.y.z\mingw\x86_64-w64-mingw32\lib\`
  - `C:\Windows\SysWOW64\` (DLLs, if not in program directory)


## Installing GLUT

You need to get a [compiled freeglut](http://www.transmissionzero.co.uk/software/freeglut-devel/). Be sure to grab the MinGW version.  The zip should contain both 32bit and 64bit libraries and DLLs, be sure to copy the correct ones.

Copy the contents of include directory in the zip file into your MinGHC install's include dir.  Copy the correct 32/64bit freeglut.a file into the lib dir **and rename to libglut32.a**.  Finally copy the correct 32/64bit DLL into your program dir, or system DLL directory **and rename to glut32.dll**.



