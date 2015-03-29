# Installing C Libraries with MinGHC

Some cabal packages, including some in the Haskell Platform, need C librarires to build and install.  Currently there is no automatic tool for this, so you will have to do it manually.

## MinGHC Default Directories

- 32bit Install (on 64bit system)
  - C:\Program Files (x86)\MinGHC-x.y.z\ghc.x.y.z\mingw\include\
  - C:\Program Files (x86)\MinGHC-x.y.z\ghc.x.y.z\mingw\lib\
  - C:\Windows\System32\ (DLLs, if not in program directory)
- 64bit Install
  - C:\Program Files (x86)\MinGHC-x.y.z\ghc.x.y.z\mingw\x86_64-w64-mingw32\include\
  - C:\Program Files (x86)\MinGHC-x.y.z\ghc.x.y.z\mingw\x86_64-w64-mingw32\lib\
  - C:\Windows\SysWOW64\ (DLLs, if not in program directory)

## Installing GLUT

You need to get a [compiled freeglut](http://www.transmissionzero.co.uk/software/freeglut-devel/). Be sure to grab the MinGW version.  The zip should contain both 32bit and 64bit libraries and DLLs, be sure to copy the correct ones.

Copy the contents of include directory in the zip file into your MinGHC install's include dir.  Copy the correct 32/64bit freeglut.a file into the lib dir **and rename to libglut32.a**.  Finally copy the correct 32/64bit DLL into your program dir, or system DLL directory **and rename to glut32.dll**.



