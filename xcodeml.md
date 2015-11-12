# XcodeML/F

This file contains some observation about XcodeML Fortran

##### Structure of a program (Xprogram)
```
Xprogram
|
|- typeTable
|- globalSymbols
|- globalDeclarations
   |- FfunctionDefinition
      |- name
      |- symbols
      |- declarations
      |- body
```

##### Structure of a loop (FdoStatement)
```
FdoStatement
  |
  |- Var
  |- indexRange
     |- lowerBound
     |- upperBound
     |- step
  |- body
```
