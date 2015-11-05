# omni-cx2x

Tool to perform XcodeML to XcodeML transformation.

Transformation flow is defined as follows:

*Fortran Code* -> **Preprocessor(1)** -> *Fortran Code* -> **F_Front (2)** -> *XcodeML Code* -> **x2x(3)** -> *XcodeML Code* ->
**omx2f(4)** -> *Fortran Code*

###### Transformation process
1. The Fortran code is passed into the preprocessor with the corresponding
flags.
2. The fortran without preprocessing macros is passed into the OMNI Compiler
front-end and produce an intermediate file containing the XcodeML intermediate
representation of the Fortran code.
3. The XcodeML intermediate representation containing the dedicated language
directive is translated. The output is a translated XcodeML intermediate
representation.
4. The XcodeML intermediate representation is passed through the OMNI compiler
back-end to produce standard Fortran code.  

###### Executables involved in the transformation process
* **Preprocessor**: preprocessor from the standard compiler available.
* **F_Front**: OMNI Compiler front-end. It converts Fortran code in XcodeML
intermediate representation.
* **cx2x**: Dedicated claw directives translator. It translates XcodeML with
directives into a transformed XcodeML intermediate representation.
* **omx2f**: OMNI Compiler back-end. It converts XcodeML intermediate
representation into Fortran code.
