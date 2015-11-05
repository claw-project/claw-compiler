# omni-cx2x

Tool to perform XcodeML to XcodeML transformation.

Transformation flow is defined as follows:

*Fortran Code* -> **F_Front (1)** -> *XcodeML Code* -> **x2x** -> *XcodeML Code* ->
**omx2f** -> *Fortran Code*

###### Executables involved in the transformation process
* **F_Front**: OMNI Compiler front-end. It converts Fortran code in XcodeML
intermediate representation.
* **cx2x**: Dedicated claw directives translator. It translates XcodeML with
directives into a transformed XcodeML intermediate representation.
* **omx2f**: OMNI Compiler back-end. It converts XcodeML intermediate
representation into Fortran code.
