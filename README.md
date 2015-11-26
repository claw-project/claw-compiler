# omni-cx2x

Tool to perform XcodeML to XcodeML transformation.

The directives that control the transformation flow are defined in the
CLAW language definition.

[CLAW language definition](https://github.com/C2SM-RCM/claw-language-definition)

We are currently evaluating the potential of such a translator. The current
development status is the following. Only limited cases have been tested.
- [x] loop-fusion
- [x] loop-interchange
- [ ] loop-extract
- [ ] loop-vector
- [ ] scalar-replace
- [ ] data


Transformation flow is defined as follows:

*Fortran Code* -> **Preprocessor(1)** -> *Fortran Code* -> **F_Front (2)** ->
*XcodeML Code* -> **ClawX2X(3)** -> *XcodeML Code* -> **omx2f(4)** -> *Fortran Code*

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
* **ClawX2X**: Dedicated claw directives translator. It translates XcodeML with
directives into a transformed XcodeML intermediate representation.
* **omx2f**: OMNI Compiler back-end. It converts XcodeML intermediate
representation into Fortran code.
