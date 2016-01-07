# CLAW Fortran Compiler development

Useful information about the development of the CLAW Fortran Compiler are given
on this page.


### Testing

#### Unit test with JUnit
Unit tests are set up using JUnit. Unit tests are built together with the
compiler if the corresponding JARs Dependencies are available.

JARs dependencies:

*  `junit-4.2.jar`
*  `hamcrest-core-1.3.jar`

#### Transformation tests
Another category of test is focusing on the the correct application of each
transformation by the translator.

Those tests can be found under `/test/`

To build all the tests, use the following command. All the tests are transformed
with the CLAW Fortran Compiler and then the original code as well as the
transformed are compiled with a standard Fortran compiler.

```bash
make transformation
```

To verify the applied transformation, use the following command. The output is
the one from `CTest`
```bash
make test
```
