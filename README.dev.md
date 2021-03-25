# CLAW Compiler development

Useful information about the development of the CLAW Compiler are given
on this page.

### Developer's guide
A guide is available under `documentation/developer`.

### Coding Style Guide
Indent with spaces. NO tabs!

#Java Coding style
Code should be compatible with automated formatting performed by Eclipse IDE. The rules are defined in file 
[resources/eclipse/java-coding-style.xml](resources/eclipse/java-coding-style.xml), which can be imported into IDE. Note
that by default Java Formatter will compress multiline comments. To avoid that, prefix them with “/*-”.

Indent with spaces. NO tabs!

#### Bash scripts
In general, all bash scripts written for the CLAW Compiler projects
follow those guidelines: https://google.github.io/styleguide/shell.xml

In addition, all scripts are tested with `shellcheck` and must pass the tested
without errors or warnings. So warnings might be disable where it make sense
for the correctness of the script.

The main word is: `Use common sense and BE CONSISTENT.`

### Git hooks
* Git hooks present in the `./scripts/git_hooks/` directory should be enabled
  in your local checkout. To enable them, you have to symlink them in the
  `./.git/hooks/` directory.

```bash
cd .git/hooks
ln -s ../../scripts/pre-commit pre-commit
```

Make sure the the file is executable.

### Helper scripts
The `scripts` directory contains helper scripts for the development and review
process of the project.

#### Updating the OMNI Compiler submodule
To update the OMNI Compiler submodule to the latest version, the developer can
run `./scripts/update_omni` in its local repository from the master branch.
The script will automatically do the following:
* Create a dedicated branch name `omni/<short_hash>`.
* Update the submodule to the current master.
* Add and commit the new submodule.
* Output the pull request message.

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
with the CLAW Compiler and then the original code as well as the
transformed are compiled with a standard Fortran compiler.

```bash
make test-suite
```

This target is a combination of three independent targets:

```bash
make clean-transformation transformation test
```

#### Developer's tools

##### Translator development/debugging
To help the development and the debugging of the translator, an option is
available in the driver to dump the list of arguments to be used to execute it
in a standalone mode.

The option can be used as follows with an example of outputs:
```bash
clawfc --dump-cx2t-args <other_args>
-- [DEBUG] Arguments for cx2t:
   --config=/claw-compiler/driver/etc/claw-default.xml --schema=/claw-compiler/driver/etc/claw_config.xsd -w 80 -l  -M/claw-compiler/test/loops/fusion1  -o /tmp/__omni_tmp__65319/original_5f_code_f90_out.xml -f transformed_code.f90 /tmp/__omni_tmp__65319/original_5f_code_f90_in.xml
```

It is recommended to use the `CLAW Compiler` with the `--debug-omni` option in order to keep the intermediate files and ease the development/debugging of the translator.

### Driver

The driver `clawfc` is driving the transformation process. This process is
defined as follows:

*Fortran Code* -> **Preprocessor(1)** -> *Fortran Code* -> **F_Front (2)** ->
*XcodeML Code* -> **Cx2x(3)** -> *XcodeML Code* -> **omx2f(4)** -> *Fortran Code*

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
* **Cx2x**: Dedicated claw directives translator. It translates XcodeML with
directives into a transformed XcodeML intermediate representation.
* **omx2f**: OMNI Compiler back-end. It converts XcodeML intermediate
representation into Fortran code.
