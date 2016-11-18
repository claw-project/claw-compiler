# Change Log
All notable changes to the CLAW FORTRAN Compiler project will be documented in
this file.

## [0.3 Unreleased]
New features:
* Option `--target=<target>` or `-t=<target>` allows to choose the target for
  code transformation.
* Option `--directive=<directive_language>` or `-d=<directive_language>` allows
  to choose the accelerator directive language used for code generation.
* Transformation order is now configurable with the option `--config=`. A
  default configuration file is available in
  `<INSTALL_DIR>/etc/claw-default.xml`.

New available transformations:
* Low-level:
  * `array-transform`
  * `kcache`
  * `call`
  * `loop-hoist`
* High abstraction:
  * `parallelize`

Modification:
* `collapse` clause can be applied to `loop-fusion` transformation.

General:
* OMNI Compiler is switched to a dedicated forked version including F2003
  parsing enhancements as well as latest change from the original OMNI
  repository. It will be switched back to official version once all the
  new features are back to the master.

## [0.1.0] - 2016-02-05
### First release
- **CX2X XcodeML library**: An abstraction of a set of the XcodeML/F
intermediate representation that let the elements easily manipulable from Java
code.
- **CX2X CLAW translator**: An XcodeML to XcodeML translator that implements the
CLAW language directive v0.1 (loop-fusion, loop-interchange, loop-extract,
remove)
- **CLAW Fortran Compiler driver**: the compiler driver that glue together all
pieces together for the full workflow.
