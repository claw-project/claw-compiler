# Change Log
All notable changes to the CLAW Fortran Compiler project will be documented in
this file.

## [0.2a Unreleased]
New features:
* Transformation order is now configurable with the option `--config=`. A
  default configuration file is available in
  `<INSTALL_DIR>/etc/claw-default.xml`.
* Option `--target=` allows to generate code for different targets.

New transformation:
* `array-transform`
* `kcache`
* `call`
* `loop-hoist`

Modification:
* `collapse` clause can be applied to `loop-fusion` transformation.

General:
* OMNI Compiler version is updated to version 1.0.0.

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
