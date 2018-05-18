# Change Log
All notable changes to the CLAW Compiler project are documented in this file.

## [X.Y.Z] - Unreleased
* User comment line can be preserved with the `--keep-comment` option.
* SCA: Fix duplicated array references inserted in some cases.
* OMNI Compiler to git hash omni-compiler/xcodml-tools@e2d6978ba47e351146ba1812b927947156c9b913
* OMNI Compiler submodule now pointing to XcodeML-tools omni-compiler/xcodeml-tools

## [1.0.1] - 2018-04-17
* Support of Intel Compiler preprocessing workflow.
* Clean up properly temporary files when driver exits.
* SCA/CPU: Fix problem with indirect promotion (missing do stmt).
* Move test from `abstraction` to `sca` folder.
* OMNI Compiler to git hash omni-compiler/omni-compiler@f59978d90cc1d93cf16de125e8dd35ae1d2a6537

## [1.0.0] - 2018-03-19
New features:
* Option `--target=<target>` or `-t=<target>` allows to choose the target for
  code transformation.
* Option `--directive=<directive_language>` or `-d=<directive_language>` allows
  to choose the accelerator directive language used for code generation.
* Transformation order is now configurable with the option `--config=`. A
  default configuration file is available in
  `<INSTALL_DIR>/etc/claw-default.xml`.
* New configuration files with user extension.
* OpenACC local arrays strategy private or promote available from configuration.

New available transformations:
* Low-level:
  * `array-transform`
  * `kcache`
  * `call`
  * `loop-hoist`
  * `if-extract`
  * `verbatim`
  * `ignore`
* High abstraction (beta):
  * Single Column Abstraction
    * `parallelize`
    * `parallelize forward`

Modification:
* `collapse` clause can be applied to `loop-fusion` transformation.
* Group configuration must now specify trigger type (translation_unit, directive)

Technical/Architecture change:
* All Java libraries now compiled with Ant.
* Execution of JUnit test cases is driven by Ant.
* Program arguments of claw.ClawX2T is now using Common CLI.
* Preprocessor specific configurations are now stored in
  `compiler/<compiler_id>.cmake` files.
* Some transformations are implemented directly into the driver as they have to
  be performed before the parsing step.
* The full workflow is now pipelined. Only in debug mode, intermediate files are
  written to disk.
* CLAW X2T libraries are split in 3 distinct ones. For more information, refer
  to the developer's guide.

General:
* OMNI Compiler submodule points to the official OMNI Compiler repository.
  The state of the repository is updated only when the latest changes are tested
  and validated.

## [0.1.0] - 2016-02-05
### First release
- **CX2X XcodeML library**: An abstraction of a set of the XcodeML/F
intermediate representation that let the elements easily manipulable from Java
code.
- **CX2X CLAW translator**: An XcodeML to XcodeML translator that implements the
CLAW language directive v0.1 (loop-fusion, loop-interchange, loop-extract,
remove)
- **CLAW Compiler driver**: the compiler driver that glue together all
pieces together for the full workflow.
