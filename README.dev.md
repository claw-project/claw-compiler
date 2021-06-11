# Build system
Is based on [CMake](https://cmake.org) and [Apache Ant](https://ant.apache.org), where Cmake acts as a wrapper for Ant.

CMake is responsible for: 
* building main dependency [xcodeml-tools](https://github.com/claw-project/xcodeml-tools);
* getting info about the platform, compilers and installed software;
* processing user-specified build parameters;
* setting up out of source build;
* generating Eclipse IDE projects;
* installation;
* command-line interaction with build-system.

Ant-based solution is responsible for:
* obtaining 3rdparty java libraries;
* compiling java sources;
* generating java libraries (JARs);
* running tests from command-line.

There are 2 ways to interact with the build system:
1. command-line interface (CLI);
  * initially CMake is used to generate out-of-source build system based on GNU Make (on linux platforms);
  * GNU Make commands can be used to run all the important functions, such as build, test and installation;
2.	integrated development environment (IDE)
  * initially CMake is used to configure parts of the source and build system which depend on the platform and user input);
  * Eclipse projects can be used for IDE-based development as well as running some of the functions, such as build and test. However, installation can only be performed with CLI.
## CMake
<table>
<thead>
  <tr>
    <th colspan="2">Important files</th>
  </tr>
  <tr>
    <th>File</th>
    <th>Description</th>
  </tr>
</thead>
<tbody>
  <tr>
    <td>

[properties.cmake](properties.cmake)
    </td>
    <td>Collection of variables with build information</td>
  </tr>
  <tr>
    <td>

[compiler/cray.cmake](compiler/cray.cmake)
    </td>
    <td rowspan="5">Compiler-specific options</td>
  </tr>
  <tr>
    <td>

[compiler/gnu.cmake](compiler/gnu.cmake)
    </td>
  <tr>
    <td>

[compiler/intel.cmake](compiler/intel.cmake)
    </td>
  <tr>
    <td>

[compiler/nag.cmake](compiler/nag.cmake)
    </td>
  <tr>
    <td>

[compiler/pgi.cmake](compiler/pgi.cmake)
    </td>
  </tr>
  <tr>
    <td>

[CMakeLists.txt](CMakeLists.txt)
    </td>
    <td>Most of the setup and functionality</td>
  </tr>
  <tr>
    <td>Other CMakeLists.txt files</td>
    <td>Mostly deal with inserting build & platform specific data into source files.</td>
  </tr>
</tbody>
</table>

<table>
<thead>
  <tr>
    <th colspan="2">Options</th>
  </tr>
  <tr>
    <th>Name</th>
    <th>Description</th>
  </tr>
</thead>
<tbody>
  <tr>
    <td> JAVA_HOME </td>
    <td> Path to Java installation </td>
  </tr>
  <tr>
    <td> CMAKE_INSTALL_PREFIX </td>
    <td> Path to target install directory </td>
  </tr>
  <tr>
    <td> ADD_OMNI_XCODEML_TOOLS_TO_INSTALL </td>
    <td> Switch for bundling OMNI with CLAW </td>
  </tr>
  <tr>
    <td> BUILD_OMNI_XCODEML_TOOLS </td>
    <td> Switch to enable OMNI build </td>
  </tr>
  <tr>
    <td> OMNI_HOME </td>
    <td> Path to existing OMNI installation </td>
  </tr>
</tbody>
</table>

As a guideline, avoid Cmake usage, apart from where it is absolutely necessary. Use Ant instead. 

## GNU Make CLI
Cmake generates GNU Make-based build system, which provides CLI.
<table>
<thead>
  <tr>
    <th colspan="2">Commands</th>
  </tr>
  <tr>
    <th>Cmd</th>
    <th>Description</th>
  </tr>
</thead>
<tbody>
  <tr>
    <td> 

```shell
make 
```
</td>
    <td> Build CLAW (and optionally OMNI) </td>
  </tr>
  <tr>
    <td> 

```shell
make install 
```
</td>
    <td> Install CLAW (and optionally bundled OMNI) </td>
  </tr>
  <tr>
    <td> 

```shell
make generate-xmods
```
</td>
    <td> Generate xmodules for Fortran modules that are supposed to be included with the compiler. This is a prerequisite for running claw-tests. </td>
  </tr>
  <tr>
    <td> 

```shell
make run-driver-unit-tests
```
</td>
    <td rowspan="4"> Run corresponding test project </td>
  </tr>
  <tr>
    <td> 

```shell
make run-driver-tests
```
</td>
  </tr>
  <tr>
    <td> 

```shell
make run-cx2t-unit-test
```
</td>
  </tr>
  <tr>
    <td> 

```shell
make run-claw-tests
```
</td>
  </tr>

  <tr>
    <td> 

```shell
make run-tests
```
</td>
    <td> Run all tests </td>
  </tr>
</tbody>
</table>

## Ant solution
While CLI is based on CMake and GNU Make, internally most of the build system functionality is implemented with 
Apache Ant-based projects. There is a separate project for each distinct part of CLAW. Additionally all of these have 
a binding to top-level project. Additionally, the common build-specific information and parameters is concentrated in 
the file [&lt;build dir&gt;/cx2t/common/claw.properties](cx2t/claw.properties.in) , which mostly mirrors variables from
[properties.cmake](properties.cmake). Majority of the projects include this file.
<table>
<thead>
  <tr>
    <th colspan="3">Projects</th>
  </tr>
  <tr>
    <th>Name</th>
    <th>Path</th>
    <th>Description</th>
  </tr>
</thead>
<tbody>
  <tr>
    <td>cx2t</td>
    <td>

[cx2t/src/build.xml](cx2t/src/build.xml)
    </td>
    <td>CLAW library</td>
  </tr>
  <tr>
    <td>fetch-3rdparty-dependencies</td>
    <td>

[cx2t/common-targets.xml](cx2t/common-targets.xml)
    </td>
    <td>Resolves dependencies on 3rdparty libraries</td>
  </tr>
  <tr>
<td>claw-unit-tests</td>
    <td>

[cx2t/unittest/build.xml](cx2t/unittest/build.xml)
    </td>
    <td>CLAW unit tests</td>
  </tr>
  <tr>
    <td>clawfc</td>
    <td>

[driver/src/build.xml](driver/src/build.xml)
    </td>
    <td>CLAW driver</td>
  </tr>
  <tr>
    <td>clawfc-tests</td>
    <td>

[driver/tests/build.xml](driver/tests/build.xml)
    </td>
    <td>CLAW driver tests</td>
  </tr>
  <tr>
    <td>clawfc-unit-tests</td>
    <td>

[driver/unittests/build.xml](driver/unittests/build.xml)
    </td>
    <td>CLAW driver unit tests</td>
  </tr>
  <tr>
    <td>claw-tests</td>
    <td>

[tests_runner/build.xml](tests_runner/build.xml)
    </td>
    <td>CLAW tests</td>
  </tr>
  <tr>
    <td>solution</td>
    <td>

[ant-solution.xml](ant-solution.xml)
    </td>
    <td>Top-level project</td>
  </tr>
</tbody>
</table>

## IDE development
Eclipse Is fully supported, tested with version 4.14. Each Ant project has a corresponding Eclipse project. They can be 
imported into IDE from <build_dir>/ide/eclipse. Builds started from IDE will use the Ant build system projects 
internally. Note that, it always uses Java installation that was specified during configuration with CMake, which does 
not necessarily match the Java installation used by Eclipse itself. This may lead to problems during debugging, so it is
recommended to ensure that the versions match. This can be set centrally in Eclipse settings for all the projects or 
individually in “project/properties/java compiler/Enable Project specific settings/JDK compliance/use compliance 
from execution environment…”.

It should be further noted that these projects are not generated by CMake, but rather are produced by configuring 
template files with CMake variables. As a consequence, each change in the projects structure and dependencies will 
require manual adjustments of the template files. These can be found in [resources/eclipse](resources/eclipse). 

# Java Coding style
Code should be compatible with automated formatting performed by Eclipse IDE. The rules are defined in file 
[resources/eclipse/java-coding-style.xml](resources/eclipse/java-coding-style.xml), which can be imported into IDE. Note
that by default Java Formatter will compress multiline comments. To avoid that, prefix them with “/*-”.

Indent with spaces. NO tabs!

# Testing
All tests are written [JUnit framework](https://junit.org/junit4/). Collections of tests are organized into projects 
by topic, which are compiled into JARs using Ant. These can be launched either from cmdline using CMake or with Eclipse,
where it is possible to attach a debugger Additionally, all tests, with exception of claw-unit-tests, have a tests runner
which has an option of running individual testcases, by running the corresponding JAR with 2 arguments 
```<full name of the test class> <name of the test method>```

# Developer's guide
A guide is available under `documentation/developer`.

## Bash scripts
In general, all bash scripts written for the CLAW Compiler projects
follow these [guidelines](https://google.github.io/styleguide/shell.xml).

In addition, all scripts are tested with `shellcheck` and must pass the tested
without errors or warnings. So warnings might be disable where it make sense
for the correctness of the script.

**Note: do not add any new Bash scripts, unless absolutely unavoidable.**

## Git hooks
* Git hooks present in the `./scripts/git_hooks/` directory should be enabled
  in your local checkout. To enable them, you have to symlink them in the
  `./.git/hooks/` directory.

```bash
cd .git/hooks
ln -s ../../scripts/pre-commit pre-commit
```

Make sure the file is executable.

## Helper scripts
The [scripts](scripts) directory contains helper scripts for the development and review
process of the project.

# Driver specification
## Entities
_**_Fortran include file_**_
```
    contains ascii text of any kind 
```
**Fortran source file**
```
    contains ascii text of any kind 
```
**Preprocessed Fortran source file**
```
    contains valid ascii Fortran source with no preprocessor or Fortran include statements
```
**Fortran Program Unit Information (UnitInfo)**
```
    type (Module/Program/Function/Subroutine)
        position in Preprocessed Fortran source file
            names and positions of used modules
            whether claw is used
```
**Fortran File Unit Information (FileUnitInfo)**
```
    list of UnitInfo for units contained in preprocessed source file.
    List of paths to include files
    Path to source file
	Path to preprocessed source file

UnitInfo is dependent on:
	Fortran source file
	Fortran include files [referenced in the source file]
```
	
**Xmod file**
```
	Contains XCodeML header for one Fortran module
```

**Xast file**
```
	Contains XCodeML AST information for one Fortran program unit
```

**Output file**
```
	contains Fortran Program Unit sources
		some, transformed by claw according to input cfg
		others copied over from preprocessed source 
```
## Algorithm
```
Verify input options
	some combinations are not allowed
	some combinations require other options
Verify existence of input files and directories
Verify that input files don't have identical names (even with different file paths)
	
Process paths of input files 
	remove duplicates
	resolve relative paths [against startup working dir]

Load all available FileUnitInfo files from input FileUnitInfo dirs [-BI]
	Discard those that reference non-existing or outdated:
		source file 
		preprocessed-source file
		include files
	Discard those referencing files outside include source dirs [-SI]
	Discard duplicates (those that were generated for the same file)

if not [--skip-pp] given		
	Load preprocessed sources for files with valid FileUnitInfo
	Preprocess all input files for which there is no valid FileUnitInfo [use include dirs -I]
	Resolve Fortran include statements [use include dirs -I]
else
	after this point, treat input files as if they are already preprocessed

Output preprocessed input files [if output dir -PO given]
	if identical, do not output

Scan [=Generate FileUnitInfo] input files that don't have corresponding FileUnitInfo

Output input files FileUnitInfo [if output dir -BO given]

if [--print-claw-files] given
	Print names of input files using CLAW and exit

Scan include directories for fortran files [-SI]

if not --skip-pp given
	Preprocess all include files for which there is no valid FileUnitInfo
	Apply ignore directive to preprocessed-sources
	if output dir [-PO] given
		Output newly-generated preprocessed include files
else
	after this point, include files as if they are already preprocessed
For include files that don't have corresponding FileUnitInfo:
	Scan preprocessed include files for FileUnitInfo

Output include files FileUnitInfo [if output dir -BO given]

Load all available xmod files [-MI]
	Discard duplicates (same name)
	For those that are referenced by FileUnitInfo
		discard outdated

Sanity check the build (create build tree starting from units with CLAW in input files)

For all input modules and their dependencies without xmod
	Generate xmods (in build tree order)

Output all xmods in the build tree [if output dir -MO given]

For all input units using CLAW [or all units if --force given]
	Apply ignore directive [if --skip-pp given]
	Apply verbatim directive to module source
	Generate XAST
if [--force] given
	For all input units NOT using CLAW, without XAST
	Generate XAST
	
Output XAST, for all input units using CLAW [if output dir -XO given]
if [--force] given
	Output XAST, for all input units not using CLAW [if output dir -XO given]

For all input units using CLAW
	Translate xast using CX2T translator
	
Output translated XAST (TXAST), for all input units with XAST [if output dir -TXO given]

For all input units with XAST
	Generate decompiled source
	Undo the effects of ignore and verbatim
	
Output decompiled source files, for all input modules using CLAW [if output dir -TSO given]
	
For all input files
	Generate output file, which includes (in order they are encountered in input file)
		decompiled source for units which have one
		preprocessed source for the rest
```

