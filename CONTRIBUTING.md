# Contributing to CLAW

First of all, thanks for taking time to contribute!

## Using pull request
Pull requests are much welcome!!
* To fix an issue, PR must reference the issue it fixes.
* For PR implementing a language specification, the PR must reference the
  feature in the language it implements.
* To propose architecture changes or new features, those should be discussed
  beforehand in the Slack workspace or in a dedicated issue.
* PR must pass all tests executed by Travis CI. A PR might add tests or update
  references if the change requests it.
* New code should be in principle be covered at 100% by unit tests.
* Eventual conflict in PR should be resolved by the issuer of the PR.  

## Code Style Guide
The CLAW Compiler project uses different languages and here are the code style
guidelines to follow for each of them.

### General
All the guidelines expressed here are not valid for the external submodule like
the OMNI Compiler where we have not power on this.

* No tab anywhere unless it is mandatory (e.g. Makefile)
* 2 spaces indentation if not defined.
* Source file have a mention of the LICENSE and point to the license file of
  this repository.
  * for Java
    ```java
    /*
     * This file is released under terms of BSD license
     * See LICENSE file for more information
     */
    ```
  * for FORTRAN
    ```fortran
    !
    ! This file is released under terms of BSD license
    ! See LICENSE file for more information
    !
    ```
  * for bash
    ```bash
    #
    # This file is released under terms of BSD license
    # See LICENSE file for more information
    #
    ```
  * for XML
    ```xml
    <!--
     This file is released under terms of BSD license
     See LICENSE file for more information
    -->
    ```

### Bash
The main `bash` source code reside in the driver. The code should be
compliant with Google's Shell Code Style. In addition, all shell scripts must
pass through `shellcheck` without any error. This is enforce in Travis CI.

* [Google's Shell Code Style](https://google.github.io/styleguide/shell.xml)
* [Shellcheck](https://github.com/koalaman/shellcheck): Version 0.4.6 is
  currently used in Travis CI.

### Java
The core of the CLAW Compiler is written in Java. All the analyzis and
transformation is implemented in the CLAW XcodeML Translator (ClawX2T).
* ClawX2T is divided in three layers. The lowest one is claw/tatsu, then
  claw/shenron and finally claw/wani. Dependency can be done from top to
  bottom but not from bottom to top.
* If using IntelliJ IDEA, the code style file is located
  [here](./cx2t/config/claw_code_style_idea.xml). Otherwise, the following
  rules apply:

##### General
* 2 spaces indentation. If the line is wrapped because of its length, the
  continued line uses 4 spaces indentation.
  ```java
    if(Context.get().getGenerator().getDirectiveLanguage()
        == CompilerDirective.NONE)
  ```
* All line wrapped to 80 columns.
  * if the opening brace `{` does not fit with all the content on the first,
    line then it has to be wrapped on a separated line.
    ```java
    // Opening brace on the first line
    if(noDependency == null) {
    }

    // Opening brace is not on the first line, wrap it to the next line.
    if(Context.get().getGenerator().getDirectiveLanguage()
        == CompilerDirective.NONE)
    {
      return nodep_counter;
    }
    ```
* One blank line separates methods
* A whitespace is used around the following elements:
  * assignments operators (=, +=, ...)
  * logical operators (&&, ||)
  * equality operator (==, !=)
  * relational operator (<, >, <=, >=)
  * bitwise operator (&, |, ^)
  * additive operator (+, -)
  * multiplicative operator (*, /, %)
  * shift operator (<<, >>, >>>)
  * lambda arrow (->)
  * before class, method, if, else, for, while, do, swicth, try, catch,
    finally, synchronized left brace
  * before keywords else, while, catch, finally
  * in ternary operator `(conditional) ? true : false`
  * after coma
  * after semi colon when it's not end of line (e.g. in `for` loop)
  * after type cast

##### Javadoc
* All class have an header in Javadoc describing their content. The header
  include the `@author` description.
* All methods have a Javadoc description. The description include `@param` for
  each method's parameters, `@return` if the method is not void and `@throw`
  if it applies.
  The description is separated from the `@param` with a blank line.
  ```java
  /**
   * Generate loop seq directives on the top of loops in the given function
   * definition.
   *
   * @param xcodeml Object representation of the current XcodeML
   *                representation in which the pragmas will be generated.
   * @param fctDef  Function definition in which do statements will be
   *                decorated.
   * @return Number of loop without dependency.
   */
  public static int generateLoopSeq(XcodeProgram xcodeml,
                                    FfunctionDefinition fctDef,
                                    String noDependencyDirective)
  ```

### FORTRAN
FORTRAN is only used in the examples and reference tests. The following code
guidelines are applied:

* 2 spaces indentation
*
