/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.language.accelerator;

import cx2x.translator.config.Configuration;
import cx2x.translator.language.accelerator.generator.AcceleratorGenerator;
import cx2x.translator.language.accelerator.generator.AcceleratorNone;
import cx2x.translator.language.accelerator.generator.OpenAcc;
import cx2x.translator.language.accelerator.generator.OpenMp;
import cx2x.translator.language.base.ClawDMD;
import cx2x.translator.language.base.ClawDirective;
import cx2x.translator.language.base.ClawLanguage;
import cx2x.xcodeml.exception.IllegalDirectiveException;
import cx2x.xcodeml.helper.XnodeUtil;
import cx2x.xcodeml.xnode.*;
import xcodeml.util.XmOption;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * The class AcceleratorHelper contains only static method to help the
 * generation of the accelerator related pragma during code transformations.
 *
 * @author clementval
 */
public class AcceleratorHelper {

  public static final int NO_COLLAPSE = 0;
  private static final String NO_CLAUSES = "";

  /**
   * Generate loop seq directives on the top of loops in the given function
   * definition.
   *
   * @param claw    ClawLanguage object that tells if the parallel clause is
   *                enable and where the start pragma is located.
   * @param xcodeml Object representation of the current XcodeML
   *                representation in which the pragmas will be generated.
   * @param fctDef  Function definition in which do statements will be
   *                decorated.
   */
  public static void generateLoopSeq(ClawLanguage claw, XcodeProgram xcodeml,
                                     XfunctionDefinition fctDef)
  {
    AcceleratorGenerator gen = claw.getAcceleratorGenerator();
    if(gen.getDirectiveLanguage() == AcceleratorDirective.NONE) {
      return;
    }

    List<Xnode> doStmts = fctDef.matchAll(Xcode.FDOSTATEMENT);
    for(Xnode doStmt : doStmts) {
      // Check if the nodep directive decorates the loop
      Xnode noDependency = isDecoratedWithNoDependency(doStmt);
      addPragmasBefore(xcodeml, gen.getStartLoopDirective(NO_COLLAPSE,
          noDependency == null, true, ""), doStmt);
      XnodeUtil.safeDelete(noDependency);

      // Debug logging
      if(XmOption.isDebugOutput()) {
        if(noDependency != null) {
          System.out.println(OpenAcc.OPENACC_DEBUG_PREFIX +
              "generated loop directive for loop at line: " + doStmt.lineNo());
        } else {
          System.out.println(OpenAcc.OPENACC_DEBUG_PREFIX +
              "generated loop seq directive for loop at line: "
              + doStmt.lineNo());
        }
      }
    }
  }

  /**
   * Generate update directives for device and host data transfer.
   *
   * @param claw    ClawLanguage object that tells if the parallel clause is
   *                enable and where the start pragma is located.
   * @param xcodeml Object representation of the current XcodeML
   *                representation in which the pragmas will be generated.
   * @param hook    Node used as a hook for insertion. Update device are
   *                generated before the hook and update host after the hook.
   * @return Last inserted pragma.
   */
  public static Xnode generateUpdate(ClawLanguage claw, XcodeProgram xcodeml,
                                     Xnode hook, List<String> vars)
  {
    AcceleratorGenerator gen = claw.getAcceleratorGenerator();
    if(gen.getDirectiveLanguage() == AcceleratorDirective.NONE
        || !claw.hasUpdateClause())
    {
      return null;
    }

    ClawDMD direction = claw.getUpdateClauseValue();
    Xnode p = null;
    if(direction == ClawDMD.DEVICE || direction == ClawDMD.BOTH) {
      p = addPragmasBefore(xcodeml,
          gen.getUpdateClause(direction == ClawDMD.BOTH ?
              ClawDMD.DEVICE : direction, vars), hook);
    }
    if(direction == ClawDMD.HOST || direction == ClawDMD.BOTH) {

      p = addPragmaAfter(xcodeml,
          gen.getUpdateClause(direction == ClawDMD.BOTH ?
              ClawDMD.HOST : direction, vars), hook);
    }
    return p;
  }

  /**
   * Check if there is a !$claw nodep directive before the do statement.
   *
   * @param doStmt Do statement to be checked.
   * @return True if the directive is present. False otherwise.
   */
  private static Xnode isDecoratedWithNoDependency(Xnode doStmt) {
    if(doStmt.opcode() != Xcode.FDOSTATEMENT) {
      return null;
    }

    Xnode sibling = doStmt.prevSibling();
    while(sibling != null && sibling.opcode() == Xcode.FPRAGMASTATEMENT) {
      try {
        ClawLanguage pragma = ClawLanguage.analyze(sibling, null, null);
        if(pragma.getDirective() == ClawDirective.NO_DEP) {
          return sibling;
        }
      } catch(IllegalDirectiveException ex) {
        // do not care about the error
      } finally {
        sibling = sibling.prevSibling();
      }
    }
    return null;
  }

  /**
   * Generate corresponding pragmas to surround the code with a parallel
   * accelerated region.
   *
   * @param claw      ClawLanguage object that tells if the parallel clause is
   *                  enable and where the start pragma is located.
   * @param xcodeml   Object representation of the current XcodeML
   *                  representation in which the pragmas will be generated.
   * @param startStmt Start statement representing the beginning of the parallel
   *                  region.
   * @param endStmt   End statement representing the end of the parallel region.
   * @return Last stmt inserted or null if nothing is inserted.
   */
  public static Xnode generateParallelClause(ClawLanguage claw,
                                             XcodeProgram xcodeml,
                                             Xnode startStmt, Xnode endStmt)
  {
    AcceleratorGenerator gen = claw.getAcceleratorGenerator();
    return insertPragmas(claw, xcodeml, startStmt, endStmt,
        gen.getStartParallelDirective(NO_CLAUSES),
        gen.getEndParallelDirective());
  }

  /**
   * Generate accelerator directive for a parallel loop.
   *
   * @param claw      ClawLanguage object that tells if the parallel clause is
   *                  enable and where the start pragma is located.
   * @param xcodeml   Object representation of the current XcodeML
   *                  representation in which the pragmas will be generated.
   * @param privates  List of variables to be set privates.
   * @param startStmt Start statement representing the beginning of the parallel
   *                  region.
   * @param endStmt   End statement representing the end of the parallel region.
   * @param collapse  If value bigger than 0, a corresponding collapse
   *                  constructs can be generated.
   */
  public static void generateParallelLoopClause(ClawLanguage claw,
                                                XcodeProgram xcodeml,
                                                List<String> privates,
                                                Xnode startStmt, Xnode endStmt,
                                                int collapse)
  {
    AcceleratorGenerator gen = claw.getAcceleratorGenerator();
    if(gen.getDirectiveLanguage() == AcceleratorDirective.NONE) {
      return;
    }

    addPragmasBefore(xcodeml, gen.getStartParallelDirective(null), startStmt);
    addPragmasBefore(xcodeml, gen.getStartLoopDirective(collapse, false, false,
        gen.getPrivateClause(privates)), startStmt);
    addPragmaAfter(xcodeml, gen.getEndParallelDirective(), endStmt);
    addPragmaAfter(xcodeml, gen.getEndLoopDirective(), endStmt);
  }

  /**
   * Generates accelerator directive for a loop region.
   *
   * @param claw      ClawLanguage object that tells if the parallel clause is
   *                  enable and where the start pragma is located.
   * @param xcodeml   Object representation of the current XcodeML
   *                  representation in which the pragmas will be generated.
   * @param startStmt Start statement representing the beginning of the loop
   *                  region.
   * @param endStmt   End statement representing the end of the loop region.
   * @param collapse  If value bigger than 0, a corresponding collapse
   *                  constructs can be generated.
   */
  public static void generateLoopDirectives(ClawLanguage claw,
                                            XcodeProgram xcodeml,
                                            Xnode startStmt, Xnode endStmt,
                                            int collapse)
  {
    AcceleratorGenerator gen = claw.getAcceleratorGenerator();
    insertPragmas(claw, xcodeml, startStmt, endStmt,
        gen.getStartLoopDirective(collapse, false, false, ""),
        gen.getEndLoopDirective());
  }

  /**
   * Generate accelerator directive for a data region.
   *
   * @param claw      ClawLanguage object that tells if the parallel clause is
   *                  enable and where the start pragma is located.
   * @param xcodeml   Object representation of the current XcodeML
   *                  representation in which the pragmas will be generated.
   * @param presents  List of variables to be set as present.
   * @param creates   List of variables to be created.
   * @param startStmt Start statement representing the beginning of the data
   *                  region.
   * @param endStmt   End statement representing the end of the data region.
   */
  public static void generateDataRegionClause(ClawLanguage claw,
                                              XcodeProgram xcodeml,
                                              List<String> presents,
                                              List<String> creates,
                                              Xnode startStmt, Xnode endStmt)
  {
    AcceleratorGenerator gen = claw.getAcceleratorGenerator();
    insertPragmas(claw, xcodeml, startStmt, endStmt,
        gen.getStartDataRegion(Arrays.asList(gen.getPresentClause(presents),
            gen.getCreateClause(creates))),
        gen.getEndDataRegion());
  }

  /**
   * Get all the function variables that are input/output parameters.
   *
   * @param xcodeml Current XcodeML program unit.
   * @param fctDef  Function definition to look in.
   * @return List of variables names that are function input/output.
   */
  public static List<String> getPresentVariables(XcodeProgram xcodeml,
                                                 XfunctionDefinition fctDef)
  {
    List<String> variables = new ArrayList<>();
    List<Xdecl> declarations = fctDef.getDeclarationTable().values();
    for(Xdecl decl : declarations) {
      if(decl.opcode() == Xcode.VARDECL) {
        Xnode name = decl.matchSeq(Xcode.NAME);
        String type = decl.getType();
        if(!(xcodeml.getTypeTable().get(type) instanceof XbasicType)) {
          continue; // Only check basic type
        }
        XbasicType bt = (XbasicType) xcodeml.getTypeTable().get(type);
        if(bt != null && (bt.getIntent() == Xintent.IN
            || bt.getIntent() == Xintent.OUT
            || bt.getIntent() == Xintent.INOUT) && bt.isArray())
        {
          variables.add(name.value());
        }
      }
    }
    return variables;
  }

  /**
   * Get all the local variables in the function definition.
   *
   * @param xcodeml Current XcodeML program unit.
   * @param fctDef  Function definition to look in.
   * @return List of variables names that are function local.
   */
  public static List<String> getLocalVariables(XcodeProgram xcodeml,
                                               XfunctionDefinition fctDef)
  {
    List<String> variables = new ArrayList<>();
    List<Xdecl> declarations = fctDef.getDeclarationTable().values();
    for(Xdecl decl : declarations) {
      if(decl.opcode() == Xcode.VARDECL) {
        Xnode name = decl.matchSeq(Xcode.NAME);
        String type = decl.getType();
        if(!XnodeUtil.isBuiltInType(type)
            && !(xcodeml.getTypeTable().get(type) instanceof XbasicType))
        {
          continue; // Only check basic type
        }
        XbasicType bt = (XbasicType) xcodeml.getTypeTable().get(type);
        if((bt == null && XnodeUtil.isBuiltInType(type))
            || (bt != null && bt.getIntent() == Xintent.NONE))
        {
          variables.add(name.value());
        }
      }
    }
    return variables;
  }

  /**
   * Get all the local variables in the function definition.
   *
   * @param xcodeml Current XcodeML program unit.
   * @param fctDef  Function definition to look in.
   * @return List of variables names that are function local.
   */
  public static List<String> getLocalArrays(XcodeProgram xcodeml,
                                            XfunctionDefinition fctDef)
  {
    List<String> variables = new ArrayList<>();
    List<Xdecl> declarations = fctDef.getDeclarationTable().values();
    for(Xdecl decl : declarations) {
      if(decl.opcode() == Xcode.VARDECL) {
        Xnode name = decl.matchSeq(Xcode.NAME);
        String type = decl.getType();
        if(!(xcodeml.getTypeTable().get(type) instanceof XbasicType)) {
          continue; // Only check basic type
        }
        XbasicType bt = (XbasicType) xcodeml.getTypeTable().get(type);
        if(bt != null && bt.getIntent() == Xintent.NONE && bt.isArray()
            && !bt.isAllocatable())
        {
          variables.add(name.value());
        }
      }
    }
    return variables;
  }

  /**
   * Generate corresponding pragmas applied directly after a CLAW pragma.
   *
   * @param claw      ClawLanguage object that tells if the parallel clause is
   *                  enable and where the start pragma is located.
   * @param startStmt Start statement representing the beginning of the parallel
   *                  region.
   * @param xcodeml   Object representation of the current XcodeML
   *                  representation in which the pragmas will be generated.
   * @return Last stmt inserted or null if nothing is inserted.
   */
  private static Xnode generateAcceleratorClause(
      ClawLanguage claw, XcodeProgram xcodeml, Xnode startStmt)
  {
    if(claw.hasAcceleratorClause()) {
      /* TODO
         OpenACC and OpenMP loop construct are pretty different ...
         have to look how to do that properly. See issue #22
       */
      return addPragmasBefore(xcodeml, claw.getAcceleratorGenerator().
          getSingleDirective(claw.getAcceleratorClauses()), startStmt);
    }
    return null;
  }

  /**
   * Generate all corresponding pragmas to be applied for accelerator.
   *
   * @param claw      ClawLanguage object that tells which accelerator pragmas
   *                  are enabled.
   * @param xcodeml   Object representation of the current XcodeML
   *                  representation in which the pragmas will be generated.
   * @param startStmt Start statement for all pragma generation.
   * @param endStmt   End statement for all pragma generation that need end
   *                  pragma statement.
   * @return Last stmt inserted.
   */
  public static Xnode generateAdditionalDirectives(
      ClawLanguage claw, XcodeProgram xcodeml, Xnode startStmt, Xnode endStmt)
  {
    if(claw.getDirectiveLanguage() == AcceleratorDirective.NONE) {
      return null;
    }

    Xnode pragma =
        generateAcceleratorClause(claw, xcodeml, startStmt);
    if(pragma != null) {
      startStmt = pragma;
    }

    if(claw.hasParallelClause()) {
      return generateParallelClause(claw, xcodeml, startStmt, endStmt);
    } else {
      return startStmt;
    }
  }

  /**
   * Generate all corresponding pragmas to be applied to an accelerated
   * function/subroutine.
   *
   * @param claw    ClawLanguage object that tells which accelerator pragmas
   *                are enabled.
   * @param xcodeml Object representation of the current XcodeML
   *                representation in which the pragmas will be generated.
   * @param fctDef  Function/subroutine in which accelerator directives are
   *                generated.
   */
  public static void generateRoutineDirectives(ClawLanguage claw,
                                               XcodeProgram xcodeml,
                                               XfunctionDefinition fctDef)
  {
    AcceleratorGenerator gen = claw.getAcceleratorGenerator();
    if(gen.getDirectiveLanguage() == AcceleratorDirective.NONE) {
      return;
    }

    // Find all fct call in the current transformed fct
    List<Xnode> fctCalls = fctDef.matchAll(Xcode.FUNCTIONCALL);
    for(Xnode fctCall : fctCalls) {
      // Do nothing for intrinsic fct
      if(fctCall.getBooleanAttribute(Xattr.IS_INTRINSIC)) {
        continue;
      }
      Xnode nameNode = fctCall.matchSeq(Xcode.NAME);
      String fctName;
      if(nameNode != null) {
        fctName = nameNode.value().toLowerCase();
      } else {
        continue;
      }
      XfunctionDefinition calledFctDef =
          xcodeml.getGlobalDeclarationsTable().getFctDefinition(fctName);
      if(calledFctDef == null) {
        Xnode meaningfulParentNode = fctDef.findParentModule();
        if(meaningfulParentNode == null) { // fct is not a module child
          meaningfulParentNode = fctDef.matchAncestor(Xcode.GLOBALDECLARATIONS);
        }
        List<Xnode> fctDefs =
            meaningfulParentNode.matchAll(Xcode.FFUNCTIONDEFINITION);
        for(Xnode fDef : fctDefs) {
          Xnode name = fDef.matchSeq(Xcode.NAME);
          if(name != null && name.value().toLowerCase().equals(fctName)) {
            calledFctDef = new XfunctionDefinition(fDef.element());
            break;
          }
        }
      }

      if(calledFctDef != null) {
        // TODO: check that the directive is not present yet.
        addPragmasBefore(xcodeml, gen.getRoutineDirective(true),
            calledFctDef.body().child(0));
        if(XmOption.isDebugOutput()) {
          System.out.println(OpenAcc.OPENACC_DEBUG_PREFIX
              + "generated routine seq directive for " + fctName
              + " subroutine/function.");
        }
      } else {
        // Could not generate directive for called function.
        xcodeml.addWarning(fctName + " has not been found. " +
                "Automatic routine directive generation could not be done.",
            fctCall.lineNo());
      }
    }
  }

  /**
   * Generate the correct clauses for private variable on accelerator.
   *
   * @param claw    ClawLanguage object that tells which accelerator pragmas
   *                are enabled.
   * @param xcodeml Object representation of the current XcodeML
   *                representation in which the pragmas will be generated.
   * @param stmt    Statement from which we looks for a parallel clause to
   *                append private clauses.
   * @param var     Variable to generate the private clause.
   */
  public static void generatePrivateClause(ClawLanguage claw,
                                           XcodeProgram xcodeml,
                                           Xnode stmt,
                                           String var)
  {
    if(claw.getDirectiveLanguage() == AcceleratorDirective.NONE
        || !claw.hasPrivateClause())
    {
      return;
    }

    Xnode hook = XnodeUtil.findPreviousPragma(stmt,
        claw.getAcceleratorGenerator().getParallelKeyword());
    // TODO do it with loop as well if hook is null

    if(hook == null) {
      xcodeml.addWarning("No parallel construct found to attach private clause",
          claw.getPragma().lineNo());
    } else {
      hook.setValue(hook.value() + " " +
          claw.getAcceleratorGenerator().getPrivateClause(var));
    }
  }

  /**
   * Constructs the correct AcceleratorGenerator object regarding the enum
   * value passed.
   *
   * @param config Configuration information object.
   * @return A specific implementation of an AcceleratorGenerator.
   */
  public static AcceleratorGenerator createAcceleratorGenerator(
      Configuration config)
  {
    switch(config.getCurrentDirective()) {
      case OPENACC:
        return new OpenAcc(config);
      case OPENMP:
        return new OpenMp(config);
    }
    return new AcceleratorNone(config);
  }

  /**
   * Create and insert a pragma statement before the reference node.
   *
   * @param xcodeml    Current XcodeML program unit.
   * @param directives Value of the newly created directive.
   * @param ref        Reference node used to insert the newly created pragma.
   * @return Newly created pragma statement as an Xnode object.
   */
  private static Xnode addPragmasBefore(XcodeProgram xcodeml,
                                        String[] directives, Xnode ref)
  {
    return insertPragmas(xcodeml, directives, ref, false);
  }

  /**
   * Create and insert a pragma statement after the reference node.
   *
   * @param xcodeml    Current XcodeML program unit.
   * @param directives Value of the newly created directive.
   * @param ref        Reference node used to insert the newly created pragma.
   * @return Newly created pragma statement as an Xnode object.
   */
  private static Xnode addPragmaAfter(XcodeProgram xcodeml, String[] directives,
                                      Xnode ref)
  {
    return insertPragmas(xcodeml, directives, ref, true);
  }

  /**
   * Create and insert a pragma statement.
   *
   * @param xcodeml    Current XcodeML program unit.
   * @param directives Value of the newly created directive.
   * @param ref        Reference node used to insert the newly created pragma.
   * @param after      By default, the pragma is inserted before the reference.
   *                   If after is set to true, the pragma is inserted after.
   * @return Newly created pragma statement as an Xnode object.
   */
  private static Xnode insertPragmas(XcodeProgram xcodeml, String[] directives,
                                     Xnode ref, boolean after)
  {
    if(directives == null) {
      return null;
    }
    Xnode pragma = null;
    for(String directive : directives) {
      pragma = new Xnode(Xcode.FPRAGMASTATEMENT, xcodeml);
      pragma.setValue(directive);
      if(after) {
        ref.insertAfter(pragma);
        ref = pragma; // Insert pragma sequentially one after another
      } else {
        ref.insertBefore(pragma);
      }
    }
    return pragma;
  }

  /**
   * Generate corresponding pragmas to surround the code with a parallel
   * accelerated region.
   *
   * @param claw           ClawLanguage object that tells if the parallel clause
   *                       is enable and where the start pragma is located.
   * @param xcodeml        Current XcodeML program unit.
   *                       representation in which the pragmas will be
   *                       generated.
   * @param startStmt      Start reference statement.
   * @param endStmt        End reference statement.
   * @param startDirective String value of the start directive.
   * @param endDirective   String value of the end directive.
   * @return Last stmt inserted or null if nothing is inserted.
   */
  private static Xnode insertPragmas(ClawLanguage claw, XcodeProgram xcodeml,
                                     Xnode startStmt, Xnode endStmt,
                                     String[] startDirective,
                                     String[] endDirective)
  {
    AcceleratorGenerator gen = claw.getAcceleratorGenerator();
    if(gen.getDirectiveLanguage() == AcceleratorDirective.NONE) {
      return null;
    }
    Xnode begin = addPragmasBefore(xcodeml, startDirective, startStmt);
    Xnode end = addPragmaAfter(xcodeml, endDirective, endStmt);
    return end != null ? end : begin;
  }

  /**
   * Skip elements in preamble and find the first element that will be included
   * in the parallel region.
   *
   * @param generator          Current accelerator generator.
   * @param functionDefinition Function definition in which body checked.
   * @param from               Optional element to start from. If null, starts
   *                           from first element in function's body.
   * @return First element for the parallel region.
   */
  public static Xnode findParallelRegionStart(AcceleratorGenerator generator,
                                              Xnode functionDefinition,
                                              Xnode from)
  {
    if(generator == null
        || functionDefinition.opcode() != Xcode.FFUNCTIONDEFINITION)
    {
      return null;
    }
    Xnode first = functionDefinition.body().firstChild();
    if(from != null) { // Start from given element
      first = from;
    }
    if(generator.getSkippedStatementsInPreamble().isEmpty()) {
      return first;
    } else {
      while(first.nextSibling() != null
          && generator.getSkippedStatementsInPreamble().
          contains(first.opcode())) {
        first = first.nextSibling();
      }
    }
    return first;
  }

  /**
   * Skip elements in epilogue and find the last element that will be included
   * in the parallel region.
   *
   * @param generator          Current accelerator generator.
   * @param functionDefinition Function definition in which body checked.
   * @param from               Optional element to start from. If null, starts
   *                           from last element in function's body.
   * @return Last element for the parallel region.
   */
  public static Xnode findParallelRegionEnd(AcceleratorGenerator generator,
                                            Xnode functionDefinition,
                                            Xnode from)
  {
    if(generator == null
        || functionDefinition.opcode() != Xcode.FFUNCTIONDEFINITION)
    {
      return null;
    }
    Xnode last = functionDefinition.body().lastChild();
    if(from != null) { // Start from given element
      last = from;
      if(last.opcode() == Xcode.FCONTAINSSTATEMENT) {
        last = last.prevSibling();
      }
    }
    if(generator.getSkippedStatementsInEpilogue().isEmpty()) {
      return last;
    } else {
      while(last.prevSibling() != null
          && generator.getSkippedStatementsInEpilogue().
          contains(last.opcode())) {
        last = last.prevSibling();
      }
    }
    return last;
  }

}
