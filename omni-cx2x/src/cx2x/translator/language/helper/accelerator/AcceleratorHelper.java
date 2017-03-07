/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.language.helper.accelerator;

import cx2x.translator.config.Configuration;
import cx2x.translator.language.base.ClawLanguage;
import cx2x.xcodeml.helper.XnodeUtil;
import cx2x.xcodeml.xnode.*;
import xcodeml.util.XmOption;

import java.util.*;

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
      addPragmasBefore(xcodeml, gen.getStartLoopDirective(NO_COLLAPSE, true),
          doStmt);
    }

    if(XmOption.isDebugOutput()) {
      System.out.println("OpenACC: generated loop seq directive for " +
          doStmts.size() + " loops");
    }
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

    addPragmasBefore(xcodeml,
        gen.getStartParallelDirective(gen.getPrivateClause(privates)),
        startStmt);
    addPragmasBefore(xcodeml, gen.getStartLoopDirective(collapse, false),
        startStmt);
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
        gen.getStartLoopDirective(collapse, false), gen.getEndLoopDirective());
  }

  /**
   * Generate accelerator directive for a data region.
   *
   * @param claw      ClawLanguage object that tells if the parallel clause is
   *                  enable and where the start pragma is located.
   * @param xcodeml   Object representation of the current XcodeML
   *                  representation in which the pragmas will be generated.
   * @param presents  List of variables to be set presents.
   * @param startStmt Start statement representing the beginning of the data
   *                  region.
   * @param endStmt   End statement representing the end of the data region.
   */
  public static void generateDataRegionClause(ClawLanguage claw,
                                              XcodeProgram xcodeml,
                                              List<String> presents,
                                              Xnode startStmt, Xnode endStmt)
  {
    AcceleratorGenerator gen = claw.getAcceleratorGenerator();
    insertPragmas(claw, xcodeml, startStmt, endStmt,
        gen.getStartDataRegion(gen.getPresentClause(presents)),
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
    Collection<Xdecl> declarations = fctDef.getDeclarationTable().getAll();
    for(Xdecl decl : declarations) {
      if(decl.opcode() == Xcode.VARDECL) {
        Xnode name = decl.matchSeq(Xcode.NAME);
        String type = name.getAttribute(Xattr.TYPE);
        if(!(xcodeml.getTypeTable().get(type) instanceof XbasicType)) {
          continue; // Only check basic type
        }
        XbasicType bt = (XbasicType) xcodeml.getTypeTable().get(type);
        if(bt != null && (bt.getIntent() == Xintent.IN
            || bt.getIntent() == Xintent.OUT
            || bt.getIntent() == Xintent.INOUT))
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
    Collection<Xdecl> declarations = fctDef.getDeclarationTable().getAll();
    for(Xdecl decl : declarations) {
      if(decl.opcode() == Xcode.VARDECL) {
        Xnode name = decl.matchSeq(Xcode.NAME);
        String type = name.getAttribute(Xattr.TYPE);
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

    List<Xnode> fctCalls = fctDef.matchAll(Xcode.FUNCTIONCALL);
    Set<String> fctNames = new HashSet<>();
    for(Xnode fctCall : fctCalls) {
      if(fctCall.getBooleanAttribute(Xattr.IS_INTRINSIC)) {
        continue;
      }
      Xnode name = fctCall.matchSeq(Xcode.NAME);
      if(name != null) {
        fctNames.add(name.value().toLowerCase());
      }
    }

    // TODO: check that the directive is not present yet.

    for(String fctName : fctNames) {
      XfunctionDefinition calledFctDef =
          xcodeml.getGlobalDeclarationsTable().getFctDefinition(fctName);
      if(calledFctDef == null) {
        XmoduleDefinition mod = fctDef.findParentModule();
        List<Xnode> fctDefs = mod.matchAll(Xcode.FFUNCTIONDEFINITION);
        for(Xnode fDef : fctDefs) {
          Xnode name = fDef.matchSeq(Xcode.NAME);
          if(name != null && name.value().toLowerCase().equals(fctName)) {
            calledFctDef = new XfunctionDefinition(fDef.element());
            break;
          }
        }
      }

      if(calledFctDef != null) {

        addPragmasBefore(xcodeml, gen.getRoutineDirective(true),
            calledFctDef.body().child(0));
        if(XmOption.isDebugOutput()) {
          System.out.println("OpenACC: generated routine seq directive for " +
              fctName + " subroutine/function.");
        }
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

}
