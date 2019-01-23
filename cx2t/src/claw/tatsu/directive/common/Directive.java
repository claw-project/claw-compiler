/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.directive.common;

import claw.tatsu.TatsuConstant;
import claw.tatsu.common.CompilerDirective;
import claw.tatsu.common.Context;
import claw.tatsu.common.Message;
import claw.tatsu.directive.generator.DirectiveGenerator;

import claw.tatsu.primitive.Pragma;
import claw.tatsu.xcodeml.xnode.XnodeUtil;
import claw.tatsu.xcodeml.xnode.common.Xattr;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import claw.tatsu.xcodeml.xnode.fortran.FfunctionDefinition;
import claw.wani.x2t.configuration.Configuration;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * The class Directive contains only static method to help the
 * generation of the directive related pragma during code transformations.
 *
 * @author clementval
 */
public final class Directive {

  public static final int NO_COLLAPSE = 0;
  private static final String NO_CLAUSES = "";

  // Avoid potential instantiation of this class
  private Directive() {
  }

  /**
   * Generate loop seq directives on the top of loops in the given function
   * definition.
   *
   * @param xcodeml               Object representation of the current XcodeML
   *                              representation in which the pragmas will be
   *                              generated.
   * @param fctDef                Function definition in which do statements
   *                              will be decorated.
   * @param noDependencyDirective Directive string used to flag a loop as
   *                              no dependency loop.
   * @return Number of independent flagged loop.
   */
  public static int generateLoopSeq(XcodeProgram xcodeml,
                                    FfunctionDefinition fctDef,
                                    String noDependencyDirective)
  {

    if(Context.get().getCompilerDirective() == CompilerDirective.NONE) {
      return 0;
    }

    int nodepCounter = 0;
    List<Xnode> doStmts = fctDef.matchAll(Xcode.F_DO_STATEMENT);
    for(Xnode doStmt : doStmts) {
      // Check if the nodep directive decorates the loop
      Xnode noDependency = isDecoratedWith(doStmt, noDependencyDirective);
      if(noDependency == null) {
        addPragmasBefore(xcodeml,
            Context.get().getGenerator().
                getStartLoopDirective(NO_COLLAPSE, true, true, ""), doStmt);
      } else {
        ++nodepCounter;
      }
      XnodeUtil.safeDelete(noDependency);

      // Debug logging
      Message.debug(String.format(
          "%s generated loop %s directive for loop at line: %d",
          Context.get().getGenerator().getPrefix(),
          (noDependency == null) ? "seq" : "", doStmt.lineNo()));
    }

    return Configuration.get().accelerator().hasCollapseStrategy()
        ? nodepCounter : 0;
  }

  /**
   * Generate update directives for device and host data transfer.
   *
   * @param xcodeml   Object representation of the current XcodeML
   *                  representation in which the pragmas will be generated.
   * @param hook      Node used as a hook for insertion. Update device are
   *                  generated before the hook and update host after the hook.
   * @param vars      List of variables inserted in the directive.
   * @param direction Direction of the update directive.
   * @return Last inserted pragma.
   */
  public static Xnode generateUpdate(XcodeProgram xcodeml, Xnode hook,
                                     List<String> vars, DataMovement direction)
  {
    if(Context.get().getGenerator().getDirectiveLanguage() ==
        CompilerDirective.NONE)
    {
      return null;
    }

    Xnode p = null;
    if(direction == DataMovement.DEVICE || direction == DataMovement.BOTH) {
      p = addPragmasBefore(xcodeml, Context.get().getGenerator().
          getUpdateClause(direction == DataMovement.BOTH ?
              DataMovement.DEVICE : direction, vars), hook);
    }
    if(direction == DataMovement.HOST || direction == DataMovement.BOTH) {
      p = addPragmaAfter(xcodeml, Context.get().getGenerator().
          getUpdateClause(direction == DataMovement.BOTH ?
              DataMovement.HOST : direction, vars), hook);
    }
    return p;
  }

  /**
   * Check if there is a !$claw nodep directive before the do statement.
   *
   * @param doStmt Do statement to be checked.
   * @return True if the directive is present. False otherwise.
   */
  private static Xnode isDecoratedWith(Xnode doStmt, String directive) {
    if(doStmt.opcode() != Xcode.F_DO_STATEMENT) {
      return null;
    }

    Xnode sibling = doStmt.prevSibling();
    while(sibling != null && sibling.opcode() == Xcode.F_PRAGMA_STATEMENT) {
      if(sibling.value().toLowerCase().contains(directive.toLowerCase())) {
        return sibling;
      }
      sibling = sibling.prevSibling();
    }
    return null;
  }

  /**
   * Generate corresponding pragmas to surround the code with a parallel
   * accelerated region.
   *
   * @param xcodeml   Object representation of the current XcodeML
   *                  representation in which the pragmas will be generated.
   * @param startStmt Start statement representing the beginning of the parallel
   *                  region.
   * @param endStmt   End statement representing the end of the parallel region.
   * @return Last stmt inserted or null if nothing is inserted.
   */
  public static Xnode generateParallelClause(XcodeProgram xcodeml,
                                             Xnode startStmt, Xnode endStmt)
  {
    return insertPragmas(xcodeml, startStmt, endStmt,
        Context.get().getGenerator().getStartParallelDirective(NO_CLAUSES),
        Context.get().getGenerator().getEndParallelDirective());
  }

  /**
   * Generate directive directive for a parallel loop.
   *
   * @param xcodeml   Object representation of the current XcodeML
   *                  representation in which the pragmas will be generated.
   * @param privates  List of variables to be set privates.
   * @param startStmt Start statement representing the beginning of the parallel
   *                  region.
   * @param endStmt   End statement representing the end of the parallel region.
   * @param collapse  If value bigger than 0, a corresponding collapse
   *                  constructs can be generated.
   */
  public static void generateParallelLoopClause(XcodeProgram xcodeml,
                                                List<String> privates,
                                                Xnode startStmt, Xnode endStmt,
                                                int collapse)
  {
    if(Context.get().getGenerator().getDirectiveLanguage()
        == CompilerDirective.NONE)
    {
      return;
    }

    DirectiveGenerator dg = Context.get().getGenerator();

    addPragmasBefore(xcodeml, dg.getStartParallelDirective(null), startStmt);
    addPragmasBefore(xcodeml, dg.getStartLoopDirective(collapse, false, false,
        dg.getPrivateClause(privates)), startStmt);
    addPragmaAfter(xcodeml, dg.getEndParallelDirective(), endStmt);
    addPragmaAfter(xcodeml, dg.getEndLoopDirective(), endStmt);
  }

  /**
   * Generates directive directive for a loop region.
   *
   * @param xcodeml   Object representation of the current XcodeML
   *                  representation in which the pragmas will be generated.
   * @param startStmt Start statement representing the beginning of the loop
   *                  region.
   * @param endStmt   End statement representing the end of the loop region.
   * @param collapse  If value bigger than 0, a corresponding collapse
   *                  constructs can be generated.
   */
  public static void generateLoopDirectives(XcodeProgram xcodeml,
                                            Xnode startStmt, Xnode endStmt,
                                            int collapse)
  {
    insertPragmas(xcodeml, startStmt, endStmt, Context.get().getGenerator().
            getStartLoopDirective(collapse, false, false, ""),
        Context.get().getGenerator().getEndLoopDirective());
  }

  /**
   * Generate directive directive for a data region. Some clauses can be ignored
   * depending on the configuration, if this results to discard all variables
   * then the directive is not generated.
   *
   * @param xcodeml   Object representation of the current XcodeML
   *                  representation in which the pragmas will be generated.
   * @param presents  List of variables to be set as present.
   * @param creates   List of variables to be created.
   * @param startStmt Start statement representing the beginning of the data
   *                  region.
   * @param endStmt   End statement representing the end of the data region.
   */
  public static void generateDataRegionClause(XcodeProgram xcodeml,
                                              List<String> presents,
                                              List<String> creates,
                                              Xnode startStmt, Xnode endStmt)
  {
    DirectiveGenerator generator = Context.get().getGenerator();
    List<String> clauses = new ArrayList<>(Arrays.asList(
        generator.getPresentClause(presents),
        generator.getCreateClause(creates)));

    clauses.removeAll(Collections.singletonList(""));

    // No need to create an empty data region
    if(!clauses.isEmpty()) {
      insertPragmas(xcodeml, startStmt, endStmt,
          generator.getStartDataRegion(clauses), generator.getEndDataRegion());
    }
  }

  /**
   * Generate corresponding pragmas applied directly after a CLAW pragma.
   *
   * @param startStmt Start statement representing the beginning of the parallel
   *                  region.
   * @param xcodeml   Object representation of the current XcodeML
   *                  representation in which the pragmas will be generated.
   * @param accClause Additional clause append at the end of the directive.
   * @return Last stmt inserted or null if nothing is inserted.
   */
  public static Xnode generateAcceleratorClause(
      XcodeProgram xcodeml, Xnode startStmt, String accClause)
  {
    /* TODO
       OpenACC and OpenMP loop construct are pretty different ...
       have to look how to do that properly. See issue #22
     */
    return addPragmasBefore(xcodeml, Context.get().getGenerator().
        getSingleDirective(accClause), startStmt);
  }

  /**
   * Generate all corresponding pragmas to be applied to an accelerated
   * function/subroutine.
   *
   * @param xcodeml Object representation of the current XcodeML
   *                representation in which the pragmas will be generated.
   * @param fctDef  Function/subroutine in which directive directives are
   *                generated.
   */
  public static void generateRoutineDirectives(XcodeProgram xcodeml,
                                               FfunctionDefinition fctDef)
  {
    if(Context.get().getGenerator().getDirectiveLanguage()
        == CompilerDirective.NONE)
    {
      return;
    }

    DirectiveGenerator dirGen = Context.get().getGenerator();

    // Find all fct call in the current transformed fct
    List<Xnode> fctCalls = fctDef.matchAll(Xcode.FUNCTION_CALL);
    for(Xnode fctCall : fctCalls) {
      // Do nothing for intrinsic fct
      if(fctCall.getBooleanAttribute(Xattr.IS_INTRINSIC)) {
        continue;
      }
      Xnode nameNode = fctCall.matchSeq(Xcode.NAME);
      String fctName;
      if(nameNode != null) {
        fctName = nameNode.value();
      } else {
        continue;
      }
      FfunctionDefinition calledFctDef =
          xcodeml.getGlobalDeclarationsTable().getFunctionDefinition(fctName);
      if(calledFctDef == null) {
        Xnode meaningfulParentNode = fctDef.findParentModule();
        if(meaningfulParentNode == null) { // fct is not a module child
          meaningfulParentNode =
              fctDef.matchAncestor(Xcode.GLOBAL_DECLARATIONS);
        }
        List<Xnode> fctDefs =
            meaningfulParentNode.matchAll(Xcode.F_FUNCTION_DEFINITION);
        for(Xnode fDef : fctDefs) {
          Xnode name = fDef.matchSeq(Xcode.NAME);
          if(name != null && name.value().equals(fctName)) {
            calledFctDef = new FfunctionDefinition(fDef);
            break;
          }
        }
      }

      if(calledFctDef != null) {
        // TODO: check that the directive is not present yet.
        addPragmasBefore(xcodeml, dirGen.getRoutineDirective(true),
            calledFctDef.body().child(0));
        Message.debug(dirGen.getPrefix()
            + "generated routine seq directive for " + fctName
            + " subroutine/function.");
      } else {
        // Could not generate directive for called function.
        xcodeml.addWarning(fctName + " has not been found. " +
                "Automatic routine directive generation could not be done.",
            fctCall.lineNo());
      }
    }
  }

  /**
   * Generate the correct clauses for private variable on directive.
   *
   * @param xcodeml Object representation of the current XcodeML
   *                representation in which the pragmas will be generated.
   * @param stmt    Statement from which we looks for a parallel clause to
   *                append private clauses.
   * @param var     Variable to generate the private clause.
   */
  public static void generatePrivateClause(XcodeProgram xcodeml,
                                           Xnode stmt,
                                           String var)
  {
    if(Context.get().getGenerator().getDirectiveLanguage() ==
        CompilerDirective.NONE)
    {
      return;
    }

    Xnode hook = Pragma.findPrevious(stmt,
        Context.get().getGenerator().getParallelKeyword());
    // TODO do it with loop as well if hook is null

    if(hook == null) {
      xcodeml.addWarning("No parallel construct found to attach private clause",
          stmt.lineNo());
    } else {
      hook.setValue(hook.value() + " " +
          Context.get().getGenerator().getPrivateClause(var));
    }
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
    for(String directive : directives) {
      List<Xnode> pragmas = xcodeml.createPragma(directive,
          Context.get().getMaxColumns());
      for(Xnode pragma : pragmas) {
        if(after) {
          ref.insertAfter(pragma);
          ref = pragma; // Insert pragma sequentially one after another
        } else {
          ref.insertBefore(pragma); // Only the first chunk needs to be inserted
          after = true;
          ref = pragma;
        }
      }
    }
    return ref;
  }

  /**
   * Generate corresponding pragmas to surround the code with a parallel
   * accelerated region.
   *
   * @param xcodeml        Current XcodeML program unit.
   *                       representation in which the pragmas will be
   *                       generated.
   * @param startStmt      Start reference statement.
   * @param endStmt        End reference statement.
   * @param startDirective String value of the start directive.
   * @param endDirective   String value of the end directive.
   * @return Last stmt inserted or null if nothing is inserted.
   */
  private static Xnode insertPragmas(XcodeProgram xcodeml,
                                     Xnode startStmt, Xnode endStmt,
                                     String[] startDirective,
                                     String[] endDirective)
  {
    if(Context.get().getGenerator().getDirectiveLanguage()
        == CompilerDirective.NONE)
    {
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
   * @param functionDefinition Function definition in which body checked.
   * @param from               Optional element to start from. If null, starts
   *                           from first element in function's body.
   * @return First element for the parallel region.
   */
  public static Xnode findParallelRegionStart(Xnode functionDefinition,
                                              Xnode from)
  {
    DirectiveGenerator dg = Context.get().getGenerator();
    if(dg.getDirectiveLanguage() == CompilerDirective.NONE
        || functionDefinition.opcode() != Xcode.F_FUNCTION_DEFINITION)
    {
      return null;
    }
    Xnode first = functionDefinition.body().firstChild();
    if(first == null) {
      return null;
    }
    if(from != null) { // Start from given element
      first = from;
    }
    if(dg.getSkippedStatementsInPreamble().isEmpty()) {
      return first;
    } else {
      while(first.nextSibling() != null
          && ((dg.getSkippedStatementsInPreamble().contains(first.opcode()))
          || isClawDirective(first))) {

        if(first.opcode() == Xcode.F_IF_STATEMENT) {
          Xnode then = first.matchDescendant(Xcode.THEN);
          if(then != null && then.hasBody()) {
            for(Xnode child : then.body().children()) {
              if(!dg.getSkippedStatementsInPreamble().
                  contains(child.opcode()))
              {
                return first;
              }
            }
          }
        } else if(first.hasBody()) {
          for(Xnode child : first.body().children()) {
            if(!dg.getSkippedStatementsInPreamble().
                contains(child.opcode()))
            {
              return first;
            }
          }
        }
        first = first.nextSibling();
      }
    }
    return first;
  }

  /**
   * Check if the node is a CLAW directive
   *
   * @param node Node to check.
   * @return True if the node is a CLAW directive. False otherwise.
   */
  private static boolean isClawDirective(Xnode node) {
    return node != null && node.opcode() == Xcode.F_PRAGMA_STATEMENT
        && node.value().toLowerCase().startsWith(TatsuConstant.DIRECTIVE_CLAW);
  }

  /**
   * Skip elements in epilogue and find the last element that will be included
   * in the parallel region.
   *
   * @param functionDefinition Function definition in which body checked.
   * @param from               Optional element to start from. If null, starts
   *                           from last element in function's body.
   * @return Last element for the parallel region.
   */
  public static Xnode findParallelRegionEnd(Xnode functionDefinition,
                                            Xnode from)
  {
    DirectiveGenerator dg = Context.get().getGenerator();

    if(dg.getDirectiveLanguage() == CompilerDirective.NONE
        || functionDefinition.opcode() != Xcode.F_FUNCTION_DEFINITION)
    {
      return null;
    }
    Xnode last = functionDefinition.body().lastChild();
    if(last == null) {
      return null;
    }
    if(from != null) { // Start from given element
      last = from;
      if(last.opcode() == Xcode.F_CONTAINS_STATEMENT) {
        last = last.prevSibling();
      }
    }
    if(dg.getSkippedStatementsInEpilogue().isEmpty()) {
      return last;
    } else {
      while(last.prevSibling() != null
          && dg.getSkippedStatementsInEpilogue().contains(last.opcode())) {
        if(last.hasBody() || last.opcode() == Xcode.F_IF_STATEMENT) {
          List<Xnode> children = (last.hasBody()) ? last.body().children()
              : last.matchDirectDescendant(Xcode.THEN).body().children();
          for(Xnode child : children) {
            if(!dg.getSkippedStatementsInEpilogue().contains(child.opcode())) {
              return last;
            }
          }
        }
        last = last.prevSibling();
      }
    }
    return last;
  }
}
