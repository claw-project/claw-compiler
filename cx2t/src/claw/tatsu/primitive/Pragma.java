/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.primitive;

import claw.tatsu.TatsuConstant;
import claw.tatsu.common.CompilerDirective;
import claw.tatsu.common.Context;
import claw.tatsu.common.Utility;
import claw.tatsu.directive.generator.OpenAcc;
import claw.tatsu.directive.generator.OpenMp;
import claw.tatsu.xcodeml.exception.IllegalTransformationException;
import claw.tatsu.xcodeml.xnode.XnodeUtil;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import claw.tatsu.xcodeml.xnode.fortran.FfunctionDefinition;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Primitive transformation and test applied to FpragmaStatement. This included:
 * - Extract directive prefix of a FpragmaStatement.
 * - Split long pragma into continued pragma.
 * - Remove comment from a pragma
 * - Find pragma into previous siblings.
 *
 * @author clementval
 */
public final class Pragma {

  private static final String COMMENT_PREFIX = "!";

  // Avoid instantiation of this class
  private Pragma() {
  }

  /**
   * Return the pragma prefix.
   *
   * @param pragma The pragma node.
   * @return Directive prefix if any. Empty string otherwise.
   */
  public static String getPrefix(Xnode pragma) {
    if(pragma == null || pragma.opcode() != Xcode.F_PRAGMA_STATEMENT
        || pragma.value().isEmpty())
    {
      return "";
    }

    if(!pragma.value().contains(" ")) {
      return pragma.value();
    }

    return pragma.value().substring(0, pragma.value().indexOf(' '));
  }

  /**
   * Split a long pragma into chunk depending on the maxColumns specified.
   * The split is done on spaces or commas.
   *
   * @param fullPragma   The original full pragma value.
   * @param maxColumns   Maximum number of columns. This length take into
   *                     account the added prefix and continuation symbol.
   * @param pragmaPrefix Prefix used by the pragma.
   * @return A list of chunks from the original pragma.
   */
  public static List<String> split(String fullPragma, int maxColumns,
                                   String pragmaPrefix)
  {
    List<String> splittedPragmas = new ArrayList<>();
    fullPragma = fullPragma.toLowerCase();
    if(fullPragma.length() > maxColumns) {
      fullPragma = Pragma.dropEndingComment(fullPragma);
      int addLength = pragmaPrefix.length() + 5; // "!$<prefix> PRAGMA &"
      while(fullPragma.length() > (maxColumns - addLength)) {
        int splitIndex =
            fullPragma.substring(0,
                maxColumns - addLength).lastIndexOf(' ');
        // Cannot cut as it should. Take first possible cutting point.
        if(splitIndex == -1) {
          splitIndex = fullPragma.substring(0,
              maxColumns - addLength).lastIndexOf(',');
          if(splitIndex == -1) {
            splitIndex =
                (fullPragma.contains(" ")) ? fullPragma.lastIndexOf(' ') :
                    (fullPragma.contains(",")) ? fullPragma.lastIndexOf(',') :
                        fullPragma.length();
          }
        }
        String splittedPragma = fullPragma.substring(0, splitIndex);
        fullPragma = fullPragma.substring(splitIndex).trim();
        splittedPragmas.add(splittedPragma);
      }
    }
    if(fullPragma.length() > 0) {
      splittedPragmas.add(fullPragma);
    }
    return splittedPragmas;
  }

  /**
   * Split the line by its length and add continuation symbols.
   *
   * @param pragma  Pragma statement to be splitted.
   * @param xcodeml The XcodeML on which the transformations are
   *                applied.
   * @param prefix  Prefix of the directive.
   * @throws IllegalTransformationException If the given element is not a
   *                                        FpragmaStatement.
   */
  public static void splitByLength(Xnode pragma, XcodeProgram xcodeml,
                                   String prefix)
      throws IllegalTransformationException
  {
    if(pragma == null || pragma.opcode() != Xcode.F_PRAGMA_STATEMENT) {
      throw new
          IllegalTransformationException(TatsuConstant.ERROR_INCOMPATIBLE);
    }
    String allPragma = pragma.value().toLowerCase();
    if(allPragma.length() > Context.get().getMaxColumns()) {
      allPragma = Pragma.dropEndingComment(allPragma);
      Xnode newlyInserted = pragma;
      List<String> splittedPragmas = Pragma.split(allPragma,
          Context.get().getMaxColumns(), prefix);

      for(int i = 0; i < splittedPragmas.size(); ++i) {
        // Create pragma with continuation symbol unless for the last item.
        newlyInserted = createAndInsertPragma(xcodeml, newlyInserted,
            pragma.filename(), pragma.lineNo(),
            splittedPragmas.get(i), i != splittedPragmas.size() - 1);
      }
      // Delete original not splitted pragma.
      pragma.delete();
    }
  }

  /**
   * Split the line by its previous continuation mark.
   *
   * @param pragma  Pragma node.
   * @param prefix  Pragma prefix.
   * @param xcodeml Current XcodeML translation unit.
   * @throws IllegalTransformationException If the given element is not a
   *                                        FpragmaStatement.
   */
  public static void splitByCont(Xnode pragma, String prefix,
                                 XcodeProgram xcodeml)
      throws IllegalTransformationException
  {
    if(pragma == null || pragma.opcode() != Xcode.F_PRAGMA_STATEMENT) {
      throw new
          IllegalTransformationException(TatsuConstant.ERROR_INCOMPATIBLE);
    }
    String allPragma = pragma.value();
    int lineIndex = pragma.lineNo();
    String splitter = prefix.trim();
    if(allPragma.contains(prefix.trim() +
        TatsuConstant.CONTINUATION_LINE_SYMBOL))
    {
      splitter = prefix.trim() + TatsuConstant.CONTINUATION_LINE_SYMBOL;
    }

    Xnode newlyInserted = pragma;
    String[] lines = allPragma.split(splitter);
    for(int i = 0; i < lines.length - 1; ++i) {
      if(!lines[i].isEmpty()) {
        newlyInserted = createAndInsertPragma(xcodeml, newlyInserted,
            pragma.filename(), lineIndex, lines[i], true);
        ++lineIndex;
      }
    }
    createAndInsertPragma(xcodeml, newlyInserted, pragma.filename(), lineIndex,
        lines[lines.length - 1], false);
    pragma.delete();
  }

  /**
   * Remove any trailing comment from a pragma string.
   *
   * @param pragma Original pragma string.
   * @return Pragma string without the trailing comment if any.
   */
  public static String dropEndingComment(String pragma) {
    if(pragma != null && pragma.contains(COMMENT_PREFIX)) {
      return pragma.substring(0, pragma.indexOf(COMMENT_PREFIX)).trim();
    }
    return pragma;
  }

  /**
   * Check if the pragma was already continued. Can happen when using the !$claw
   * primitive directive
   *
   * @param pragma Pragma statement to be checked.
   * @return True if the pragma was previously continued.
   */
  public static boolean fromClawPrimitive(Xnode pragma) {
    if(pragma == null || pragma.opcode() != Xcode.F_PRAGMA_STATEMENT) {
      return false;
    }
    String allPragma = pragma.value().toLowerCase();

    String prefixCont = Context.get().getGenerator().getPrefixCont();
    String prefix = Context.get().getGenerator().getPrefix();

    return allPragma.contains(prefixCont) ||
        Utility.countOccurrences(allPragma, prefix + " ") > 1;
  }

  public static CompilerDirective getCompilerDirective(Xnode pragma) {
    if(pragma == null || pragma.opcode() != Xcode.F_PRAGMA_STATEMENT) {
      return CompilerDirective.NONE;
    }
    if(pragma.value().toLowerCase().contains(OpenAcc.OPENACC_PREFIX)) {
      return CompilerDirective.OPENACC;
    } else if(pragma.value().toLowerCase().contains(OpenMp.OPENMP_PREFIX)) {
      return CompilerDirective.OPENMP;
    } else if(pragma.value().toLowerCase().
        contains(TatsuConstant.CLAW_PREFIX))
    {
      return CompilerDirective.CLAW;
    }
    return CompilerDirective.NONE;
  }

  /**
   * Create a new pragma node and insert it after the hook.
   *
   * @param xcodeml   Current XcodeML file unit.
   * @param hook      Hook node. New node will be inserted after this one.
   * @param filename  Filename set to the enhanced info.
   * @param lineNo    Line index specify the offset of the line number for the
   *                  new node from the original pragma node.
   * @param value     Value of the pragma node.
   * @param continued If true, continuation symbol is added at the end of the
   *                  line.
   * @return The newly created node to be able to insert after it.
   */
  private static Xnode createAndInsertPragma(XcodeProgram xcodeml, Xnode hook,
                                             String filename,
                                             int lineNo, String value,
                                             boolean continued)
  {
    if(value == null || value.isEmpty()) {
      return null;
    }

    Xnode p = xcodeml.createNode(Xcode.F_PRAGMA_STATEMENT);
    if(filename != null && !filename.isEmpty()) {
      p.setFilename(filename);
    }
    if(lineNo > 0) {
      p.setLine(lineNo);
    }

    String prefix = Context.get().getGenerator().getPrefix();
    value = value.trim().toLowerCase();
    boolean notStartWithPrefix = !value.startsWith(prefix);

    if(continued) {
      if(notStartWithPrefix) {
        p.setValue(prefix + " " + value + " " +
            TatsuConstant.CONTINUATION_LINE_SYMBOL);
      } else {
        p.setValue(value + " " + TatsuConstant.CONTINUATION_LINE_SYMBOL);
      }
    } else {
      if(notStartWithPrefix) {
        p.setValue(prefix + " " + value);
      } else {
        p.setValue(value);
      }
    }
    hook.insertAfter(p);
    return p;
  }

  /**
   * Find a pragma element in the previous nodes containing a given keyword.
   *
   * @param from    Element to start from.
   * @param keyword Keyword to be found in the pragma.
   * @return The pragma if found. Null otherwise.
   */
  public static Xnode findPrevious(Xnode from, String keyword) {
    if(from == null || from.element() == null) {
      return null;
    }
    Xnode prev = from.prevSibling();
    Xnode parent = from;
    do {
      while(prev != null) {
        if(prev.opcode() == Xcode.F_PRAGMA_STATEMENT
            && prev.value().toLowerCase().contains(keyword.toLowerCase()))
        {
          return prev;
        }
        prev = prev.prevSibling();
      }
      parent = parent.ancestor();
      prev = parent;
    } while(parent != null);
    return null;
  }

  /**
   * If the first pragma statement is located as the first statement of the
   * execution block, the OMNI Compiler front-end places it with the declaration
   * block. If this doesn't make sense for a specific pragma, this method will
   * move it to the execution block.
   *
   * @param pragma The pragma to be moved.
   */
  public static void moveInExecution(Xnode pragma) {
    if(pragma == null || pragma.opcode() != Xcode.F_PRAGMA_STATEMENT) {
      return;
    }

    if(pragma.ancestor().opcode() == Xcode.DECLARATIONS) {
      FfunctionDefinition fdef = pragma.findParentFunction();
      if(fdef != null) {
        if(fdef.body().firstChild() != null &&
            fdef.body().firstChild().opcode() == Xcode.F_PRAGMA_STATEMENT)
        {
          Xnode hook = null;
          Xnode crtNode = fdef.body().firstChild();
          while(crtNode != null && crtNode.opcode() == Xcode.F_PRAGMA_STATEMENT
              && pragma.lineNo() > crtNode.lineNo()) {
            hook = crtNode;
            crtNode = crtNode.nextSibling();
          }
          if(hook != null) {
            hook.insertAfter(pragma);
          } else {
            fdef.body().append(pragma);
          }
        } else {
          fdef.body().insert(pragma);
        }
      }
    }
  }

  /**
   * Delete all pragmas statement between the given start/end nodes.
   *
   * @param directive Compiler directive to delete.
   *                  NONE means both OpenACC/OpenMP.
   * @param start     Start node.
   * @param end       End node.
   */
  public static void remove(CompilerDirective directive, Xnode start, Xnode end)
  {
    if(start == null || end == null) {
      return;
    }

    List<Xnode> pragmas = XnodeUtil.getNodes(start, end,
        Collections.singletonList(Xcode.F_PRAGMA_STATEMENT));

    for(Xnode p : pragmas) {
      CompilerDirective pDir = getCompilerDirective(p);
      if(directive == pDir || (directive == CompilerDirective.NONE
          && (pDir == CompilerDirective.OPENACC
          || pDir == CompilerDirective.OPENMP)))
      {
        p.delete();
      }
    }
  }
}
