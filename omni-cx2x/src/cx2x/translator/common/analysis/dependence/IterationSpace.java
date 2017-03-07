/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.translator.common.analysis.dependence;

import cx2x.translator.language.base.ClawLanguage;
import cx2x.translator.transformation.loop.LoopFusion;
import cx2x.xcodeml.transformation.DependentTransformationGroup;
import cx2x.xcodeml.transformation.Transformer;
import cx2x.xcodeml.xnode.Xcode;
import cx2x.xcodeml.xnode.XcodeProgram;
import cx2x.xcodeml.xnode.Xnode;

import java.util.ArrayList;
import java.util.List;

/**
 * Represents the different do statements part of the iteration space in a
 * subroutine.
 *
 * @author clementval
 */
public class IterationSpace {

  private final List<List<DependenceAnalysis>> _levels;

  /**
   * Constructs an iteration space object and populate it with the given do
   * statements.
   *
   * @param doStatements List of do statements part of the iteration space.
   */
  public IterationSpace(List<Xnode> doStatements) throws Exception {
    _levels = new ArrayList<>();
    load(doStatements);
  }

  /**
   * Operation needed after a fusion. Reload and re-analyze all the do
   * statements and their dependencies.
   *
   * @param doStatements New list of do statements.
   * @throws Exception If a node is not a do statement.
   */
  public void reload(List<Xnode> doStatements) throws Exception {
    _levels.clear();
    load(doStatements);
  }

  /**
   * Create and categorize do statements based on their nesting level.
   *
   * @param doStatements List of do statements to categorize.
   * @throws Exception If a node is not a do statement.
   */
  private void load(List<Xnode> doStatements) throws Exception {
    _levels.add(0, new ArrayList<DependenceAnalysis>()); // Init the level 0
    DependenceAnalysis baseLoopLevel0 = null;
    for(Xnode doStmt : doStatements) {
      if(doStmt.opcode() != Xcode.FDOSTATEMENT) {
        throw new Exception("Only do statements node can be part of an " +
            "iteration space");
      }

      if(baseLoopLevel0 == null) {
        baseLoopLevel0 = new DependenceAnalysis(doStmt);
        _levels.get(0).add(baseLoopLevel0);
      } else {
        int crtLevel = 0;
        int insertedLevel = -1;

        while(_levels.size() > crtLevel) {
          for(DependenceAnalysis dep : _levels.get(crtLevel)) {
            if(doStmt.isNestedIn(dep.getDoStmt())) {
              insertedLevel = crtLevel + 1;
              break;
            }
          }
          ++crtLevel;
        }

        if(insertedLevel != -1) {
          addAtLevel(insertedLevel, new DependenceAnalysis(doStmt));
        } else {
          addAtLevel(0, new DependenceAnalysis(doStmt));
        }
      }
    }
  }

  /**
   * Add dependence analysis object at the correct level in the iteration space.
   * Create the level in case it is not created yet.
   *
   * @param level Level at which the dependence analysis object should be
   *              inserted.
   * @param dep   Dependence analysis object to insert.
   */
  private void addAtLevel(int level, DependenceAnalysis dep) {
    if(_levels.size() <= level) {
      _levels.add(level, new ArrayList<DependenceAnalysis>());
    }
    _levels.get(level).add(dep);
  }

  /**
   * Get the number of levels.
   *
   * @return Number of levels.
   */
  public int getNbLevel() {
    return _levels.size();
  }

  /**
   * Get a specific level.
   *
   * @param level Index of the level to be returned.
   * @return List of all the dependence analysis object in the requested level.
   */
  public List<DependenceAnalysis> getLevel(int level) {
    return (_levels.size() > level) ? _levels.get(level) : null;
  }

  /**
   * Print some debugging information about the iteration space.
   *
   * @param inner If true, DependenceAnalysis information are printed too.
   */
  public void printDebug(boolean inner) {
    System.out.println("Iteration space:");
    for(int i = 0; i < _levels.size(); ++i) {
      List<DependenceAnalysis> loopsAtLevel = _levels.get(i);
      System.out.println("Level: " + i + " Number of loops: " +
          loopsAtLevel.size());
      if(inner) {
        for(DependenceAnalysis dep : loopsAtLevel) {
          System.out.println(dep.getInfoMsg());
        }
      }
    }
  }

  /**
   * Analyze the dependence information at each level and try to merge
   * independent do statements.
   *
   * @param xcodeml     Current XcodeML/F program unit.
   * @param transformer Current transformer.
   * @param master      ClawLanguage that triggered this transformation.
   * @throws Exception If the fusion fails.
   */
  public void tryFusion(XcodeProgram xcodeml, Transformer transformer,
                        ClawLanguage master)
      throws Exception
  {
    for(int i = _levels.size() - 1; i >= 0; --i) {
      List<DependenceAnalysis> loopsAtLevel = getLevel(i);
      DependentTransformationGroup fusions =
          new DependentTransformationGroup("parallelize-fusion");
      for(DependenceAnalysis dep : loopsAtLevel) {
        if(dep.isIndependent()) {
          ClawLanguage l = ClawLanguage.createLoopFusionLanguage(master);
          LoopFusion fusion = new LoopFusion(dep.getDoStmt(), l);
          fusions.add(fusion);
        }
      }
      fusions.applyTranslations(xcodeml, transformer);
    }
  }

}
