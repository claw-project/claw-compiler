/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.common.analysis.dependence;

import cx2x.xcodeml.xnode.Xcode;
import cx2x.xcodeml.xnode.Xnode;

import java.util.List;

/**
 * This class hold methods to help analysis of loop dependencies on XcodeML/F
 * intermediate representation.
 *
 * @author clementval
 */
public class DependenceAnalysis {

  private final Xnode _mainLoop;
  private DependenceDirection _directionVector;
  private Integer _distanceVector;
  private String _inductionVariable;

  public DependenceAnalysis(Xnode loop) {
    _mainLoop = loop;
  }

  public void analyze() throws Exception {
    if(_mainLoop == null || _mainLoop.opcode() != Xcode.FDOSTATEMENT) {
      throw new Exception("Analysis only on FdoStatement node");
    }

    Xnode inductionVarNode = _mainLoop.matchDirectDescendant(Xcode.VAR);
    _inductionVariable = inductionVarNode.value();

    Xnode body = _mainLoop.matchDescendant(Xcode.BODY);
    List<Xnode> arrayRefs = body.matchAll(Xcode.FARRAYREF);

    _distanceVector = 0;
    _directionVector = DependenceDirection.NONE;
    for(Xnode arrayRef : arrayRefs) {
      List<Xnode> arrayIndexes = arrayRef.matchAll(Xcode.ARRAYINDEX);
      for(Xnode arrayIndex : arrayIndexes) {
        if(arrayIndex.firstChild().opcode() == Xcode.MINUSEXPR
            || arrayIndex.firstChild().opcode() == Xcode.PLUSEXPR)
        {
          Xnode expr = arrayIndex.firstChild();
          Xnode var = expr.firstChild();
          Xnode intConst = expr.lastChild();
          if(var.value().endsWith(_inductionVariable)) {
            _distanceVector = Integer.parseInt(intConst.value());
            switch(arrayIndex.firstChild().opcode()) {
              case MINUSEXPR:
                _directionVector = DependenceDirection.BACKWARD;
                break;
              case PLUSEXPR:
                _directionVector = DependenceDirection.FORWARD;
                break;
            }
          }
        }
      }
    }

  }

  public String getInductionVariable() {
    return _inductionVariable;
  }

  public int getDistanceVector() {
    return _distanceVector;
  }

  public DependenceDirection getDirectionVector() {
    return _directionVector;
  }

  public boolean isIndependent() {
    return _directionVector == DependenceDirection.NONE && _distanceVector == 0;
  }

  public Xnode getDoStmt(){
    return _mainLoop;
  }

  public String getInfoMsg(){


    String msg = isIndependent() ? ", Loop is parallelizable over "
        : (_directionVector == DependenceDirection.BACKWARD)
        ? ", Loop carried backward dependence over "
        : ", Loop carried forward dependence over ";
    return _mainLoop.lineNo() + msg + _inductionVariable;
  }



}
