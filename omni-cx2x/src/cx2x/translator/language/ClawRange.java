/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.language;

import cx2x.translator.common.Constant;
import cx2x.xcodeml.xelement.XloopIterationRange;

/**
 * Store the information from a range option in a CLAW directives
 *
 * @author clementval
 */
public class ClawRange {

  private String _inductionVar = null;
  private String _lowerBound = null;
  private String _upperBound = null;
  private String _step = Constant.DEFAULT_STEP_VALUE;

  /**
   * Constructs null initialized ClawRange object.
   */
  public ClawRange() {
  }

  /**
   * Constructs a new ClawRange object with all parameters initialization.
   * @param inductionVar  The induction variable value.
   * @param lowerBound    The lower bound value.
   * @param upperBound    The upper bound value.
   * @param step          The step value.
   */
  public ClawRange(String inductionVar, String lowerBound, String upperBound,
                   String step)
  {
    setInductionVar(inductionVar);
    setLowerBound(lowerBound);
    setUpperBound(upperBound);
    setStep(step);
  }

  /**
   * Get the induction variable value.
   * @return The induction variable value. Null if not defined.
   */
  public String getInductionVar() {
    return _inductionVar;
  }

  /**
   * Set the induction variable value.
   * @param inductionVar The induction varible value.
   */
  public void setInductionVar(String inductionVar) {
    if(inductionVar != null){
      this._inductionVar = inductionVar.trim();
    }
  }

  /**
   * Get the lower bound value.
   * @return The lower bound value. Null if not defined.
   */
  public String getLowerBound() {
    return _lowerBound;
  }

  /**
   * Set the lower bound value.
   * @param lowerBound The lower bound value.
   */
  public void setLowerBound(String lowerBound) {
    if(lowerBound != null) {
      this._lowerBound = lowerBound.trim();
    }
  }

  /**
   * Get the upper bound value.
   * @return The upper bound value. Null if not set.
   */
  public String getUpperBound() {
    return _upperBound;
  }

  /**
   * Set the upper bound value.
   * @param upperBound The upper bound value.
   */
  public void setUpperBound(String upperBound) {
    if(upperBound != null){
      this._upperBound = upperBound.trim();
    }
  }

  /**
   * Get the step value.
   * @return The step value. Null if not defined.
   */
  public String getStep() {
    return _step;
  }

  /**
   * Set the step value.
   * @param step The step value.
   */
  public void setStep(String step) {
    if(step != null) {
      this._step = step.trim();
    }
  }

  /**
   * Compare a ClawRange with a XloopIterationRange representation.
   * @param iterationRange A XloopIterationRange object.
   * @return True if the iteration range share the same property.
   */
  public boolean equals(XloopIterationRange iterationRange){
    if(iterationRange.getInductionVar() == null ||
        _inductionVar == null ||
        !iterationRange.getInductionVar().getValue().equals(_inductionVar))
    {
      return false;
    }

    if(iterationRange.getIndexRange() == null){
      return false;
    }

    if(iterationRange.getIndexRange().getLowerBound() == null ||
        _lowerBound == null ||
        !iterationRange.getIndexRange().getLowerBound().getValue()
            .equals(_lowerBound))
    {
      return false;
    }

    if(iterationRange.getIndexRange().getUpperBound() == null ||
        _upperBound == null ||
        !iterationRange.getIndexRange().getUpperBound().getValue()
            .equals(_upperBound))
    {
      return false;
    }


    if(iterationRange.getIndexRange().getStep() == null &&
        _step == null)
    {
      return true;
    }


    if(iterationRange.getIndexRange().getStep() == null ||
        _step == null || !_step.equals(iterationRange.getIndexRange().getStep().getValue()))
    {
      return false;
    }


    return true;
  }

}
