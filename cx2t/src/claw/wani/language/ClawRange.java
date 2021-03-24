/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.language;

import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import claw.wani.ClawConstant;

/**
 * Store the information from a range clause in a CLAW directives.
 *
 * @author clementval
 */
public class ClawRange
{

    private String _inductionVar = null;
    private String _lowerBound = null;
    private String _upperBound = null;
    private String _step = ClawConstant.DEFAULT_STEP_VALUE;

    /**
     * Constructs null initialized ClawRange object.
     */
    public ClawRange()
    {
    }

    /**
     * Constructs a new ClawRange object with all parameters initialization.
     *
     * @param inductionVar The induction variable value.
     * @param lowerBound   The lower bound value.
     * @param upperBound   The upper bound value.
     * @param step         The step value.
     */
    public ClawRange(String inductionVar, String lowerBound, String upperBound, String step)
    {
        setInductionVar(inductionVar);
        setLowerBound(lowerBound);
        setUpperBound(upperBound);
        setStep(step);
    }

    /**
     * Get the induction variable value.
     *
     * @return The induction variable value. Null if not defined.
     */
    public String getInductionVar()
    {
        return _inductionVar;
    }

    /**
     * Set the induction variable value.
     *
     * @param inductionVar The induction variable value.
     */
    public void setInductionVar(String inductionVar)
    {
        if (inductionVar != null)
        {
            this._inductionVar = inductionVar.trim();
        }
    }

    /**
     * Get the lower bound value.
     *
     * @return The lower bound value. Null if not defined.
     */
    public String getLowerBound()
    {
        return _lowerBound;
    }

    /**
     * Set the lower bound value.
     *
     * @param lowerBound The lower bound value.
     */
    public void setLowerBound(String lowerBound)
    {
        if (lowerBound != null)
        {
            this._lowerBound = lowerBound.trim();
        }
    }

    /**
     * Get the upper bound value.
     *
     * @return The upper bound value. Null if not set.
     */
    public String getUpperBound()
    {
        return _upperBound;
    }

    /**
     * Set the upper bound value.
     *
     * @param upperBound The upper bound value.
     */
    public void setUpperBound(String upperBound)
    {
        if (upperBound != null)
        {
            this._upperBound = upperBound.trim();
        }
    }

    /**
     * Get the step value.
     *
     * @return The step value. Null if not defined.
     */
    public String getStep()
    {
        return _step;
    }

    /**
     * Set the step value.
     *
     * @param step The step value.
     */
    public void setStep(String step)
    {
        if (step != null)
        {
            this._step = step.trim();
        }
    }

    /**
     * Compare a ClawRange with a do statement.
     *
     * @param doStmt The do statement to compare iteration range.
     * @return True if the iteration range share the same property.
     */
    public boolean compareToDoStmt(Xnode doStmt)
    {
        if (!Xnode.isOfCode(doStmt, Xcode.F_DO_STATEMENT))
        {
            return false;
        }

        Xnode inductionVar = doStmt.matchDirectDescendant(Xcode.VAR);
        Xnode indexRange = doStmt.matchDirectDescendant(Xcode.INDEX_RANGE);
        Xnode lower = indexRange.matchDirectDescendant(Xcode.LOWER_BOUND).child(0);
        Xnode upper = indexRange.matchDirectDescendant(Xcode.UPPER_BOUND).child(0);
        Xnode step = indexRange.matchDirectDescendant(Xcode.STEP).child(0);

        return !(inductionVar == null || _inductionVar == null || !inductionVar.value().equals(_inductionVar))
                && !(lower == null || _lowerBound == null || !lower.value().equals(_lowerBound))
                && !(upper == null || _upperBound == null || !upper.value().equals(_upperBound))
                && (step == null && _step == null || !(step == null || _step == null || !_step.equals(step.value())));
    }
}
