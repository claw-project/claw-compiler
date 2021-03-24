/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.transformation.ll.loop;

import claw.shenron.transformation.Transformation;
import claw.shenron.translator.Translator;
import claw.tatsu.primitive.Body;
import claw.tatsu.xcodeml.xnode.XnodeUtil;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import claw.wani.language.ClawPragma;
import claw.wani.transformation.ClawTransformation;

/**
 * If extraction transformation
 *
 * @author clementval
 */
public class IfExtract extends ClawTransformation
{

    private Xnode _doStmt = null;
    private Xnode _ifStmt = null;

    public IfExtract(ClawPragma directive)
    {
        super(directive);
    }

    @Override
    public boolean analyze(XcodeProgram xcodeml, Translator translator)
    {
        _doStmt = _claw.getPragma().matchSibling(Xcode.F_DO_STATEMENT);
        if (_doStmt == null)
        {
            xcodeml.addError("Do statement missing after directive.", _claw.getPragma().lineNo());
            return false;
        }
        _ifStmt = _doStmt.body().matchDirectDescendant(Xcode.F_IF_STATEMENT);
        if (_ifStmt == null)
        {
            xcodeml.addError("If statement not found in the do statement.", _claw.getPragma().lineNo());
            return false;
        }
        int counterIfStmt = 0;
        for (Xnode n : _doStmt.body().children())
        {
            if (!n.is(Xcode.F_IF_STATEMENT) && !n.is(Xcode.F_PRAGMA_STATEMENT))
            {
                xcodeml.addError("If statement is not purely nested in the do statement", _claw.getPragma().lineNo());
                return false;
            } else if (n.is(Xcode.F_IF_STATEMENT))
            {
                ++counterIfStmt;
            }
        }
        if (counterIfStmt > 1)
        {
            xcodeml.addError("Only one if statement can be present for extraction.", _claw.getPragma().lineNo());
            return false;
        }
        return true;
    }

    @Override
    public boolean canBeTransformedWith(XcodeProgram xcodeml, Transformation other)
    {
        return false;
    }

    @Override
    public void transform(XcodeProgram xcodeml, Translator translator, Transformation other) throws Exception
    {
        // Copy the body of the if statement inside the body of the do statement
        Xnode thenBlock = _ifStmt.matchDirectDescendant(Xcode.THEN);
        Xnode thenDoStmt = _doStmt.cloneNode();
        Body.append(thenDoStmt.body(), thenBlock.body());

        // Copy the if statement and clean its body
        Xnode newIfStmt = _ifStmt.cloneNode();
        Xnode newThen = newIfStmt.matchDirectDescendant(Xcode.THEN);
        for (Xnode n : newThen.body().children())
        {
            n.delete();
        }

        // Add the new if statement after the do statement
        _doStmt.insertAfter(newIfStmt);

        // Insert the do statement in the new if-then statement
        newThen.body().insert(thenDoStmt, false);

        Xnode elseBlock = _ifStmt.matchDirectDescendant(Xcode.ELSE);
        if (elseBlock != null)
        {
            Xnode elseDoStmt = _doStmt.cloneNode();
            Body.append(elseDoStmt.body(), elseBlock.body());
            Xnode newElse = newIfStmt.matchDirectDescendant(Xcode.ELSE);
            for (Xnode n : newElse.body().children())
            {
                n.delete();
            }
            newElse.body().insert(elseDoStmt, false);
            Xnode duplicateIf = elseDoStmt.body().matchDirectDescendant(Xcode.F_IF_STATEMENT);
            XnodeUtil.safeDelete(duplicateIf);
        }

        // Delete the old statements and pragma
        Xnode duplicateIf = thenDoStmt.body().matchDirectDescendant(Xcode.F_IF_STATEMENT);
        duplicateIf.delete();
        XnodeUtil.safeDelete(_ifStmt);
        XnodeUtil.safeDelete(_doStmt);
        removePragma();
    }
}
