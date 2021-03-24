/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.abstraction;

import claw.tatsu.xcodeml.xnode.common.*;
import claw.tatsu.xcodeml.xnode.fortran.*;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

/**
 * Abstraction of functionCall XcodeML/F node.
 *
 * @author clementval
 */
public class FunctionCall extends Xnode
{

    private final String _name;
    private final Xnode _arguments;
    private final boolean _isTbp;

    /**
     * Basic ctor from Xnode.
     *
     * @param node Raw node.
     */
    public FunctionCall(Xnode node)
    {
        super(node == null ? null : node.element());

        _name = getFctNameFromFctCall();
        _isTbp = Xnode.isOfCode(firstChild(), Xcode.F_MEMBER_REF);
        _arguments = matchDirectDescendant(Xcode.ARGUMENTS);
    }

    /**
     * Get called function name.
     *
     * @return Function call called.
     */
    public String getFctName()
    {
        return _name;
    }

    /**
     * Get list of arguments of the function call.
     *
     * @return List of arguments nodes.
     */
    public List<Xnode> arguments()
    {
        return _arguments != null ? _arguments.children() : Collections.emptyList();
    }

    /**
     * Check whether the call is a call to a type-bound procedure.
     *
     * @return True if the call is a type-bound procedure call.
     */
    public boolean isTbpCall()
    {
        return _isTbp;
    }

    public void addArguments(Xnode newArg)
    {
        _arguments.append(newArg);
    }

    /**
     * Extract the name of the function in a function call.
     *
     * @return Function name if can be extracted. Null otherwise.
     */
    private String getFctNameFromFctCall()
    {
        if (Xnode.isOfCode(firstChild(), Xcode.F_MEMBER_REF))
        {
            return firstChild().getAttribute(Xattr.MEMBER);
        } else
        {
            return matchSeq(Xcode.NAME).value();
        }
    }

    /**
     * Check if the function call is an intrinsic call of the given type.
     *
     * @param intrinsic Intrinsic to be checked for.
     * @return True if the function call is an intrinsic call of the given
     *         intrinsic. False otherwise.
     */
    public boolean isIntrinsicCall(Xintrinsic intrinsic)
    {
        if (!getBooleanAttribute(Xattr.IS_INTRINSIC))
        {
            return false;
        }
        return getFctName().equalsIgnoreCase(intrinsic.toString());
    }

    /**
     * Adapt a SUM() call after change in the array argument. - Remove DIM parameter
     * if not necessary anymore.
     */
    public void adaptIntrinsicSumCall()
    {
        if (!isIntrinsicCall(Xintrinsic.SUM))
        {
            return;
        }
        Xnode namedValue = matchDescendant(Xcode.NAMED_VALUE);
        if (namedValue != null && namedValue.hasAttribute(Xattr.NAME)
                && namedValue.getAttribute(Xattr.NAME).equalsIgnoreCase("dim"))
        {
            long nbIndexRanges = matchAll(Xcode.INDEX_RANGE).size();

            long nbAssumedShape = matchAll(Xcode.INDEX_RANGE).stream()
                    .filter(x -> x.getBooleanAttribute(Xattr.IS_ASSUMED_SHAPE)).count();

            if (nbAssumedShape / nbIndexRanges <= 1)
            {
                namedValue.delete();
            }
        }
    }

    /**
     * Adapt a SPREAD() call after change in the array argument if source is a
     * constant.
     */
    public void adaptIntrinsicSpreadCall()
    {
        if (!isIntrinsicCall(Xintrinsic.SPREAD))
        {
            return;
        }
        Xnode arg0 = arguments().get(0);

        if (arg0.isConstant())
        {
            insertAfter(arg0.cloneNode());
            delete();
        } // TODO add warning if cannot be adapted
    }

    /**
     * Find specific argument in a function call.
     *
     * @param argName Name of the argument to be found.
     * @return The argument if found. Null otherwise.
     */
    public Optional<Xnode> findArg(String argName)
    {
        if (arguments().isEmpty())
        {
            return Optional.empty();
        }
        return arguments().stream().filter(x -> argName.equalsIgnoreCase(x.value())).findFirst();
    }

    /**
     * Gather string representation of arguments in a function call.
     *
     * @param xcodeml       Current XcodeML translation unit.
     * @param fctType       FfunctionType information for parameters.
     * @param fctTypeHolder XcodeML holding the function type information. Might be
     *                      identical to fctCall.
     * @param intent        Intent to use for gathering.
     * @param arrayOnly     If true, gather only arrays arguments.
     * @return List of arguments as their string representation.
     */
    public List<String> gatherArguments(XcodeProgram xcodeml, FfunctionType fctType, XcodeML fctTypeHolder,
            Intent intent, boolean arrayOnly, boolean nameOnly)
    {
        List<String> gatheredArguments = new ArrayList<>();
        // Retrieve function type to check intents and types of parameters
        List<Xnode> parameters = fctType.getParameters();
        for (int i = 0; i < parameters.size(); ++i)
        {
            // TODO handle optional arguments, named value args

            if (i >= arguments().size())
            { // avoid getting args out of list
                break;
            }

            Xnode parameter = parameters.get(i);
            Xnode arg = arguments().get(i);

            if (Xnode.isOfCode(arg, Xcode.NAMED_VALUE))
            {
                arg = arg.firstChild();
            }

            String nodeRepresentation = "";
            if (FortranType.isBuiltInType(arg.getType()) && !arrayOnly
                    && fctTypeHolder.getTypeTable().isBasicType(parameter))
            {
                FbasicType btParameter = xcodeml.getTypeTable().getBasicType(parameter);
                if (!intent.isCompatible(btParameter.getIntent()))
                {
                    continue;
                }
                nodeRepresentation = arg.constructRepresentation(false, nameOnly);
            } else if (fctTypeHolder.getTypeTable().isBasicType(parameter) && xcodeml.getTypeTable().isBasicType(arg))
            {
                FbasicType btParameter = fctTypeHolder.getTypeTable().getBasicType(parameter);
                FbasicType btArg = xcodeml.getTypeTable().getBasicType(arg);
                if ((arrayOnly && !btArg.isArray() && !btArg.isAllocatable())
                        || !intent.isCompatible(btParameter.getIntent()))
                {
                    continue;
                }
                nodeRepresentation = arg.constructRepresentation(false, nameOnly);
            }
            if (nodeRepresentation != null && !nodeRepresentation.isEmpty())
            {
                gatheredArguments.add(nodeRepresentation);
            }
        }
        return gatheredArguments;
    }

}
