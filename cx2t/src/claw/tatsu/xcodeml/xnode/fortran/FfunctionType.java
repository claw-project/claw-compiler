/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode.fortran;

import claw.tatsu.xcodeml.xnode.Xname;
import claw.tatsu.xcodeml.xnode.common.Xattr;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.Xnode;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

/**
 * The FfunctionType represents the FfunctionType (3.4) element in XcodeML
 * intermediate representation.
 *
 * Elements: (params?) - Optional: - params Attributes: - Required: type (text),
 * return_type (text) - Optional: result_name (text), is_recursive (bool),
 * is_program (bool), is_internal (bool), is_elemental (bool), is_pure (bool),
 * bind (text), bind_name (text)
 *
 * @author clementval
 */
public class FfunctionType extends Xnode
{

    private final List<Xnode> _parameters;
    private final Xnode _params;

    /**
     * Basic ctor from Xnode.
     *
     * @param node Raw node.
     */
    public FfunctionType(Xnode node)
    {
        super(node == null ? null : node.element());
        _params = matchSeq(Xcode.PARAMS);
        _parameters = (_params != null) ? _params.matchAll(Xcode.NAME) : Collections.emptyList();
    }

    /**
     * Get the function result name.
     *
     * @return Result name value.
     */
    public String getResultName()
    {
        return getAttribute(Xattr.RESULT_NAME);
    }

    /**
     * Check whether function is recursive.
     *
     * @return True if the function is recursive. False otherwise.
     */
    public boolean isRecursive()
    {
        return getBooleanAttribute(Xattr.IS_RECURSIVE);
    }

    /**
     * Check whether function is internal.
     *
     * @return True if the function is internal. False otherwise.
     */
    public boolean isInternal()
    {
        return getBooleanAttribute(Xattr.IS_INTERNAL);
    }

    /**
     * Get the function return type.
     *
     * @return The function's return type as String.
     */
    public String getReturnType()
    {
        return getAttribute(Xattr.RETURN_TYPE);
    }

    /**
     * Check whether function is the program function.
     *
     * @return True if the function is the program function. False otherwise.
     */
    public boolean isProgram()
    {
        return getBooleanAttribute(Xattr.IS_PROGRAM);
    }

    /**
     * Check whether function has the pure attribute set to true.
     *
     * @return True if the function has the pure attribute set to true. False
     *         otherwise.
     */
    public boolean isPure()
    {
        return getBooleanAttribute(Xattr.IS_PURE);
    }

    /**
     * Check if the function type is a subroutine.
     *
     * @return True if the function type is a subroutine.
     */
    public boolean isSubroutine()
    {
        return getReturnType() == null || getReturnType().equalsIgnoreCase(Xname.TYPE_F_VOID);
    }

    /**
     * Check if the function type is a function.
     *
     * @return True if the function type is a subroutine.
     */
    public boolean isFunction()
    {
        return getReturnType() != null && !getReturnType().equalsIgnoreCase(Xname.TYPE_F_VOID);
    }

    /**
     * Check whether function has the elemental attribute set to true.
     *
     * @return True if the function has the elemental attribute set to true. False
     *         otherwise.
     */
    public boolean isElemental()
    {
        return getBooleanAttribute(Xattr.IS_ELEMENTAL);
    }

    /**
     * Get the list of all parameters name node.
     *
     * @return List of parameters Xnode objects.
     */
    public List<Xnode> getParameters()
    {
        return _parameters;
    }

    /**
     * Add a name element to the parameters list.
     *
     * @param name The name element to add.
     */
    public void addParameters(Xnode name)
    {
        if (name != null)
        {
            _parameters.add(name);
            _params.append(name);
        }
    }

    /**
     * Add a name element to the parameters list before the referenced element.
     *
     * @param ref  Referenced element. New element will be added before.
     * @param name The name element to add.
     */
    public void addParameters(Xnode ref, Xnode name)
    {
        if (name != null)
        {
            int index = _parameters.indexOf(ref);
            if (index >= 0)
            {
                _parameters.add(index, name);
                ref.insertBefore(name);
            } else
            {
                _parameters.add(name);
                _params.append(name);
            }
        }
    }

    /**
     * Get a list of string representing the function parameters.
     *
     * @return List of string.
     */
    public List<String> getParamsNames()
    {
        return _parameters.stream().map(Xnode::value).collect(Collectors.toList());
    }

    /**
     * A new object FfunctionType that is the clone of the current object.
     *
     * @return A new FfunctionType that is a clone of the current one.
     */
    @Override
    public FfunctionType cloneNode()
    {
        return new FfunctionType(super.cloneNode());
    }

    /**
     * Check if a parameter is part of the function definition.
     *
     * @param paramName Parameter's name.
     * @return True if the parameter is found in the function definition. False
     *         otherwise.
     */
    public boolean hasParam(String paramName)
    {
        return _parameters.stream().map(Xnode::value).anyMatch(paramName::equalsIgnoreCase);
    }

    @Override
    public String toString()
    {
        return String.format("%s type=\"%s\"", opcode(), getType());
    }
}
