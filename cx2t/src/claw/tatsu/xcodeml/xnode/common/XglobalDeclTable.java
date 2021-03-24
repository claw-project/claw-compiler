/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode.common;

import claw.tatsu.xcodeml.xnode.fortran.FfunctionDefinition;
import claw.tatsu.xcodeml.xnode.fortran.FmoduleDefinition;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * The XglobalDeclTable represents the typeTable (5.1) element in XcodeML
 * intermediate representation.
 *
 * Elements: (FfunctionDefinition | FmoduleDefinition)* - Optional: -
 * FfunctionDefinition (FfunctionDefinition) - FmoduleDefinition
 * (FmoduleDefinition)
 *
 * @author clementval
 */

public class XglobalDeclTable extends Xnode
{

    /*
     * Hashtable containing the global declaration elements. Key is the name of the
     * function or the module.
     */
    private final Map<String, Xnode> _table;

    /**
     * Element standard ctor. Pass the base element to the base class and read inner
     * information (elements and attributes).
     *
     * @param node Raw node.
     */
    public XglobalDeclTable(Xnode node)
    {
        super(node == null ? null : node.element());
        _table = new HashMap<>();
        readTable();
    }

    /**
     * Read the declaration table
     */
    private void readTable()
    {
        Xnode crt = firstChild();
        while (crt != null)
        {
            if (crt.is(Xcode.F_FUNCTION_DEFINITION))
            {
                FfunctionDefinition fctDef = new FfunctionDefinition(crt);
                _table.put(fctDef.getName(), fctDef);
            } else if (crt.is(Xcode.F_MODULE_DEFINITION))
            {
                FmoduleDefinition moduleDef = new FmoduleDefinition(crt);
                _table.put(moduleDef.getName(), moduleDef);
            }
            crt = crt.nextSibling();
        }
    }

    /**
     * Get the number of declarations in the table.
     *
     * @return The number of declarations in the table.
     */
    public int size()
    {
        return _table.size();
    }

    /**
     * Get a specific module declaration based on its name.
     *
     * @param name The name of the module to be returned.
     * @return A FmoduleDefinition object if key is found. Null otherwise.
     */
    public FmoduleDefinition getModuleDefinition(String name)
    {
        if (_table.containsKey(name))
        {
            Xnode el = _table.get(name);
            if (el instanceof FmoduleDefinition)
            {
                return (FmoduleDefinition) el;
            }
        }
        return null;
    }

    /**
     * Retrieve function definition in the current declaration table or recursively
     * in the modules' declaration tables.
     *
     * @param fctName Function's name.
     * @return The function definition if found. Null otherwise.
     */
    public FfunctionDefinition getFunctionDefinition(String fctName)
    {
        if (_table.containsKey(fctName))
        {
            Xnode el = _table.get(fctName);
            if (el instanceof FfunctionDefinition)
            {
                return (FfunctionDefinition) el;
            }
        } else
        {
            List<FmoduleDefinition> mods = _table.values().stream().filter(FmoduleDefinition.class::isInstance)
                    .map(FmoduleDefinition.class::cast).collect(Collectors.toList());

            for (FmoduleDefinition mod : mods)
            {
                Optional<FfunctionDefinition> fctDef = mod.getFunctionDefinition(fctName);
                if (fctDef.isPresent())
                {
                    return fctDef.get();
                }
            }
        }
        return null;
    }

    /**
     * Check if there is a module definition for the given name.
     *
     * @param name Name to be searched.
     * @return True if there is a definition. False otherwise.
     */
    public boolean hasModuleDefinition(String name)
    {
        return _table.containsKey(name) && (_table.get(name) instanceof FmoduleDefinition);
    }

    /**
     * Check if there is a function definition for the given name.
     *
     * @param name Name to be searched.
     * @return True if there is a definition. False otherwise.
     */
    public boolean hasFunctionDefinition(String name)
    {
        return _table.containsKey(name) && (_table.get(name) instanceof FfunctionDefinition);
    }

    /**
     * Check if there is a definition for the given name.
     *
     * @param name Name to be searched.
     * @return True if there is a definition. False otherwise.
     */
    public boolean hasDefinition(String name)
    {
        return _table.containsKey(name);
    }

    @Override
    public XglobalDeclTable cloneNode()
    {
        return new XglobalDeclTable(super.cloneNode());
    }
}
