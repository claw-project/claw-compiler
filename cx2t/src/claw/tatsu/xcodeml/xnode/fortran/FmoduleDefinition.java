/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode.fortran;

import claw.tatsu.xcodeml.xnode.common.*;

import java.util.Optional;

/**
 * The FmoduleDefinition represents the FmoduleDefinition (5.7) element in
 * XcodeML intermediate representation.
 *
 * Elements: (symbols?, declarations?, FcontainsStatement?) - Optional: -
 * symbols (XsymbolTable) - declarations (XdeclTable) - FcontainsStatement
 * (Xnode)
 *
 * Attributes: - Required: name (text)
 *
 * Can have lineno and file attributes
 *
 * @author clementval
 */
public class FmoduleDefinition extends Xnode
{

    private final String _name;
    private final XsymbolTable _symbols;
    private final XdeclTable _declarations;

    /**
     * Element standard ctor. Pass the base element to the base class and read inner
     * information (elements and attributes).
     *
     * @param node Raw node.
     */
    public FmoduleDefinition(Xnode node)
    {
        super(node == null ? null : node.element());
        _name = getAttribute(Xattr.NAME);
        Xnode symbols = matchSeq(Xcode.SYMBOLS);
        _symbols = (symbols != null) ? new XsymbolTable(symbols) : null;
        Xnode declarations = matchSeq(Xcode.DECLARATIONS);
        _declarations = (declarations != null) ? new XdeclTable(declarations) : null;
    }

    /**
     * Get module name.
     *
     * @return Module name.
     */
    public String getName()
    {
        return _name;
    }

    /**
     * Get the module's symbols table.
     *
     * @return A XsymbolTable object containing the module's symbols.
     */
    public XsymbolTable getSymbolTable()
    {
        return _symbols;
    }

    /**
     * Get the module's declarations table.
     *
     * @return A XdeclTable object containing the module's declarations.
     */
    public XdeclTable getDeclarationTable()
    {
        return _declarations;
    }

    /**
     * Retrieve a function definition in a module definition based on its name.
     *
     * @param name Name of the function to be found.
     * @return A function definition element if found. Null otherwise.
     */
    public Optional<FfunctionDefinition> getFunctionDefinition(String name)
    {
        if (name == null || name.isEmpty())
        {
            return Optional.empty();
        }
        return matchAll(Xcode.F_FUNCTION_DEFINITION).stream().map(FfunctionDefinition::new)
                .filter(x -> x.getName().equalsIgnoreCase(name)).findFirst();
    }

    @Override
    public FmoduleDefinition cloneNode()
    {
        return new FmoduleDefinition(super.cloneNode());
    }
}
