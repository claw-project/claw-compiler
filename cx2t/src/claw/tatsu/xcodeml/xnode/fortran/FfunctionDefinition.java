/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode.fortran;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import claw.tatsu.common.Context;
import claw.tatsu.primitive.Body;
import claw.tatsu.primitive.Loop;
import claw.tatsu.primitive.Xmod;
import claw.tatsu.xcodeml.abstraction.AssignStatement;
import claw.tatsu.xcodeml.exception.IllegalTransformationException;
import claw.tatsu.xcodeml.xnode.common.Xattr;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.tatsu.xcodeml.xnode.common.XdeclTable;
import claw.tatsu.xcodeml.xnode.common.Xid;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import claw.tatsu.xcodeml.xnode.common.XsymbolTable;

/**
 * The FfunctionDefinition represents the FfunctionDefinition (5.3) element in
 * XcodeML intermediate representation.
 *
 * Elements: (name, symbols?, params?, declarations?, body) - Required: - name
 * (text) - body - Optional: - symbols (XsymbolTable) - params - declarations
 * (XdeclTable)
 *
 * Can have lineno and file attributes
 *
 * @author clementval
 */

public class FfunctionDefinition extends Xnode
{

    // Elements
    private final XsymbolTable _symbolTable;
    private final Xnode _params;
    private final XdeclTable _declTable;
    private final Xnode _name;

    /**
     * Constructs new FfunctionDefinition instance.
     *
     * @param node Raw node.
     */
    public FfunctionDefinition(Xnode node)
    {
        super(node == null ? null : node.element());
        Xnode symbols = matchSeq(Xcode.SYMBOLS);
        assert (symbols != null);
        _symbolTable = new XsymbolTable(symbols);
        Xnode declarations = matchSeq(Xcode.DECLARATIONS);
        assert (declarations != null);
        _declTable = new XdeclTable(declarations);
        _params = matchSeq(Xcode.PARAMS);
        _name = matchSeq(Xcode.NAME);
        assert (_name != null);
        assert (body() != null);
    }

    /**
     * Get the function's symbols table.
     *
     * @return A XsymbolTable object containing the function's symbols.
     */
    public XsymbolTable getSymbolTable()
    {
        return _symbolTable;
    }

    /**
     * Get the function's declarations table.
     *
     * @return A XdeclTable object containing the function's declarations.
     */
    public XdeclTable getDeclarationTable()
    {
        return _declTable;
    }

    /**
     * Get the function name.
     *
     * @return Name of the function as an Xname object.
     */
    public String getName()
    {
        return _name.value();
    }

    /**
     * Get the name node.
     *
     * @return name node of the FfunctionDefinition.
     */
    public Xnode name()
    {
        return _name;
    }

    /**
     * Get the parameters list.
     *
     * @return Parameters list.
     */
    public Xnode getParams()
    {
        return _params;
    }

    /**
     * Find module containing the function and read its .xmod file.
     *
     * @return FortranModule object if the module has been found and read. Null
     *         otherwise.
     */
    public FortranModule findContainingXmod(Context context)
    {
        FmoduleDefinition mod = findParentModule();
        if (mod == null)
        {
            return null;
        }
        return Xmod.find(context, mod.getAttribute(Xattr.NAME));
    }

    /**
     * Check if function body is empty.
     *
     * @return True if function body is empty. False otherwise (no body or not
     *         empty).
     */
    public boolean hasEmptyBody()
    {
        if (body() == null)
        {
            return false;
        }

        try
        {
            return Body.isEmpty(body());
        } catch (IllegalTransformationException itex)
        {
            return false;
        }
    }

    /**
     * Create an identical copy of the current function definition.
     *
     * @return A new FfunctionDefinition object that is the clone of this function
     *         definition.
     */
    @Override
    public FfunctionDefinition cloneNode()
    {
        return new FfunctionDefinition(super.cloneNode());
    }

    /**
     * Gather all assignment statements in the function definition.
     *
     * @return List of assignment statement in AST order. Empty list if function
     *         definition is null or no statement found.
     */
    public List<AssignStatement> gatherAssignStatements()
    {
        if (body() == null)
        {
            return Collections.emptyList();
        }
        return body().matchAll(Xcode.F_ASSIGN_STATEMENT).stream().map(Xnode::element).map(AssignStatement::new)
                .collect(Collectors.toList());
    }

    /**
     * Gather all assignment statements in the function definition.
     *
     * @return List of assignment statement in AST order. Empty list if function
     *         definition is null or no statement found.
     */
    public List<AssignStatement> gatherAssignStatementsByLhsName(String var)
    {
        if (body() == null)
        {
            return Collections.emptyList();
        }
        return body().matchAll(Xcode.F_ASSIGN_STATEMENT).stream().map(Xnode::element).map(AssignStatement::new)
                .filter(x -> x.getLhsName().equalsIgnoreCase(var)).collect(Collectors.toList());
    }

    /**
     * Get all the function variables that are input/output parameters.
     *
     * @param xcodeml Current XcodeML program unit.
     * @return List of variables names that are function input/output.
     */
    public List<String> getPresentVariables(XcodeProgram xcodeml)
    {
        return getVariables(xcodeml, true, false, true);
    }

    /**
     * Get all the local variables in the function definition.
     *
     * @param xcodeml   Current XcodeML program unit.
     * @param onlyArray If true, filter only arrays.
     * @return List of variables names that are function local.
     */
    public List<String> getLocalVariables(XcodeProgram xcodeml, boolean onlyArray)
    {
        return getVariables(xcodeml, false, true, onlyArray);
    }

    /**
     * Get variables declared in the function.
     *
     * @param xcodeml    Current translation unit.
     * @param parameters If true, parameters are returned.
     * @param temporary  If true, local variables are returned.
     * @param onlyArray  If true, only arrays are returned.
     * @return List of variables names.
     */
    private List<String> getVariables(XcodeProgram xcodeml, boolean parameters, boolean temporary, boolean onlyArray)
    {
        List<String> variables = new ArrayList<>();
        List<Xnode> declarations = getDeclarationTable().values();
        for (Xnode decl : declarations)
        {
            if (decl.is(Xcode.VAR_DECL))
            {
                Xnode name = decl.matchSeq(Xcode.NAME);
                if (!(xcodeml.getTypeTable().isBasicType(decl)))
                {
                    continue; // Only check basic type
                }
                FbasicType bt = xcodeml.getTypeTable().getBasicType(decl);
                if ((parameters && isParameterVariable(bt, onlyArray))
                        || (temporary && isTemporaryVariable(bt, onlyArray)))
                {
                    variables.add(name.value());
                }
            }
        }
        return variables;
    }

    /**
     * Check if the variable is a parameter with intent.
     *
     * @param bt        FbasicType to be checked.
     * @param onlyArray If true, check for arrays only.
     * @return True if the variable is a parameter and pass the onlyArray filter.
     */
    private boolean isParameterVariable(FbasicType bt, boolean onlyArray)
    {
        return bt != null
                && (bt.getIntent() == Intent.IN || bt.getIntent() == Intent.OUT || bt.getIntent() == Intent.INOUT)
                && (!onlyArray || bt.isArray());
    }

    /**
     * Check if the variable is a temporary.
     *
     * @param bt        FbasicType to be checked.
     * @param onlyArray If true, check for arrays only.
     * @return True if the variable is a temporary and pass the onlyArray filter.
     */
    private boolean isTemporaryVariable(FbasicType bt, boolean onlyArray)
    {
        return bt != null && bt.getIntent() == Intent.NONE && (!onlyArray || bt.isArray());
    }

    /**
     * Detect all induction variables in the function body.
     *
     * @return Set of induction variables stored in a set.
     */
    public Set<String> detectInductionVariables()
    {
        return body().matchAll(Xcode.F_DO_STATEMENT).stream().map(Loop::extractInductionVariable)
                .collect(Collectors.toSet());
    }

    /**
     * Find the id element in the current function definition or in parent function
     * definition if nested.
     *
     * @param name Id name to be searched for.
     * @return The id if found. Null otherwise.
     */
    public Xid findId(String name)
    {
        if (getSymbolTable().contains(name))
        {
            return getSymbolTable().get(name);
        }
        FfunctionDefinition upperDef = findParentFunction();
        if (upperDef == null)
        {
            return null;
        }
        return upperDef.findId(name);
    }

    /**
     * Find the declaration element in the current function definition or in parent
     * if nested.
     *
     * @param name Declaration name to be searched for.
     * @return The element if found. Null otherwise.
     */
    public Xnode findDecl(String name)
    {
        if (getSymbolTable().contains(name))
        {
            return getDeclarationTable().get(name);
        }
        FfunctionDefinition upperDef = findParentFunction();
        if (upperDef == null)
        {
            return null;
        }
        return upperDef.findDecl(name);
    }

}
