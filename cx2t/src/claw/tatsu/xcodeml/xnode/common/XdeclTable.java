/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode.common;

import java.util.*;

/**
 * The XdeclTable represents the typeTable (5.2) element in XcodeML intermediate
 * representation.
 *
 * Elements: ( varDecl | FstructDecl | externDecl | FuseDecl | FuseOnlyDecl |
 * FinterfaceDecl | FnamelistDecl | FequivalenceDecl | FcommonDecl )*
 *
 * - Optional: - varDecl - FstructDecl - externDecl - FuseDecl - FuseOnlyDecl -
 * FinterfaceDecl - FnamelistDecl - FequivalenceDecl - FcommonDecl
 *
 * @author clementval
 */

public class XdeclTable extends Xnode
{

    /*
     * Some transformation needs to know the order of the declarations. Therefore,
     * we use a LinkedHashMap to be able to give back the table with its order.
     */
    private final LinkedHashMap<String, Xnode> _table;

    /**
     * Element standard ctor. Pass the base element to the base class and read inner
     * information (elements and attributes).
     *
     * @param node Raw node.
     */
    public XdeclTable(Xnode node)
    {
        super(node == null ? null : node.element());
        _table = new LinkedHashMap<>();
        readTable();
    }

    /**
     * Read the declaration table.
     */
    private void readTable()
    {
        List<Xnode> declarations = children();
        for (Xnode n : declarations)
        {
            String key;
            switch (n.opcode())
            {
            case EXTERN_DECL:
            case F_STRUCT_DECL:
            case VAR_DECL:
                key = n.matchSeq(Xcode.NAME).value();
                break;
            case F_USE_DECL:
            case F_USE_ONLY_DECL:
            case F_INTERFACE_DECL:
                key = n.getAttribute(Xattr.NAME);
                break;
            case F_NAMELIST_DECL:
                key = n.matchSeq(Xcode.VAR_LIST).getAttribute(Xattr.NAME);
                break;
            case F_COMMON_DECL:
                key = Xcode.F_COMMON_DECL.toString() + UUID.randomUUID();
                break;
            case F_EQUIVALENCE_DECL:
                key = Xcode.F_EQUIVALENCE_DECL.toString() + UUID.randomUUID();
                break;
            default:
                continue;
            }
            _table.put(key, n);
        }

    }

    /**
     * Replace a declaration in the table.
     *
     * @param decl The new declaration to be inserted.
     * @param name Name describing the declaration in the table to be replaced.
     */
    public void replace(Xnode decl, String name)
    {
        if (!_table.containsKey(name))
        {
            append(decl);
            _table.put(name, decl);
        } else
        {
            Xnode oldDecl = _table.get(name);
            oldDecl.insertAfter(decl);
            oldDecl.delete();
            _table.remove(name);
            _table.put(name, decl);
        }
    }

    /**
     * Add a new declaration as last element if key is not used yet.
     *
     * @param decl The new declaration object.
     */
    public void add(Xnode decl)
    {
        String key = decl.matchSeq(Xcode.NAME).value();
        if (!_table.containsKey(key))
        {
            _baseElement.appendChild(decl.cloneRawNode());
            _table.put(key, decl);
        }
    }

    /**
     * Add a new declaration as last element if key is not used yet.
     *
     * @param decl The new declaration object.
     */
    public void addFirst(Xnode decl)
    {
        if (_baseElement.getFirstChild() != null)
        {
            String key = decl.matchSeq(Xcode.NAME).value();
            if (!_table.containsKey(key))
            {
                _baseElement.insertBefore(decl.cloneRawNode(), _baseElement.getFirstChild());
                _table.put(decl.matchSeq(Xcode.NAME).value(), decl);
            }
        } else
        {
            add(decl);
        }
    }

    /**
     * Get a specific declaration based on its name.
     *
     * @param key The name of the declaration to be returned.
     * @return A XvarDecl object if key is found. Null otherwise.
     */
    public Xnode get(String key)
    {
        if (_table.containsKey(key))
        {
            return _table.get(key);
        }
        return null;
    }

    /**
     * Get all elements in the table.
     *
     * @return All elements stored in the table.
     */
    public List<Xnode> values()
    {
        List<Xnode> orderedDeclarations = new ArrayList<>();
        for (Map.Entry<String, Xnode> entry : _table.entrySet())
        {
            orderedDeclarations.add(entry.getValue());
        }
        return orderedDeclarations;
    }

    /**
     * Get all declarations of a specific kind of elements.
     *
     * @param decl Kind of elements to return.
     * @return A list of all declarations of this kind.
     */
    public List<Xnode> values(Xcode decl)
    {
        return values(Collections.singletonList(decl));
    }

    /**
     * Get all declarations of a specific kind of elements.
     *
     * @param declarations Kind of elements to return.
     * @return A list of all declarations of this kind.
     */
    public List<Xnode> values(List<Xcode> declarations)
    {
        List<Xnode> orderedFilteredDeclarations = new ArrayList<>();
        for (Map.Entry<String, Xnode> entry : _table.entrySet())
        {
            if (declarations.contains(entry.getValue().opcode()))
            {
                orderedFilteredDeclarations.add(entry.getValue());
            }
        }
        return orderedFilteredDeclarations;
    }

    /**
     * Get all the use and use only declaration in the table.
     *
     * @return List of FuseDecl and FuseOnlyDecl nodes.
     */
    public List<Xnode> uses()
    {
        return values(Arrays.asList(Xcode.F_USE_DECL, Xcode.F_USE_ONLY_DECL));
    }

    /**
     * Get the number of declarations in the table.
     *
     * @return The number of declarations in the table.
     */
    public int count()
    {
        return _table.size();
    }

    /**
     * Create and insert a FuseDecl node at the beginning of the table if not
     * present in the table yet.
     *
     * @param xcodeml    Current XcodeML translation unit.
     * @param moduleName Module name inserted in the FuseDecl node.
     */
    public void insertUseDecl(XcodeProgram xcodeml, String moduleName)
    {
        if (_table.containsKey(moduleName.toLowerCase()))
        {
            return;
        }
        Xnode useDecl = xcodeml.createUseDecl(moduleName);
        this.insert(useDecl);
        _table.put(moduleName.toLowerCase(), useDecl);
    }

    /**
     * Create and insert a FuseOnlyDecl node at the beginning of the table if not
     * present in the table yet.
     *
     * @param xcodeml    Current XcodeML translation unit.
     * @param moduleName Module name inserted in the FuseDecl node.
     * @param names      List of use name to be added in the ONLY list.
     */
    public void insertUseOnlyDecl(XcodeProgram xcodeml, String moduleName, List<String> names)
    {
        if (_table.containsKey(moduleName.toLowerCase()))
        {
            return;
        }
        Xnode useOnlyDecl = xcodeml.createUseOnlyDecl(moduleName, names);
        this.insert(useOnlyDecl);
        _table.put(moduleName.toLowerCase(), useOnlyDecl);
    }

    /**
     * Check if a name is already present in the declaration table.
     *
     * @param name String value of the name to check.
     * @return True if the name is already in the table. False otherwise.
     */
    public boolean contains(String name)
    {
        return _table.containsKey(name);
    }

    @Override
    public XdeclTable cloneNode()
    {
        return new XdeclTable(super.cloneNode());
    }
}
