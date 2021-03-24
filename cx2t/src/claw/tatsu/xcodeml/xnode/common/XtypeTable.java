/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode.common;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import claw.tatsu.xcodeml.xnode.fortran.FbasicType;
import claw.tatsu.xcodeml.xnode.fortran.FfunctionType;
import claw.tatsu.xcodeml.xnode.fortran.FortranType;
import claw.tatsu.xcodeml.xnode.fortran.FstructType;

/**
 * The XtypeTable represents the typeTable (3.1) element in XcodeML intermediate
 * representation.
 *
 * Elements: ( FbasicType | FfunctionType | FstructType ) * - Optional: -
 * FbasicType (FbasicType) - FfunctionType (FfunctionType) - FstructType
 * (FstructType)
 *
 * @author clementval
 */
public class XtypeTable extends Xnode
{

    private final Map<String, Xnode> _table;

    /**
     * Element standard ctor. Pass the base element to the base class and read inner
     * information (elements and attributes).
     *
     * @param node Raw node.
     */
    public XtypeTable(Xnode node)
    {
        super(node == null ? null : node.element());
        _table = new LinkedHashMap<>();
        readTable();
    }

    /**
     * Read the type table.
     */
    private void readTable()
    {
        List<Xnode> elements = children();
        for (Xnode n : elements)
        {
            switch (n.opcode())
            {
            case F_BASIC_TYPE:
                FbasicType bt = new FbasicType(n);
                _table.put(bt.getType(), bt);
                break;
            case F_FUNCTION_TYPE:
                FfunctionType ft = new FfunctionType(n);
                _table.put(ft.getType(), ft);
                break;
            case F_STRUCT_TYPE:
                FstructType st = new FstructType(n);
                _table.put(st.getType(), st);
                break;
            default:
                break;
            }
        }
    }

    /**
     * Check if the node is of type FbasicType.
     *
     * @param node Node to check.
     * @return True if the node is of type FbasicType.
     */
    public boolean isBasicType(Xnode node)
    {
        return isBasicType(node.getType());
    }

    /**
     * Check if the hash correspond to a FbasicType.
     *
     * @param hash Hash of type to check.
     * @return True if the hash correspond to a FbasicType.
     */
    public boolean isBasicType(String hash)
    {
        return isType(FbasicType.class, hash);
    }

    /**
     * Check if the node is of type FfunctionType.
     *
     * @param node Node to check.
     * @return True if the node is of type FfunctionType.
     */
    public boolean isFunctionType(Xnode node)
    {
        return isFunctionType(node.getType());
    }

    /**
     * Check if the hash correspond to a FfunctionType.
     *
     * @param hash Hash of type to check.
     * @return True if the hash correspond to a FfunctionType.
     */
    public boolean isFunctionType(String hash)
    {
        return isType(FfunctionType.class, hash);
    }

    /**
     * Check if the hash correspond to a FstructType.
     *
     * @param hash Hash of type to check.
     * @return True if the hash correspond to a FstructType.
     */
    public boolean isStructType(String hash)
    {
        return isType(FstructType.class, hash);
    }

    /**
     * Check if the hash correspond to a FstructType.
     *
     * @param node Node to check.
     * @return True if the hash correspond to a FstructType.
     */
    public boolean isStructType(Xnode node)
    {
        return isStructType(node.getType());
    }

    /**
     * Check if the corresponding type node is of given type.
     *
     * @param typeClass Class type to check against.
     * @param hash      Hash of type to retrieved.
     * @return True of the type is retrieved and is of given type. False in any
     *         other case.
     */
    private boolean isType(Class<?> typeClass, String hash)
    {
        if (hash == null || hash.isEmpty())
        {
            return false;
        }
        Xnode t = get(hash);
        return typeClass.isInstance(t);
    }

    /**
     * Get number of elements in the type table.
     *
     * @return Number of elements in the table.
     */
    public int size()
    {
        return _table.size();
    }

    /**
     * Add a new element in the type table.
     *
     * @param type The new type to be added.
     */
    public void add(Xnode type)
    {
        if (!type.getType().isEmpty())
        {
            // TODO should be cloned?
            _baseElement.appendChild(type.cloneRawNode());
            _table.put(type.getType(), type);
        }
    }

    /**
     * Get the FbasicType associated with the node if any.
     *
     * @param node Node to look for type.
     * @return FbasicType if associated. Null otherwise.
     */
    public FbasicType getBasicType(Xnode node)
    {
        return getBasicType(node.getType());
    }

    /**
     * Get the FbasicType associated with the given hash value.
     *
     * @param hash Hash value to check for.
     * @return FbasicType if associated. Null otherwise.
     */
    public FbasicType getBasicType(String hash)
    {
        if (isBasicType(hash))
        {
            return (FbasicType) get(hash);
        }
        return null;
    }

    /**
     * Get the FfunctionType associated with the node if any.
     *
     * @param node Node to look for type.
     * @return FfunctionType if associated. Null otherwise.
     */
    public FfunctionType getFunctionType(Xnode node)
    {
        return getFunctionType(node.getType());
    }

    /**
     * Get the FfunctionType associated with the given hash value.
     *
     * @param hash Hash value to check for.
     * @return FfunctionType if associated. Null otherwise.
     */
    public FfunctionType getFunctionType(String hash)
    {
        if (isFunctionType(hash))
        {
            return (FfunctionType) get(hash);
        }
        return null;
    }

    /**
     * Get the FstructType associated with the node if any.
     *
     * @param node Node to look for type.
     * @return FstructType if associated. Null otherwise.
     */
    public FstructType getStructType(Xnode node)
    {
        return getStructType(node.getType());
    }

    /**
     * Get the FstructType associated with the given hash value.
     *
     * @param hash Hash value to check for.
     * @return FstructType if associated. Null otherwise.
     */
    public FstructType getStructType(String hash)
    {
        if (isStructType(hash))
        {
            return (FstructType) get(hash);
        }
        return null;
    }

    /**
     * Get an element from the type table.
     *
     * @param hash Hash of type node to be retrieved.
     * @return Xnode object if found in the table. Null otherwise.
     */
    protected Xnode get(String hash)
    {
        if (_table.containsKey(hash))
        {
            return _table.get(hash);
        }
        return null;
    }

    /**
     * Check if a type is present in the type table
     *
     * @param hash Hash of type node to be checked.
     * @return True if the element is present. False otherwise.
     */
    public boolean hasType(String hash)
    {
        return _table.containsKey(hash);
    }

    /**
     * Generate a unique hash in the current type table.
     *
     * @param type Type to generate the hash.
     * @return New unique hash.
     */
    public String generateHash(FortranType type)
    {
        if (type == null)
        {
            return "";
        }
        String hash;
        do
        {
            hash = type.generateHash();
        } while (hasType(hash));
        return hash;
    }

    /**
     * Returns a Collection view of the values contained in this XtypeTable.
     *
     * @return A view of the values contained in this map
     */
    public Collection<Xnode> values()
    {
        return _table.values();
    }

    @Override
    public XtypeTable cloneNode()
    {
        return new XtypeTable(super.cloneNode());
    }
}
