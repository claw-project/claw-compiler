/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode.fortran;

import claw.tatsu.xcodeml.xnode.XnodeUtil;
import claw.tatsu.xcodeml.xnode.common.Xattr;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.Xnode;

import java.util.ArrayList;
import java.util.List;

/**
 * The FbasicType represents the FbasicType (3.3) element in XcodeML
 * intermediate representation.
 *
 * Elements: (kind?, (len | (arrayIndex | indexRange)+)?, coShape?) - Optional:
 * - kind - len - arrayIndex - indexRange - coShape - Not used in CLAW at the
 * moment Attributes: - Required: type (text), ref (text) - Optional: is_public
 * (bool), is_private (bool), is_pointer (bool), is_target (bool), is_external
 * (bool),is_intrinsic (bool), is_optional (bool), is_save (bool), is_parameter
 * (bool), is_allocatable (bool), intent (text: in, out, inout)
 *
 * @author clementval
 */
public class FbasicType extends Xnode
{

    private static final int APPEND = -1;
    private boolean _isArray = false;
    // Optional elements
    private List<Xnode> _dimensions = null;
    private Xnode _kind = null;
    private Xnode _length = null;

    /**
     * Element standard ctor. Pass the base element to the base class and read inner
     * information (elements and attributes).
     *
     * @param node Raw node.
     */
    public FbasicType(Xnode node)
    {
        super(node == null ? null : node.element());
        readBasicTypeInformation();
    }

    /**
     * Read inner element information.
     */
    private void readBasicTypeInformation()
    {
        _dimensions = XnodeUtil.findIndexes(this);
        // is array ?
        if (!_dimensions.isEmpty())
        {
            _isArray = true;
        }

        // has length ?
        _length = matchSeq(Xcode.LEN);

        // has kind ?
        _kind = matchSeq(Xcode.KIND);
    }

    /**
     * Get the indexRange object for the given dimension.
     *
     * @param index The position of the dimension. For the first dimension, index is
     *              0, for the second is 1 and so on.
     * @return A XindexRange object representing the index range of a specific
     *         dimension.
     */
    public Xnode getDimensions(int index)
    {
        if (index >= _dimensions.size() || index < 0)
        {
            return null;
        }
        return _dimensions.get(index);
    }

    /**
     * Check whether the type is an array type.
     *
     * @return True if the type is an array type. False otherwise.
     */
    public boolean isArray()
    {
        return _isArray;
    }

    /**
     * Check whether the type has a length element.
     *
     * @return True if the type has a length element. False otherwise.
     */
    public boolean hasLength()
    {
        return _length != null;
    }

    /**
     * Get the len element.
     *
     * @return Len element. Null if the basic type has no len element.
     */
    public Xnode getLength()
    {
        return _length;
    }

    /**
     * Check whether the type has a kind element.
     *
     * @return True if the type has a kind element. False otherwise.
     */
    public boolean hasKind()
    {
        return _kind != null;
    }

    /**
     * Get the kind element.
     *
     * @return Kind element. Null if the basic type has no kind element.
     */
    public Xnode getKind()
    {
        return _kind;
    }

    /**
     * Get the array dimensions.
     *
     * @return The dimensions of the array type.
     */
    public int getDimensions()
    {
        return _dimensions.size();
    }

    /**
     * Get ref attribute value.
     *
     * @return The ref attribute value as String.
     */
    public String getRef()
    {
        return getAttribute(Xattr.REF);
    }

    /**
     * Set the value of ref attribute.
     *
     * @param value New value of ref attribute.
     */
    public void setRef(String value)
    {
        setAttribute(Xattr.REF, value);
    }

    /**
     * Check whether the type is public.
     *
     * @return True if the type is public. False otherwise.
     */
    public boolean isPublic()
    {
        return getBooleanAttribute(Xattr.IS_PUBLIC);
    }

    /**
     * Check whether the type is private.
     *
     * @return True if the type is private. False otherwise.
     */
    public boolean isPrivate()
    {
        return getBooleanAttribute(Xattr.IS_PRIVATE);
    }

    /**
     * Check whether the type is a pointer.
     *
     * @return True if the type is a pointer. False otherwise.
     */
    public boolean isPointer()
    {
        return getBooleanAttribute(Xattr.IS_POINTER);
    }

    /**
     * Check whether the type is a target.
     *
     * @return True if the type is a target. False otherwise.
     */
    public boolean isTarget()
    {
        return getBooleanAttribute(Xattr.IS_TARGET);
    }

    /**
     * Check whether the type is external.
     *
     * @return True if the type is external. False otherwise.
     */
    public boolean isExternal()
    {
        return getBooleanAttribute(Xattr.IS_EXTERNAL);
    }

    /**
     * Check whether the type is intrinsic.
     *
     * @return True if the type is intrinsic. False otherwise.
     */
    public boolean isIntrinsic()
    {
        return getBooleanAttribute(Xattr.IS_INTRINSIC);
    }

    /**
     * Check whether the type is optional.
     *
     * @return True if the type is optional. False otherwise.
     */
    public boolean isOptional()
    {
        return getBooleanAttribute(Xattr.IS_OPTIONAL);
    }

    /**
     * Check whether the type is save.
     *
     * @return True if the type is save. False otherwise.
     */
    public boolean isSave()
    {
        return getBooleanAttribute(Xattr.IS_SAVE);
    }

    /**
     * Check whether the type is a parameter.
     *
     * @return True if the type is a parameter. False otherwise.
     */
    public boolean isParameter()
    {
        return getBooleanAttribute(Xattr.IS_PARAMETER);
    }

    /**
     * Check whether the type is allocatable.
     *
     * @return True if the type is allocatable. False otherwise.
     */
    public boolean isAllocatable()
    {
        return getBooleanAttribute(Xattr.IS_ALLOCATABLE);
    }

    /**
     * Check whether the type has an intent.
     *
     * @return True if the type has an intent. False otherwise.
     */
    public boolean hasIntent()
    {
        return Intent.fromString(getAttribute(Xattr.INTENT)) != Intent.NONE;
    }

    /**
     * Get the intent of the type.
     *
     * @return Intent. Null if the type has no intent.
     */
    public Intent getIntent()
    {
        return Intent.fromString(getAttribute(Xattr.INTENT));
    }

    /**
     * Set the intent of the type.
     *
     * @param value Intent value to be set.
     */
    public void setIntent(Intent value)
    {
        if (value != null && value != Intent.NONE)
        {
            setAttribute(Xattr.INTENT, value.toString());
        }
    }

    /**
     * Remove all dimension from the type
     */
    public void resetDimension()
    {
        for (Xnode idx : _dimensions)
        {
            idx.delete();
        }
        _dimensions.clear();
        _isArray = false;
    }

    /**
     * Remove the dimensions not in the given list. Dimension index starts at 1.
     *
     * @param keptDimensions List of dimension index to be kept.
     */
    public void removeDimension(List<Integer> keptDimensions)
    {
        List<Xnode> keptDim = new ArrayList<>();
        for (int i = 0; i < _dimensions.size(); i++)
        {
            if (keptDimensions.contains(i + 1))
            {
                keptDim.add(_dimensions.get(i));
            } else
            {
                _dimensions.get(i).delete();
            }
        }
        if (keptDim.isEmpty())
        {
            _isArray = false;
        }
        _dimensions = keptDim;
    }

    public void addDimension(Xnode index)
    {
        addDimension(index, APPEND);
    }

    /**
     * Add a dimension to the basic type.
     *
     * @param index    Index element to add as the new dimension.
     * @param position Position compared to already existing element. If -1,
     *                 dimension is added at the end.
     */
    public void addDimension(Xnode index, int position)
    {
        if (_dimensions.isEmpty() || position == APPEND)
        {
            append(index);
            _dimensions.add(index);
            _isArray = true;
        } else
        {
            Xnode crtPos = _dimensions.get(position);
            crtPos.insertBefore(index);
            _dimensions.add(position, index);
        }
    }

    /**
     * Check if the array type is specified with deferred dimension or not.
     *
     * @return True if all current dimensions are deferred. False otherwise.
     */
    public boolean isAllAssumedShape()
    {
        if (!isArray())
        {
            return false;
        }
        return _dimensions.stream().allMatch(x -> x.getBooleanAttribute(Xattr.IS_ASSUMED_SHAPE));
    }

    /**
     * Copy the attributes from the type to this one.
     *
     * @param from Type to copy attributes from.
     */
    public void copyAttributes(FbasicType from)
    {
        if (from == null)
        {
            return;
        }

        if (from.isAllocatable())
            setBooleanAttribute(Xattr.IS_ALLOCATABLE, true);
        if (from.isOptional())
            setBooleanAttribute(Xattr.IS_OPTIONAL, true);
        if (from.isPointer())
            setBooleanAttribute(Xattr.IS_POINTER, true);
        if (from.isTarget())
            setBooleanAttribute(Xattr.IS_TARGET, true);
        if (from.isParameter())
            setBooleanAttribute(Xattr.IS_PARAMETER, true);
        if (from.isPrivate())
            setBooleanAttribute(Xattr.IS_PRIVATE, true);
        if (from.isPublic())
            setBooleanAttribute(Xattr.IS_PUBLIC, true);
        if (from.isSave())
            setBooleanAttribute(Xattr.IS_SAVE, true);
    }

    @Override
    public FbasicType cloneNode()
    {
        return new FbasicType(super.cloneNode());
    }

    /**
     * Return a brief description of the FbasicType.
     *
     * @return String description of the FbasicType as "FbasicType (type=type-value,
     *         ref=ref-value)".
     */
    @Override
    public String toString()
    {
        return String.format("FbasicType (type=\"%s\", ref=\"%s\", intent=\"%s\")", getType(), getRef(),
                getIntent().toString());
    }
}
