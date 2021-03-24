/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.x2t.configuration;

/**
 * Class holding group information from configuration.
 *
 * @author clementval
 */
public class GroupConfiguration
{

    private final String _setName;
    private final String _name;
    private final String _cPath;
    private final GroupType _type;
    private final TriggerType _trigger;
    private final String _directivePrefix;
    private final Class _transformationClass;

    /**
     * Constructs a new GroupConfiguration element with all mandatory information.
     *
     * @param setName   Transformation set name.
     * @param name      User friendly name of the group.
     * @param type      Type of the group.
     * @param trigger   Type of the trigger action.
     * @param cPath     Path to the transformation class.
     * @param directive If trigger is directive, directive prefix. Otherwise null.
     * @param c         Actual class of the transformation.
     */
    public GroupConfiguration(String setName, String name, GroupType type, TriggerType trigger, String cPath,
            String directive, Class c)
    {
        _setName = setName;
        _name = name;
        _cPath = cPath;
        _type = type;
        _trigger = trigger;
        _transformationClass = c;
        _directivePrefix = directive;
    }

    /**
     * Get the name value.
     *
     * @return Name value.
     */
    public String getName()
    {
        return _name;
    }

    /**
     * Get the set name where this group configuration belongs to.
     *
     * @return Set name value.
     */
    public String getSetName()
    {
        return _setName;
    }

    /**
     * Get the class path value.
     *
     * @return Class path value.
     */
    public String getTransformationClassName()
    {
        return _cPath;
    }

    /**
     * Get the type value.
     *
     * @return Type value.
     */
    public GroupType getType()
    {
        return _type;
    }

    /**
     * Get the trigger type.
     *
     * @return Trigger type.
     */
    public TriggerType getTriggerType()
    {
        return _trigger;
    }

    /**
     * Get the directive prefix associated.
     *
     * @return Directive prefix.
     */
    public String getDirective()
    {
        return _directivePrefix;
    }

    /**
     * Get the transformation class value.
     *
     * @return Transformation class value.
     */
    public Class getTransformationClass()
    {
        return _transformationClass;
    }

    public enum GroupType {
        DEPENDENT, INDEPENDENT
    }

    public enum TriggerType {
        DIRECTIVE, TRANSLATION_UNIT
    }
}
