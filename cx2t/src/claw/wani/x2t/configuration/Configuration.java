/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.x2t.configuration;

import static java.lang.String.format;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import javax.xml.validation.Validator;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import claw.wani.ClawVersion;
import claw.shenron.transformation.BlockTransformation;
import claw.tatsu.common.CompilerDirective;
import claw.tatsu.common.Context;
import claw.tatsu.common.Target;
import claw.tatsu.directive.configuration.AcceleratorConfiguration;
import claw.tatsu.directive.configuration.OpenAccConfiguration;
import claw.tatsu.directive.configuration.OpenMpConfiguration;
import claw.tatsu.xcodeml.xnode.Xname;
import claw.wani.transformation.ClawBlockTransformation;

/**
 * Configuration class is used to read the configuration file and expose its
 * information to the translator.
 *
 * @author clementval
 */
public class Configuration
{
    private static final Path DEFAULT_CONFIG_DIR_IN_JAR_PATH = Paths.get("/claw/config");
    public static final String TRANSLATOR = "translator";
    // Specific keys
    private static final String DEFAULT_TARGET = "default_target";
    private static final String DEFAULT_DIRECTIVE = "default_directive";
    public static final String DEFAULT_CONFIG_FILE = "claw-default.xml";
    private static final String XML_EXT = ".xml";
    public static final String CONFIG_XSD = "claw_config.xsd";
    public static final String SET_XSD = "claw_transformation_set.xsd";
    // Element and attribute names
    private static final String GLOBAL_ELEMENT = "global";
    private static final String GROUPS_ELEMENT = "groups";
    private static final String GROUP_ELEMENT = "group";
    private static final String SETS_ELEMENT = "sets";
    private static final String SET_ELEMENT = "set";
    private static final String PARAMETER_ELEMENT = "parameter";
    private static final String CLASS_ATTR = "class";
    private static final String NAME_ATTR = "name";
    private static final String TYPE_ATTR = "type";
    private static final String KEY_ATTR = "key";
    private static final String VALUE_ATTR = "value";
    private static final String VERSION_ATTR = "version";
    private static final String TRIGGER_ATTR = "trigger";
    private static final String DIRECTIVE_ATTR = "directive";
    private static final String EXT_CONF_TYPE = "extension";
    private static final String JAR_ATTR = "jar";
    // Transformation set
    private static final String TRANSFORMATION_ELEMENT = "transformation";
    // Specific values
    private static final String DEPENDENT_GR_TYPE = "dependent";
    private static final String INDEPENDENT_GR_TYPE = "independent";
    private static final String DIRECTIVE_TR_TYPE = "directive";
    private static final String TRANSLATION_UNIT_TR_TYPE = "translation_unit";
    // OpenMP specific values
    public static final String CPU_STRATEGY = "cpu_trans_strategy";
    public static final String CPU_STRATEGY_SINGLE = "single";
    public static final String CPU_STRATEGY_FUSION = "fusion";
    // SCA configuration keys
    public static final String SCA_ELEMENTAL_PROMOTION_ASSUMED = "sca_elemental_promotion_assumed";
    public static final String SCA_SERIALIZATION_ENABLED = "sca_serialization_enabled";
    public static final String SCA_SERIALIZATION_ENABLED_DIRECTION = "sca_serialization_enabled_direction";
    public static final String SCA_SERIALIZATION_READ = "read";
    public static final String SCA_SERIALIZATION_WRITE = "write";
    public static final String SCA_SERIALIZATION_READ_WRITE = "all";
    public static final String SCA_FORWARD_UPDATE_ENABLED = "sca_forward_update_enabled";
    public static final String SCA_FORWARD_UPDATE_DIRECTION = "sca_forward_update_enabled_direction";
    public static final String SCA_FORWARD_UPDATE_IN = "in";
    public static final String SCA_FORWARD_UPDATE_OUT = "out";
    public static final String SCA_FORWARD_UPDATE_INOUT = "inout";

    // env var
    private static final String CLAW_TRANS_SET_PATH = "CLAW_TRANS_SET_PATH";

    // Local objects
    private Map<String, String> _parameters;
    private List<GroupConfiguration> _groups;
    private Map<String, GroupConfiguration> _availableGroups;
    private AcceleratorConfiguration _accelerator;
    private final String[] _transSetPaths;
    private boolean _forcePure = false;
    private final ModelConfig _modelConfig;
    private final int _userMaxColumns;
    final Context _context;

    public int getUserMaxColumns()
    {
        return _userMaxColumns;
    }

    public Context context()
    {
        return _context;
    }

    /**
     * Constructs basic configuration object.
     *
     * @param directive Accelerator directive language.
     * @param target    Target architecture.
     */
    public void init(CompilerDirective directive, Target target)
    {
        _parameters = new HashMap<>();

        if (directive == null)
        {
            directive = CompilerDirective.NONE;
        }
        _parameters.put(DEFAULT_DIRECTIVE, directive.toString());

        if (target != null)
        {
            _parameters.put(DEFAULT_TARGET, target.toString());
        }

        // Init specific configuration if needed
        switch (directive)
        {
        case OPENACC:
            _accelerator = new OpenAccConfiguration(_parameters);
            break;
        case OPENMP:
            _accelerator = new OpenMpConfiguration(_parameters);
            break;
        default:
            _accelerator = new AcceleratorConfiguration(_parameters);
            break;
        }

        _groups = new ArrayList<>();
        _availableGroups = new HashMap<>();
    }

    /**
     * Constructs a new configuration object from the give configuration file.
     *
     * @param cfgDirPath           Path to the directory with configuration files
     *                             and XSD schemas
     * @param userConfigFilePath   Path to non-default configuration file
     * @param modelConfigFilePath  Path to SCA specific model configuration file
     * @param userDefinedTarget    Target platform
     * @param userDefinedDirective Accelerator directive language
     * @param userMaxColumns       Max columns per Fortran line
     * @throws Exception
     */
    public static Configuration load(final Path cfgDirPath, final Path userConfigFilePath, Path modelConfigFilePath,
            final String userDefinedTarget, final String userDefinedDirective, Integer userMaxColumns, Context context)
            throws Exception
    {
        Configuration cfg = new Configuration(cfgDirPath, userConfigFilePath, modelConfigFilePath, userDefinedTarget,
                userDefinedDirective, userMaxColumns, context);
        return cfg;
    }

    protected Configuration()
    {
        _context = null;
        _modelConfig = new ModelConfig();
        _userMaxColumns = 0;
        _transSetPaths = null;
    }

    private Configuration(final Path cfgDirPath, final Path userConfigFilePath, Path modelConfigFilePath,
            final String userDefinedTarget, final String userDefinedDirective, Integer userMaxColumns, Context context)
            throws Exception
    {
        _context = context;
        _modelConfig = new ModelConfig();
        _parameters = new HashMap<>();
        _groups = new ArrayList<>();
        _availableGroups = new HashMap<>();
        boolean readDefault = true;
        Document userConf = null;

        // Read the environment variable for external transformation sets
        if (System.getenv(CLAW_TRANS_SET_PATH) != null)
        {
            _transSetPaths = System.getenv(CLAW_TRANS_SET_PATH).split(";");
        } else
        {
            _transSetPaths = new String[0];
        }

        // Configuration has been given by the user. Read it first.
        if (userConfigFilePath != null)
        {
            try (InputStream userCfgFile = Files.newInputStream(userConfigFilePath))
            {
                userConf = validateConfiguration(cfgDirPath, userCfgFile);
            }
            readDefault = isExtension(userConf);
        }
        if (readDefault)
        { // There is no user defined configuration or it is just an extension.
            Document defaultConf;
            try (InputStream defaultConfigFile = this.getCfgFileAsInputStream(cfgDirPath, DEFAULT_CONFIG_FILE))
            {
                defaultConf = validateConfiguration(cfgDirPath, defaultConfigFile);
            }
            readConfiguration(cfgDirPath, defaultConf, false);
            if (userConf != null)
            { // Read extension
                readConfiguration(cfgDirPath, userConf, true);
            }
        } else
        { // User defined configuration is a full configuration.
          // Then the default one is not read.
            readConfiguration(cfgDirPath, userConf, false);
        }

        setUserDefinedTarget(userDefinedTarget);
        setUserDefineDirective(userDefinedDirective);

        switch (getCurrentDirective())
        {
        case OPENACC:
            _accelerator = new OpenAccConfiguration(_parameters);
            break;
        case OPENMP:
            _accelerator = new OpenMpConfiguration(_parameters);
            break;
        default:
            _accelerator = new AcceleratorConfiguration(_parameters);
        }

        if (userMaxColumns != null)
        {
            _userMaxColumns = userMaxColumns;
        } else
        {
            _userMaxColumns = 0;
        }

        context().init(getCurrentDirective(), getCurrentTarget(), accelerator(), getUserMaxColumns());

        if (modelConfigFilePath != null)
        {
            getModelConfig().load(modelConfigFilePath);
        }
    }

    /**
     * Check whether the configuration file is an extension of the default
     * configuration or if it is a standalone configuration.
     *
     * @param configurationDocument XML document representing the configuration
     *                              file.
     * @return True if the configuration is an extension. False otherwise.
     */
    private boolean isExtension(Document configurationDocument)
    {
        Element root = configurationDocument.getDocumentElement();
        Element global = (Element) root.getElementsByTagName(GLOBAL_ELEMENT).item(0);
        return global != null && global.hasAttribute(TYPE_ATTR) && global.getAttribute(TYPE_ATTR).equals(EXT_CONF_TYPE);
    }

    /**
     * Validate configuration file against its XSD schema.
     *
     * @param configurationFile Configuration file.
     * @return XML Document of the configuration file.
     * @throws Exception If the configuration file cannot be validated.
     */
    private Document validateConfiguration(final Path cfgDirPath, InputStream configurationFile) throws Exception
    {
        final Schema cfgSchema = loadSchema(cfgDirPath, CONFIG_XSD);

        Document doc = parseAndValidate(configurationFile, cfgSchema);
        Element root = doc.getDocumentElement();
        checkVersion(root.getAttribute(VERSION_ATTR));
        return doc;
    }

    /**
     * Read different parts of the configuration file.
     *
     * @param configurationDocument XML document representing the configuration
     *                              file.
     * @param isExtension           Flag stating if the configuration is an
     *                              extension.
     * @throws Exception If the configuration has errors.
     */
    private void readConfiguration(final Path cfgDirPath, Document configurationDocument, boolean isExtension)
            throws Exception
    {
        Element root = configurationDocument.getDocumentElement();
        // Read the global parameters
        Element global = (Element) root.getElementsByTagName(GLOBAL_ELEMENT).item(0);
        readParameters(global, isExtension);

        // Read the used transformation sets
        Element sets = (Element) root.getElementsByTagName(SETS_ELEMENT).item(0);
        if (isExtension)
        { // Sets are overridden by extension
            if (sets != null)
            {
                _availableGroups.clear();
                readSets(cfgDirPath, sets);
            }
        } else
        {
            // For root configuration, the sets element is mandatory.
            if (sets == null)
            {
                throw new Exception("Root configuration must have sets element!");
            }
            readSets(cfgDirPath, sets);
        }

        // Read the transformation groups definition and order
        Element groups = (Element) root.getElementsByTagName(GROUPS_ELEMENT).item(0);

        if (isExtension)
        {
            if (groups != null)
            { // Groups are overridden by extension
                _groups.clear();
                readGroups(groups);
            }
        } else
        {
            // For root configuration, the groups element is mandatory.
            if (groups == null)
            {
                throw new Exception("Root configuration must have groups element!");
            }
            readGroups(groups);
        }
    }

    /**
     * Parse the configuration file as an XML document and validate it against its
     * XSD schema.
     *
     * @param inputFileStrm Configuration file.
     * @param xsdSchema     XSD schema.
     * @return XML document representing the configuration file.
     * @throws Exception If the configuration file does not validate.
     */
    private Document parseAndValidate(InputStream inputFileStrm, Schema xsdSchema) throws Exception
    {
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        DocumentBuilder builder = factory.newDocumentBuilder();
        Document document = builder.parse(inputFileStrm);
        try
        {
            validate(document, xsdSchema);
        } catch (Exception e)
        {
            throw new Exception(format("Error: Configuration file is not well formatted: %s", e.getMessage()), e);
        }
        return document;
    }

    /**
     * Validate the configuration file with the XSD schema.
     *
     * @param document XML Document.
     * @param schema   XSD schema.
     * @throws SAXException If configuration file is not valid.
     * @throws IOException  If schema is not found.
     */
    private void validate(Document document, Schema schema) throws SAXException, IOException
    {
        Validator validator = schema.newValidator();
        validator.validate(new DOMSource(document));
    }

    private InputStream getCfgFileAsInputStream(Path cfgDirPath, String fileName) throws IOException
    {
        if (cfgDirPath != null)
        {
            final Path cfgFilePath = cfgDirPath.resolve(fileName);
            return Files.newInputStream(cfgFilePath);
        } else
        {
            final Path cfgFilePath = DEFAULT_CONFIG_DIR_IN_JAR_PATH.resolve(fileName);
            return Configuration.class.getResourceAsStream(cfgFilePath.toString());
        }
    }

    private Schema loadSchema(final Path cfgDirPath, final String name) throws SAXException, IOException
    {
        Schema schema;
        try (InputStream schemaStream = getCfgFileAsInputStream(cfgDirPath, name))
        {
            SchemaFactory sf = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
            schema = sf.newSchema(new StreamSource(schemaStream));
        }
        return schema;
    }

    /**
     * Get value of a parameter.
     *
     * @param key Key of the parameter.
     * @return Value of the parameter. Null if parameter doesn't exists.
     */
    public String getParameter(String key)
    {
        return _parameters.getOrDefault(key, null);
    }

    /**
     * Get boolean value of a parameter.
     *
     * @param key Key of the parameter.
     * @return Value of the parameter. False if parameter doesn't exists or its
     *         value is false.
     */
    public boolean getBooleanParameter(String key)
    {
        return _parameters.containsKey(key) && _parameters.get(key).equalsIgnoreCase(Xname.TRUE);
    }

    /**
     * Get the GPU specific configuration information.
     *
     * @return The GPU configuration object.
     */
    public AcceleratorConfiguration accelerator()
    {
        return _accelerator;
    }

    /**
     * Get all the group configuration information.
     *
     * @return List of group configuration.
     */
    public List<GroupConfiguration> getGroups()
    {
        return _groups;
    }

    /**
     * Get the current directive directive defined in the configuration.
     *
     * @return Current directive value.
     */
    public CompilerDirective getCurrentDirective()
    {
        return CompilerDirective.fromString(getParameter(DEFAULT_DIRECTIVE));
    }

    /**
     * Get the current target defined in the configuration or by the user on the
     * command line.
     *
     * @return Current target value.
     */
    public Target getCurrentTarget()
    {
        return Target.fromString(getParameter(DEFAULT_TARGET));
    }

    /**
     * Read all the transformation sets.
     *
     * @param sets Parent element "sets" for the transformation set.
     * @throws Exception If transformation set file does not exist or not well
     *                   formatted.
     */
    private void readSets(final Path cfgDirPath, Element sets) throws Exception
    {
        final Schema xsdSchema = loadSchema(cfgDirPath, SET_XSD);

        NodeList transformationSets = sets.getElementsByTagName(SET_ELEMENT);
        for (int i = 0; i < transformationSets.getLength(); ++i)
        {
            Element e = (Element) transformationSets.item(i);
            final String setName = e.getAttribute(NAME_ATTR);
            Document setDocument;
            try (InputStream setFile = getCfgFileAsInputStream(cfgDirPath, setName + XML_EXT))
            {
                setDocument = parseAndValidate(setFile, xsdSchema);
            } catch (Exception ex)
            {
                String errMsg = format("Failed to load transformation set \"%s\"", setName);
                throw new Exception(errMsg, ex);
            }
            Element root = setDocument.getDocumentElement();
            boolean isExternal = root.hasAttribute(JAR_ATTR);

            // Try to locate the external jar
            if (isExternal)
            {
                String externalJar = root.getAttribute(JAR_ATTR);
                URLClassLoader loader = loadExternalJar(externalJar);
                readTransformations(setName, root, loader);
            } else
            {
                readTransformations(setName, root, null);
            }
        }
    }

    /**
     * Load the give jar file if located in one of the defined CLAW_TRANS_SET_PATH
     *
     * @param jarFile Name of the jar file to load with .jar extension.
     * @return The URLClassLoader associated with the jar file if found. Null
     *         otherwise.
     * @throws FileNotFoundException If jar file not found.
     * @throws MalformedURLException If classpath is malformed.
     */
    private URLClassLoader loadExternalJar(String jarFile) throws FileNotFoundException, MalformedURLException
    {
        if (_transSetPaths.length == 0)
        {
            throw new FileNotFoundException(
                    "No path defined in " + CLAW_TRANS_SET_PATH + ". Unable to load transformation set: " + jarFile);
        }
        URLClassLoader external;
        for (String path : _transSetPaths)
        {
            Path jar = Paths.get(path, jarFile);
            if (jar.toFile().exists())
            {
                external = new URLClassLoader(new URL[] { new URL("file://" + jar.toString()) },
                        this.getClass().getClassLoader());
                return external;
            }
        }
        throw new FileNotFoundException("Cannot find jar file " + jarFile);
    }

    /**
     * Read all the parameter element and store their key/value pair in the map.
     *
     * @param globalElement Parent element "global" for the parameters.
     */
    private void readParameters(Element globalElement, boolean overwrite)
    {
        NodeList parameters = globalElement.getElementsByTagName(PARAMETER_ELEMENT);
        for (int i = 0; i < parameters.getLength(); ++i)
        {
            Element e = (Element) parameters.item(i);
            String key = e.getAttribute(KEY_ATTR);
            if (overwrite)
            { // Parameter overwritten
                _parameters.remove(key);
            }
            _parameters.put(key, e.getAttribute(VALUE_ATTR));
        }
    }

    /**
     * Read all the transformation element and store their information in a list of
     * available GroupConfiguration objects.
     *
     * @param transformationsNode Parent element "groups" for the group elements.
     * @throws Exception Group information not valid.
     */
    private void readTransformations(String setName, Element transformationsNode, URLClassLoader loader)
            throws Exception
    {
        NodeList transformationElements = transformationsNode.getElementsByTagName(TRANSFORMATION_ELEMENT);
        for (int i = 0; i < transformationElements.getLength(); ++i)
        {
            if (transformationElements.item(i).getNodeType() == Node.ELEMENT_NODE)
            {
                Element g = (Element) transformationElements.item(i);
                String name = g.getAttribute(NAME_ATTR);
                String type = g.getAttribute(TYPE_ATTR);
                // Read group type
                GroupConfiguration.GroupType gType;
                switch (type)
                {
                case DEPENDENT_GR_TYPE:
                    gType = GroupConfiguration.GroupType.DEPENDENT;
                    break;
                case INDEPENDENT_GR_TYPE:
                    gType = GroupConfiguration.GroupType.INDEPENDENT;
                    break;
                default:
                    throw new Exception("Invalid group type specified.");
                }
                // Read transformation class path
                String cPath = g.getAttribute(CLASS_ATTR);
                if (cPath == null || cPath.isEmpty())
                {
                    throw new Exception("Invalid group class transformation definition.");
                }
                // Read trigger type
                String triggerTypeAttr = g.getAttribute(TRIGGER_ATTR);
                GroupConfiguration.TriggerType triggerType;
                switch (triggerTypeAttr)
                {
                case DIRECTIVE_TR_TYPE:
                    triggerType = GroupConfiguration.TriggerType.DIRECTIVE;
                    break;
                case TRANSLATION_UNIT_TR_TYPE:
                    triggerType = GroupConfiguration.TriggerType.TRANSLATION_UNIT;
                    break;
                default:
                    throw new Exception("Invalid trigger type specified.");
                }
                String directive = null;
                if (triggerType == GroupConfiguration.TriggerType.DIRECTIVE)
                {
                    directive = g.getAttribute(DIRECTIVE_ATTR);
                    if (directive == null)
                    {
                        throw new Exception(
                                "Transformation with trigger type directive " + "must have the directive attribute.");
                    }
                }
                // Find actual class
                Class<?> transClass;
                try
                {
                    // Check if class is there
                    if (loader != null)
                    {
                        transClass = Class.forName(cPath, true, loader);
                    } else
                    {
                        transClass = Class.forName(cPath);
                    }
                } catch (ClassNotFoundException e)
                {
                    throw new Exception("Transformation class " + cPath + " not available");
                }

                // Check that translation unit trigger type are not block transformation
                if (triggerType == GroupConfiguration.TriggerType.TRANSLATION_UNIT
                        && (transClass.getSuperclass() == BlockTransformation.class
                                || transClass.getSuperclass() == ClawBlockTransformation.class))
                {
                    throw new Exception("Translation unit trigger cannot be block " + "transformation");
                }

                // Store group configuration
                if (_availableGroups.containsKey(name))
                {
                    throw new Exception("Transformation " + name + " has name conflict!");
                }
                _availableGroups.put(name,
                        new GroupConfiguration(setName, name, gType, triggerType, cPath, directive, transClass));
            }
        }
    }

    /**
     * Read defined transformation groups in configuration. Order determines
     * application order of transformation.
     *
     * @param groupsNode The "groups" element of the configuration.
     * @throws Exception If the group name is not an available in any transformation
     *                   set.
     */
    private void readGroups(Element groupsNode) throws Exception
    {
        NodeList groupElements = groupsNode.getElementsByTagName(GROUP_ELEMENT);
        for (int i = 0; i < groupElements.getLength(); ++i)
        {
            if (groupElements.item(i).getNodeType() == Node.ELEMENT_NODE)
            {
                Element g = (Element) groupElements.item(i);
                String name = g.getAttribute(NAME_ATTR);
                if (_availableGroups.containsKey(name))
                {
                    GroupConfiguration gc = _availableGroups.get(name);
                    if (_groups.contains(gc))
                    {
                        throw new Exception("Duplicated transformation group creation: " + name);
                    }
                    _groups.add(_availableGroups.get(name));
                } else
                {
                    throw new Exception("No transformation found for " + name + " in available transformation sets!");
                }
            }
        }
    }

    /**
     * Set the user defined target in the configuration.
     *
     * @param option Option passed as argument. Has priority over configuration
     *               file.
     */
    private void setUserDefinedTarget(String option)
    {
        if (option != null)
        {
            _parameters.put(DEFAULT_TARGET, option);
        }
    }

    /**
     * Check if SCA forward update is applied at input.
     *
     * @return True if applied.
     */
    public boolean updateAtInput()
    {
        return getParameter(SCA_FORWARD_UPDATE_DIRECTION).equals(SCA_FORWARD_UPDATE_IN)
                || getParameter(SCA_FORWARD_UPDATE_DIRECTION).equals(SCA_FORWARD_UPDATE_INOUT);
    }

    /**
     * Check if SCA forward update is applied at output.
     *
     * @return True if applied.
     */
    public boolean updateAtOutput()
    {
        return getParameter(SCA_FORWARD_UPDATE_DIRECTION).equals(SCA_FORWARD_UPDATE_OUT)
                || getParameter(SCA_FORWARD_UPDATE_DIRECTION).equals(SCA_FORWARD_UPDATE_INOUT);
    }

    /**
     * Check if SCA serialization is applied at read savepoints.
     *
     * @return True if applied.
     */
    public boolean seriliazeRead()
    {
        return getParameter(SCA_SERIALIZATION_ENABLED_DIRECTION).equalsIgnoreCase(SCA_SERIALIZATION_READ)
                || getParameter(SCA_SERIALIZATION_ENABLED_DIRECTION).equalsIgnoreCase(SCA_SERIALIZATION_READ_WRITE);
    }

    /**
     * Check if SCA serialization is applied at write savepoints.
     *
     * @return True if applied.
     */
    public boolean seriliazeWrite()
    {
        return getParameter(SCA_SERIALIZATION_ENABLED_DIRECTION).equalsIgnoreCase(SCA_SERIALIZATION_WRITE)
                || getParameter(SCA_SERIALIZATION_ENABLED_DIRECTION).equalsIgnoreCase(SCA_SERIALIZATION_READ_WRITE);
    }

    /**
     * Set the user defined directive in the configuration.
     *
     * @param option Option passed as argument. Has priority over configuration
     *               file.
     */
    private void setUserDefineDirective(String option)
    {
        if (option != null)
        {
            _parameters.put(DEFAULT_DIRECTIVE, option);
        }
    }

    /**
     * Enable the force pure option.
     */
    public void setForcePure()
    {
        _forcePure = true;
    }

    /**
     * Check whether the force pure option is enabled or not.
     *
     * @return If true, the function is enabled. Disabled otherwise.
     */
    public boolean isForcePure()
    {
        return _forcePure;
    }

    /**
     * Check whether the configuration file version is high enough with the compiler
     * version.
     *
     * @param configVersion Version string from the configuration file.
     * @throws Exception If the configuration version is not high enough.
     */
    protected void checkVersion(String configVersion) throws Exception
    {
        int[] configMajMin = getMajorMinor(configVersion);
        int[] compilerMajMin = getMajorMinor(ClawVersion.VERSION);

        if (configMajMin[0] < compilerMajMin[0])
        {
            throw new Exception("Configuration version is smaller than " + "CLAW Compiler version: " + compilerMajMin[0]
                    + "." + compilerMajMin[1]);
        }
    }

    /**
     * Extract major and minor version number from the full version String.
     *
     * @param version Full version String. Format: major.minor.fixes
     * @return Two dimensional array with the major number at index 0 and the minor
     *         at index 1.
     * @throws Exception If the version String is not of the correct format.
     */
    protected int[] getMajorMinor(String version) throws Exception
    {
        Pattern p = Pattern.compile("^(\\d+)\\.(\\d+)\\.?(\\d+)?");
        Matcher m = p.matcher(version);
        if (!m.matches())
        {
            throw new Exception("Configuration version not well formatted");
        }

        int major = Integer.parseInt(m.group(1));
        int minor = Integer.parseInt(m.group(2));
        return new int[] { major, minor };
    }

    @Override
    public String toString()
    {
        StringBuilder sb = new StringBuilder();
        sb.append(format("- CLAW Compiler configuration -%n\n"));
        sb.append(format("Default directive: %s%n\n", getCurrentDirective()));
        sb.append(format("Default target: %s%n\n", getCurrentTarget()));
        sb.append("Current transformation order:\n");
        int i = 0;
        sb.append(format("  %3s %-20s %-20s %-15s %-20s %-10s %-60s%n\n", "Id", "set", "name", "type", "trigger",
                "directive", "class"));
        sb.append(format("  %3s %-20s %-20s %-15s %-20s %-10s %-60s%n\n", "--", "---", "----", "----", "-------",
                "---------", "-----"));
        for (GroupConfiguration g : getGroups())
        {
            sb.append(format("  %2d) %-20s %-20s %-15s %-20s %-10s %-60s%n\n", i, g.getSetName(), g.getName(),
                    g.getType(), g.getTriggerType(),
                    g.getTriggerType() == GroupConfiguration.TriggerType.DIRECTIVE ? g.getDirective() : "-",
                    g.getTransformationClassName()));
            ++i;
        }
        final String s = sb.toString();
        return s;
    }

    /**
     * Override a configuration key-value parameter.
     *
     * @param key   Key of the parameter.
     * @param value Value of the parameter.
     */
    public void overrideConfigurationParameter(String key, String value)
    {
        if (value != null && !value.isEmpty())
        {
            _parameters.remove(key.toLowerCase());
            _parameters.put(key, value);
        }
    }

    /**
     * Get the global model configuration.
     *
     * @return Global model config instance.
     */
    public ModelConfig getModelConfig()
    {
        return _modelConfig;
    }
}
