/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.x2t.configuration;

import claw.ClawVersion;
import claw.shenron.transformation.BlockTransformation;
import claw.tatsu.common.CompilerDirective;
import claw.tatsu.common.Target;
import claw.tatsu.directive.generator.DirectiveGenerator;
import claw.wani.transformation.ClawBlockTransformation;
import claw.wani.x2t.configuration.openacc.OpenAccConfiguration;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import javax.xml.validation.Validator;
import java.io.File;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Configuration class is used to read the configuration file and expose its
 * information to the translator.
 *
 * @author clementval
 */
public class Configuration {

  public static final String TRANSLATOR = "translator";
  // Specific keys
  private static final String DEFAULT_TARGET = "default_target";
  private static final String DEFAULT_DIRECTIVE = "default_directive";
  private static final String DEFAULT_CONFIG_FILE = "claw-default.xml";
  private static final String XML_EXT = ".xml";
  private static final String CONFIG_XSD = "claw_config.xsd";
  private static final String SET_XSD = "claw_transformation_set.xsd";
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
  // env var
  private static final String CLAW_TRANS_SET_PATH = "CLAW_TRANS_SET_PATH";
  private static Configuration _instance = null;
  private String _configuration_path;
  private Map<String, String> _parameters;
  private List<GroupConfiguration> _groups;
  private Map<String, GroupConfiguration> _availableGroups;
  private OpenAccConfiguration _openacc;
  private String[] _transSetPaths;
  private boolean _forcePure = false;
  private int _maxColumns; // Max column for code formatting

  private DirectiveGenerator _generator;

  /**
   * private ctor
   */
  private Configuration() {
  }

  /**
   * Get the unique instance.
   *
   * @return Unique Configuration instance.
   */
  public static Configuration get() {
    if(_instance == null) {
      _instance = new Configuration();
    }
    return _instance;
  }

  /**
   * Constructs basic configuration object.
   *
   * @param dir    Accelerator directive language.
   * @param target Target architecture.
   */
  public void init(CompilerDirective dir, Target target) {
    _parameters = new HashMap<>();
    if(dir != null) {
      _parameters.put(DEFAULT_DIRECTIVE, dir.toString());
    }
    if(target != null) {
      _parameters.put(DEFAULT_TARGET, target.toString());
    }
    _openacc = new OpenAccConfiguration(_parameters);
    _groups = new ArrayList<>();
    _availableGroups = new HashMap<>();
    _configuration_path = null;
  }

  /**
   * Constructs a new configuration object from the give configuration file.
   *
   * @param configPath     Path to the configuration files and XSD schemas.
   * @param userConfigFile Path to the alternative configuration.
   * @throws Exception If configuration cannot be loaded properly.
   */
  public void load(String configPath, String userConfigFile)
      throws Exception
  {
    _configuration_path = configPath;
    _parameters = new HashMap<>();
    _groups = new ArrayList<>();
    _availableGroups = new HashMap<>();
    boolean readDefault = true;
    Document userConf = null;

    // Read the environment variable for external transformation sets
    _transSetPaths = new String[0];

    if(System.getenv(CLAW_TRANS_SET_PATH) != null) {
      _transSetPaths = System.getenv(CLAW_TRANS_SET_PATH).split(";");
    }

    // Configuration has been given by the user. Read it first.
    if(userConfigFile != null) {
      File userConfiguration = Paths.get(userConfigFile).toFile();
      userConf = validateConfiguration(userConfiguration);
      readDefault = isExtension(userConf);
    }

    if(readDefault) {
      // There is no user defined configuration or it is just an extension.
      File defaultConfigFile =
          Paths.get(_configuration_path, DEFAULT_CONFIG_FILE).toFile();
      Document defaultConf = validateConfiguration(defaultConfigFile);
      readConfiguration(defaultConf, false);
      if(userConf != null) { // Read extension
        readConfiguration(userConf, true);
      }
    } else {
      // User defined configuration is a full configuration.
      // Then the default one is not read.
      readConfiguration(userConf, false);
    }

    _openacc = new OpenAccConfiguration(_parameters);
  }

  /**
   * Check whether the configuration file is an extension of the default
   * configuration or if it is a standalone configuration.
   *
   * @param configurationDocument XML document representing the configuration
   *                              file.
   * @return True if the configuration is an extension. False otherwise.
   */
  private boolean isExtension(Document configurationDocument) {
    Element root = configurationDocument.getDocumentElement();
    Element global =
        (Element) root.getElementsByTagName(GLOBAL_ELEMENT).item(0);
    return global != null && global.hasAttribute(TYPE_ATTR)
        && global.getAttribute(TYPE_ATTR).equals(EXT_CONF_TYPE);
  }

  /**
   * Validate configuration file against its XSD schema.
   *
   * @param configurationFile File object representing the configuration file.
   * @return XML Document of the configuration file.
   * @throws Exception If the configuration file cannot be validated.
   */
  private Document validateConfiguration(File configurationFile)
      throws Exception
  {
    File configurationSchema =
        Paths.get(_configuration_path, CONFIG_XSD).toFile();

    Document doc = parseAndValidate(configurationFile, configurationSchema);
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
  private void readConfiguration(Document configurationDocument,
                                 boolean isExtension) throws Exception
  {
    Element root = configurationDocument.getDocumentElement();
    // Read the global parameters
    Element global =
        (Element) root.getElementsByTagName(GLOBAL_ELEMENT).item(0);
    readParameters(global, isExtension);

    // Read the used transformation sets
    Element sets =
        (Element) root.getElementsByTagName(SETS_ELEMENT).item(0);
    if(isExtension) { // Sets are overridden by extension
      if(sets != null) {
        _availableGroups.clear();
        readSets(sets);
      }
    } else {
      // For root configuration, the sets element is mandatory.
      if(sets == null) {
        throw new Exception("Root configuration must have sets element!");
      }
      readSets(sets);
    }

    // Read the transformation groups definition and order
    Element groups =
        (Element) root.getElementsByTagName(GROUPS_ELEMENT).item(0);

    if(isExtension) {
      if(groups != null) { // Groups are overridden by extension
        _groups.clear();
        readGroups(groups);
      }
    } else {
      // For root configuration, the groups element is mandatory.
      if(groups == null) {
        throw new Exception("Root configuration must have groups element!");
      }
      readGroups(groups);
    }
  }

  /**
   * Parse the configuration file as an XML document and validate it against its
   * XSD schema.
   *
   * @param xmlFile   File object pointing to the configuration file.
   * @param xsdSchema File object pointing to the XSD schema.
   * @return XML document representing the configuration file.
   * @throws Exception If the configuration file does not validate.
   */
  private Document parseAndValidate(File xmlFile, File xsdSchema)
      throws Exception
  {
    DocumentBuilderFactory factory =
        DocumentBuilderFactory.newInstance();
    DocumentBuilder builder = factory.newDocumentBuilder();
    Document document = builder.parse(xmlFile);

    try {
      validate(document, xsdSchema);
    } catch(Exception e) {
      throw new Exception("Error: Configuration file " + xmlFile.getName()
          + " is not well formatted: " + e.getMessage());
    }
    return document;
  }

  /**
   * Validate the configuration file with the XSD schema.
   *
   * @param xsd File representing the XSD schema.
   * @throws Exception If configuration file is not valid.
   */
  private void validate(Document document, File xsd)
      throws Exception
  {
    SchemaFactory factory =
        SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
    Source schemaFile = new StreamSource(xsd);
    Schema schema = factory.newSchema(schemaFile);
    Validator validator = schema.newValidator();
    validator.validate(new DOMSource(document));
  }

  /**
   * Get value of a parameter.
   *
   * @param key Key of the parameter.
   * @return Value of the parameter. Null if parameter doesn't exists.
   */
  public String getParameter(String key) {
    return (_parameters.containsKey(key)) ? _parameters.get(key) : null;
  }

  /**
   * Get the OpenACC specific configuration information.
   *
   * @return The OpenACC configuration object.
   */
  public OpenAccConfiguration openACC() {
    return _openacc;
  }

  /**
   * Get all the group configuration information.
   *
   * @return List of group configuration.
   */
  public List<GroupConfiguration> getGroups() {
    return _groups;
  }

  /**
   * Get the current directive directive defined in the configuration.
   *
   * @return Current directive value.
   */
  public CompilerDirective getCurrentDirective() {
    return CompilerDirective.fromString(getParameter(DEFAULT_DIRECTIVE));
  }

  /**
   * Get the current target defined in the configuration or by the user on
   * the command line.
   *
   * @return Current target value.
   */
  public Target getCurrentTarget() {
    return Target.fromString(getParameter(DEFAULT_TARGET));
  }

  /**
   * Read all the transformation sets.
   *
   * @param sets Parent element "sets" for the transformation set.
   * @throws Exception If transformation set file does not exist or not well
   *                   formatted.
   */
  private void readSets(Element sets) throws Exception {
    File xsdSchema = Paths.get(_configuration_path, SET_XSD).toFile();

    NodeList transformation_sets = sets.getElementsByTagName(SET_ELEMENT);
    for(int i = 0; i < transformation_sets.getLength(); ++i) {
      Element e = (Element) transformation_sets.item(i);
      String setName = e.getAttribute(NAME_ATTR);
      File setFile = Paths.get(_configuration_path, setName + XML_EXT).toFile();
      if(!setFile.exists()) {
        throw new Exception("Transformation set " + setName
            + " cannot be found!");
      }

      Document setDocument = parseAndValidate(setFile, xsdSchema);
      Element root = setDocument.getDocumentElement();
      boolean isExternal = root.hasAttribute(JAR_ATTR);

      // Try to locate the external jar
      if(isExternal) {
        String externalJar = root.getAttribute(JAR_ATTR);
        URLClassLoader loader = loadExternalJar(externalJar);
        readTransformations(setName, root, loader);
      } else {
        readTransformations(setName, root, null);
      }
    }
  }

  /**
   * Load the give jar file if located in one of the defined CLAW_TRANS_SET_PATH
   *
   * @param jarFile Name of the jar file to load with .jar extension.
   * @return The URLClassLoader associated with the jar file if found. Null
   * otherwise.
   * @throws Exception If no path defined
   */
  private URLClassLoader loadExternalJar(String jarFile) throws Exception {
    if(_transSetPaths.length == 0) {
      throw new Exception("No path defined in " + CLAW_TRANS_SET_PATH
          + ". Unable to load transformation set: " + jarFile);
    }
    URLClassLoader external;
    for(String path : _transSetPaths) {
      Path jar = Paths.get(path, jarFile);
      if(jar.toFile().exists()) {
        external = new URLClassLoader(new URL[]{
            new URL("file://" + jar.toString())},
            this.getClass().getClassLoader());
        return external;
      }
    }
    throw new Exception("Cannot find jar file " + jarFile);
  }

  /**
   * Read all the parameter element and store their key/value pair in the map.
   *
   * @param globalElement Parent element "global" for the parameters.
   */
  private void readParameters(Element globalElement, boolean overwrite) {
    NodeList parameters = globalElement.getElementsByTagName(PARAMETER_ELEMENT);
    for(int i = 0; i < parameters.getLength(); ++i) {
      Element e = (Element) parameters.item(i);
      String key = e.getAttribute(KEY_ATTR);
      if(overwrite && _parameters.containsKey(key)) { // Parameter overwritten
        _parameters.remove(key);
      }
      _parameters.put(key, e.getAttribute(VALUE_ATTR));
    }
  }

  /**
   * Read all the transformation element and store their information in a list
   * of available GroupConfiguration objects.
   *
   * @param transformationsNode Parent element "groups" for the group elements.
   * @throws Exception Group information not valid.
   */
  private void readTransformations(String setName, Element transformationsNode,
                                   URLClassLoader loader) throws Exception
  {
    NodeList transformationElements =
        transformationsNode.getElementsByTagName(TRANSFORMATION_ELEMENT);
    for(int i = 0; i < transformationElements.getLength(); ++i) {
      if(transformationElements.item(i).getNodeType() == Node.ELEMENT_NODE) {
        Element g = (Element) transformationElements.item(i);
        String name = g.getAttribute(NAME_ATTR);
        String type = g.getAttribute(TYPE_ATTR);
        // Read group type
        GroupConfiguration.GroupType gType;
        switch(type) {
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
        if(cPath == null || cPath.isEmpty()) {
          throw new Exception("Invalid group class transformation definition.");
        }
        // Read trigger type
        String trigger_type = g.getAttribute(TRIGGER_ATTR);
        GroupConfiguration.TriggerType triggerType;
        switch(trigger_type) {
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
        if(triggerType == GroupConfiguration.TriggerType.DIRECTIVE) {
          directive = g.getAttribute(DIRECTIVE_ATTR);
          if(directive == null) {
            throw new Exception("Transformation with trigger type directive " +
                "must have the directive attribute.");
          }
        }
        // Find actual class
        Class transClass;
        try {
          // Check if class is there
          if(loader != null) {
            transClass = Class.forName(cPath, true, loader);
          } else {
            transClass = Class.forName(cPath);
          }
        } catch(ClassNotFoundException e) {
          throw new Exception("Transformation class " + cPath +
              " not available");
        }

        // Check that translation unit trigger type are not block transformation
        if(triggerType == GroupConfiguration.TriggerType.TRANSLATION_UNIT
            && (transClass.getSuperclass() == BlockTransformation.class
            || transClass.getSuperclass() == ClawBlockTransformation.class))
        {
          throw new Exception("Translation unit trigger cannot be block " +
              "transformation");
        }

        // Store group configuration
        if(_availableGroups.containsKey(name)) {
          throw new Exception("Transformation " + name + " has name conflict!");
        }
        _availableGroups.put(name, new GroupConfiguration(setName, name, gType,
            triggerType, cPath, directive, transClass));
      }
    }
  }

  /**
   * Read defined transformation groups in configuration. Order determines
   * application order of transformation.
   *
   * @param groupsNode The "groups" element of the configuration.
   * @throws Exception If the group name is not an available in any
   *                   transformation set.
   */
  private void readGroups(Element groupsNode) throws Exception {
    NodeList groupElements = groupsNode.getElementsByTagName(GROUP_ELEMENT);
    for(int i = 0; i < groupElements.getLength(); ++i) {
      if(groupElements.item(i).getNodeType() == Node.ELEMENT_NODE) {
        Element g = (Element) groupElements.item(i);
        String name = g.getAttribute(NAME_ATTR);
        if(_availableGroups.containsKey(name)) {
          GroupConfiguration gc = _availableGroups.get(name);
          if(_groups.contains(gc)) {
            throw new Exception("Duplicated transformation group creation: "
                + name);
          }
          _groups.add(_availableGroups.get(name));
        } else {
          throw new Exception("No transformation found for " + name
              + " in available transformation sets!");
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
  public void setUserDefinedTarget(String option) {
    if(option != null) {
      _parameters.put(DEFAULT_TARGET, option);
    }
  }

  /**
   * Set the user defined directive in the configuration.
   *
   * @param option Option passed as argument. Has priority over configuration
   *               file.
   */
  public void setUserDefineDirective(String option) {
    if(option != null) {
      _parameters.put(DEFAULT_DIRECTIVE, option);
    }
  }

  /**
   * Enable the force pure option.
   */
  public void setForcePure() {
    _forcePure = true;
  }

  /**
   * Check whether the force pure option is enabled or not.
   *
   * @return If true, the function is enabled. Disabled otherwise.
   */
  public boolean isForcePure() {
    return _forcePure;
  }

  /**
   * Check whether the configuration file version is high enough with the
   * compiler version.
   *
   * @param configVersion Version string from the configuration file.
   * @throws Exception If the configuration version is not high enough.
   */
  protected void checkVersion(String configVersion) throws Exception {
    int[] configMajMin = getMajorMinor(configVersion);
    int[] compilerMajMin = getMajorMinor(ClawVersion.getVersion());

    if(configMajMin[0] < compilerMajMin[0]
        || (configMajMin[0] == compilerMajMin[0]
        && configMajMin[1] < compilerMajMin[1]))
    {
      throw new Exception("Configuration version is smaller than " +
          "CLAW Compiler version: " + compilerMajMin[0] + "." +
          compilerMajMin[1]);
    }
  }

  /**
   * Extract major and minor version number from the full version String.
   *
   * @param version Full version String. Format: major.minor.fixes
   * @return Two dimensional array with the major number at index 0 and the
   * minor at index 1.
   * @throws Exception If the version String is not of the correct format.
   */
  protected int[] getMajorMinor(String version) throws Exception {
    Pattern p = Pattern.compile("^(\\d+)\\.(\\d+)\\.?(\\d+)?");
    Matcher m = p.matcher(version);
    if(!m.matches()) {
      throw new Exception("Configuration version not well formatted");
    }

    int major = Integer.parseInt(m.group(1));
    int minor = Integer.parseInt(m.group(2));
    return new int[]{major, minor};
  }

  /**
   * Get the defined max column parameter.
   *
   * @return Int value representing the max column.
   */
  public int getMaxColumns() {
    return _maxColumns;
  }

  /**
   * Set the max column value.
   *
   * @param value New value of the max column parameter.
   */
  public void setMaxColumns(int value) {
    _maxColumns = value;
  }

  /**
   * Display the loaded configuration.
   */
  public void displayConfig() {
    System.out.println("- CLAW Compiler configuration -\n");
    System.out.println("Default directive directive: " +
        getCurrentDirective() + "\n");
    System.out.println("Default target: " + getCurrentTarget() + "\n");
    System.out.println("Current transformation order:");
    int i = 0;
    System.out.printf("  %3s %-20s %-20s %-15s %-20s %-10s %-60s\n",
        "Id", "set", "name", "type", "trigger", "directive", "class");
    System.out.printf("  %3s %-20s %-20s %-15s %-20s %-10s %-60s\n",
        "--", "---", "----", "----", "-------", "---------", "-----");
    for(GroupConfiguration g : getGroups()) {
      System.out.printf("  %2d) %-20s %-20s %-15s %-20s %-10s %-60s\n",
          i++, g.getSetName(), g.getName(), g.getType(), g.getTriggerType(),
          g.getTriggerType() == GroupConfiguration.TriggerType.DIRECTIVE
              ? g.getDirective() : "-", g.getTransformationClassName());
    }
  }
}
