/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.common;

import cx2x.translator.language.helper.accelerator.AcceleratorDirective;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import java.util.ArrayList;
import java.util.List;

/**
 * Class to help interaction with the configuration files.
 *
 * @author clementval
 */
public class ConfigurationHelper {

  private static final String DEPENDENT_GR_TYPE = "dependent";
  private static final String INDEPENDENT_GR_TYPE = "independent";

  // Elements and attributes constant name
  private static final String GROUP_ELEMENT = "group";
  private static final String TARGET_ELEMENT = "target";
  private static final String CLASS_ATTR = "class";
  private static final String DEFAULT_ATTR = "default";
  private static final String NAME_ATTR = "name";
  private static final String TYPE_ATTR = "type";

  private static Document document = null;


  /**
   * read all the group element of the configuration file.
   * @param path Path to the configuration file.
   * @return A list of String arrays with 3 elements. 0) type, 1) name, 2) class
   * @throws Exception When configuration file cannot be read
   */
  public static List<GroupConfiguration> readGroups(String path)
      throws Exception
  {
    List<GroupConfiguration> groups = new ArrayList<>();
    Element root = open(path);
    NodeList groupElements = root.getElementsByTagName(GROUP_ELEMENT);
    for (int i = 0; i < groupElements.getLength(); ++i){
      if(groupElements.item(i).getNodeType() == Node.ELEMENT_NODE){
        Element g = (Element) groupElements.item(i);
        String name = g.getAttribute(NAME_ATTR);
        String type = g.getAttribute(TYPE_ATTR);
        GroupConfiguration.GroupType gType;
        switch (type) {
          case DEPENDENT_GR_TYPE:
            gType = GroupConfiguration.GroupType.DEPENDENT;
            break;
          case INDEPENDENT_GR_TYPE:
            gType = GroupConfiguration.GroupType.INDEPENDENT;
            break;
          default:
            throw new Exception("Invalid group type specified.");
        }
        String cPath = g.getAttribute(CLASS_ATTR);
        if(cPath == null || cPath.isEmpty()){
          throw new Exception("Invalid group class transformation definition.");
        }
        Class transClass;
        try {
          // Check if class is there
          transClass = Class.forName(cPath);
        } catch (ClassNotFoundException e) {
          throw new Exception("Transformation class " + cPath +
              " not available");
        }
        groups.add(new GroupConfiguration(name, gType, cPath, transClass));
      }
    }
    return groups;
  }

  /**
   * Read the default target from the configuration file.
   * @param path Path to the configuration file.
   * @return Enum value corresponding to the target. NONE if nothing defined.
   */
  public static AcceleratorDirective readDefaultTarget(String path) {
    try {
      Element root = open(path);
      NodeList targets = root.getElementsByTagName(TARGET_ELEMENT);
      if(targets.getLength() != 1){
        return AcceleratorDirective.NONE;
      }
      Element target = (Element)targets.item(0);
      String defaultTarget = target.getAttribute(DEFAULT_ATTR);
      return AcceleratorDirective.fromString(defaultTarget);
    } catch (Exception ignored){
      return AcceleratorDirective.NONE;
    }
  }

  /**
   * Open the configuration file and get the root element.
   * @param configPath Path to the configuration file.
   * @return The root element.
   * @throws Exception When the XML file cannot be opened.
   */
  private static Element open(String configPath) throws Exception {
    if(document == null) {
      DocumentBuilderFactory factory =
          DocumentBuilderFactory.newInstance();
      DocumentBuilder builder = factory.newDocumentBuilder();
      document = builder.parse(configPath);
    }
    return document.getDocumentElement();
  }
}
