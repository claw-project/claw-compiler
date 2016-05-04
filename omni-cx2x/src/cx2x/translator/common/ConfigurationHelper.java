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
  public static List<String[]> readGroups(String path) throws Exception {
    List<String[]> groups = new ArrayList<>();
    Element root = open(path);
    NodeList groupElements = root.getElementsByTagName(GROUP_ELEMENT);
    for (int i = 0; i < groupElements.getLength(); ++i){
      if(groupElements.item(i).getNodeType() == Node.ELEMENT_NODE){
        Element g = (Element) groupElements.item(i);
        String name = g.getAttribute(NAME_ATTR);
        String type = g.getAttribute(TYPE_ATTR);
        String gClass = g.getAttribute(CLASS_ATTR);
        groups.add(new String[]{name, type, gClass});
      }
    }
    return groups;
  }

  /**
   * Read the default target from the configuration file.
   * @param path Path to the configuration file.
   * @return Enum value corresponding to the target. NONE if nothing defined.
   * @throws Exception When configuration file cannot be read
   */
  public static AcceleratorDirective readDefaultTarget(String path)
      throws Exception
  {
    Element root = open(path);
    NodeList targets = root.getElementsByTagName(TARGET_ELEMENT);
    if(targets.getLength() != 1){
      return AcceleratorDirective.NONE;
    }
    Element target = (Element)targets.item(0);
    String defaultTarget = target.getAttribute(DEFAULT_ATTR);
    return AcceleratorDirective.fromString(defaultTarget);
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
