/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.pragma;

import java.util.ArrayList;

/**
 * A ClawMapping object holds the loop-extract mapping option representation
 * var_list:mapping_list
 *
 * @author clementval
 */
 
public class ClawMapping {

  private ArrayList<String> _mappedVariables = null;
  private ArrayList<String> _mappingVariables = null;

  /**
   * ClawMapping ctor.
   * @param mappingClause inner part of the mapping clause like
   *                      var_list:mapping_list
   */
  public ClawMapping(String mappingClause){
    _mappedVariables = new ArrayList<String>();
    _mappingVariables = new ArrayList<String>();

    String[] parts = mappingClause.split(":");
    if(parts.length != 2) {
      // TODO throw exception mappingClause is wrong
    }
    String[] vars = parts[0].split(",");
    String[] mappings = parts[1].split(",");
    if(vars.length == 0 || mappings.length == 0){
      // TODO throw exception mappingClause is wrong
      System.err.println("Fatal error mapping !!");
    }
    for(String var : vars){
      _mappedVariables.add(var);
    }
    for(String mapping : mappings){
      _mappingVariables.add(mapping);
    }
  }

  /**
   * Get a list of all mapping variables.
   * @return List of mapping variable as String.
   */
  public ArrayList<String> getMappingVariables(){
    return _mappingVariables;
  }

  /**
   * Get a list of all mapped variables.
   * @return List of mapped variable as String.
   */
  public ArrayList<String> getMappedVariables(){
    return _mappedVariables;
  }

  /**
   * get the number of dimension mapped in this ClawMapping object
   * @return the number of dimension to be mapped
   */
  public int getMappedDimensions(){
    return _mappingVariables.size();
  }

}
