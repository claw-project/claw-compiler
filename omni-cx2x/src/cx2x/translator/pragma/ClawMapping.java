/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.pragma;

import java.util.List;
import java.util.ArrayList;
import java.util.Collections;

/**
 * A ClawMapping object holds the loop-extract mapping option representation
 * var_list:mapping_list
 *
 * @author clementval
 */
 
public class ClawMapping {

  private List<String> _mappedVariables = null;
  private List<ClawMappingVar> _mappingVariables = null;


  private static final String VAR_SEPARATOR = ",";
  private static final String MAPPING_SEPARATOR = "/";

  /**
   * ClawMapping ctor.
   * @param mappingClause inner part of the mapping clause like
   *                      var_list:mapping_list
   */
  public ClawMapping(String mappingClause){
    _mappedVariables = new ArrayList<>();
    _mappingVariables = new ArrayList<>();

    String[] parts = mappingClause.split(":");
    if(parts.length != 2) {
      // TODO throw exception mappingClause is wrong
    }
    String[] vars = parts[0].split(VAR_SEPARATOR);
    String[] mappings = parts[1].split(VAR_SEPARATOR);
    if(vars.length == 0 || mappings.length == 0){
      // TODO throw exception mappingClause is wrong
      System.err.println("Fatal error mapping !!");
    }
    for (String mapping : mappings) {
      String[] advancedMapping = mapping.split(MAPPING_SEPARATOR);
      if(advancedMapping.length == 0 || advancedMapping.length > 2){
        // TODO throw exception mappingClause if wrong.
      }
      if(advancedMapping.length > 1){
        _mappingVariables.add(
            new ClawMappingVar(advancedMapping[0], advancedMapping[1])
        );
      } else {
        _mappingVariables.add(
            new ClawMappingVar(advancedMapping[0], advancedMapping[0])
        );
      }
    }
    Collections.addAll(_mappedVariables, vars);
  }

  /**
   * Get a list of all mapping variables.
   * @return List of mapping variable as ClawMappingVar.
   */
  public List<ClawMappingVar> getMappingVariables(){
    return _mappingVariables;
  }

  /**
   * Get a list of all mapped variables.
   * @return List of mapped variable as String.
   */
  public List<String> getMappedVariables(){
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
