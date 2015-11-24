package x2x.translator.pragma;

import java.util.ArrayList;

public class CLAWmapping {

  private ArrayList<String> _mappedVariables = null;
  private ArrayList<String> _mappingVariables = null;

  /**
   * loop-extract mapping representation
   * @param mappingClause inner part of the mapping clause like
   * var_list:mapping_list
   */
  public CLAWmapping(String mappingClause){
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
   * @return the number of dimension to be mapped
   */
  public int getMappedDimension(){
    return _mappingVariables.size();
  }

}
