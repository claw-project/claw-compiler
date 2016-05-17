/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.language;

/**
 * Class holding information about defined dimension.
 *
 * @author clementval
 */
public class ClawDimension {
  private final int _lowerBound;
  private final int _upperBound;
  private final String _lowerBoundId;
  private final String _upperBoundId;
  private final String _identifier;


  /**
   * Constructs a new dimension object from the extracted information.
   * @param id          Identifier of the defined dimension.
   * @param lowerBound  Lower bound of the dimension.
   * @param upperBound  Upper bound of the dimension.
   * TODO maybe add step information (in the grammar as well)
   */
  public ClawDimension(String id, String lowerBound, String upperBound){
    _identifier = id;

    int tempLb, tempUb;
    String tempLbStr, tempUbStr;
    try {
      tempLb = Integer.parseInt(lowerBound);
      tempLbStr = null;
    } catch (NumberFormatException ex){
      tempLb = -1;
      tempLbStr = lowerBound;
    }

    try {
      tempUb = Integer.parseInt(lowerBound);
      tempUbStr = null;
    } catch (NumberFormatException ex){
      tempUb = -1;
      tempUbStr = upperBound;
    }

    _lowerBound = tempLb;
    _upperBound = tempUb;
    _lowerBoundId = tempLbStr;
    _upperBoundId = tempUbStr;
  }


  /**
   * Check whether lower bound is an identifier variable.
   * @return True if the lower bound is an identifier variable.
   */
  public boolean lowerBoundIsVar(){
    return _lowerBoundId != null;
  }

  /**
   * Check whether upper bound is an identifier variable.
   * @return True if the upper bound is an identifier variable.
   */
  public boolean upperBoundIsVar(){
    return _upperBoundId != null;
  }

  /**
   * @return Lower bound value. -1 if lower bound is an identifier variable.
   */
  public int getLowerBoundInt(){
    return _lowerBound;
  }

  /**
   * @return Upper bound value. -1 if upper bound is an identifier variable.
   */
  public int getUpperBoundInt(){
    return _upperBound;
  }

  /**
   * @return Lower bound value. Null if lower bound is an integer constant.
   */
  public String getLowerBoundId(){
    return _lowerBoundId;
  }

  /**
   * @return Upper bound value. Null if upper bound is an integer constant.
   */
  public String getUpperBoundId(){
    return _upperBoundId;
  }

  /**
   * Get the identifier for the current dimension. 
   * @returnThe identifier of the current dimension.
   */
  public String getIdentifier(){
    return _identifier;
  }

}
