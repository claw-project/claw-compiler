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



  public boolean lowerBoundIsVar(){
    return _lowerBoundId != null;
  }

  public boolean upperBoundIsVar(){
    return _upperBoundId != null;
  }

  public int getLowerBoundInt(){
    return _lowerBound;
  }

  public int getUpperBoundInt(){
    return _upperBound;
  }

  public String getLowerBoundId(){
    return _lowerBoundId;
  }

  public String getUpperBoundId(){
    return _upperBoundId;
  }

  public String getIdentifier(){
    return _identifier;
  }

}
