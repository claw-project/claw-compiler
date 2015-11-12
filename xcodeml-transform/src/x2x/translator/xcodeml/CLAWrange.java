package x2x.translator.xcodeml;

import org.w3c.dom.Element;

public class CLAWrange {

  protected String _iterationVar;
  protected String _lowerBoundValue;
  protected String _upperBoundValue;
  protected String _stepValue;


  public CLAWrange(String iterationVar, String lowerBound, String upperBound,
    String step){
      _iterationVar = iterationVar;
      _lowerBoundValue = lowerBound;
      _upperBoundValue = upperBound;
      _stepValue = step;
  }

  public CLAWrange(Element rangeElement){

  }

  public String getIterationVariableValue(){
    return _iterationVar;
  }

  public String getLowerBoundValue(){
    return _lowerBoundValue;
  }

  public String getUpperBoundValue(){
    return _upperBoundValue;
  }

  public String getStepValue(){
    return _stepValue;
  }

  @Override
  public boolean equals(Object ob) {
    if (ob == null) return false;
    if (ob.getClass() != getClass()) return false;
    CLAWrange other = (CLAWrange)ob;
    if (!_iterationVar.toLowerCase()
      .equals(other.getIterationVariableValue().toLowerCase()))
      return false;
    if (!_lowerBoundValue.toLowerCase().equals(other.getLowerBoundValue().toLowerCase()))
      return false;
    if (!(_upperBoundValue.toLowerCase().equals(other.getUpperBoundValue().toLowerCase())))
      return false;
    if (!(_stepValue.toLowerCase().equals(other.getStepValue().toLowerCase())))
      return false;
    return true;
  }

  @Override
  public int hashCode() {
    return _iterationVar.hashCode() ^ _lowerBoundValue.hashCode()
      ^ _upperBoundValue.hashCode() ^ _stepValue.hashCode();
  }
}
