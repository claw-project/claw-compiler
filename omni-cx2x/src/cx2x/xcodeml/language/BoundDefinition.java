/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.xcodeml.language;

import cx2x.xcodeml.xnode.*;

/**
 * Class holding information about bound (upper/lower).
 *
 * @author clementval
 */
class BoundDefinition {

  private String _boundType = null;
  private String _strBoundValue = null;
  private int _intBoundValue;


  BoundDefinition(String boundValue) {
    try {
      _intBoundValue = Integer.parseInt(boundValue);
      _strBoundValue = null;
    } catch(NumberFormatException ex) {
      _intBoundValue = -1;
      _strBoundValue = boundValue;
    }
  }

  public boolean isVar() {
    return _strBoundValue != null;
  }

  public void setType(String value) {
    _boundType = value;
  }

  public Xnode generate(XcodeML xcodeml) {
    if(isVar()) {
      if(_boundType == null) {
        _boundType = xcodeml.getTypeTable().generateIntegerTypeHash();
        XbasicType bType = xcodeml.createBasicType(_boundType, Xname.TYPE_F_INT,
            Xintent.IN);
        xcodeml.getTypeTable().add(bType);
      }
      return xcodeml.createVar(_boundType, _strBoundValue, Xscope.LOCAL);
    } else {
      Xnode boundValue = xcodeml.createNode(Xcode.FINTCONSTANT);
      boundValue.setAttribute(Xattr.TYPE, Xname.TYPE_F_INT);
      boundValue.setValue(String.valueOf(_intBoundValue));
      return boundValue;
    }
  }
}
