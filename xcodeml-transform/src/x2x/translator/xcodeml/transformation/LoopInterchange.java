package x2x.translator.xcodeml.transformation;

import x2x.translator.pragma.CLAWpragma;
import x2x.translator.xcodeml.xelement.*;
import x2x.translator.xcodeml.transformer.Transformer;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import xcodeml.util.XmOption;

public class LoopInterchange implements Transformation<LoopInterchange> {

  private String _newOrderOption = null;
  private boolean _transformationDone = false;

  private XdoStatement _loopLevel0 = null;
  private XdoStatement _loopLevel1 = null;
  private XdoStatement _loopLevel2 = null;
  private Xpragma _loopInterchangePragma = null;


  private String _baseLoop0 = null;
  private String _baseLoop1 = null;
  private String _baseLoop2 = null;
  private String _newLoop0 = null;
  private String _newLoop1 = null;
  private String _newLoop2 = null;

  // New ordering of the loops. Values are initial position.
  private int _loopNewPos0 = 0;
  private int _loopNewPos1 = 1;
  private int _loopNewPos2 = 2;

  public LoopInterchange(Xpragma pragma){
    _loopInterchangePragma = pragma;
    _newOrderOption = CLAWpragma
      .getSimpleOptionValue(_loopInterchangePragma.getData());
  }

  public void transform(XcodeProg xcodeml, Transformer transformer,
    LoopInterchange other)
  {

    if(XmOption.isDebugOutput()){
      System.out.println("loop-interchange transformation");
      System.out.println("  loop 0: " + _loopLevel0.getFormattedRange());
      System.out.println("  loop 1: " + _loopLevel1.getFormattedRange());
      if(_loopLevel2 != null){
        System.out.println("  loop 2: " + _loopLevel2.getFormattedRange());
      }
    }

    /* To perform the loop interchange, only the ranges and iteration
     * variables are swapped
     */
    if(_loopLevel1 != null && _loopLevel2 == null){
      // Loop interchange between 2 loops
      swapLoops(_loopLevel0, _loopLevel1);
    } else if (_loopLevel1 != null && _loopLevel2 != null){
      // loop interchange between 3 loops with new-order
      computeLoopNewPosition();
      printTransformDebugInfo();

      if(needDoubleSwap()){
        // Case 201
        if (_loopNewPos0 == 2 && _loopNewPos1 == 0 && _loopNewPos2 == 1){
          printTransformSwapInfo(201);
          swapLoops(_loopLevel0, _loopLevel2);
          swapLoops(_loopLevel0, _loopLevel1);
        // Case 120
        } else if (_loopNewPos0 == 1 && _loopNewPos1 == 2 && _loopNewPos2 == 0){
          printTransformSwapInfo(120);
          swapLoops(_loopLevel0, _loopLevel2);
          swapLoops(_loopLevel1, _loopLevel2);
        }
      } else {
        // Only one loop swap is needed
        XdoStatement from = null;
        XdoStatement to = null;
        if(_loopNewPos0 == 0){ // Loop 0 stay in place 0
          from = _loopLevel1;
          to = _loopLevel2;
        } else if(_loopNewPos1 == 1){ // Loop 1 stay in place 1
          from = _loopLevel0;
          to = _loopLevel2;
        } else if(_loopNewPos2 == 2){ // Loop 2 stay in place 2
          from = _loopLevel0;
          to = _loopLevel1;
        }
        swapLoops(from, to);
      }
    }
    _transformationDone = true;

  }

  private void printTransformDebugInfo(){
    if(XmOption.isDebugOutput()){
      System.out.println("  transform from " + _baseLoop0 + "," + _baseLoop1
        + "," + _baseLoop2 + " (012) to " + _newLoop0 + "," + _newLoop1 + "," +
        _newLoop2 + " (" + _loopNewPos0 + _loopNewPos1 +
          _loopNewPos2 + ")");

      if(needDoubleSwap()){
        System.out.println("    double swap required");
      }
    }
  }

  private void printTransformSwapInfo(int swapCase) {
    if(XmOption.isDebugOutput() && swapCase == 120){
      System.out.println("    swap 1: " + _baseLoop0 + " <--> " + _baseLoop2);
      System.out.println("    swap 2: " + _baseLoop1 + " <--> " + _baseLoop0);
    } else if (XmOption.isDebugOutput() && swapCase == 201){
      System.out.println("    swap 1: " + _baseLoop0 + " <--> " + _baseLoop2);
      System.out.println("    swap 2: " + _baseLoop2 + " <--> " + _baseLoop1);
    }
  }

  private void swapLoops(XdoStatement loop1, XdoStatement loop2){
    // Save most inner loop iteration variable and range
    XloopIterationRange tmpIterationRange = loop2.getIterationRange().cloneObject();

    // Set the range of loop 0 to loop 2
    loop2.setNewRange(loop1.getIterationRange());
    // Remove the previous range of loop 2
    loop2.deleteRangeElements();
    // Set new range of loop 2 to loop 0
    loop1.setNewRange(tmpIterationRange);
    // Remove the previous range of loop 0
    loop1.deleteRangeElements();

    // recompute the range elements
    loop2.findRangeElements();
    loop1.findRangeElements();

  }

  private boolean needDoubleSwap(){
    if((_loopNewPos0 == 2 && _loopNewPos1 == 0 && _loopNewPos2 == 1) ||
      (_loopNewPos0 == 1 && _loopNewPos1 == 2 && _loopNewPos2 == 0)){
      return true;
    }
    return false;
  }


  private void computeLoopNewPosition(){
    if (_baseLoop0.equals(_newLoop1)){
      _loopNewPos0 = 1;
    } else if (_baseLoop0.equals(_newLoop2)){
      _loopNewPos0 = 2;
    }

    if (_baseLoop1.equals(_newLoop0)){
      _loopNewPos1 = 0;
    } else if (_baseLoop1.equals(_newLoop2)){
      _loopNewPos1 = 2;
    }

    if (_baseLoop2.equals(_newLoop0)){
      _loopNewPos2 = 0;
    } else if (_baseLoop2.equals(_newLoop1)){
      _loopNewPos2 = 1;
    }
  }


  public boolean analyze(XcodeProg xcodeml, Transformer transformer){
    // Find next loop after pragma
    Element loopElement =
      XelementHelper.findNextLoop(_loopInterchangePragma.getBaseElement());

    if(loopElement == null){
      // TODO give the reason and stops analysis
      return false;
    }

    _loopLevel0 = new XdoStatement(loopElement);

    Element body = _loopLevel0.getBodyElement();
    Element loop1 = findChildLoop(body);
    if(loop1 == null){
      return false;
    }

    _loopLevel1 = new XdoStatement(loop1);

    if(_newOrderOption != null){
      String[] vars = _newOrderOption.split(",");
      // TODO error handling
      if(vars.length != 3){
        abort("new-order option has not enough parameters");
      }


      Element loop1Body = _loopLevel1.getBodyElement();
      Element loop2 = findChildLoop(loop1Body);
      _loopLevel2 = new XdoStatement(loop2);

      _baseLoop0 = _loopLevel0.getIterationVariableValue();
      _baseLoop1 = _loopLevel1.getIterationVariableValue();
      _baseLoop2 = _loopLevel2.getIterationVariableValue();

      checkNewOrderOption(vars);

      _newLoop0 = vars[0];
      _newLoop1 = vars[1];
      _newLoop2 = vars[2];
    }


    return true;
  }

  private void checkNewOrderOption(String[] vars){
    for(String var : vars){
      if(!var.equals(_baseLoop0) && !var.equals(_baseLoop1)
        && !var.equals(_baseLoop2))
      {
        abort("invalid iteration varibale in new-order option. " + var);
      }
    }
  }


  private void abort(String message){
    System.err.println("claw-error: " + message + ", " + _loopInterchangePragma.getFilename()
      + ":" + _loopInterchangePragma.getLine());
    System.exit(1);
  }

  private Element findChildLoop(Node from){
    Node nextNode = from.getFirstChild();
    boolean elementFound = false;
    while (nextNode != null){
      if(nextNode.getNodeType() == Node.ELEMENT_NODE){
        Element element = (Element) nextNode;
        if(element.getTagName().equals("FdoStatement")){
          return element;
        }
      }
      nextNode = nextNode.getNextSibling();
    }
    return null;
  }

  public boolean isTransformed() {
    return true; // TODO
  }

  public boolean canBeTransformedWith(LoopInterchange other){
    return true; // Always true as independent transformation
  }



}
