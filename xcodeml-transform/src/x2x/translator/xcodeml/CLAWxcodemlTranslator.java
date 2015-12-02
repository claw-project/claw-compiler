package x2x.translator.xcodeml;

import x2x.translator.xcodeml.xelement.*;
import x2x.translator.xcodeml.transformation.*;
import x2x.translator.xcodeml.transformer.*;

import org.w3c.dom.Document;
import org.w3c.dom.NodeList;
import org.w3c.dom.Node;
import org.w3c.dom.Element;
import java.io.File;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import javax.xml.xpath.*;

import java.util.*;

import exc.xcodeml.*;
import exc.object.Xcode;
import xcodeml.util.XmOption;
import x2x.translator.pragma.CLAWpragma;

public class CLAWxcodemlTranslator {
  private String _xcodemlInputFile = null;
  private String _xcodemlOutputFile = null;
  private boolean _canTransform = false;

  private ClawTransformer _transformer = null;
  private XcodeProg _program = null;

  private static final int INDENT_OUTPUT = 2; // Number of spaces for indent

  public CLAWxcodemlTranslator(String xcodemlInputFile,
    String xcodemlOutputFile)
  {
    _xcodemlInputFile = xcodemlInputFile;
    _xcodemlOutputFile = xcodemlOutputFile;
    _transformer = new ClawTransformer();
  }

  public void analyze() throws Exception {
    _program = new XcodeProg(_xcodemlInputFile);
    _program.load();

    if(!_program.isXcodeMLvalid()){
      System.err.println("XcodeML document is not valid");
      return;
    }

    // Read information from the type table
    _program.readTypeTable();
    _program.readGlobalSymbolsTable();

    NodeList pragmaList = XelementHelper.getPragmas(_program.getDocument());

    for (int i = 0; i < pragmaList.getLength(); i++) {
      Node pragmaNode = pragmaList.item(i);
      if (pragmaNode.getNodeType() == Node.ELEMENT_NODE) {
        Element pragmaElement = (Element) pragmaNode;
        Xpragma pragma = new Xpragma(pragmaElement);

        if(!CLAWpragma.startsWithClaw(pragma.getData())){
          continue; // Not CLAW pragma, we do nothing
        }

        if(CLAWpragma.isValid(pragma.getData())){
          CLAWpragma clawDirective = CLAWpragma.getDirective(pragma.getData());

          if(clawDirective == CLAWpragma.LOOP_FUSION){
            LoopFusion trans = new LoopFusion(pragma);
            if(trans.analyze(_program, _transformer)){
              _transformer.addTransformation(trans);
            } // TODO maybe exit on failed analysis
          } else if(clawDirective == CLAWpragma.LOOP_INTERCHANGE){
            LoopInterchange trans = new LoopInterchange(pragma);
            if(trans.analyze(_program, _transformer)){
              _transformer.addTransformation(trans);
            } // TODO maybe exit on failed analysis
          } else if(clawDirective == CLAWpragma.LOOP_EXTRACT){
            LoopExtraction trans = new LoopExtraction(pragma);
            if(trans.analyze(_program, _transformer)){
              _transformer.addTransformation(trans);
            } // TODO maybe exit on failed analysis
          }

        } else {
          System.out.println("INVALID PRAGMA: !$" + pragma.getData());
          System.exit(1);
        }
      }
    }


    // Analysis done, the transformation can be performed.
    _canTransform = true;
  }

  public void transform() {
    try {
      if(!_canTransform){
        // TODO handle return value
        XelementHelper.writeXcodeML(_program, _xcodemlOutputFile, INDENT_OUTPUT);
        return;
      }

      // Do the transformation here

      if(XmOption.isDebugOutput()){
        for(TransformationGroup t : _transformer.getGroups()){
          System.out.println("transform " + t.transformationName () + ": "
          + t.count());
        }
      }

      for(TransformationGroup t : _transformer.getGroups()){
        if(XmOption.isDebugOutput()){
          System.out.println("Apply transfomation: " + t.transformationName());
        }
        t.applyTranslations(_program, _transformer);
      }

      // TODO handle the return value
      XelementHelper.writeXcodeML(_program, _xcodemlOutputFile, INDENT_OUTPUT);

    } catch (Exception ex) {
      // TODO handle exception
      System.out.println("Transformation exception: ");
      ex.printStackTrace();
    }
  }

}
