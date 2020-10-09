/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.ut;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ConsoleErrorListener;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.Parser;

import junit.framework.TestCase;

import clawfc.depscan.parser.*;

public class FortranDepScannerTest
    extends TestCase 
{
    private FortranDepScannerParser parser;
    private FortranDepScannerLexer lexer;
    
    @Override
    protected void setUp() throws Exception 
    {
        lexer = new FortranDepScannerLexer(toCharStream(""));
        lexer.removeErrorListener(ConsoleErrorListener.INSTANCE);
        parser = new FortranDepScannerParser(toTokenStream(""));
        parser.removeErrorListener(ConsoleErrorListener.INSTANCE);
    }
    
    private static CharStream toCharStream(String str) throws IOException
    {
        InputStream inStrm = new ByteArrayInputStream(str.getBytes(StandardCharsets.UTF_8));  
        CharStream chrStrm = CharStreams.fromStream(inStrm, StandardCharsets.UTF_8);
        return chrStrm;
    }
    
    private CommonTokenStream toTokenStream(String str) throws IOException
    {
        CharStream chrStrm = toCharStream(str);
        lexer.reset();
        lexer.setInputStream(chrStrm);
        CommonTokenStream tokStrm = new CommonTokenStream(lexer);
        return tokStrm;
    }
    
    protected void acceptString(String str) throws IOException
    {
        parser.reset();
        parser.setInputStream(toTokenStream(str));
        parser.root();
        assertTrue(String.format("Failed to accept string \"%s\"", str), parser.isMatchedEOF() && parser.getNumberOfSyntaxErrors() == 0);
    }
    
    protected void rejectString(String str) throws IOException
    {
        parser.reset();
        parser.setInputStream(toTokenStream(str));
        parser.root();
        assertTrue(String.format("Failed to reject string \"%s\"", str), !parser.isMatchedEOF() && parser.getNumberOfSyntaxErrors() > 0);
    }
    
    protected String flipChrCase(String str, int idx)
    {
        char c = str.charAt(idx);
        if(c >= 'a' && c <= 'z')
        { Character.toUpperCase(c); }
        else 
        { Character.toLowerCase(c); }
        str = str.substring(0, idx) + c + str.substring(idx + 1);
        return str;      
    }
  public void testIdentifier() throws Exception
  {
      for(char l = 'a'; l < 'z'; ++l)
      {
          acceptString("module " + l); 
          acceptString("module " + l + '_');
          for(char l2 = '0'; l2 < '9'; ++l2)
          {
              acceptString("module " + l + l2);
          }
      }
      for(char l = 'A'; l < 'Z'; ++l)
      {
          acceptString("module " + l); 
          acceptString("module " + l + '_');
          for(char l2 = '0'; l2 < '9'; ++l2)
          {
              acceptString("module " + l + l2);
          }          
      }
      rejectString("module _");
      for(char l2 = '0'; l2 < '9'; ++l2)
      {
          rejectString("module " + l2);
      }    
  }

  public void testModuleOpenStatement() throws Exception
  {
      acceptString("module x");
      acceptString(" module x");
      acceptString("\tmodule x");
      acceptString("\rmodule x");
      acceptString("module x ");
      acceptString("module x\r");
      acceptString("module x\t");
      for(int i = 0, n = "module".length(); i < n; ++i)
      {
          String str = flipChrCase("module", i) + " x";
          acceptString(str);
      }
      rejectString("module");
      rejectString("modulex");  
      rejectString("module x !");  
      rejectString("module x y");  
  }

  public void testModuleCloseStatement() throws Exception
  {
      acceptString("end module x");
      acceptString(" end module x");
      acceptString("\rend module x");
      acceptString("\tend module x");
      acceptString("\tend\r\rmodule  x ");
      for(int i = 0, n = "end".length(); i < n; ++i)
      {
          String str = flipChrCase("end", i) + " module x";
          acceptString(str);
      }
      for(int i = 0, n = "module".length(); i < n; ++i)
      {
          String str = "end " + flipChrCase("module", i) + " x";
          acceptString(str);
      }
      rejectString("\tend\r\rmodule  x z");
  }

  public void testProgramOpenStatement() throws Exception
  {
      acceptString("program x");
      acceptString(" program x");
      acceptString("\tprogram x");
      acceptString("\rprogram x");
      acceptString("program x ");
      acceptString("program x\r");
      acceptString("program x\t");
      for(int i = 0, n = "program".length(); i < n; ++i)
      {
          String str = flipChrCase("program", i) + " x";
          acceptString(str);
      }
      rejectString("program");
      rejectString("programx");  
      rejectString("program x !");  
      rejectString("program x y");  
  }

  public void testProgramCloseStatement() throws Exception
  {
      acceptString("end Program x");
      acceptString(" end Program x");
      acceptString("\rend Program x");
      acceptString("\tend Program x");
      acceptString("\tend\r\rProgram  xy_23");
      for(int i = 0, n = "end".length(); i < n; ++i)
      {
          String str = flipChrCase("end", i) + " Program x";
          acceptString(str);
      }
      for(int i = 0, n = "Program".length(); i < n; ++i)
      {
          String str = "end " + flipChrCase("Program", i) + " x";
          acceptString(str);
      }
      rejectString("\tend\r\rprogram x z");
  }

  public void testUseStatement() throws Exception
  {
      acceptString("use x");
      for(int i = 0, n = "use".length(); i < n; ++i)
      {
          String str = flipChrCase("use", i) + " x";
          acceptString(str);
      }
      acceptString(" use x");
      acceptString(" use x ");
      acceptString("use x,y1=>z1");
      acceptString("use x,  y1=>z1");
      acceptString("use x,  y1  =>z1");
      acceptString("use x,  y1  =>  z1");
      acceptString("use x,  y1  =>  z1  ");
      acceptString("use x,  y1  =>  z1, y2 => z2 ");
      acceptString("use x,  y1  =>  z1, y2 => z2, y3 => z3 ");
      acceptString("use x,  ONLY: y1");
      for(int i = 0, n = "only".length(); i < n; ++i)
      {
          String str = "use x,  " + flipChrCase("only", i) + ":x";
          acceptString(str);
      }
      acceptString("use x ,  ONLY : y1");
      acceptString("use x,  ONLY : y1");
      acceptString("use x,  ONLY: y1,y2");
      acceptString("use x,  ONLY: y1,y2,y3");
      acceptString("use x,  ONLY:y1=>z1");
      acceptString("use x,  ONLY: y1 =>z1");
      acceptString("use x,  ONLY: y1  =>z1");
      acceptString("use x,  ONLY: y1  =>  z1");
      acceptString("use x,  ONLY: y1  =>  z1  ");
      acceptString("use x,  ONLY: y1  =>  z1, y2 => z2, y3 => z3 ");
      rejectString("usex");
      rejectString("use x,");
      rejectString("use x y ");
      rejectString("use x, y,");
      rejectString("use x, ONLY ");
      rejectString("use x, ONLY: x y");
  }
}
