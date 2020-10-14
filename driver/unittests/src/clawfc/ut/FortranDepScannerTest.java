/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.ut;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ConsoleErrorListener;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import junit.framework.TestCase;
import clawfc.depscan.*;
import clawfc.depscan.parser.*;

class FortranDepScannerListener
	extends FortranDepScannerBaseListener
{
	public ArrayList<String> programOpen = new ArrayList<String>();
	public ArrayList<String> programClose = new ArrayList<String>();
	public ArrayList<String> moduleOpen = new ArrayList<String>();
	public ArrayList<String> moduleClose = new ArrayList<String>();
	public ArrayList<String> use = new ArrayList<String>();
	public ArrayList<String> other = new ArrayList<String>();
    
    @Override
    public void exitModule_open_stmt(FortranDepScannerParser.Module_open_stmtContext ctx) 
    {
    	moduleOpen.add(ctx.getText());
    }
    @Override
    public void exitModule_close_stmt(FortranDepScannerParser.Module_close_stmtContext ctx) 
    {
    	moduleClose.add(ctx.getText());
    }
    @Override
    public void exitProgram_open_stmt(FortranDepScannerParser.Program_open_stmtContext ctx) 
    {
    	programOpen.add(ctx.getText());
    }
    @Override
    public void exitProgram_close_stmt(FortranDepScannerParser.Program_close_stmtContext ctx) 
    {
    	programClose.add(ctx.getText());
    }
    @Override
    public void exitUse_stmt(FortranDepScannerParser.Use_stmtContext ctx) 
    {
    	use.add(ctx.getText());
    }
} 

public class FortranDepScannerTest
    extends TestCase 
{
    FortranDepScannerParser parser;
    FortranDepScannerLexer lexer;
    FortranDepParser depParser;
    
    @Override
    protected void setUp() throws Exception 
    {
        lexer = new FortranDepScannerLexer(toCharStream(""));
        lexer.removeErrorListener(ConsoleErrorListener.INSTANCE);
        parser = new FortranDepScannerParser(toTokenStream(""));
        parser.removeErrorListener(ConsoleErrorListener.INSTANCE);
        depParser = new FortranDepParser();
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
        assertTrue(String.format("Failed to reject string \"%s\"", str), parser.getNumberOfSyntaxErrors() > 0);
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
    
    void acceptIdentifierString(String str) throws IOException
    {
        acceptString("module " + str + "\n end module x\n" );
    }
    
    void rejectIdentifierString(String str) throws IOException
    {
        rejectString("module " + str + "\n end module x\n" );
    }
    
  public void testIdentifier() throws Exception
  {
	  
      for(char l = 'a'; l < 'z'; ++l)
      {
    	  acceptIdentifierString("" + l); 
    	  acceptIdentifierString(l + "_");
          for(char l2 = '0'; l2 < '9'; ++l2)
          {
        	  acceptIdentifierString(("" + l) + l2);
          }
      }
      for(char l = 'A'; l < 'Z'; ++l)
      {
    	  acceptIdentifierString("" + l); 
    	  acceptIdentifierString(l + "_");
          for(char l2 = '0'; l2 < '9'; ++l2)
          {
        	  acceptIdentifierString(("" + l) + l2);
          }         
      }
      rejectIdentifierString("_");
      for(char l2 = '0'; l2 < '9'; ++l2)
      {
          rejectIdentifierString("" + l2);
      }    
  }
  
  void acceptModuleOpenString(String str) throws IOException
  {
      acceptString(str + "\n end module x\n" );
  }
  
  void rejectModuleOpenString(String str) throws IOException
  {
      rejectString(str + "\n end module x\n" );
  }

  public void testModuleOpenStatement() throws Exception
  {
	  acceptModuleOpenString("module module");
	  acceptModuleOpenString("module x");
	  acceptModuleOpenString(" module x");
	  acceptModuleOpenString("\tmodule x");
	  acceptModuleOpenString("\rmodule x");
	  acceptModuleOpenString("module x ");
	  acceptModuleOpenString("module x\r");
      acceptModuleOpenString("module x\t");
      for(int i = 0, n = "module".length(); i < n; ++i)
      {
          String str = flipChrCase("module", i) + " x";
          acceptModuleOpenString(str);
      }
      rejectModuleOpenString("module");
      rejectModuleOpenString("modulex");  
      rejectModuleOpenString("module x !");  
      rejectModuleOpenString("module x y");  
  }
  
  void acceptModuleCloseString(String str) throws IOException
  {
      acceptString("module x\n" + str);
  }
  
  void rejectModuleCloseString(String str) throws IOException
  {
      rejectString("module x\n" + str);
  }

  public void testModuleCloseStatement() throws Exception
  {
	  acceptModuleCloseString("end module x");
	  acceptModuleCloseString(" end module x");
	  acceptModuleCloseString("\rend module x");
	  acceptModuleCloseString("\tend module x");
	  acceptModuleCloseString("\tend\r\rmodule  x ");
      for(int i = 0, n = "end".length(); i < n; ++i)
      {
          String str = flipChrCase("end", i) + " module x";
          acceptModuleCloseString(str);
      }
      for(int i = 0, n = "module".length(); i < n; ++i)
      {
          String str = "end " + flipChrCase("module", i) + " x";
          acceptModuleCloseString(str);
      }
      rejectModuleCloseString("\tend\r\rmodule  x z");
  }

  
  void acceptProgramOpenString(String str) throws IOException
  {
      acceptString(str + "\n end program x\n" );
  }
  
  void rejectProgramOpenString(String str) throws IOException
  {
      rejectString(str + "\n end program x\n" );
  }

  public void testProgramOpenStatement() throws Exception
  {
	  acceptProgramOpenString("program x");
	  acceptProgramOpenString(" program x");
	  acceptProgramOpenString("\tprogram x");
	  acceptProgramOpenString("\rprogram x");
	  acceptProgramOpenString("program x ");
      acceptProgramOpenString("program x\r");
      acceptProgramOpenString("program x\t");
      for(int i = 0, n = "program".length(); i < n; ++i)
      {
          String str = flipChrCase("program", i) + " x";
          acceptProgramOpenString(str);
      }
      rejectProgramOpenString("program");
      rejectProgramOpenString("programx");  
      rejectProgramOpenString("program x !");  
      rejectProgramOpenString("program x y");  
  }
  
  void acceptProgramCloseString(String str) throws IOException
  {
      acceptString("program x\n" + str);
  }
  
  void rejectProgramCloseString(String str) throws IOException
  {
      rejectString("program x\n" + str);
  }

  public void testProgramCloseStatement() throws Exception
  {
	  acceptProgramCloseString("end Program x");
	  acceptProgramCloseString(" end Program x");
	  acceptProgramCloseString("\rend Program x");
	  acceptProgramCloseString("\tend Program x");
      acceptProgramCloseString("\tend\r\rProgram  xy_23");
      for(int i = 0, n = "end".length(); i < n; ++i)
      {
          String str = flipChrCase("end", i) + " Program x";
          acceptProgramCloseString(str);
      }
      for(int i = 0, n = "Program".length(); i < n; ++i)
      {
          String str = "end " + flipChrCase("Program", i) + " x";
          acceptProgramCloseString(str);
      }
      rejectProgramCloseString("\tend\r\rprogram x z");
  }

  
  void acceptUseString(String str) throws IOException
  {
      acceptString("program x\n" + str + "\nend program x\n");
  }
  
  void rejectUseString(String str) throws IOException
  {
	  FortranDepScannerListener res = runParser("program x\n" + str + "\nend program x\n");
	  assertEquals(1, res.programOpen.size());
	  //"Unfit" use strings are classified as "other" and therefore skipped
	  assertEquals(0, res.use.size());
	  assertEquals(1, res.programClose.size());
	  assertEquals("program x", res.programOpen.get(0));
	  assertEquals("end program x", res.programClose.get(0));	
  }
  
  public void testUseStatement() throws Exception
  {
      acceptUseString("use x");
      for(int i = 0, n = "use".length(); i < n; ++i)
      {
          String str = flipChrCase("use", i) + " x";
          acceptUseString(str);
      }
      acceptUseString(" use x");
      acceptUseString(" use x ");
      acceptUseString("use x,y1=>z1");
      acceptUseString("use x,  y1=>z1");
      acceptUseString("use x,  y1  =>z1");
      acceptUseString("use x,  y1  =>  z1");
      acceptUseString("use x,  y1  =>  z1  ");
      acceptUseString("use x,  y1  =>  z1, y2 => z2 ");
      acceptUseString("use x,  y1  =>  z1, y2 => z2, y3 => z3 ");
      acceptUseString("use x,  ONLY: y1");
      for(int i = 0, n = "only".length(); i < n; ++i)
      {
          String str = "use x,  " + flipChrCase("only", i) + ":x";
          acceptUseString(str);
      }
      acceptUseString("use x ,  ONLY : y1");
      acceptUseString("use x,  ONLY : y1");
      acceptUseString("use x,  ONLY: y1,y2");
      acceptUseString("use x,  ONLY: y1,y2,y3");
      acceptUseString("use x,  ONLY:y1=>z1");
      acceptUseString("use x,  ONLY: y1 =>z1");
      acceptUseString("use x,  ONLY: y1  =>z1");
      acceptUseString("use x,  ONLY: y1  =>  z1");
      acceptUseString("use x,  ONLY: y1  =>  z1  ");
      acceptUseString("use x,  ONLY: y1  =>  z1, y2 => z2, y3 => z3 ");
      rejectUseString("usex");
      rejectUseString("use x,");
      rejectUseString("use x y ");
      rejectUseString("use x, y,");
      rejectUseString("use x, ONLY ");
      rejectUseString("use x, ONLY: x y");
  }
  
  FortranDepScannerListener runParser(String inputStr) throws IOException
  {
	  FortranDepScannerListener listener = new FortranDepScannerListener();
      parser.reset();
      parser.setInputStream(toTokenStream(inputStr));
      parser.setBuildParseTree(true);
      ParseTree tree = parser.root();
      ParseTreeWalker walker = new ParseTreeWalker();
      walker.walk(listener, tree);
      assertTrue(String.format("Failed to accept string \"%s\"", inputStr), 
    		     parser.isMatchedEOF() && parser.getNumberOfSyntaxErrors() == 0);
      return listener;
  }
  
  public void testProgramStatement() throws Exception
  {
	  FortranDepScannerListener res = runParser("program x\n"+
			                                    "use y\n" +
                                                "end program x\n");
	  assertEquals(1, res.programOpen.size());
	  assertEquals(1, res.use.size());
	  assertEquals(1, res.programClose.size());
	  assertEquals("program x", res.programOpen.get(0));
	  assertEquals("use y", res.use.get(0));
	  assertEquals("end program x", res.programClose.get(0));	  
  }
  
  public void testModuleStatement() throws Exception
  {
	  FortranDepScannerListener res = runParser("module x\n" +
		                                        "use y\n" +
                                                "end module x\n");
	  assertEquals(1, res.moduleOpen.size());
	  assertEquals(1, res.use.size());
	  assertEquals(1, res.moduleClose.size());
	  assertEquals("module x", res.moduleOpen.get(0));
	  assertEquals("use y", res.use.get(0));
	  assertEquals("end module x", res.moduleClose.get(0));	  
  }
  
  FortranFileSummary parse(String s) throws IOException, FortranException, Exception
  {
	  return depParser.parse(Utils.toInputStream(s));
  }
  
  void verifyParse(String s,
		  	       FortranModuleDependencies[] expModules,
		           FortranModuleDependencies expectedProgram) throws IOException, FortranException, Exception
  {
	  FortranFileSummary res = parse(s);
	  FortranFileSummary expRes = new FortranFileSummary(expModules, expectedProgram);
	  assertEquals(expRes, res);	  
  }
  
  public void testParsing() throws IOException, FortranException, Exception
  {
	  verifyParse("", 
			      new FortranModuleDependencies[0],
			      (FortranModuleDependencies)null);
	  verifyParse("module x\n"+
			      "end module x\n", 
			      new FortranModuleDependencies[]{new FortranModuleDependencies("x", new String[0])},
			      (FortranModuleDependencies)null);
	  verifyParse("module x\n"+
		          "use y\n"+ 
		          "end module x\n", 
			      new FortranModuleDependencies[]{new FortranModuleDependencies("x", new String[] {"y"})},
			      (FortranModuleDependencies)null);
	  verifyParse("module x\n"+
	              "use y\n"+ 
	              "bla\n"+ 
	              "use z\n"+ 
	              "end module x\n", 
			      new FortranModuleDependencies[]{new FortranModuleDependencies("x", new String[] {"y", "z"})},
			      (FortranModuleDependencies)null);
	  verifyParse(
			  "module x\n"+
              "use y\n"+ 
              "bla\n"+ 
              "use z\n"+ 
              "end module x\n"+
			  "module x1\n"+
              "use y1\n"+ 
              "bla\n"+ 
              "use z1\n"+ 
              "end module x1\n", 
              new FortranModuleDependencies[]{new FortranModuleDependencies("x", new String[] {"y", "z"}),
		                          new FortranModuleDependencies("x1", new String[] {"y1", "z1"})},
		      (FortranModuleDependencies)null);
	  verifyParse(
			  "module x\n"+
	          "use y\n"+ 
	          "bla\n"+ 
	          "use z\n"+ 
	          "end module x\n"+
			  "module x1\n"+
	          "use y1\n"+ 
	          "bla\n"+ 
	          "use z1\n"+ 
	          "end module x1\n"+
			  "program p1\n"+
	          "use y1\n"+ 
	          "bla\n"+ 
	          "use z1\n"+ 
	          "end program p1\n", 
				new FortranModuleDependencies[]{new FortranModuleDependencies("x", new String[] {"y", "z"}),
					                            new FortranModuleDependencies("x1", new String[] {"y1", "z1"})},
				new FortranModuleDependencies("p1", new String[] {"y1", "z1"}));
	  
  }
  
  public void testSyntaxError() throws IOException, FortranException, Exception
  {
	  try
	  {
		  parse("end module x\n");
	  }
	  catch(FortranSyntaxException e)
	  {
		  assertTrue(e.getMessage().contains("extraneous input 'end module x'"));
		  assertEquals(1, e.line());
		  return;		  
	  }
	  assertTrue(false);
  }
  
  public void testModuleNamesMismatchError() throws IOException, FortranException, Exception
  {
	  try
	  {
		  parse("module x\n"+
				"end module y\n");
	  }
	  catch(FortranSemanticException e)
	  {
		  assertEquals("End module name \"y\" does not match current module name \"x\"",
				       e.getMessage());
		  assertEquals(2, e.line());
		  return;		  
	  }
	  assertTrue(false);
  }
  
  public void testDoubleDefError() throws IOException, FortranException, Exception
  {
	  try
	  {
		  parse("module x\n"+
				"end module x\n"+
				"module x\n"+
				"end module x\n");
	  }
	  catch(FortranSemanticException e)
	  {
		  assertEquals("Double definition of module \"x\"",
				       e.getMessage());
		  assertEquals(3, e.line());
		  return;		  
	  }
	  assertTrue(false);
  }
}
