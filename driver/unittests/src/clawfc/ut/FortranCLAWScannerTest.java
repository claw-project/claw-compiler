/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.ut;

import clawfc.depscan.FortranCLAWScanner;
import clawfc.depscan.FortranFileCLAWLinesInfo;
import clawfc.depscan.FortranSyntaxException;
import clawfc.depscan.parser.*;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.*;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ConsoleErrorListener;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.tree.*;

import junit.framework.TestCase;

class FortranCLAWScannerListener
    extends FortranCLAWScannerBaseListener
{
    public ArrayList<String> clawDirectives = new ArrayList<String>();
    public ArrayList<String> clawGuards = new ArrayList<String>();
    
    @Override
    public void exitClaw_directive_line(FortranCLAWScannerParser.Claw_directive_lineContext ctx)
    {
        clawDirectives.add(ctx.getStart().getText());
    }
    
    @Override
    public void exitClaw_guard_line(FortranCLAWScannerParser.Claw_guard_lineContext ctx)
    {
        clawGuards.add(ctx.getStart().getText());
    }
} 

public class FortranCLAWScannerTest 
    extends TestCase 
{
    private FortranCLAWScannerParser parser;
    private FortranCLAWScannerLexer lexer;
    private FortranCLAWScanner scanner;
    
    @Override
    protected void setUp() throws Exception 
    {
        lexer = new FortranCLAWScannerLexer(toCharStream(""));
        lexer.removeErrorListener(ConsoleErrorListener.INSTANCE);
        parser = new FortranCLAWScannerParser(toTokenStream(""));
        parser.removeErrorListener(ConsoleErrorListener.INSTANCE);
        scanner = new FortranCLAWScanner();
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
    
    private void verifyRecognition(String str, 
                                   List<String> directives, 
                                   List<String> guards) throws Exception
    {
        FortranCLAWScannerListener listener = new FortranCLAWScannerListener();
        parser.reset();
        parser.setInputStream(toTokenStream(str));
        parser.setBuildParseTree(true);
        ParseTree tree = parser.root();
        ParseTreeWalker walker = new ParseTreeWalker();
        walker.walk(listener, tree);
        assertTrue(String.format("Failed to accept string \"%s\"", str), parser.isMatchedEOF() && parser.getNumberOfSyntaxErrors() == 0); 
        assertEquals(directives, listener.clawDirectives);
        assertEquals(guards, listener.clawGuards);
    }
    
    private void verifyDirective(String str) throws Exception
    {
        verifyRecognition(str,  Arrays.asList(str), Arrays.asList());
    }
    
    private void verifyGuard(String str) throws Exception
    {
        verifyRecognition(str,  Arrays.asList(), Arrays.asList(str));
    }
    
    public void testExtraction() throws Exception
    {
        verifyRecognition("", Arrays.asList(), Arrays.asList());
        verifyDirective("!$claw");
        verifyDirective("!$CLaw");
        verifyDirective("!$clAW");
        verifyDirective(" \r\t!$claw");
        verifyDirective(" \r\t! \r\t$claw");
        verifyDirective(" \r\t! \r\t$claw \r\t");
        verifyDirective(" \r\t! \r\t$claw \r\t bla bla bla");
        verifyDirective(" \r\t! \r\t$claw \r\t bla &");
        verifyGuard("!$omp claw");
        verifyGuard("!$oMp claw");
        verifyGuard("!$OmP claw");
        verifyGuard(" \r\t!$omp claw");
        verifyGuard(" \r\t! \r\t$omp claw");
        verifyGuard(" \r\t! \r\t$omp\r\t claw");
        verifyGuard(" \r\t! \r\t$omp\r\t claw \r\t");
        verifyGuard("!$acc claw");
        verifyGuard("!$aCc claw");
        verifyGuard("!$AcC claw");
        verifyGuard(" \r\t!$acc claw");
        verifyGuard(" \r\t! \r\t$acc claw");
        verifyGuard(" \r\t! \r\t$acc\r\t claw");
        verifyGuard(" \r\t! \r\t$acc\r\t claw \r\t");
        verifyRecognition("!$claw\n"+
                          " \r\t! \r\t$claw\n", Arrays.asList("!$claw", " \r\t! \r\t$claw"), 
                                                Arrays.asList());
        verifyRecognition("!$claw\n"+
                "!$omp claw\n"+
                " \r\t! \r\t$claw\n"+
                " \r\t! \r\t$omp\r\t claw \r\t\n", Arrays.asList("!$claw", " \r\t! \r\t$claw"), 
                                      Arrays.asList("!$omp claw", " \r\t! \r\t$omp\r\t claw \r\t"));
    }
    
    private void verifyLineNumbers(String str, 
            List<Integer> directives, 
            List<Integer> guards) throws Exception
    {
        InputStream inStrm = new ByteArrayInputStream(str.getBytes(StandardCharsets.US_ASCII));
        FortranFileCLAWLinesInfo info = scanner.run(inStrm);
        assertEquals(directives, info.clawDirectives);
        assertEquals(guards, info.clawGuards);       
    }
    
    public void testLineNumbersExtraction() throws Exception
    {
        verifyLineNumbers("!$claw", Arrays.asList(1), Arrays.asList());
        verifyLineNumbers("!$omp claw", Arrays.asList(), Arrays.asList(1));
        verifyLineNumbers("!$claw\n"+
                          "!$omp claw", Arrays.asList(1), Arrays.asList(2));
        verifyLineNumbers("1\n"+
                          "2\n"+  
                          "!$claw\n"+
                          "4\n"+
                          "!$omp claw", Arrays.asList(3), Arrays.asList(5));
        verifyLineNumbers("!$claw\n"+
                "2\n"+  
                "!$claw\n"+
                "4\n"+
                "!$omp claw", Arrays.asList(1, 3), Arrays.asList(5));
    }
}
