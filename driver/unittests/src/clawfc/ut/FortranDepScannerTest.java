/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.ut;

import static clawfc.Utils.toInputStream;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;

import clawfc.depscan.FortranDepParser;
import clawfc.depscan.FortranDepParser.StatementInfo;
import clawfc.depscan.FortranDepScanner;
import clawfc.depscan.FortranException;
import clawfc.depscan.FortranFileBasicSummary;
import clawfc.depscan.FortranFileProgramUnitInfo;
import clawfc.depscan.FortranFileProgramUnitInfoDeserializer;
import clawfc.depscan.FortranFileProgramUnitInfoSerializer;
import clawfc.depscan.FortranIncludesResolver;
import clawfc.depscan.FortranProgramUnitBasicInfo;
import clawfc.depscan.FortranProgramUnitInfo;
import clawfc.depscan.FortranSemanticException;
import clawfc.depscan.FortranStatementBasicPosition;
import clawfc.depscan.FortranStatementPosition;
import clawfc.depscan.serial.FortranProgramUnitType;
import clawfc.utils.AsciiArrayIOStream;
import clawfc.utils.ByteArrayIOStream;
import junit.framework.TestCase;

public class FortranDepScannerTest extends TestCase
{
    FortranDepParser depStmtsScanner;
    FortranDepScanner depScanner;

    @Override
    protected void setUp() throws Exception
    {
        depStmtsScanner = new FortranDepParser();
        depScanner = new FortranDepScanner();
    }

    FortranFileBasicSummary parse(String s) throws IOException, FortranException, Exception
    {
        List<StatementInfo> statements = depStmtsScanner.parse(toInputStream(s));
        FortranFileBasicSummary summary = FortranDepParser.getSummary(statements);
        return summary;
    }

    void verifyParse(String s, List<FortranProgramUnitBasicInfo> expModules)
            throws IOException, FortranException, Exception
    {
        {
            FortranFileBasicSummary res = parse(s);
            FortranFileBasicSummary expRes = new FortranFileBasicSummary(expModules);
            assertEquals(expRes, res);
        }
        {
            s = s.replace("\n", ";");
            FortranFileBasicSummary res = parse(s);
            FortranFileBasicSummary expRes = new FortranFileBasicSummary(expModules);
            assertEquals(expRes, res);
        }
    }

    static FortranStatementBasicPosition BPos(String name, int startCharIdx, int endCharIdx)
    {
        return new FortranStatementBasicPosition(name, startCharIdx, endCharIdx);
    }

    static FortranProgramUnitBasicInfo BasicModInfo(FortranStatementBasicPosition unit,
            List<FortranStatementBasicPosition> useModules)
    {
        return new FortranProgramUnitBasicInfo(FortranProgramUnitType.MODULE, unit, useModules);
    }

    static FortranProgramUnitBasicInfo BasicProgInfo(FortranStatementBasicPosition unit,
            List<FortranStatementBasicPosition> useModules)
    {
        return new FortranProgramUnitBasicInfo(FortranProgramUnitType.PROGRAM, unit, useModules);
    }

    static FortranProgramUnitBasicInfo BasicBDataInfo(FortranStatementBasicPosition unit,
            List<FortranStatementBasicPosition> useModules)
    {
        return new FortranProgramUnitBasicInfo(FortranProgramUnitType.BLOCK_DATA, unit, useModules);
    }

    static FortranProgramUnitBasicInfo BasicFInfo(FortranStatementBasicPosition unit,
            List<FortranStatementBasicPosition> useModules)
    {
        return new FortranProgramUnitBasicInfo(FortranProgramUnitType.FUNCTION, unit, useModules);
    }

    static FortranProgramUnitBasicInfo BasicSubInfo(FortranStatementBasicPosition unit,
            List<FortranStatementBasicPosition> useModules)
    {
        return new FortranProgramUnitBasicInfo(FortranProgramUnitType.SUBROUTINE, unit, useModules);
    }

    public void testParsing() throws IOException, FortranException, Exception
    {
        verifyParse("", Arrays.asList());
        verifyParse("module x\n" + "end module\n", Arrays.asList(BasicModInfo(BPos("x", 0, 19), Arrays.asList())));
        verifyParse("module x\n" + "end module \n", Arrays.asList(BasicModInfo(BPos("x", 0, 19), Arrays.asList())));
        verifyParse("module x\n" + "end module x\n", Arrays.asList(BasicModInfo(BPos("x", 0, 21), Arrays.asList())));
        verifyParse("module x\n" + "endmodule x\n", Arrays.asList(BasicModInfo(BPos("x", 0, 20), Arrays.asList())));
        verifyParse("module x\n" + "endmodule\n", Arrays.asList(BasicModInfo(BPos("x", 0, 18), Arrays.asList())));
        verifyParse("module x\n" + "end\n", Arrays.asList(BasicModInfo(BPos("x", 0, 12), Arrays.asList())));
        verifyParse(" \t\tmodule x\n" + "end module x\n",
                Arrays.asList(BasicModInfo(BPos("x", 3, 24), Arrays.asList())));
        verifyParse(" \t\tmodule x\n" + "end module x \t\t\n",
                Arrays.asList(BasicModInfo(BPos("x", 3, 24), Arrays.asList())));

        verifyParse("end\n", Arrays.asList(BasicProgInfo(BPos("_unnamed_program", 0, 3), Arrays.asList())));
        verifyParse(" \t\tprogram x\n" + "end program x\n",
                Arrays.asList(BasicProgInfo(BPos("x", 3, 26), Arrays.asList())));
        verifyParse(" \t\tprogram x\n" + "end program x \t\t\n",
                Arrays.asList(BasicProgInfo(BPos("x", 3, 26), Arrays.asList())));
        verifyParse(" \t\tprogram x\n" + "endprogram x \t\t\n",
                Arrays.asList(BasicProgInfo(BPos("x", 3, 25), Arrays.asList())));
        verifyParse(" \t\tprogram x\n" + "endprogram \t\t\n",
                Arrays.asList(BasicProgInfo(BPos("x", 3, 23), Arrays.asList())));
        verifyParse(" \t\tprogram x\n" + "end \t\t\n", Arrays.asList(BasicProgInfo(BPos("x", 3, 16), Arrays.asList())));

        verifyParse("function x();end function x;", Arrays.asList(BasicFInfo(BPos("x", 0, 27), Arrays.asList())));
        verifyParse("function x();endfunction x;", Arrays.asList(BasicFInfo(BPos("x", 0, 26), Arrays.asList())));
        verifyParse("function x();endfunction;", Arrays.asList(BasicFInfo(BPos("x", 0, 24), Arrays.asList())));
        verifyParse("function x();end;", Arrays.asList(BasicFInfo(BPos("x", 0, 16), Arrays.asList())));
        verifyParse("function x();function y();end;end;", Arrays.asList(BasicFInfo(BPos("x", 0, 33), Arrays.asList())));

        verifyParse("module m;contains;function x();contains;function y();end;end;end;",
                Arrays.asList(BasicModInfo(BPos("m", 0, 64), Arrays.asList())));

        verifyParse("subroutine x();end subroutine x;", Arrays.asList(BasicSubInfo(BPos("x", 0, 31), Arrays.asList())));
        verifyParse("subroutine x();endsubroutine x;", Arrays.asList(BasicSubInfo(BPos("x", 0, 30), Arrays.asList())));
        verifyParse("subroutine x();endsubroutine;", Arrays.asList(BasicSubInfo(BPos("x", 0, 28), Arrays.asList())));
        verifyParse("subroutine x();end;", Arrays.asList(BasicSubInfo(BPos("x", 0, 18), Arrays.asList())));
        verifyParse("subroutine x();contains;subroutine y(); end;end;",
                Arrays.asList(BasicSubInfo(BPos("x", 0, 47), Arrays.asList())));

        verifyParse("module m; function x();end function x; end;",
                Arrays.asList(BasicModInfo(BPos("m", 0, 42), Arrays.asList())));
        verifyParse("module m; subroutine x();end subroutine x; end;",
                Arrays.asList(BasicModInfo(BPos("m", 0, 46), Arrays.asList())));

        verifyParse("blockdata;end;",
                Arrays.asList(BasicBDataInfo(BPos("_unnamed_blockdata", 0, 13), Arrays.asList())));
        verifyParse("blockdata x;end;", Arrays.asList(BasicBDataInfo(BPos("x", 0, 15), Arrays.asList())));
        verifyParse("blockdata x;endblockdata;", Arrays.asList(BasicBDataInfo(BPos("x", 0, 24), Arrays.asList())));
        verifyParse("blockdata x;end blockdata;", Arrays.asList(BasicBDataInfo(BPos("x", 0, 25), Arrays.asList())));
        verifyParse("blockdata x;end block data;", Arrays.asList(BasicBDataInfo(BPos("x", 0, 26), Arrays.asList())));
        verifyParse("blockdata x;end block data x;", Arrays.asList(BasicBDataInfo(BPos("x", 0, 28), Arrays.asList())));

        verifyParse("module x\n" + "use y\n" + "end module x\n",
                Arrays.asList(BasicModInfo(BPos("x", 0, 27), Arrays.asList(BPos("y", 9, 14)))));
        verifyParse("module x\n" + " \t\tuse y\n" + "end module x\n",
                Arrays.asList(BasicModInfo(BPos("x", 0, 30), Arrays.asList(BPos("y", 12, 17)))));
        verifyParse("module x\n" + " \t\tuse y \t\t\n" + "end module x\n",
                Arrays.asList(BasicModInfo(BPos("x", 0, 33), Arrays.asList(BPos("y", 12, 17)))));
        verifyParse("module x\n" + "use y\n" + "bla\n" + "use z\n" + "end module x\n",
                Arrays.asList(BasicModInfo(BPos("x", 0, 37), Arrays.asList(BPos("y", 9, 14), BPos("z", 19, 24)))));
        verifyParse(
                "module x\n" + "use y\n" + "bla\n" + "use z\n" + "end module x\n" + "module x1\n" + "use y1\n" + "bla\n"
                        + "use z1\n" + "end module x1\n",
                Arrays.asList(BasicModInfo(BPos("x", 0, 37), Arrays.asList(BPos("y", 9, 14), BPos("z", 19, 24))),
                        BasicModInfo(BPos("x1", 38, 79), Arrays.asList(BPos("y1", 48, 54), BPos("z1", 59, 65)))));

        verifyParse(
                "module x\n" + "use y\n" + "bla\n" + "use z\n" + "end module x\n" + "module x1\n" + "use y1\n" + "bla\n"
                        + "use z1\n" + "end module x1\n" + "program p1\n" + "use y1\n" + "bla\n" + "use z1\n"
                        + "end program p1\n",
                Arrays.asList(BasicModInfo(BPos("x", 0, 37), Arrays.asList(BPos("y", 9, 14), BPos("z", 19, 24))),
                        BasicModInfo(BPos("x1", 38, 79), Arrays.asList(BPos("y1", 48, 54), BPos("z1", 59, 65))),
                        BasicProgInfo(BPos("p1", 80, 123), Arrays.asList(BPos("y1", 91, 97), BPos("z1", 102, 108)))));

    }

    public void testUnmatchedModuleEndError() throws IOException, FortranException, Exception
    {
        try
        {
            parse("end module x\n");
        } catch (FortranSemanticException e)
        {
            assertEquals("Unmatched END MODULE x", e.getMessage());
            assertEquals(null, e.getLineIndex());
            assertEquals(null, e.getCharIdxInLine());
            assertEquals(Integer.valueOf(0), e.getCharIdxInFile());
            return;
        }
        assertTrue(false);
    }

    public void testModuleNamesMismatchError() throws IOException, FortranException, Exception
    {
        try
        {
            parse("module x\n" + "end module y\n");
        } catch (FortranSemanticException e)
        {
            assertEquals("MODULE open name \"x\" does not match close name \"y\"", e.getMessage());
            assertEquals(null, e.getLineIndex());
            assertEquals(null, e.getCharIdxInLine());
            assertEquals(Integer.valueOf(9), e.getCharIdxInFile());
            return;
        }
        assertTrue(false);
    }

    public void testDoubleDefError() throws IOException, FortranException, Exception
    {
        try
        {
            parse("module x\n" + "end module x\n" + "module x\n" + "end module x\n");
        } catch (FortranSemanticException e)
        {
            assertEquals("MODULE \"x\" has the same name as prior MODULE", e.getMessage());
            assertEquals(null, e.getLineIndex());
            assertEquals(null, e.getCharIdxInLine());
            assertEquals(Integer.valueOf(22), e.getCharIdxInFile());
            return;
        }
        assertTrue(false);
    }

    FortranFileBasicSummary basicScan(String s) throws IOException, FortranException, Exception
    {
        return depScanner.basicScan(toInputStream(s));
    }

    FortranFileProgramUnitInfo scan(String s) throws IOException, FortranException, Exception
    {
        return depScanner.scan(toInputStream(s));
    }

    void verifyBasicScan(String s, List<FortranProgramUnitBasicInfo> expUnits)
            throws IOException, FortranException, Exception
    {
        FortranFileBasicSummary res = basicScan(s);
        FortranFileBasicSummary expRes = new FortranFileBasicSummary(expUnits);
        assertEquals(expRes, res);
    }

    void verifyScan(String s, List<FortranProgramUnitInfo> expModules) throws IOException, FortranException, Exception
    {
        FortranFileProgramUnitInfo res = scan(s);
        FortranFileProgramUnitInfo expRes = new FortranFileProgramUnitInfo(expModules);
        assertEquals(expRes, res);
    }

    public static FortranStatementPosition Pos(String name, int startCharIdx, int endCharIdx, int startLineIdx,
            int endLineIdx)
    {
        return new FortranStatementPosition(name, startCharIdx, endCharIdx, startLineIdx, endLineIdx);
    }

    static FortranProgramUnitInfo ModInfo(clawfc.depscan.FortranStatementPosition pos,
            List<clawfc.depscan.FortranStatementPosition> useModules, boolean usesClaw)
    {
        return new FortranProgramUnitInfo(FortranProgramUnitType.MODULE, pos, useModules, usesClaw);
    }

    static FortranProgramUnitInfo ProgInfo(clawfc.depscan.FortranStatementPosition pos,
            List<clawfc.depscan.FortranStatementPosition> useModules, boolean usesClaw)
    {
        return new FortranProgramUnitInfo(FortranProgramUnitType.PROGRAM, pos, useModules, usesClaw);
    }

    public void testScanning() throws IOException, FortranException, Exception
    {
        verifyScan("", Arrays.asList());
        verifyScan("module x\n" + "end module\n",
                Arrays.asList(ModInfo(Pos("x", 0, 19, 0, 2), Arrays.asList(), false)));
        verifyScan("program x\n" + "end program\n",
                Arrays.asList(ProgInfo(Pos("x", 0, 21, 0, 2), Arrays.asList(), false)));
        verifyScan("module x\n" + "end module \n",
                Arrays.asList(ModInfo(Pos("x", 0, 19, 0, 2), Arrays.asList(), false)));
        verifyScan("module x\n" + "end module x\n",
                Arrays.asList(ModInfo(Pos("x", 0, 21, 0, 2), Arrays.asList(), false)));
        verifyScan("module x;" + "end module x;\n",
                Arrays.asList(ModInfo(Pos("x", 0, 21, 0, 1), Arrays.asList(), false)));

        verifyScan("module x\n" + "use y\n" + "end module x\n",
                Arrays.asList(ModInfo(Pos("x", 0, 27, 0, 3), Arrays.asList(Pos("y", 9, 14, 1, 2)), false)));
        verifyScan("module x\n" + "use y\n" + "bla\n" + "use z\n" + "end module x\n", Arrays.asList(
                ModInfo(Pos("x", 0, 37, 0, 5), Arrays.asList(Pos("y", 9, 14, 1, 2), Pos("z", 19, 24, 3, 4)), false)));
        verifyScan(
                "module x\n" + "use y\n" + "bla\n" + "use z\n" + "end module x\n" + "module x1\n" + "use y1\n" + "bla\n"
                        + "use z1\n" + "end module x1\n",
                Arrays.asList(
                        ModInfo(Pos("x", 0, 37, 0, 5), Arrays.asList(Pos("y", 9, 14, 1, 2), Pos("z", 19, 24, 3, 4)),
                                false),
                        ModInfo(Pos("x1", 38, 79, 5, 10),
                                Arrays.asList(Pos("y1", 48, 54, 6, 7), Pos("z1", 59, 65, 8, 9)), false)));

        verifyScan(
                "module x\n" + "use y\n" + "bla\n" + "use z\n" + "end module x\n" + "module x1\n" + "use y1\n" + "bla\n"
                        + "use z1\n" + "end module x1\n" + "program p1\n" + "use y1\n" + "bla\n" + "use z1\n"
                        + "end program p1\n",
                Arrays.asList(
                        ModInfo(Pos("x", 0, 37, 0, 5), Arrays.asList(Pos("y", 9, 14, 1, 2), Pos("z", 19, 24, 3, 4)),
                                false),
                        ModInfo(Pos("x1", 38, 79, 5, 10),
                                Arrays.asList(Pos("y1", 48, 54, 6, 7), Pos("z1", 59, 65, 8, 9)), false),
                        ProgInfo(Pos("p1", 80, 123, 10, 15),
                                Arrays.asList(Pos("y1", 91, 97, 11, 12), Pos("z1", 102, 108, 13, 14)), false)));
    }

    public void testScanWithComments() throws IOException, FortranException, Exception
    {
        verifyScan("module x  ! comment1 \n" + "end module x ! comment2\n",
                Arrays.asList(ModInfo(Pos("x", 0, 34, 0, 2), Arrays.asList(), false)));
        verifyScan("module x  ! comment1 \n" + "use z\n" + "end module x ! comment2\n",
                Arrays.asList(ModInfo(Pos("x", 0, 40, 0, 3), Arrays.asList(Pos("z", 22, 27, 1, 2)), false)));
        verifyScan("program x  ! comment1 \n" + "end program x ! comment2\n",
                Arrays.asList(ProgInfo(Pos("x", 0, 36, 0, 2), Arrays.asList(), false)));
    }

    public void testScanWithLineBreaks() throws IOException, FortranException, Exception
    {
        verifyScan("mo&  \n" + "&dule&\n" + "x\n" + "end module x\n",
                Arrays.asList(ModInfo(Pos("x", 0, 27, 0, 4), Arrays.asList(), false)));
        verifyScan("pro&  \n" + "&gram&\n" + "x\n" + "end program x\n",
                Arrays.asList(ProgInfo(Pos("x", 0, 29, 0, 4), Arrays.asList(), false)));
    }

    public void testScanWithCommentsAndLineBreaks() throws IOException, FortranException, Exception
    {
        verifyScan("mod&   \n" + "   &ule x  ! comment1 \n" + "end mod&   \n" + "    &ule x ! comment2\n",
                Arrays.asList(ModInfo(Pos("x", 0, 53, 0, 4), Arrays.asList(), false)));
    }

    public void testWithClawDirectives() throws Exception
    {
        verifyScan("module x\n" + "!$claw\n" + "end module x\n",
                Arrays.asList(ModInfo(Pos("x", 0, 28, 0, 3), Arrays.asList(), true)));
        verifyScan("program x\n" + "!$claw\n" + "end program x\n",
                Arrays.asList(ProgInfo(Pos("x", 0, 30, 0, 3), Arrays.asList(), true)));
    }

    protected final Path RES_DIR = clawfc.ut.Resources.DIR;

    public void testWithIncludes() throws Exception
    {
        final Path IN_DIR = RES_DIR.resolve("scan/include/input");
        final Path IN_FILEPATH = IN_DIR.resolve("2.f90");
        List<Path> incSearchPath = Arrays.asList(IN_DIR);
        AsciiArrayIOStream resOutStrm = new AsciiArrayIOStream();
        FortranFileProgramUnitInfo res = depScanner.scan(Files.newInputStream(IN_FILEPATH), IN_FILEPATH, resOutStrm,
                incSearchPath);
        AsciiArrayIOStream refOutStrm = new AsciiArrayIOStream();
        FortranFileProgramUnitInfo ref = null;
        Set<Path> refIncFiles = null;
        {
            FortranIncludesResolver resolver = new FortranIncludesResolver();
            refIncFiles = resolver.run(IN_FILEPATH, new AsciiArrayIOStream(IN_FILEPATH), refOutStrm, incSearchPath);
        }
        assertEquals(clawfc.Utils.collectIntoString(refOutStrm.getAsInputStreamUnsafe()),
                clawfc.Utils.collectIntoString(resOutStrm.getAsInputStreamUnsafe()));
        {
            FortranFileProgramUnitInfo refBase = depScanner.scan(refOutStrm.getAsInputStreamUnsafe());
            ref = new FortranFileProgramUnitInfo(refBase.getUnits(), new ArrayList<Path>(refIncFiles));
        }
        assertEquals(ref, res);
    }

    void verifySerialization(FortranFileProgramUnitInfo obj) throws Exception
    {
        FortranFileProgramUnitInfoSerializer serializer = new FortranFileProgramUnitInfoSerializer();
        FortranFileProgramUnitInfoDeserializer deserializer = new FortranFileProgramUnitInfoDeserializer(true);
        ByteArrayIOStream buf = new ByteArrayIOStream();
        serializer.serialize(obj, buf);
        FortranFileProgramUnitInfo deObj = deserializer.deserialize(buf.getAsInputStreamUnsafe());
        assertEquals(obj, deObj);
    }

    public void testSerialization() throws Exception
    {
        verifySerialization(new FortranFileProgramUnitInfo(Arrays.asList()));
        verifySerialization(
                new FortranFileProgramUnitInfo(Arrays.asList(ModInfo(Pos("x", 0, 21, 0, 2), Arrays.asList(), false))));
        verifySerialization(
                new FortranFileProgramUnitInfo(Arrays.asList(ModInfo(Pos("x", 0, 21, 0, 1), Arrays.asList(), false))));
        verifySerialization(new FortranFileProgramUnitInfo(
                Arrays.asList(ModInfo(Pos("x", 0, 27, 0, 3), Arrays.asList(Pos("y", 9, 14, 1, 2)), false))));
        verifySerialization(new FortranFileProgramUnitInfo(Arrays.asList(
                ModInfo(Pos("x", 0, 37, 0, 5), Arrays.asList(Pos("y", 9, 14, 1, 2), Pos("z", 19, 24, 3, 4)), false))));
        verifySerialization(new FortranFileProgramUnitInfo(Arrays.asList(
                ModInfo(Pos("x", 0, 37, 0, 5), Arrays.asList(Pos("y", 9, 14, 1, 2), Pos("z", 19, 24, 3, 4)), false),
                ModInfo(Pos("x1", 38, 79, 5, 10), Arrays.asList(Pos("y1", 48, 54, 6, 7), Pos("z1", 59, 65, 8, 9)),
                        false))));
        verifySerialization(new FortranFileProgramUnitInfo(Arrays.asList(
                ModInfo(Pos("x", 0, 37, 0, 5), Arrays.asList(Pos("y", 9, 14, 1, 2), Pos("z", 19, 24, 3, 4)), false),
                ModInfo(Pos("x1", 38, 79, 5, 10), Arrays.asList(Pos("y1", 48, 54, 6, 7), Pos("z1", 59, 65, 8, 9)),
                        false),
                ProgInfo(Pos("p1", 80, 123, 10, 15),
                        Arrays.asList(Pos("y1", 91, 97, 11, 12), Pos("z1", 102, 108, 13, 14)), false)),
                Paths.get("/tmp/bla-dir/bla.file")));
        verifySerialization(new FortranFileProgramUnitInfo(Arrays.asList(
                ModInfo(Pos("x", 0, 37, 0, 5), Arrays.asList(Pos("y", 9, 14, 1, 2), Pos("z", 19, 24, 3, 4)), false),
                ModInfo(Pos("x1", 38, 79, 5, 10), Arrays.asList(Pos("y1", 48, 54, 6, 7), Pos("z1", 59, 65, 8, 9)),
                        false),
                ProgInfo(Pos("p1", 80, 123, 10, 15),
                        Arrays.asList(Pos("y1", 91, 97, 11, 12), Pos("z1", 102, 108, 13, 14)), false)),
                Paths.get("/tmp/bla-dir/bla.file"), Arrays.asList(Paths.get("/inc_dir1"), Paths.get("inc_dir2"))));
    }
}
