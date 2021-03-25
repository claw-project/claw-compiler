/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc.ut;

import static clawfc.Utils.fileExists;
import static clawfc.Utils.removeDir;
import static clawfc.Utils.sprintf;
import static clawfc.Utils.touch;
import static clawfc.ut.FortranDepScannerTest.Pos;

import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import clawfc.FortranFileProgramUnitInfoData;
import clawfc.depscan.FortranFileProgramUnitInfo;
import clawfc.depscan.FortranFileProgramUnitInfoSerializer;
import clawfc.depscan.FortranProgramUnitInfo;
import clawfc.depscan.serial.FortranProgramUnitType;
import junit.framework.TestCase;

public class FortranFileProgramUnitInfoDataTest extends TestCase
{
    static FortranFileProgramUnitInfo createInfo(List<Path> incFiles)
    {
        return new FortranFileProgramUnitInfo(
                Arrays.asList(
                        new FortranProgramUnitInfo(FortranProgramUnitType.MODULE, Pos("x", 0, 37, 0, 5),
                                Arrays.asList(Pos("y", 9, 14, 1, 2), Pos("z", 19, 24, 3, 4)), false),
                        new FortranProgramUnitInfo(FortranProgramUnitType.MODULE, Pos("x1", 38, 79, 5, 10),
                                Arrays.asList(Pos("y1", 48, 54, 6, 7), Pos("z1", 59, 65, 8, 9)), false),
                        new FortranProgramUnitInfo(FortranProgramUnitType.PROGRAM, Pos("p1", 80, 123, 10, 15),
                                Arrays.asList(Pos("y1", 91, 97, 11, 12), Pos("z1", 102, 108, 13, 14)), false)),
                null, incFiles != null ? incFiles : Collections.emptyList());
    }

    static FortranFileProgramUnitInfo createInfo()
    {
        return createInfo(null);
    }

    void verifyCreateDataException(FortranFileProgramUnitInfo info, String exceptionTxt)
    {
        boolean exceptionCaught = false;
        try
        {
            FortranFileProgramUnitInfoData data = new FortranFileProgramUnitInfoData(info);
        } catch (Exception e)
        {
            exceptionCaught = true;
            assertTrue(e.getMessage().contains(exceptionTxt));
        }
        assertTrue(exceptionCaught);
    }

    void verifyCreateDataTS(FortranFileProgramUnitInfo info, Path filePath) throws Exception
    {
        touch(filePath);
        FortranFileProgramUnitInfoData data = new FortranFileProgramUnitInfoData(info);
        assertEquals(Files.getLastModifiedTime(filePath), data.getTimestamp());
    }

    public void testConstructor() throws Exception
    {
        Path tmpDir = Files.createTempDirectory(null);
        try
        {
            Path incFilePath1 = tmpDir.resolve("1.inc");
            Path incFilePath2 = tmpDir.resolve("2.inc");
            FortranFileProgramUnitInfo info = createInfo(Arrays.asList(incFilePath1, incFilePath2));
            verifyCreateDataException(info, "source file not set");
            Path srcFilePath = tmpDir.resolve("src.f90");
            info.setSrcFilePath(srcFilePath);
            verifyCreateDataException(info, sprintf("Failed to stat source file \"%s\"", srcFilePath));
            Files.write(srcFilePath, "src".getBytes());
            // verifyCreateDataException(info, sprintf("preprocessed source file not set"));
            Path ppSrcFilePath = tmpDir.resolve("pp.src.f90");
            info.setPPSrcFilePath(ppSrcFilePath);
            verifyCreateDataException(info, sprintf("Failed to stat preprocessed source file \"%s\"", ppSrcFilePath));
            Files.write(ppSrcFilePath, "pp.src".getBytes());
            verifyCreateDataException(info, sprintf("Failed to stat include file \"%s\"", incFilePath1));
            Files.write(incFilePath1, "inc1".getBytes());
            verifyCreateDataException(info, sprintf("Failed to stat include file \"%s\"", incFilePath2));
            Files.write(incFilePath2, "inc2".getBytes());
            FortranFileProgramUnitInfoData data = new FortranFileProgramUnitInfoData(info);
            assertEquals(info, data.getInfo());
            // -------------------------------------
            verifyCreateDataTS(info, srcFilePath);
            verifyCreateDataTS(info, ppSrcFilePath);
            verifyCreateDataTS(info, incFilePath1);
            verifyCreateDataTS(info, incFilePath2);
        } finally
        {
            if (tmpDir != null)
            {
                removeDir(tmpDir);
            }
        }
    }

    public void testSave() throws Exception
    {
        Path tmpDir = Files.createTempDirectory(null);
        try
        {
            Path srcFilePath = tmpDir.resolve("src.f90");
            Files.write(srcFilePath, "src".getBytes());
            Path ppSrcFilePath = tmpDir.resolve("pp.src.f90");
            Files.write(ppSrcFilePath, "pp.src".getBytes());
            Path incFilePath1 = tmpDir.resolve("1.inc");
            Files.write(incFilePath1, "inc1".getBytes());
            Path incFilePath2 = tmpDir.resolve("2.inc");
            Files.write(incFilePath2, "inc2".getBytes());
            FortranFileProgramUnitInfo info = createInfo(Arrays.asList(incFilePath1, incFilePath2));
            info.setSrcFilePath(srcFilePath);
            info.setPPSrcFilePath(ppSrcFilePath);
            FortranFileProgramUnitInfoData data = new FortranFileProgramUnitInfoData(info);
            Path filePath = data.save(tmpDir, "hash", null);
            assertEquals(filePath, tmpDir.resolve("hash_src.f90.fif"));
            assertTrue(fileExists(filePath));
            FileTime ts = Files.getLastModifiedTime(filePath);
            Path filePath2 = data.save(tmpDir, "hash", null);
            assertEquals(filePath, filePath2);
            FileTime ts2 = Files.getLastModifiedTime(filePath);
            assertTrue(ts2.compareTo(ts) > 0);

        } finally
        {
            if (tmpDir != null)
            {
                removeDir(tmpDir);
            }
        }
    }

    void save(FortranFileProgramUnitInfo info, Path path) throws Exception
    {
        FortranFileProgramUnitInfoSerializer serializer = new FortranFileProgramUnitInfoSerializer();
        try (OutputStream out = Files.newOutputStream(path))
        {
            serializer.serialize(info, out);
        }
    }

    void verifyLoadDataException(FortranFileProgramUnitInfo info, Path filePath, String exceptionTxt) throws Exception
    {
        boolean exceptionCaught = false;
        try
        {
            if (info != null)
            {
                save(info, filePath);
            }
            FortranFileProgramUnitInfoData data = FortranFileProgramUnitInfoData.load(filePath, null);
        } catch (FortranFileProgramUnitInfoData.LoadFailed e)
        {
            exceptionCaught = true;
            assertTrue(e.getMessage().contains(exceptionTxt));
        }
        assertTrue(exceptionCaught);
    }

    void verifyLoadDataTS(Path infoFilePath, Path filePath, String type) throws Exception
    {
        touch(filePath);
        boolean exceptionCaught = false;
        try
        {
            FortranFileProgramUnitInfoData data = FortranFileProgramUnitInfoData.load(infoFilePath, null);
        } catch (FortranFileProgramUnitInfoData.LoadFailed e)
        {
            exceptionCaught = true;
            String errMsg = sprintf(
                    "Build information file \"%s\" is not valid. It is older than referenced %s file \"%s\"",
                    infoFilePath, type, filePath);

            assertEquals(errMsg, e.getMessage());
        }
        assertTrue(exceptionCaught);
        touch(infoFilePath);
        FortranFileProgramUnitInfoData data = FortranFileProgramUnitInfoData.load(infoFilePath, null);
        assertEquals(Files.getLastModifiedTime(filePath), data.getTimestamp());
    }

    public void testLoad() throws Exception
    {
        Path tmpDir = Files.createTempDirectory(null);
        try
        {
            Path srcFilePath = tmpDir.resolve("src.f90");
            Path ppSrcFilePath = tmpDir.resolve("pp.src.f90");
            Path incFilePath1 = tmpDir.resolve("1.inc");
            Path incFilePath2 = tmpDir.resolve("2.inc");
            FortranFileProgramUnitInfo info = createInfo(Arrays.asList(incFilePath1, incFilePath2));
            Path infoFilePath = tmpDir.resolve("info.fif");
            verifyLoadDataException(null, infoFilePath,
                    sprintf("Exception thrown while deserializing build information file \"%s\"", infoFilePath));
            verifyLoadDataException(info, infoFilePath,
                    sprintf("Build information file \"%s\" cannot be verified. It does not contain source file path.",
                            infoFilePath));
            info.setSrcFilePath(srcFilePath);
            verifyLoadDataException(info, infoFilePath,
                    sprintf("Build information file \"%s\" is not valid. It refers to non-existing source file \"%s\"",
                            infoFilePath, srcFilePath));
            Files.write(srcFilePath, "src".getBytes());
            verifyLoadDataException(info, infoFilePath, sprintf(
                    "Build information file \"%s\" cannot be verified. It does not contain preprocessed source file path.",
                    infoFilePath));
            info.setPPSrcFilePath(ppSrcFilePath);
            verifyLoadDataException(info, infoFilePath, sprintf(
                    "Build information file \"%s\" is not valid. It refers to non-existing preprocessed source file \"%s\"",
                    infoFilePath, ppSrcFilePath));
            Files.write(ppSrcFilePath, "pp.src".getBytes());
            verifyLoadDataException(info, infoFilePath,
                    sprintf("Build information file \"%s\" is not valid. It refers to non-existing include file \"%s\"",
                            infoFilePath, incFilePath1));
            Files.write(incFilePath1, "inc1".getBytes());
            verifyLoadDataException(info, infoFilePath,
                    sprintf("Build information file \"%s\" is not valid. It refers to non-existing include file \"%s\"",
                            infoFilePath, incFilePath2));
            Files.write(incFilePath2, "inc2".getBytes());
            // ------------------
            FortranFileProgramUnitInfoData data = new FortranFileProgramUnitInfoData(info);
            infoFilePath = data.save(tmpDir, "hash", null);
            {
                FortranFileProgramUnitInfoData loadedData = FortranFileProgramUnitInfoData.load(infoFilePath, null);
                assertEquals(data.getInfo(), loadedData.getInfo());
                data = loadedData;
            }
            {
                FileTime ts = Files.getLastModifiedTime(infoFilePath);
                Path infoFilePath2 = data.save(tmpDir, "hash", null);
                FileTime ts2 = Files.getLastModifiedTime(infoFilePath);
                assertEquals(ts2, ts);// Save is not performed, as load was from the same file
            }
            // -------------------------------------
            verifyLoadDataTS(infoFilePath, srcFilePath, "source");
            verifyLoadDataTS(infoFilePath, ppSrcFilePath, "preprocessed source");
            verifyLoadDataTS(infoFilePath, incFilePath1, "include");
            verifyLoadDataTS(infoFilePath, incFilePath2, "include");

        } finally
        {
            if (tmpDir != null)
            {
                removeDir(tmpDir);
            }
        }

    }
}
