/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.ut;

import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.FileTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import clawfc.BuildInfo;
import clawfc.Utils;
import clawfc.depscan.FortranSemanticException;
import junit.framework.TestCase;

public class FortranBuildInfoTest extends TestCase
{
    public void testCreateDirList() throws Exception
    {
        Path tmpDir = Files.createTempDirectory(null);
        try
        {
            assertNotNull(tmpDir);
            String tmpPath = tmpDir.toString();
            {// Single dir
                List<Path> pathsLst = BuildInfo.createDirList(Arrays.asList(tmpPath));
                assertEquals(Arrays.asList(Paths.get(tmpPath)), pathsLst);
            }
            {// Same dir multiple times
                List<Path> pathsLst = BuildInfo.createDirList(Arrays.asList(tmpPath, tmpPath, tmpPath + "/./"));
                assertEquals(Arrays.asList(Paths.get(tmpPath)), pathsLst);
            }
            Path tmpDir2 = Files.createTempDirectory(tmpDir, null);
            {// Multiple dirs
                List<Path> pathsLst = BuildInfo.createDirList(Arrays.asList(tmpPath, tmpDir2.toString()));
                assertEquals(Arrays.asList(Paths.get(tmpPath), tmpDir2), pathsLst);
            }
            {// Multiple dirs sort order
                List<Path> pathsLst = BuildInfo.createDirList(Arrays.asList(tmpDir2.toString(), tmpPath));
                assertEquals(Arrays.asList(Paths.get(tmpPath), tmpDir2), pathsLst);
            }
            {// Missing dir
                boolean exceptionCaught = false;
                try
                {
                    List<Path> pathsLst = BuildInfo.createDirList(Arrays.asList(tmpPath + "/dir-not-exist"));
                } catch (FileNotFoundException e)
                {
                    exceptionCaught = true;
                }
                assertTrue(exceptionCaught);
            }
        } finally
        {
            if (tmpDir != null)
            {
                Utils.removeDir(tmpDir);
            }
        }
    }

    public void testcreateDirFileList() throws Exception
    {
        Path tmpDir = Files.createTempDirectory(null);
        try
        {
            for (int i = 0; i < 10; ++i)
            {
                Files.createTempFile(tmpDir, null, ".tst");
            }
            ArrayList<Path> expFiles = new ArrayList<Path>();
            for (String ext : Utils.FORTRAN_FILE_EXTENSIONS)
            {
                for (int i = 0; i < 10; ++i)
                {
                    expFiles.add(Files.createTempFile(tmpDir, null, "." + ext));
                }
            }
            Collections.sort(expFiles);
            List<Path> files = BuildInfo.createDirFileList(tmpDir);
            assertEquals(expFiles, files);
        } finally
        {
            if (tmpDir != null)
            {
                Utils.removeDir(tmpDir);
            }
        }
    }

    public void testcreateDirFileLists() throws Exception
    {
        Path tmpDir = Files.createTempDirectory(null);
        try
        {
            HashMap<Path, List<Path>> expDirFileLists = new HashMap<Path, List<Path>>();
            ArrayList<Path> dirs = new ArrayList<Path>();
            for (int j = 0; j < 2; ++j)
            {
                ArrayList<Path> expFiles = new ArrayList<Path>();
                Path tmpDir_j = Files.createTempDirectory(tmpDir, null);
                dirs.add(tmpDir_j);
                for (int i = 0; i < 10; ++i)
                {
                    Files.createTempFile(tmpDir_j, null, ".tst");
                }
                for (String ext : Utils.FORTRAN_FILE_EXTENSIONS)
                {
                    for (int i = 0; i < 10; ++i)
                    {
                        expFiles.add(Files.createTempFile(tmpDir_j, null, "." + ext));
                    }
                }
                Collections.sort(expFiles);
                expDirFileLists.put(tmpDir_j, expFiles);
            }
            Map<Path, List<Path>> dirFileLists = BuildInfo.createDirFileLists(dirs);
            assertEquals(expDirFileLists, dirFileLists);
        } finally
        {
            if (tmpDir != null)
            {
                Utils.removeDir(tmpDir);
            }
        }
    }

    public void testFortranFileInfo() throws Exception
    {
        Path tmpDir = Files.createTempDirectory(null);
        try
        {
            String text = String.join("\n", "module a", "   use e", "   use c", "   use d", "end module a", "module b",
                    "   use a", "end module b");
            Path filePath = createTxtFile(tmpDir, "test.f95", text);
            BuildInfo.FortranFileInfo fInfo = new BuildInfo.FortranFileInfo(filePath);
            BuildInfo.FortranFileInfo expFInfo = new BuildInfo.FortranFileInfo(Arrays.asList("a", "b"),
                    Arrays.asList("c", "d", "e"), Files.getLastModifiedTime(filePath));
            assertEquals(expFInfo, fInfo);
        } finally
        {
            if (tmpDir != null)
            {
                Utils.removeDir(tmpDir);
            }
        }
    }

    static Path createTxtFile(Path dirPath, String basename, String text) throws FileNotFoundException
    {
        Path filePath = dirPath.resolve(basename);
        try (PrintWriter out = new PrintWriter(filePath.toString()))
        {
            out.println(text);
        }
        return filePath;
    }

    public void testScanFortranFiles() throws Exception
    {
        Path tmpDir = Files.createTempDirectory(null);
        try
        {
            Path fPath1, fPath2;
            {
                String text = String.join("\n", "module a", "   use e", "   use c", "   use d", "end module a",
                        "module b", "   use a", "end module b");
                fPath1 = createTxtFile(tmpDir, "test1.f95", text);
            }
            {
                String text = String.join("\n", "module a1", "   use e1", "   use c1", "   use d1", "end module a1",
                        "module b1", "   use a1", "end module b1");
                fPath2 = createTxtFile(tmpDir, "test2.f95", text);
            }
            Map<Path, List<Path>> dirFileLists = Collections.singletonMap(tmpDir, Arrays.asList(fPath1, fPath2));
            Map<Path, BuildInfo.FortranFileInfo> res = BuildInfo.scanFortranFiles(dirFileLists);
            Map<Path, BuildInfo.FortranFileInfo> expRes;
            {
                expRes = new HashMap<Path, BuildInfo.FortranFileInfo>();
                expRes.put(fPath1, new BuildInfo.FortranFileInfo(Arrays.asList("a", "b"), Arrays.asList("c", "d", "e"),
                        Files.getLastModifiedTime(fPath1)));
                expRes.put(fPath2, new BuildInfo.FortranFileInfo(Arrays.asList("a1", "b1"),
                        Arrays.asList("c1", "d1", "e1"), Files.getLastModifiedTime(fPath2)));
            }
            assertEquals(expRes, res);
        } finally
        {
            if (tmpDir != null)
            {
                Utils.removeDir(tmpDir);
            }
        }
    }

    public void testScanFortranFilesWithMultithreading() throws Exception
    {
        Path tmpDir = Files.createTempDirectory(null);
        try
        {
            {// Normal
                final int n = 100;
                BuildInfo.FortranFileInfo expResFInfo;
                Map<Path, BuildInfo.FortranFileInfo> expRes = new HashMap<Path, BuildInfo.FortranFileInfo>();
                Map<Path, List<Path>> dirFileLists = new HashMap<Path, List<Path>>();
                for (int i = 0; i < n; ++i)
                {
                    Path tmpDiri = Files.createTempDirectory(tmpDir, "dir");
                    Path fPathi;
                    String text = String.join("\n", "module a", "   use e", "   use c", "   use d", "end module a",
                            "module b", "   use a", "end module b");
                    fPathi = createTxtFile(tmpDiri, String.format("test%s.f95", i), text);
                    expRes.put(fPathi, new BuildInfo.FortranFileInfo(Arrays.asList("a", "b"),
                            Arrays.asList("c", "d", "e"), Files.getLastModifiedTime(fPathi)));
                    dirFileLists.put(tmpDiri, Arrays.asList(fPathi));
                }
                Map<Path, BuildInfo.FortranFileInfo> singleRes = BuildInfo.scanFortranFiles(dirFileLists);
                assertEquals(expRes, singleRes);
                Map<Path, BuildInfo.FortranFileInfo> multiRes = BuildInfo.scanFortranFiles(dirFileLists, true);
                assertEquals(expRes, multiRes);
            }
            {// Exception
                BuildInfo.FortranFileInfo expResFInfo;
                Map<Path, List<Path>> dirFileLists = new HashMap<Path, List<Path>>();
                Path fPath;
                String text = String.join("\n", "module a", "end module b");
                fPath = createTxtFile(tmpDir, "test.f95", text);
                dirFileLists.put(tmpDir, Arrays.asList(fPath));
                boolean exceptionCaught = false;
                try
                {
                    BuildInfo.scanFortranFiles(dirFileLists);
                } catch (Exception e)
                {
                    exceptionCaught = true;
                }
                assertTrue(exceptionCaught);
            }
        } finally
        {
            if (tmpDir != null)
            {
                Utils.removeDir(tmpDir);
            }
        }
    }

    public void testCyclicDependencyDetector() throws Exception
    {
        Path tmpDir = Files.createTempDirectory(null);
        try
        {
            {// Independent files
                Map<Path, Set<Path>> fileDeps = new HashMap<Path, Set<Path>>();
                fileDeps.put(tmpDir.resolve("a"), new HashSet<Path>());
                fileDeps.put(tmpDir.resolve("b"), new HashSet<Path>());
                fileDeps.put(tmpDir.resolve("c"), new HashSet<Path>());
                BuildInfo.CyclicDependencyDetector.run(fileDeps);
            }
            {// Dependent files
                Path a = tmpDir.resolve("a"), b = tmpDir.resolve("b"), c = tmpDir.resolve("c");
                Map<Path, Set<Path>> fileDeps = new HashMap<Path, Set<Path>>();
                fileDeps.put(a, new HashSet<>(Arrays.asList(b)));
                fileDeps.put(b, new HashSet<>(Arrays.asList(c)));
                fileDeps.put(c, new HashSet<Path>());
                BuildInfo.CyclicDependencyDetector.run(fileDeps);
            }
            {// Cycle with no unreferenced files
                Path a = tmpDir.resolve("a"), b = tmpDir.resolve("b"), c = tmpDir.resolve("c");
                Map<Path, Set<Path>> fileDeps = new HashMap<Path, Set<Path>>();
                fileDeps.put(a, new HashSet<>(Arrays.asList(b)));
                fileDeps.put(b, new HashSet<>(Arrays.asList(c)));
                fileDeps.put(c, new HashSet<Path>(Arrays.asList(a)));
                boolean exceptionCaught = false;
                try
                {
                    BuildInfo.CyclicDependencyDetector.run(fileDeps);
                } catch (FortranSemanticException e)
                {
                    exceptionCaught = true;
                }
                assertTrue(exceptionCaught);
            }
            {// Cycle
                Path a = tmpDir.resolve("a"), b = tmpDir.resolve("b"), c = tmpDir.resolve("c"), d = tmpDir.resolve("d"),
                        e = tmpDir.resolve("e");
                Map<Path, Set<Path>> fileDeps = new HashMap<Path, Set<Path>>();
                fileDeps.put(a, new HashSet<>(Arrays.asList(b)));
                fileDeps.put(b, new HashSet<>(Arrays.asList(c)));
                fileDeps.put(c, new HashSet<Path>(Arrays.asList(a)));
                fileDeps.put(d, new HashSet<Path>(Arrays.asList(e)));
                fileDeps.put(e, new HashSet<Path>());
                boolean exceptionCaught = false;
                try
                {
                    BuildInfo.CyclicDependencyDetector.run(fileDeps);
                } catch (FortranSemanticException err)
                {
                    exceptionCaught = true;
                }
                assertTrue(exceptionCaught);
            }
        } finally
        {
            if (tmpDir != null)
            {
                Utils.removeDir(tmpDir);
            }
        }
    }

    public void testGetFilesDependencies() throws Exception
    {
        Path tmpDir = Files.createTempDirectory(null);
        try
        {
            Map<Path, BuildInfo.FortranFileInfo> fileInfo;
            Path a = tmpDir.resolve("a.f"), b = tmpDir.resolve("b.f"), c = tmpDir.resolve("c.f"),
                    d = tmpDir.resolve("d.f");
            {// Normal
                {
                    fileInfo = new HashMap<Path, BuildInfo.FortranFileInfo>();
                    fileInfo.put(a, new BuildInfo.FortranFileInfo(Arrays.asList("a"), Arrays.asList("b"),
                            FileTime.fromMillis(0)));
                    fileInfo.put(b, new BuildInfo.FortranFileInfo(Arrays.asList("b"), Arrays.asList("c", "d"),
                            FileTime.fromMillis(0)));
                    fileInfo.put(c,
                            new BuildInfo.FortranFileInfo(Arrays.asList("c"), Arrays.asList(), FileTime.fromMillis(0)));
                    fileInfo.put(d,
                            new BuildInfo.FortranFileInfo(Arrays.asList("d"), Arrays.asList(), FileTime.fromMillis(0)));
                }
                Map<Path, Set<Path>> fileDeps = BuildInfo.getFilesDependencies(fileInfo);
                Map<Path, Set<Path>> expFileDeps;
                {
                    expFileDeps = new HashMap<Path, Set<Path>>();
                    expFileDeps.put(a, new HashSet<>(Arrays.asList(b)));
                    expFileDeps.put(b, new HashSet<>(Arrays.asList(c, d)));
                    expFileDeps.put(c, new HashSet<>());
                    expFileDeps.put(d, new HashSet<>());
                }
                assertEquals(expFileDeps, fileDeps);
            }
            {// Double definition
                {
                    fileInfo = new HashMap<Path, BuildInfo.FortranFileInfo>();
                    fileInfo.put(a, new BuildInfo.FortranFileInfo(Arrays.asList("a"), Arrays.asList("b"),
                            FileTime.fromMillis(0)));
                    fileInfo.put(b, new BuildInfo.FortranFileInfo(Arrays.asList("a", "b"), Arrays.asList(),
                            FileTime.fromMillis(0)));
                }
                boolean exceptionCaught = false;
                try
                {
                    Map<Path, Set<Path>> fileDeps = BuildInfo.getFilesDependencies(fileInfo);
                } catch (FortranSemanticException err)
                {
                    exceptionCaught = true;
                }
                assertTrue(exceptionCaught);
            }
            {// Undefined module
                {
                    fileInfo = new HashMap<Path, BuildInfo.FortranFileInfo>();
                    fileInfo.put(a, new BuildInfo.FortranFileInfo(Arrays.asList("a"), Arrays.asList("b", "c"),
                            FileTime.fromMillis(0)));
                    fileInfo.put(b,
                            new BuildInfo.FortranFileInfo(Arrays.asList("b"), Arrays.asList(), FileTime.fromMillis(0)));
                }
                boolean exceptionCaught = false;
                try
                {
                    Map<Path, Set<Path>> fileDeps = BuildInfo.getFilesDependencies(fileInfo);
                } catch (FortranSemanticException err)
                {
                    exceptionCaught = true;
                }
                assertTrue(exceptionCaught);
            }
        } finally
        {
            if (tmpDir != null)
            {
                Utils.removeDir(tmpDir);
            }
        }
    }

    public void testBuildinfo() throws Exception
    {
        Path tmpDir = Files.createTempDirectory(null);
        try
        {
            Path tmpDir1 = Files.createTempDirectory(tmpDir, "dir1");
            Path tmpDir2 = Files.createTempDirectory(tmpDir, "dir2");
            Path fPath1, fPath2;
            {
                String text = String.join("\n", "module a", "   use a1", "end module a", "module a1", "   use b",
                        "end module a1");
                fPath1 = createTxtFile(tmpDir1, "test1.f95", text);
            }
            {
                String text = String.join("\n", "module b", "end module b");
                fPath2 = createTxtFile(tmpDir2, "test2.f95", text);
            }
            BuildInfo buildInfo = new BuildInfo(Arrays.asList(tmpDir1.toString(), tmpDir2.toString()));
            Map<Path, BuildInfo.FortranFileInfo> expFileInfo;
            {
                expFileInfo = new HashMap<Path, BuildInfo.FortranFileInfo>();
                expFileInfo.put(fPath1, new BuildInfo.FortranFileInfo(Arrays.asList("a", "a1"), Arrays.asList("b"),
                        Files.getLastModifiedTime(fPath1)));
                expFileInfo.put(fPath2, new BuildInfo.FortranFileInfo(Arrays.asList("b"), Arrays.asList(),
                        Files.getLastModifiedTime(fPath2)));
            }
            assertEquals(expFileInfo, buildInfo.fileInformation());
            Map<Path, Set<Path>> expFileDeps;
            {
                expFileDeps = new HashMap<Path, Set<Path>>();
                expFileDeps.put(fPath1, new HashSet<>(Arrays.asList(fPath2)));
                expFileDeps.put(fPath2, new HashSet<>());
            }
            assertEquals(expFileDeps, buildInfo.fileDependencies());
        } finally
        {
            if (tmpDir != null)
            {
                Utils.removeDir(tmpDir);
            }
        }
    }

}
