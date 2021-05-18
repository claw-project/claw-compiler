/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc.ut;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import clawfc.Build;
import clawfc.BuildOrder;
import clawfc.FortranFileProgramUnitInfoData;
import clawfc.ProgramUnitInfo;
import clawfc.XmodData;
import clawfc.depscan.FortranDepScanner;
import clawfc.depscan.FortranFileProgramUnitInfo;
import clawfc.depscan.FortranProgramUnitInfo;
import clawfc.depscan.FortranSemanticException;
import clawfc.depscan.serial.FortranProgramUnitType;
import clawfc.utils.AsciiArrayIOStream;
import clawfc.utils.FileInfo;
import junit.framework.TestCase;

class TestModuleInfo implements clawfc.ProgramUnitInfo
{
    final String name;
    final FortranFileProgramUnitInfo fileSrcInfo;
    final FortranProgramUnitInfo moduleInfo;
    final Path filePath;
    final FileInfo fileInfo;

    public TestModuleInfo(String name, Path filePath, FortranFileProgramUnitInfo fileInfo,
            FortranProgramUnitInfo moduleInfo) throws Exception
    {
        this.name = name;
        this.fileSrcInfo = fileInfo;
        this.moduleInfo = moduleInfo;
        this.filePath = filePath;
        this.fileInfo = new clawfc.utils.FileInfoImpl(filePath);
    }

    @Override
    public String getName()
    {
        return name;
    }

    @Override
    public boolean isModule()
    {
        throw new RuntimeException("Not implemented");
    }

    @Override
    public boolean isInput()
    {
        throw new RuntimeException("Not implemented");
    }

    @Override
    public boolean usesCLAW()
    {
        return moduleInfo.getUsesClaw();
    }

    @Override
    public boolean hasSource()
    {
        return moduleInfo != null;
    }

    @Override
    public Collection<String> getUsedModules()
    {
        return moduleInfo.getUsedModuleNames();
    }

    @Override
    public FortranProgramUnitInfo getSrcInfo()
    {
        return moduleInfo;
    }

    @Override
    public AsciiArrayIOStream getPreprocessedSrc(boolean preserveOffset) throws IOException
    {
        throw new RuntimeException("Not implemented");
    }

    @Override
    public Path getSrcPath()
    {
        return this.filePath;
    }

    @Override
    public FortranFileProgramUnitInfoData getSrcFileBinfoData()
    {
        throw new RuntimeException("Not implemented");
    }

    @Override
    public XmodData getXMod()
    {
        throw new RuntimeException("Not implemented");
    }

    @Override
    public Path getPPSrcPath()
    {
        throw new RuntimeException("Not implemented");
    }

    @Override
    public AsciiArrayIOStream getXast()
    {
        throw new RuntimeException("Not implemented");
    }

    @Override
    public AsciiArrayIOStream getTransXast()
    {
        throw new RuntimeException("Not implemented");
    }

    @Override
    public AsciiArrayIOStream getTransSrc()
    {
        throw new RuntimeException("Not implemented");
    }

    @Override
    public AsciiArrayIOStream getTransReport()
    {
        throw new RuntimeException("Not implemented");
    }

    @Override
    public FortranProgramUnitType getType()
    {
        return FortranProgramUnitType.MODULE;
    }

}

public class BuildTest extends TestCase
{
    protected final Path RES_DIR = clawfc.ut.Resources.DIR;

    Map<String, ProgramUnitInfo> getModulesInfo(Path filePath) throws Exception
    {
        Map<String, ProgramUnitInfo> res = new LinkedHashMap<String, ProgramUnitInfo>();
        FortranDepScanner depScanner = new FortranDepScanner();
        FortranFileProgramUnitInfo fileInfo = depScanner.scan(Files.newInputStream(filePath));
        for (FortranProgramUnitInfo moduleInfo : fileInfo.getUnits())
        {
            String name = moduleInfo.getName();
            res.put(name, new TestModuleInfo(name, filePath, fileInfo, moduleInfo));
        }
        return res;
    }

    public void testSanityCheck() throws Exception
    {
        final Path IN_DIR = RES_DIR.resolve("build/sanity_check/input");
        final Path NORMAL_FILEPATH = IN_DIR.resolve("normal.f90");
        final Path MUL_TARGETS_FILEPATH = IN_DIR.resolve("mul_targets.f90");
        final Path UNDEFINED_MOD_FILEPATH = IN_DIR.resolve("undefined_module.f90");
        final Path CIRCLE_DEP_FILEPATH = IN_DIR.resolve("circle_dep.f90");
        {
            Map<String, ProgramUnitInfo> modules = getModulesInfo(NORMAL_FILEPATH);
            Build.sanityCheck(modules, Stream.of("p1").collect(Collectors.toSet()));
        }
        {
            Map<String, ProgramUnitInfo> modules = getModulesInfo(MUL_TARGETS_FILEPATH);
            Build.sanityCheck(modules, Stream.of("t1", "t2", "t3").collect(Collectors.toSet()));
        }
        {
            Map<String, ProgramUnitInfo> modules = getModulesInfo(UNDEFINED_MOD_FILEPATH);
            boolean exCaught = false;
            try
            {
                Build.sanityCheck(modules, Stream.of("p1").collect(Collectors.toSet()));
            } catch (FortranSemanticException e)
            {
                String errMsg = e.getMessage();
                assertTrue(errMsg.contains("Module \"mod_undefined\" used by MODULE p1"));
                assertTrue(
                        errMsg.contains("undefined_module.f90:12) is not defined in any file under given search path"));
                exCaught = true;
            }
            assertTrue(exCaught);
        }
        {
            Map<String, ProgramUnitInfo> modules = getModulesInfo(CIRCLE_DEP_FILEPATH);
            boolean exCaught = false;
            try
            {
                Build.sanityCheck(modules, Stream.of("t").collect(Collectors.toSet()));
            } catch (FortranSemanticException e)
            {
                String errMsg = e.getMessage();
                assertTrue(errMsg.contains("Circle dependency between MODULE mod1"));
                assertTrue(errMsg.contains("circle_dep.f90:1) and MODULE t"));
                assertTrue(errMsg.contains("circle_dep.f90:13"));
                exCaught = true;
            }
            assertTrue(exCaught);
        }
    }

    public void testRemoveUnreferencedModules() throws Exception
    {
        final Path IN_DIR = RES_DIR.resolve("build/remove_unref_modules/input");
        final Path FILEPATH = IN_DIR.resolve("unref_modules.f90");
        Map<String, ProgramUnitInfo> modules = getModulesInfo(FILEPATH);
        Map<String, ProgramUnitInfo> res = Build.removeUnreferencedModules(modules,
                Stream.of("t").collect(Collectors.toSet()));
        assertEquals(Stream.of("mod1", "t").collect(Collectors.toSet()), res.keySet());
    }

    List<String> getBuildSeq(Map<String, ProgramUnitInfo> modules, Set<String> targets)
    {
        List<String> res = new ArrayList<String>();
        BuildOrder order = Build.getParallelOrder(modules, targets);
        assertEquals(modules, order.getUsedModules());
        assertEquals(targets, order.getTargetModules());
        while (!order.done())
        {
            String next = order.next();
            res.add(next);
            order.onProcessed(next);
            assertTrue(order.getProcessedModules().contains(next));
        }
        return res;
    }

    void verifyParallelBuildOrder(Path filePath, List<String> targets, List<String> expBuildSeq) throws Exception
    {
        Map<String, ProgramUnitInfo> modules = getModulesInfo(filePath);
        List<String> ordSeq = getBuildSeq(modules, Stream.of("t").collect(Collectors.toSet()));
        assertEquals(expBuildSeq, ordSeq);
    }

    public void testParallelBuildOrder() throws Exception
    {
        final Path IN_DIR = RES_DIR.resolve("build/parallel_order/input");
        final Path FILEPATH1 = IN_DIR.resolve("1.f90");
        final Path FILEPATH2 = IN_DIR.resolve("2.f90");
        final Path FILEPATH3 = IN_DIR.resolve("3.f90");
        verifyParallelBuildOrder(FILEPATH1, Arrays.asList("t"), Arrays.asList("m11", "m12", "m1", "m2", "m3", "t"));
        verifyParallelBuildOrder(FILEPATH2, Arrays.asList("t"), Arrays.asList("m11", "m12", "m2", "m3", "m1", "t"));
        verifyParallelBuildOrder(FILEPATH3, Arrays.asList("t", "t2", "t3"),
                Arrays.asList("m11", "m12", "m2", "m3", "m1", "t", "t2", "t3"));
    }
}
