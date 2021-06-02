/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package claw.utils;

import static claw.utils.Utils.runClawfc;
import static claw.utils.Utils.runDiff;
import static claw.utils.Utils.runFC;
import static clawfc.Utils.dirExists;
import static clawfc.Utils.fileExists;
import static clawfc.Utils.recreateDir;
import static clawfc.Utils.sprintf;
import static java.util.Collections.emptyList;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import claw.tests.Resources;
import junit.framework.TestCase;

public abstract class AdvancedTestCase extends TestCase
{
    protected static final Path RES_DIR = claw.tests.Resources.DIR;
    protected static final Path WORKING_DIR = claw.tests.Resources.DEFAULT_WORKING_DIR;
    protected static final Path DRIVER_PATH = claw.tests.Resources.DRIVER_PATH();

    enum FileType {
        MAIN, ORIGINAL, ORIGINAL_EXTRA
    }

    enum Target {
        CPU, GPU_ACC, GPU_OMP
    }

    public static class InputParams
    {
        boolean debugClawfc = false;
        boolean debug = false;
        boolean compile = true;
        boolean link = false;
        boolean compare = false;
        boolean ignore = false;
        boolean enableCPUOpenMPTarget = false;
        final String name;
        String originalMainFilename = "main.f90";
        String originalFilename = "mo_column.f90";
        String originalExtraFilename = "mo_column_extra.f90";
        String transformedCPUFilename = "transformed_code_cpu.f90";
        String transformedACCFilename = "transformed_code_acc.f90";
        String transformedOMPFilename = "transformed_code_omp.f90";
        String transformedCPUMainFilename = "transformed_main_cpu.f90";
        String transformedACCMainFilename = "transformed_main_acc.f90";
        String transformedOMPMainFilename = "transformed_main_omp.f90";
        String transformedCPUExtraFilename = "transformed_extra_cpu.f90";
        String transformedACCExtraFilename = "transformed_extra_acc.f90";
        String transformedOMPExtraFilename = "transformed_extra_omp.f90";
        String referenceCPUFilename = "reference_cpu.f90";
        String referenceACCFilename = "reference_acc.f90";
        String referenceOMPFilename = "reference_omp.f90";
        String referenceCPUMainFilename = "reference_main_cpu.f90";
        String referenceACCMainFilename = "reference_main_acc.f90";
        String referenceOMPMainFilename = "reference_main_omp.f90";
        String referenceCPUExtraFilename = "reference_extra_cpu.f90";
        String referenceACCExtraFilename = "reference_extra_acc.f90";
        String referenceOMPExtraFilename = "reference_extra_omp.f90";

        String configFilename = null;
        String modelConfigFilename = null;

        final Path resDir;
        final Path workingDir;

        public boolean isDebugClawfc()
        {
            return debugClawfc;
        }

        public void setDebugClawfc(boolean debugClawfc)
        {
            this.debugClawfc = debugClawfc;
        }

        public String getInputDirName()
        {
            return inputDirName;
        }

        public void setInputDirName(String inputDirName)
        {
            this.inputDirName = inputDirName;
        }

        public String getRefDirName()
        {
            return refDirName;
        }

        public void setRefDirName(String refDirName)
        {
            this.refDirName = refDirName;
        }

        public String getOutputDirName()
        {
            return outputDirName;
        }

        public void setOutputDirName(String outputDirName)
        {
            this.outputDirName = outputDirName;
        }

        String inputDirName = "input";
        String refDirName = "reference";
        String outputDirName = "output";
        String xmodDirName = "xmod";
        String modDirName = "mod";
        List<String> clawFlags = emptyList();
        Path clawTransSetPath = null;

        public Path getXmodDir(Target target)
        {
            Path path = getWorkingDir().resolve(xmodDirName);
            return path.resolve(target.toString().toLowerCase());
        }

        public Path getModDir(Target target)
        {
            Path path = getWorkingDir().resolve(modDirName);
            return path.resolve(target.toString().toLowerCase());
        }

        public Path getClawTransSetPath()
        {
            return clawTransSetPath;
        }

        public void setClawTransSetPath(Path clawTransSetPath)
        {
            this.clawTransSetPath = clawTransSetPath;
        }

        public Path inputDirPath()
        {
            return resDir.resolve(inputDirName);
        }

        public Path refDirPath()
        {
            return resDir.resolve(refDirName);
        }

        public Path originalMainFilePath()
        {
            return inputDirPath().resolve(originalMainFilename);
        }

        public Path originalExtraFilePath()
        {
            return inputDirPath().resolve(originalExtraFilename);
        }

        public Path originalFilePath()
        {
            return inputDirPath().resolve(originalFilename);
        }

        public Path referenceCPUFilePath()
        {
            return refDirPath().resolve(referenceCPUFilename);
        }

        public Path referenceACCFilePath()
        {
            return refDirPath().resolve(referenceACCFilename);
        }

        public Path referenceOMPFilePath()
        {
            return refDirPath().resolve(referenceOMPFilename);
        }

        public Path referenceCPUMainFilePath()
        {
            return refDirPath().resolve(referenceCPUMainFilename);
        }

        public Path referenceACCMainFilePath()
        {
            return refDirPath().resolve(referenceACCMainFilename);
        }

        public Path referenceOMPMainFilePath()
        {
            return refDirPath().resolve(referenceOMPMainFilename);
        }

        public Path referenceCPUExtraFilePath()
        {
            return refDirPath().resolve(referenceCPUExtraFilename);
        }

        public Path referenceACCExtraFilePath()
        {
            return refDirPath().resolve(referenceACCExtraFilename);
        }

        public Path referenceOMPExtraFilePath()
        {
            return refDirPath().resolve(referenceOMPExtraFilename);
        }

        public Path outputDirPath()
        {
            return workingDir.resolve(outputDirName);
        }

        public Path transformedCPUFilePath()
        {
            return outputDirPath().resolve(transformedCPUFilename);
        }

        public Path transformedACCFilePath()
        {
            return outputDirPath().resolve(transformedACCFilename);
        }

        public Path transformedOMPFilePath()
        {
            return outputDirPath().resolve(transformedOMPFilename);
        }

        public Path transformedCPUMainFilePath()
        {
            return outputDirPath().resolve(transformedCPUMainFilename);
        }

        public Path transformedACCMainFilePath()
        {
            return outputDirPath().resolve(transformedACCMainFilename);
        }

        public Path transformedOMPMainFilePath()
        {
            return outputDirPath().resolve(transformedOMPMainFilename);
        }

        public Path transformedCPUExtraFilePath()
        {
            return outputDirPath().resolve(transformedCPUExtraFilename);
        }

        public Path transformedACCExtraFilePath()
        {
            return outputDirPath().resolve(transformedACCExtraFilename);
        }

        public Path transformedOMPExtraFilePath()
        {
            return outputDirPath().resolve(transformedOMPExtraFilename);
        }

        public boolean isDebug()
        {
            return debug;
        }

        public void setDebug(boolean debug)
        {
            this.debug = debug;
        }

        public boolean isCompile()
        {
            return compile;
        }

        public void setCompile(boolean compile)
        {
            this.compile = compile;
        }

        public boolean isLink()
        {
            return link;
        }

        public void setLink(boolean link)
        {
            this.link = link;
        }

        public boolean isCompare()
        {
            return compare;
        }

        public void setCompare(boolean compare)
        {
            this.compare = compare;
        }

        public boolean isIgnore()
        {
            return ignore;
        }

        public void setIgnore(boolean ignore)
        {
            this.ignore = ignore;
        }

        public String getName()
        {
            return name;
        }

        public String getOriginal()
        {
            return originalFilename;
        }

        public void setOriginal(String original)
        {
            this.originalFilename = original;
        }

        public String getTransformed()
        {
            return transformedCPUFilename;
        }

        public void setTransformed(String transformed)
        {
            this.transformedCPUFilename = transformed;
        }

        public String getReference()
        {
            return referenceCPUFilename;
        }

        public void setReference(String reference)
        {
            this.referenceCPUFilename = reference;
        }

        public Path getResDir()
        {
            return resDir;
        }

        public Path getWorkingDir()
        {
            return workingDir;
        }

        public List<String> getClawFlags()
        {
            return clawFlags;
        }

        public void setClawFlags(List<String> clawFlags)
        {
            this.clawFlags = clawFlags;
        }

        public void setConfigFilename(String filename)
        {
            this.configFilename = filename;
        }

        public void setModelConfigFilename(String filename)
        {
            this.modelConfigFilename = filename;
        }

        public Path configFilePath()
        {
            return inputDirPath().resolve(configFilename);
        }

        public boolean hasConfig()
        {
            return configFilename != null;
        }

        public Path modelConfigFilePath()
        {
            return inputDirPath().resolve(modelConfigFilename);
        }

        public boolean hasModelConfig()
        {
            return modelConfigFilename != null;
        }

        boolean useCPUOpenMPTarget()
        {
            return enableCPUOpenMPTarget;
        }

        public void setUseCPUOpenMPTarget(boolean val)
        {
            enableCPUOpenMPTarget = val;
        }

        public InputParams(String name, Path resDir, Path workingDir)
        {
            super();
            this.name = name;
            this.resDir = resDir;
            this.workingDir = workingDir;
        }
    }

    protected void run(final InputParams p) throws Exception
    {
        verifyInputParams(p);
        recreateDir(p.getWorkingDir());
        recreateDir(p.outputDirPath());
        final boolean hasExtra = fileExists(p.originalExtraFilePath());
        for (final Target target : Target.values())
        {
            // Transform original files
            final Path xmodDirPath = p.getXmodDir(target);
            recreateDir(xmodDirPath);
            transformFile(p, FileType.ORIGINAL, target);
            if (hasExtra)
            {
                transformFile(p, FileType.ORIGINAL_EXTRA, target);
            }
            transformFile(p, FileType.MAIN, target);
            if (p.isCompile())
            {
                final Path modDirPath = p.getModDir(target);
                recreateDir(modDirPath);
                List<Path> objFiles = new ArrayList<Path>();
                objFiles.add(compileFile(p, FileType.ORIGINAL, target, modDirPath));
                if (hasExtra)
                {
                    objFiles.add(compileFile(p, FileType.ORIGINAL_EXTRA, target, modDirPath));
                }
                objFiles.add(compileFile(p, FileType.MAIN, target, modDirPath));
                if (p.isLink())
                {
                    linkFiles(p, objFiles, target);
                }
            }
            if (!p.isIgnore())
            {
                compareFile(p, FileType.ORIGINAL, target);
                if (hasExtra)
                {
                    compareFile(p, FileType.ORIGINAL_EXTRA, target);
                }
                compareFile(p, FileType.MAIN, target);
            }
        }
    }

    void compareFile(final InputParams p, FileType fileType, final Target target) throws Exception
    {
        final Path transformedFilepath = getTransformedFilepath(p, fileType, target);
        final Path refFilepath = getReferenceFilepath(p, fileType, target);
        List<String> args = Arrays.asList("--ignore-all-space", "--ignore-blank-lines", transformedFilepath.toString(),
                refFilepath.toString());
        runDiff(args, p.getWorkingDir());
    }

    Path compileFile(final InputParams p, FileType fileType, final Target target, final Path modDirPath)
            throws Exception
    {
        final Path srcFilepath = getTransformedFilepath(p, fileType, target);
        final Path outputFilePath = srcFilepath.resolveSibling(srcFilepath.getFileName() + ".o");
        List<String> args = new ArrayList<String>();
        args.addAll(Arrays.asList("-c", srcFilepath.toString(), "-o", outputFilePath.toString()));
        args.addAll(getTargetFCFlags(target));
        runFC(args, p.getWorkingDir());
        assertTrue(fileExists(outputFilePath));
        return outputFilePath;
    }

    Path linkFiles(final InputParams p, List<Path> objFiles, final Target target) throws Exception
    {
        final Path outputFilePath = getExecFilepath(p, target);
        List<String> args = new ArrayList<String>();
        args.addAll(Arrays.asList("-o", outputFilePath.toString()));
        for (Path objFilePath : objFiles)
        {
            args.add(objFilePath.toString());
        }
        runFC(args, p.getWorkingDir());
        assertTrue(fileExists(outputFilePath));
        return outputFilePath;
    }

    Path getInputFilepath(final InputParams p, FileType type)
    {
        switch (type)
        {
        case MAIN:
        {
            return p.originalMainFilePath();
        }
        case ORIGINAL:
        {
            return p.originalFilePath();
        }
        case ORIGINAL_EXTRA:
        {
            return p.originalExtraFilePath();
        }
        }
        return null;
    }

    Path getTransformedFilepath(final InputParams p, FileType type, Target target)
    {
        switch (type)
        {
        case MAIN:
        {
            switch (target)
            {
            case CPU:
                return p.transformedCPUMainFilePath();
            case GPU_ACC:
                return p.transformedACCMainFilePath();
            case GPU_OMP:
                return p.transformedOMPMainFilePath();
            }
        }
        case ORIGINAL:
        {
            switch (target)
            {
            case CPU:
                return p.transformedCPUFilePath();
            case GPU_ACC:
                return p.transformedACCFilePath();
            case GPU_OMP:
                return p.transformedOMPFilePath();
            }
        }
        case ORIGINAL_EXTRA:
        {
            switch (target)
            {
            case CPU:
                return p.transformedCPUExtraFilePath();
            case GPU_ACC:
                return p.transformedACCExtraFilePath();
            case GPU_OMP:
                return p.transformedOMPExtraFilePath();
            }
        }
        }
        return null;
    }

    Path getReferenceFilepath(final InputParams p, FileType type, Target target)
    {
        switch (type)
        {
        case MAIN:
        {
            switch (target)
            {
            case CPU:
                return p.referenceCPUMainFilePath();
            case GPU_ACC:
                return p.referenceACCMainFilePath();
            case GPU_OMP:
                return p.referenceOMPMainFilePath();
            }
        }
        case ORIGINAL:
        {
            switch (target)
            {
            case CPU:
                return p.referenceCPUFilePath();
            case GPU_ACC:
                return p.referenceACCFilePath();
            case GPU_OMP:
                return p.referenceOMPFilePath();
            }
        }
        case ORIGINAL_EXTRA:
        {
            switch (target)
            {
            case CPU:
                return p.referenceCPUExtraFilePath();
            case GPU_ACC:
                return p.referenceACCExtraFilePath();
            case GPU_OMP:
                return p.referenceOMPExtraFilePath();
            }
        }
        }
        return null;
    }

    Path getExecFilepath(final InputParams p, Target target)
    {
        final String filename = sprintf("%s.exec", target).toLowerCase();
        return p.outputDirPath().resolve(filename);
    }

    final List<String> CPU_FLAGS = Arrays.asList("--target=cpu", "--directive=none");
    final List<String> CPU_OMP_FLAGS = Arrays.asList("--target=cpu", "--directive=openmp");
    final List<String> GPU_ACC_FLAGS = Arrays.asList("--target=gpu", "--directive=openacc");
    final List<String> GPU_OMP_FLAGS = Arrays.asList("--target=gpu", "--directive=openmp");

    List<String> getTargetClawFlags(InputParams p, Target target)
    {
        switch (target)
        {
        case CPU:
            return !p.useCPUOpenMPTarget() ? CPU_FLAGS : CPU_OMP_FLAGS;
        case GPU_ACC:
            return GPU_ACC_FLAGS;
        case GPU_OMP:
            return GPU_OMP_FLAGS;
        }
        return null;
    }

    List<String> getTargetFCFlags(Target target)
    {
        switch (target)
        {
        case CPU:
            return Arrays.asList(Resources.FC_TEST_BASE_FLAGS);
        case GPU_ACC:
            return Arrays.asList(Resources.FC_ACC_FLAGS);
        case GPU_OMP:
            return Arrays.asList(Resources.FC_OMP_FLAGS);
        }
        return null;
    }

    void transformFile(final InputParams p, FileType fileType, final Target target) throws Exception
    {
        final Path inFilepath = getInputFilepath(p, fileType);
        final Path outFilepath = getTransformedFilepath(p, fileType, target);
        final Path xmodDirPath = p.getXmodDir(target);
        final Path workingDir = p.getWorkingDir();
        List<String> args = new ArrayList<String>();
        args.addAll(Arrays.asList(inFilepath.toString(), "-o", outFilepath.toString()));
        args.addAll(p.getClawFlags());
        if (p.getClawTransSetPath() != null)
        {
            args.add("-td");
            args.add(p.getClawTransSetPath().toString());
        }
        if (p.isDebug())
        {
            args.add("--debug");
            args.add("--debug-omni");
        }
        args.addAll(getTargetClawFlags(p, target));
        args.addAll(Arrays.asList("-J", xmodDirPath.toString()));
        if (p.hasConfig())
        {
            args.add(sprintf("--config=%s", p.configFilePath()));
        }
        if (p.hasModelConfig())
        {
            args.add(sprintf("--model-config=%s", p.modelConfigFilePath()));
        }
        runClawfc(args, workingDir, p.isDebugClawfc());
        assertTrue(fileExists(outFilepath));
    }

    void verifyInputParams(final InputParams p) throws Exception
    {
        if (!dirExists(p.inputDirPath()))
        {
            throw new Exception(sprintf("Input dir %s not found", p.inputDirPath()));
        }
        if (!fileExists(p.originalFilePath()))
        {
            throw new Exception(sprintf("Original file %s not found", p.originalFilePath()));
        }
        if (!fileExists(p.originalMainFilePath()))
        {
            throw new Exception(sprintf("Original main file %s not found", p.originalMainFilePath()));
        }
        if (p.isCompare())
        {
            if (!dirExists(p.refDirPath()))
            {
                throw new Exception(sprintf("Reference dir %s not found", p.refDirPath()));
            }
            if (!fileExists(p.referenceCPUFilePath()))
            {
                throw new Exception(sprintf("Reference CPU file %s not found", p.referenceCPUFilePath()));
            }
            if (!fileExists(p.referenceACCFilePath()))
            {
                throw new Exception(sprintf("Reference OpenACC file %s not found", p.referenceACCFilePath()));
            }
            if (!fileExists(p.referenceOMPFilePath()))
            {
                throw new Exception(sprintf("Reference OpenMP file %s not found", p.referenceOMPFilePath()));
            }
            if (!fileExists(p.referenceCPUMainFilePath()))
            {
                throw new Exception(sprintf("Reference CPU main file %s not found", p.referenceCPUMainFilePath()));
            }
            if (!fileExists(p.referenceACCMainFilePath()))
            {
                throw new Exception(sprintf("Reference OpenACC main file %s not found", p.referenceACCMainFilePath()));
            }
            if (!fileExists(p.referenceOMPMainFilePath()))
            {
                throw new Exception(sprintf("Reference OpenMP main file %s not found", p.referenceOMPMainFilePath()));
            }
            if (fileExists(p.originalExtraFilePath()))
            {
                if (!fileExists(p.referenceCPUExtraFilePath()))
                {
                    throw new Exception(
                            sprintf("Reference CPU extra file %s not found", p.referenceCPUExtraFilePath()));
                }
                if (!fileExists(p.referenceACCExtraFilePath()))
                {
                    throw new Exception(
                            sprintf("Reference OpenACC extra file %s not found", p.referenceACCExtraFilePath()));
                }
                if (!fileExists(p.referenceOMPExtraFilePath()))
                {
                    throw new Exception(
                            sprintf("Reference OpenMP extra file %s not found", p.referenceOMPExtraFilePath()));
                }
            }
        }
        if (p.getClawTransSetPath() != null)
        {
            if (!dirExists(p.getClawTransSetPath()))
            {
                throw new Exception(sprintf("Transformation set dir %s not found", p.getClawTransSetPath()));
            }
        }
        if (p.hasConfig())
        {
            if (!fileExists(p.configFilePath()))
            {
                throw new Exception(sprintf("Config file %s not found", p.configFilePath()));
            }
        }
        if (p.hasModelConfig())
        {
            if (!fileExists(p.modelConfigFilePath()))
            {
                throw new Exception(sprintf("Model config file %s not found", p.modelConfigFilePath()));
            }
        }
    }
}
