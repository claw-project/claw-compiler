/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package claw.utils;

import static claw.utils.Utils.runClawfc;
import static claw.utils.Utils.runDiff;
import static claw.utils.Utils.runExecutable;
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

import junit.framework.TestCase;

public abstract class BasicTestCase extends TestCase
{
    protected static final Path RES_DIR = claw.tests.Resources.DIR;
    protected static final Path WORKING_DIR = claw.tests.Resources.DEFAULT_WORKING_DIR;
    protected static final Path DRIVER_PATH = claw.tests.Resources.DRIVER_PATH();

    public static class InputParams
    {
        boolean debugClawfc = false;
        boolean debug = false;
        boolean compile = true;
        boolean compare = false;
        boolean ignore = false;
        final String name;
        String original = "original_code.f90";
        String transformed = "transformed_code.f90";
        String reference = "reference.f90";
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
        List<String> clawFlags = emptyList();
        Path clawTransSetPath = null;

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

        public Path originalFilePath()
        {
            return inputDirPath().resolve(original);
        }

        public Path referenceFilePath()
        {
            return refDirPath().resolve(reference);
        }

        public Path outputDirPath()
        {
            return workingDir.resolve(outputDirName);
        }

        public Path transformedFilePath()
        {
            return outputDirPath().resolve(transformed);
        }

        public Path originalExecPath()
        {
            return outputDirPath().resolve("original_" + name);
        }

        public Path transformedExecPath()
        {
            return outputDirPath().resolve("transformed_" + name);
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
            return original;
        }

        public void setOriginal(String original)
        {
            this.original = original;
        }

        public String getTransformed()
        {
            return transformed;
        }

        public void setTransformed(String transformed)
        {
            this.transformed = transformed;
        }

        public String getReference()
        {
            return reference;
        }

        public void setReference(String reference)
        {
            this.reference = reference;
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

        public InputParams(String name, Path resDir, Path workingDir)
        {
            super();
            this.name = name;
            this.resDir = resDir;
            this.workingDir = workingDir;
        }
    }

    // void run(final Path inputDirRelPath, String inputFilename) throws Exception
    // final Path inputDirPath = inputDirRelPath.resolve(inputDirRelPath);
    protected void run(final InputParams p) throws Exception
    {
        if (!dirExists(p.inputDirPath()))
        {
            throw new Exception(sprintf("Input dir %s not found", p.inputDirPath()));
        }
        if (!fileExists(p.originalFilePath()))
        {
            throw new Exception(sprintf("Original file %s not found", p.originalFilePath()));
        }
        if (p.isCompare())
        {
            if (!dirExists(p.refDirPath()))
            {
                throw new Exception(sprintf("Reference dir %s not found", p.refDirPath()));
            }
            if (!fileExists(p.referenceFilePath()))
            {
                throw new Exception(sprintf("Reference file %s not found", p.referenceFilePath()));
            }
        }
        if (p.getClawTransSetPath() != null)
        {
            if (!dirExists(p.getClawTransSetPath()))
            {
                throw new Exception(sprintf("Transformation set dir %s not found", p.getClawTransSetPath()));
            }
        }
        recreateDir(p.outputDirPath());
        {// Transform original file
            List<String> args = new ArrayList<String>();
            args.addAll(Arrays.asList(p.originalFilePath().toString(), "-o", p.transformedFilePath().toString()));
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
            runClawfc(args, p.getResDir(), p.isDebugClawfc());
            assertTrue(fileExists(p.transformedFilePath()));
        }
        if (!p.isIgnore())
        {
            List<String> args = Arrays.asList("--ignore-all-space", "--ignore-blank-lines",
                    p.transformedFilePath().toString(), p.referenceFilePath().toString());
            runDiff(args, p.getWorkingDir());
        }
        if (p.isCompile())
        {
            {// Compile original
                List<String> args = Arrays.asList(p.originalFilePath().toString(), "-o",
                        p.originalExecPath().toString());
                runFC(args, p.getWorkingDir());
                assertTrue(fileExists(p.originalExecPath()));
            }
            {// Compile clawfc output
                List<String> args = Arrays.asList(p.transformedFilePath().toString(), "-o",
                        p.transformedExecPath().toString());
                runFC(args, p.getWorkingDir());
                assertTrue(fileExists(p.transformedExecPath()));
            }
            if (p.isCompare())
            {
                final String originalOutput = runExecutable("original executable", p.originalExecPath(), emptyList(),
                        p.getWorkingDir()).stdout;
                final String transformedOutput = runExecutable("transformed executable", p.transformedExecPath(),
                        emptyList(), p.getWorkingDir()).stdout;
                assertEquals(originalOutput, transformedOutput);
            }
        }
    }
}
