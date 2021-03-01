/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package claw.utils;

import static claw.utils.Utils.runClawfc;
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

public abstract class FailureTestCase extends TestCase
{
    protected static final Path RES_DIR = claw.tests.Resources.DIR;
    protected static final Path WORKING_DIR = claw.tests.Resources.DEFAULT_WORKING_DIR;
    protected static final Path DRIVER_PATH = claw.tests.Resources.DRIVER_PATH();

    public enum Target {
        CPU, GPU
    }

    public enum Directive {
        NONE, OPENACC, OPENMP
    }

    public static class InputParams
    {
        Target target;
        Directive directive;
        String errMsg;
        boolean debugClawfc = false;
        boolean debug = false;
        final String name;
        String original = "original_code.f90";
        String transformed = "transformed_code.f90";
        final Path resDir;
        final Path workingDir;

        public String getExpectedErrorMessage()
        {
            return errMsg;
        }

        public Target getTarget()
        {
            return target;
        }

        public Directive getDirective()
        {
            return directive;
        }

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

        public String getOutputDirName()
        {
            return outputDirName;
        }

        public void setOutputDirName(String outputDirName)
        {
            this.outputDirName = outputDirName;
        }

        String inputDirName = "input";
        String outputDirName = "output";
        String xmodDirName = "xmod";
        List<String> clawFlags = emptyList();
        Path clawTransSetPath = null;

        public Path getXmodDir()
        {
            Path path = getWorkingDir().resolve(xmodDirName);
            return path;
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

        public Path originalFilePath()
        {
            return inputDirPath().resolve(original);
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

        public InputParams(String name, Path resDir, Path workingDir, Target target, Directive directive, String errMsg)
        {
            super();
            this.name = name;
            this.resDir = resDir;
            this.workingDir = workingDir;
            this.target = target;
            this.directive = directive;
            this.errMsg = errMsg;
        }
    }

    List<String> getTargetClawFlags(InputParams p)
    {
        List<String> flags = new ArrayList<String>();
        switch (p.getTarget())
        {
        case CPU:
            flags.add("--target=cpu");
            break;
        case GPU:
            flags.add("--target=gpu");
            break;
        }
        switch (p.getDirective())
        {
        case NONE:
            flags.add("--directive=none");
            break;
        case OPENACC:
            flags.add("--directive=openacc");
            break;
        case OPENMP:
            flags.add("--directive=openmp");
            break;
        }
        return flags;
    }

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
        if (p.getClawTransSetPath() != null)
        {
            if (!dirExists(p.getClawTransSetPath()))
            {
                throw new Exception(sprintf("Transformation set dir %s not found", p.getClawTransSetPath()));
            }
        }
        final Path xmodDirPath = p.getXmodDir();
        recreateDir(p.outputDirPath());
        recreateDir(xmodDirPath);
        {// Transform original file
            List<String> args = new ArrayList<String>();
            args.addAll(Arrays.asList(p.originalFilePath().toString(), "-o", p.transformedFilePath().toString()));
            args.addAll(p.getClawFlags());
            List<String> targetArgs = getTargetClawFlags(p);
            args.addAll(targetArgs);
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
            args.addAll(Arrays.asList("-J", xmodDirPath.toString()));
            final String expErrMsg = p.getExpectedErrorMessage();
            try
            {
                runClawfc(args, p.getWorkingDir(), p.isDebugClawfc());
            } catch (Exception e)
            {
                if (e.getCause().getMessage().contains(expErrMsg))
                {
                    return;
                } else
                {
                    throw new Exception(sprintf("Expected error message \"%s\" not caught", expErrMsg), e);
                }
            }
            throw new Exception(sprintf("Expected error message \"%s\" not caught", expErrMsg));
        }
    }
}