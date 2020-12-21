/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc;

import static clawfc.Utils.ASCII_NEWLINE_VALUE;
import static clawfc.Utils.sprintf;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import clawfc.depscan.FortranIncludesResolver;
import clawfc.depscan.PreprocessorOutputScanner;
import clawfc.utils.AsciiArrayIOStream;
import clawfc.utils.SubprocessFailed;

public class Preprocessor
{
    public static class PreprocessorInfo
    {
        final String cmd;
        final String type;
        final String version;
        final List<String> flags;
        final String getVersionFlag;
        final String macro;
        final boolean supportsRedirection;

        public PreprocessorInfo(String cmd, String type) throws Exception
        {
            this.cmd = cmd;
            this.type = type;
            switch (type)
            {
            case "Cray":
            {
                flags = Collections.unmodifiableList(Arrays.asList("-eP", "-hnoomp"));
                macro = "_CRAYFTN";
                supportsRedirection = false;
                getVersionFlag = "--version";
                break;
            }
            case "Intel":
            {
                flags = Collections.unmodifiableList(Arrays.asList("-preprocess-only", "-E"));
                macro = "__ICC";
                supportsRedirection = true;
                getVersionFlag = "--version";
                break;
            }
            case "GNU":
            {
                flags = Collections.unmodifiableList(Arrays.asList("-E", "-cpp"));
                macro = "_GNU";
                supportsRedirection = true;
                getVersionFlag = "--version";
                break;
            }
            case "NAG":
            {
                flags = Collections.unmodifiableList(Arrays.asList("-Wp,-w,-P", "-F", "-fpp"));
                macro = "NAGFOR";
                supportsRedirection = true;
                getVersionFlag = "-V";
                break;
            }
            case "PGI":
            {
                flags = Collections.unmodifiableList(Arrays.asList("-E", "-cpp"));
                macro = "_PGI";
                supportsRedirection = true;
                getVersionFlag = "--version";
                break;
            }
            default:
                throw new RuntimeException("Unknown compiler type");
            }
            ;
            this.version = Utils.getCmdOutput(cmd, getVersionFlag);
        }
    }

    final PreprocessorInfo info;
    final List<String> cmdArgsTemplate;

    class ThreadLocalData
    {
        public final Path workingDir;
        public final FortranIncludesResolver includesResolver;
        public final PreprocessorOutputScanner outputScanner;

        public ThreadLocalData(Path workingDir) throws Exception
        {
            this.workingDir = workingDir;
            includesResolver = new FortranIncludesResolver();
            outputScanner = new PreprocessorOutputScanner();
        }
    }

    final ThreadLocal<ThreadLocalData> threadLocalData;
    final Path driverTempDir;

    final List<Path> ppIncSearchPath;

    public Preprocessor(Configuration cfg, Options opts, Path driverTempDir) throws Exception
    {
        String ppCmd = opts.fortranCompilerCmd();
        if (ppCmd == null)
        {
            ppCmd = cfg.defaultFortranCompilerCmd();
        }
        String ppType = opts.fortranCompilerType();
        if (ppType == null)
        {
            ppType = cfg.defaultFortranCompilerType();
        }
        info = new PreprocessorInfo(ppCmd, ppType);
        cmdArgsTemplate = Collections.unmodifiableList(prepareArgs(info, opts.acceleratorDirectiveLanguage(),
                opts.predefinedMacros(), opts.preprocessorIncludeDirs()));
        this.threadLocalData = new ThreadLocal<ThreadLocalData>();
        this.driverTempDir = driverTempDir;
        ppIncSearchPath = opts.preprocessorIncludeDirs();
    }

    public static class Failed extends SubprocessFailed
    {
        public Failed(List<String> args, InputStream stdin, InputStream stderr) throws IOException
        {
            super(args, stdin, stderr);
        }
    }

    static final Set<String> FORTRAN_FILE_EXTENSIONS_SET = Collections
            .unmodifiableSet(new HashSet<String>(Arrays.asList(Utils.FORTRAN_FILE_EXTENSIONS)));

    public static String outputFilename(Path inputFile)
    {
        String basename = inputFile.getFileName().toString();
        int i = basename.lastIndexOf('.');
        if (i != -1)
        {
            String ext = basename.substring(i + 1);
            if (FORTRAN_FILE_EXTENSIONS_SET.contains(ext))
            {
                return basename.substring(0, i) + ".pp." + ext;
            }
        }
        return basename + ".pp";
    }

    static Path internalOutputFilePath(PreprocessorInfo info, Path inFilePath, Path workingDir) throws Exception
    {
        switch (info.type)
        {
        case "Cray":
            return workingDir.resolve(inFilePath.getFileName() + ".i");
        default:
            throw new Exception("Not supported");
        }
    }

    public static List<String> prepareArgs(PreprocessorInfo info, String accDirLanguage, List<String> predefinedMacros,
            List<Path> ppIncludeDirs) throws Exception
    {
        List<String> args = new ArrayList<String>();
        args.add(info.cmd);
        args.add("-D_CLAW");
        args.addAll(info.flags);
        args.add("-D" + info.macro);
        if (accDirLanguage != null)
        {
            accDirLanguage = accDirLanguage.toLowerCase();
            switch (accDirLanguage)
            {
            case "acc":
            case "openacc":
            {
                args.add("-D_OPENACC");
                break;
            }
            case "openmp":
            case "omp":
            {
                args.add("-D_OPENMP");
                break;
            }
            default:
            {
                throw new Exception(sprintf("Unknown accelerator directive language \"%s\"", accDirLanguage));
            }
            }
        }
        for (String macro : predefinedMacros)
        {
            args.add("-D" + macro);
        }
        for (Path dir : ppIncludeDirs)
        {
            if (!dir.isAbsolute())
            {
                throw new Exception("Preprocessor include directories should be given with absolute paths."
                        + sprintf(" \"%s\" does not satisfy the restriction", dir));
            }
            args.add("-I" + dir.toString());
        }
        return args;
    }

    public static AsciiArrayIOStream run(Path inputFilePath, Set<Path> outIncFilePaths, Path workingDir,
            PreprocessorInfo info, List<String> cmdArgsTemplate, FortranIncludesResolver includesResolver,
            List<Path> ppIncSearchPath, PreprocessorOutputScanner scanner) throws Exception, Failed
    {
        if (outIncFilePaths != null)
        {
            outIncFilePaths.clear();
        }
        List<String> args = new ArrayList<String>();
        args.addAll(cmdArgsTemplate);
        args.add(inputFilePath.toString());
        if (!inputFilePath.isAbsolute())
        {
            throw new Exception("Input source file should be given with absolute path. "
                    + sprintf(" \"%s\" does not satisfy the requirement", inputFilePath));
        }
        // ------------------------------------------
        ProcessBuilder pb = new ProcessBuilder(args);
        pb.directory(workingDir.toFile());
        Process p = pb.start();
        final int retCode = p.waitFor();
        if (retCode == 0)
        {
            AsciiArrayIOStream bufPP;
            if (info.supportsRedirection)
            {
                bufPP = new AsciiArrayIOStream();
                try (InputStream inStrm = p.getInputStream())
                {
                    Utils.copy(inStrm, bufPP);
                }
            } else
            {
                Path outFilePath = internalOutputFilePath(info, inputFilePath, workingDir);
                bufPP = new AsciiArrayIOStream(outFilePath);
                Files.delete(outFilePath);
            }
            final Byte lastChr = bufPP.getChr(bufPP.size() - 1);
            if (lastChr != null && lastChr != ASCII_NEWLINE_VALUE)
            {
                bufPP.write(ASCII_NEWLINE_VALUE);
            }
            AsciiArrayIOStream bufNoMarkers = new AsciiArrayIOStream();
            Set<Path> resIncFilePaths = scanner.run(bufPP.getAsInputStreamUnsafe(), bufNoMarkers);
            bufPP = null;
            resIncFilePaths.remove(inputFilePath);
            AsciiArrayIOStream bufNoFtnInc = new AsciiArrayIOStream();
            Set<Path> ftnIncFilePaths = includesResolver.run(inputFilePath, bufNoMarkers, bufNoFtnInc, ppIncSearchPath);
            resIncFilePaths.addAll(ftnIncFilePaths);
            if (outIncFilePaths != null)
            {
                outIncFilePaths.addAll(resIncFilePaths);
            }
            return bufNoFtnInc;
        } else
        {
            try (InputStream pStderr = p.getErrorStream())
            {
                throw new Failed(args, null, pStderr);
            }
        }
    }

    public AsciiArrayIOStream run(Path inputFilePath, Set<Path> outIncFilePaths) throws Failed, Exception
    {
        ThreadLocalData localData = threadLocalData.get();
        if (localData == null)
        {
            localData = new ThreadLocalData(Files.createTempDirectory(driverTempDir, "fpp"));
            threadLocalData.set(localData);
        }
        return run(inputFilePath, outIncFilePaths, localData.workingDir, info, cmdArgsTemplate,
                localData.includesResolver, ppIncSearchPath, localData.outputScanner);
    }
}
