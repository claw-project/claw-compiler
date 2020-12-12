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
    final ThreadLocal<Path> workingDir;
    final Path driverTempDir;
    final PreprocessorOutputScanner outputScanner;

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
                opts.predefinedMacros(), opts.preprocessingIncludeDirs()));
        this.workingDir = new ThreadLocal<Path>();
        this.driverTempDir = driverTempDir;
        outputScanner = new PreprocessorOutputScanner();
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
            PreprocessorInfo info, List<String> cmdArgsTemplate, PreprocessorOutputScanner scanner)
            throws Exception, Failed
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
            AsciiArrayIOStream buf;
            if (info.supportsRedirection)
            {
                buf = new AsciiArrayIOStream();
                try (InputStream inStrm = p.getInputStream())
                {
                    Utils.copy(inStrm, buf);
                }
            } else
            {
                Path outFilePath = internalOutputFilePath(info, inputFilePath, workingDir);
                buf = new AsciiArrayIOStream(outFilePath);
                Files.delete(outFilePath);
            }
            final Byte lastChr = buf.getChr(buf.size() - 1);
            if (lastChr != null && lastChr != ASCII_NEWLINE_VALUE)
            {
                buf.write(ASCII_NEWLINE_VALUE);
            }
            AsciiArrayIOStream res = new AsciiArrayIOStream();
            Set<Path> resIncFilePaths = scanner.run(buf.getAsInputStreamUnsafe(), res);
            resIncFilePaths.remove(inputFilePath);
            if (outIncFilePaths != null)
            {
                outIncFilePaths.addAll(resIncFilePaths);
            }
            return res;
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
        Path localWorkingDir = workingDir.get();
        if (localWorkingDir == null)
        {
            localWorkingDir = Files.createTempDirectory(driverTempDir, "fpp");
            workingDir.set(localWorkingDir);
        }
        return run(inputFilePath, outIncFilePaths, localWorkingDir, info, cmdArgsTemplate, outputScanner);
    }
}
