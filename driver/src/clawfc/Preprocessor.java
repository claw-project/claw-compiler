/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc;

import static clawfc.Utils.ASCII_NEWLINE_VALUE;
import static clawfc.Utils.copy;
import static clawfc.Utils.recreateDir;
import static clawfc.Utils.sprintf;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import clawfc.Configuration.FortranCompilerVendor;
import clawfc.depscan.FortranIncludesResolver;
import clawfc.depscan.PreprocessorOutputScanner;
import clawfc.utils.AsciiArrayIOStream;
import clawfc.utils.Subprocess;
import clawfc.utils.SubprocessFailed;

public class Preprocessor
{
    public static class PreprocessorInfo
    {
        final String cmd;
        final FortranCompilerVendor vendor;
        final String version;
        final List<String> flags;
        final String getVersionFlag;
        final String macro;
        final boolean supportsRedirection;

        public PreprocessorInfo(String cmd, FortranCompilerVendor vendor) throws Exception
        {
            this.cmd = cmd;
            this.vendor = vendor;
            switch (vendor)
            {
            case cray:
            {
                flags = Collections.unmodifiableList(Arrays.asList("-eP", "-hnoomp"));
                macro = "_CRAYFTN";
                supportsRedirection = false;
                getVersionFlag = "--version";
                break;
            }
            case intel:
            {
                flags = Collections.unmodifiableList(Arrays.asList("-preprocess-only", "-E"));
                macro = "__ICC";
                supportsRedirection = true;
                getVersionFlag = "--version";
                break;
            }
            case gnu:
            {
                flags = Collections.unmodifiableList(Arrays.asList("-E", "-cpp"));
                macro = "_GNU";
                supportsRedirection = true;
                getVersionFlag = "--version";
                break;
            }
            case nag:
            {
                flags = Collections.unmodifiableList(Arrays.asList("-Wp,-w,-P", "-F", "-fpp"));
                macro = "NAGFOR";
                supportsRedirection = true;
                getVersionFlag = "-V";
                break;
            }
            case portland:
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
        public final AddIgnoreDirectiveFilter addIgnoreFilter;
        public final TrailingBackslashCommentsFilter trailingBSFilter;

        public ThreadLocalData(Path workingDir) throws Exception
        {
            final long threadId = Thread.currentThread().getId();
            this.workingDir = workingDir.resolve(sprintf("pp/%s", threadId));
            includesResolver = new FortranIncludesResolver();
            outputScanner = new PreprocessorOutputScanner();
            addIgnoreFilter = new AddIgnoreDirectiveFilter();
            trailingBSFilter = new TrailingBackslashCommentsFilter();
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
        FortranCompilerVendor ppType = opts.fortranCompilerType();
        if (ppType == null)
        {
            ppType = cfg.defaultFortranCompilerVendor();
        }
        info = new PreprocessorInfo(ppCmd, ppType);
        cmdArgsTemplate = Collections
                .unmodifiableList(prepareArgs(info, opts.acceleratorDirectiveLanguage(), opts.predefinedMacros()));
        this.threadLocalData = new ThreadLocal<ThreadLocalData>();
        this.driverTempDir = driverTempDir;
        ppIncSearchPath = opts.preprocessorIncludeDirs();
    }

    public static class Failed extends SubprocessFailed
    {
        public Failed(List<String> args, InputStream stdin, InputStream stderr) throws IOException
        {
            super(args, stdin, stderr, null);
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
        switch (info.vendor)
        {
        case cray:
            return workingDir.resolve(inFilePath.getFileName() + ".i");
        default:
            throw new Exception("Not supported");
        }
    }

    public static List<String> prepareArgs(PreprocessorInfo info, String accDirLanguage, List<String> predefinedMacros)
            throws Exception
    {
        List<String> args = new ArrayList<String>();
        args.add(info.cmd);
        args.add("-D_CLAW");
        args.addAll(info.flags);
        args.add("-D" + info.macro);
        if (accDirLanguage != null)
        {
            accDirLanguage = accDirLanguage.toLowerCase().trim();
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
            case "none":
            {
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
        return args;
    }

    public static List<String> prepareIncDirsArgs(Path inFilepath, List<Path> ppIncludeDirs) throws Exception
    {
        List<String> args = new ArrayList<String>();
        final Path inFilePathDir = inFilepath.getParent();
        args.add("-I" + inFilePathDir.toString());
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

    public static AsciiArrayIOStream run(final Path inputFilePath, Set<Path> outIncFilePaths, Path workingDir,
            PreprocessorInfo info, List<String> cmdArgsTemplate, FortranIncludesResolver includesResolver,
            List<Path> ppIncSearchPath, PreprocessorOutputScanner scanner, AddIgnoreDirectiveFilter addIgnoreFilter,
            TrailingBackslashCommentsFilter trailingBSFilter) throws Exception, Failed
    {

        recreateDir(workingDir);
        if (outIncFilePaths != null)
        {
            outIncFilePaths.clear();
        }
        if (!inputFilePath.isAbsolute())
        {
            throw new Exception("Input source file should be given with absolute path. "
                    + sprintf(" \"%s\" does not satisfy the requirement", inputFilePath));
        }
        final Path intInputFilePath;
        if (trailingBSFilter != null)
        {
            final Path notrailingBSFilePath = workingDir.resolve(inputFilePath.getFileName());
            try (InputStream in = Files.newInputStream(inputFilePath);
                    AsciiArrayIOStream bufNoTrailingBS = new AsciiArrayIOStream();
                    OutputStream outNoTrailingBS = Files.newOutputStream(notrailingBSFilePath))
            {
                trailingBSFilter.run(in, bufNoTrailingBS);
                copy(bufNoTrailingBS.getAsInputStreamUnsafe(), outNoTrailingBS);
            }
            intInputFilePath = notrailingBSFilePath;
        } else
        {
            intInputFilePath = inputFilePath;
        }
        List<String> args = new ArrayList<String>();
        args.addAll(cmdArgsTemplate);
        args.addAll(prepareIncDirsArgs(inputFilePath, ppIncSearchPath));
        args.add(intInputFilePath.toString());
        // ------------------------------------------
        final AsciiArrayIOStream ppStdout = new AsciiArrayIOStream();
        final AsciiArrayIOStream ppStderr = new AsciiArrayIOStream();
        final int retCode = Subprocess.call(args, workingDir, ppStdout, ppStderr);
        if (retCode == 0)
        {
            AsciiArrayIOStream bufPP;
            if (info.supportsRedirection)
            {
                bufPP = ppStdout;
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
            resIncFilePaths.remove(intInputFilePath);
            AsciiArrayIOStream bufWithIgnore = new AsciiArrayIOStream();
            addIgnoreFilter.run(bufNoMarkers.getAsInputStreamUnsafe(), bufWithIgnore);
            bufNoMarkers = null;
            AsciiArrayIOStream bufNoFtnInc = new AsciiArrayIOStream();
            Set<Path> ftnIncFilePaths = includesResolver.run(inputFilePath, bufWithIgnore, bufNoFtnInc,
                    ppIncSearchPath);
            resIncFilePaths.addAll(ftnIncFilePaths);
            if (outIncFilePaths != null)
            {
                outIncFilePaths.addAll(resIncFilePaths);
            }
            return bufNoFtnInc;
        } else
        {
            try (InputStream pStderrStrm = ppStderr.getAsInputStreamUnsafe())
            {
                throw new Failed(args, null, pStderrStrm);
            }
        }
    }

    public AsciiArrayIOStream run(Path inputFilePath, Set<Path> outIncFilePaths) throws Failed, Exception
    {
        ThreadLocalData lData = threadLocalData.get();
        if (lData == null)
        {
            lData = new ThreadLocalData(Files.createTempDirectory(driverTempDir, "fpp"));
            threadLocalData.set(lData);
        }
        return run(inputFilePath, outIncFilePaths, lData.workingDir, info, cmdArgsTemplate, lData.includesResolver,
                ppIncSearchPath, lData.outputScanner, lData.addIgnoreFilter, lData.trailingBSFilter);
    }
}
