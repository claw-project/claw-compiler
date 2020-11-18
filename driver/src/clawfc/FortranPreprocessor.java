/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import clawfc.utils.AsciiArrayIOStream;

class FortranPreprocessorInfo
{
    final String cmd;
    final String type;
    final String version;
    final List<String> flags;
    final String getVersionFlag;
    final String macro;
    final boolean supportsRedirection;

    protected FortranPreprocessorInfo(String cmd, String type) throws Exception
    {
        this.cmd = cmd;
        this.type = type;
        switch (type)
        {
        case "Cray":
        {
            flags = Collections.unmodifiableList(Arrays.asList("-e", "P", "-hnoomp"));
            macro = "_CRAYFTN";
            supportsRedirection = false;
            getVersionFlag = "--version";
            break;
        }
        case "Intel":
        {
            flags = Collections.unmodifiableList(Arrays.asList("-fpp", "-E"));
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
            flags = Collections.unmodifiableList(Arrays.asList("-E"));
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

public class FortranPreprocessor
{
    final FortranPreprocessorInfo info;
    final Configuration cfg;
    final Options opts;

    public FortranPreprocessor(Configuration cfg, Options opts) throws Exception
    {
        this.cfg = cfg;
        this.opts = opts;
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
        info = new FortranPreprocessorInfo(ppCmd, ppType);
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

    public static class Failed extends Exception
    {
        final String stderr;

        public Failed(String msg, String stderr)
        {
            super(msg);
            this.stderr = stderr;
        }
    }

    public AsciiArrayIOStream apply(Path inputFilePath) throws Failed, Exception
    {
        List<String> args = new ArrayList<String>();
        args.add(info.cmd);
        args.add("-D_CLAW");
        args.addAll(info.flags);
        args.add("-D" + info.macro);
        String accDirLanguage = opts.acceleratorDirectiveLanguage();
        if (accDirLanguage != null)
        {
            accDirLanguage = accDirLanguage.toLowerCase();
            switch (opts.acceleratorDirectiveLanguage())
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
                throw new RuntimeException(
                        String.format("Unknown accelerator directive language \"%s\"", accDirLanguage));
            }
            }
        }
        for (String macro : opts.predefinedMacros())
        {
            args.add("-D" + macro);
        }
        for (Path dir : opts.preprocessingIncludeDirs())
        {
            args.add("-I" + dir.toString());
        }
        Path tmpDir = null;
        Path outFilePath = null;
        if (!info.supportsRedirection)
        {
            tmpDir = Files.createTempDirectory(Paths.get(Utils.DEFAULT_TOP_TEMP_DIR), "clawfc");
            outFilePath = tmpDir.resolve(outputFilename(inputFilePath));
            args.add("-o");
            args.add(outFilePath.toString());
        }
        args.add(inputFilePath.toString());
        // ------------------------------------------
        ProcessBuilder pb = new ProcessBuilder(args);
        Process p = pb.start();
        final int retCode = p.waitFor();
        if (retCode != 0)
        {
            String errStrm = Utils.collectIntoString(p.getErrorStream());
            throw new Failed(String.format("Cmd \"%s\" failed with return code %s", String.join(" ", args), retCode),
                    errStrm);
        }
        AsciiArrayIOStream res = new AsciiArrayIOStream();
        if (info.supportsRedirection)
        {
            InputStream inStrm = null;
            inStrm = p.getInputStream();
            Utils.copy(inStrm, res);
        } else
        {
            try (FileInputStream inStrm = new FileInputStream(outFilePath.toString()))
            {
                Utils.copy(inStrm, res);
            }
            Files.delete(outFilePath);
        }
        return res;
    }

    public AsciiArrayIOStream apply(Path inputFilePath, Path outFilePath) throws Exception
    {
        AsciiArrayIOStream resStrm = apply(inputFilePath);
        if (outFilePath != null)
        {
            String outFilename = outputFilename(inputFilePath);
            Path outDirPath = Utils.dirPath(outFilePath);
            if (!Utils.dirExists(outDirPath))
            {
                throw new RuntimeException(
                        String.format("Output directory \"%s\" does not exist", outDirPath.toString()));
            }
            File outFile = new File(outFilePath.toString());
            if (outFile.exists())
            {
                outFile.delete();
            }
            try (FileOutputStream outStrm = new FileOutputStream(outFile))
            {
                Utils.copy(resStrm.getAsInputStream(), outStrm);
            }
        }
        return resStrm;
    }
}
