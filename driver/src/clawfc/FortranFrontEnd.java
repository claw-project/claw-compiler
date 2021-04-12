/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc;

import static clawfc.Utils.copy;
import static clawfc.Utils.getOrCreateDir;
import static clawfc.Utils.replaceInLines;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import clawfc.utils.AsciiArrayIOStream;
import clawfc.utils.Subprocess;
import clawfc.utils.SubprocessFailed;

public class FortranFrontEnd
{
    class DirData
    {
        public final Path workingDir;
        public final Path xmodOutDir;

        public DirData(Path topTempDir) throws IOException
        {
            workingDir = Files.createTempDirectory(topTempDir, "ffront");
            xmodOutDir = Files.createTempDirectory(topTempDir, "ffront");
        }
    }

    final Configuration driverCfg;
    final List<Path> modDirs;
    final List<String> astOpts;
    final List<String> xmodOpts;
    final ThreadLocal<DirData> dirData;
    final Path outModDir;
    final Path driverTempDir;

    public Path getOutModDir()
    {
        return outModDir;
    }

    public static class Failed extends SubprocessFailed
    {
        public Failed(List<String> args, InputStream stdin, InputStream stderr, Exception cause) throws IOException
        {
            super(args, stdin, stderr, cause);
        }
    }

    public static Path intOutputDir(Path driverIntDir)
    {
        return driverIntDir.resolve("xmods");
    }

    public FortranFrontEnd(Configuration cfg, Options opts, Path driverTempDir) throws Exception
    {
        this.driverCfg = cfg;
        this.driverTempDir = driverTempDir;
        Set<Path> modDirs = new LinkedHashSet<Path>();
        modDirs.add(cfg.defaultStdXmodDir());
        modDirs.addAll(opts.moduleIncludeDirs());
        Path outModDir = null;
        if (opts.xmodOutputDir() != null)
        {
            outModDir = opts.xmodOutputDir();
        } else
        {
            outModDir = intOutputDir(driverTempDir);
        }
        getOrCreateDir(outModDir);
        this.outModDir = outModDir;
        modDirs.add(outModDir);
        this.modDirs = Collections.unmodifiableList(new ArrayList<Path>(modDirs));
        List<String> commonOpts = new ArrayList<String>(opts.translatorOptions());
        if (opts.debugOmniFFront())
        {
            commonOpts.add("-d");
        }
        if (opts.keepComments())
        {
            commonOpts.add("-fleave-comment");
        }
        if (opts.disableOmniFFrontModuleCache())
        {
            commonOpts.add("-no-module-cache");
        }
        commonOpts.addAll(opts.OmniFFrontOptions());
        List<String> astOpts = new ArrayList<String>(commonOpts);
        List<String> xmodOpts = new ArrayList<String>(commonOpts);
        xmodOpts.add("-module-compile");
        this.astOpts = Collections.unmodifiableList(astOpts);
        this.xmodOpts = Collections.unmodifiableList(xmodOpts);
        this.dirData = new ThreadLocal<DirData>();
    }

    /**
     * @param args Output parameter, only for error handling. Input contents
     *             ignored.
     * @throws Exception
     */
    public static boolean run(Configuration cfg, InputStream inputSrc, OutputStream output, OutputStream outputErr,
            Collection<Path> inputModDirs, Path modOutDir, Path workingDir, Collection<String> options,
            Path inputFilepath, List<String> args) throws Exception
    {
        final String inputFilepathStr = inputFilepath != null ? inputFilepath.toString() : null;
        if (args == null)
        {
            args = new ArrayList<String>();
        } else
        {
            args.clear();
        }
        args.add(cfg.omniFrontEnd().toString());
        args.add("-M" + modOutDir.toString());
        // Current Omni compiler will add the default path anyway, no matter the input
        // options
        // args.add("-M" + cfg.omniDefaultStdXmodDir().toString());
        for (Path modIncDir : inputModDirs)
        {
            args.add("-M" + modIncDir.toString());
        }
        args.addAll(options);
        final AsciiArrayIOStream ppStdout = new AsciiArrayIOStream();
        final AsciiArrayIOStream ppStderr = new AsciiArrayIOStream();
        final int retCode = Subprocess.call(args, workingDir, ppStdout, ppStderr, inputSrc);
        if (retCode == 0)
        {
            try (InputStream pStdout = ppStdout.getAsInputStreamUnsafe())
            {
                if (inputFilepathStr != null)
                {
                    replaceInLines(pStdout, output, "&lt;stdin&gt;", inputFilepathStr);
                } else
                {
                    copy(pStdout, output);
                }
            }
            return true;
        } else
        {
            try (InputStream pStderr = ppStderr.getAsInputStreamUnsafe())
            {
                if (inputFilepathStr != null)
                {
                    replaceInLines(pStderr, outputErr, "<stdin>", inputFilepathStr);
                } else
                {
                    copy(pStderr, outputErr);
                }
            }
            return false;
        }
    }

    public static boolean run(Configuration cfg, InputStream inputSrc, OutputStream outputSrc, OutputStream outputErr,
            Collection<Path> inputModDirs, Path modOutDir, Path workingDir, Collection<String> options,
            Path inputFilepath) throws Exception
    {
        return run(cfg, inputSrc, outputSrc, outputErr, inputModDirs, modOutDir, workingDir, options, inputFilepath,
                null);
    }

    public static boolean run(Configuration cfg, InputStream inputSrc, OutputStream outputSrc, OutputStream outputErr,
            Collection<Path> inputModDirs, Path modOutDir, Path workingDir, Collection<String> options) throws Exception
    {
        return run(cfg, inputSrc, outputSrc, outputErr, inputModDirs, modOutDir, workingDir, options, null, null);
    }

    void run(Path inputFilepath, InputStream inputSrc, OutputStream output, Collection<String> options) throws Exception
    {
        DirData localTmp = dirData.get();
        if (localTmp == null)
        {
            localTmp = new DirData(driverTempDir);
            dirData.set(localTmp);
        }
        AsciiArrayIOStream stderr = new AsciiArrayIOStream();
        List<String> args = new ArrayList<String>();
        try
        {
            boolean res = run(driverCfg, inputSrc, output, stderr, modDirs, localTmp.xmodOutDir, localTmp.workingDir,
                    options, inputFilepath, args);
            if (!res)
            {
                throw new Exception("Call to FortranFrontEnd failed");
            }
        } catch (Exception e)
        {
            inputSrc.reset();
            throw new Failed(args, inputSrc, stderr.getAsInputStreamUnsafe(), e);
        }
    }

    void generateXmod(Path inputFilepath, InputStream inputSrc, OutputStream output) throws Exception
    {
        run(inputFilepath, inputSrc, output, xmodOpts);
    }

    void generateAST(Path inputFilepath, InputStream inputSrc, OutputStream output) throws Exception
    {
        run(inputFilepath, inputSrc, output, astOpts);
    }
}
