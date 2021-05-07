/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc;

import static clawfc.Utils.copy;
import static clawfc.Utils.dumpIntoFile;
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
import java.util.List;

import clawfc.utils.AsciiArrayIOStream;
import clawfc.utils.Subprocess;
import clawfc.utils.SubprocessFailed;

public class FortranFrontEnd
{
    class DirData
    {
        public final Path workingDir;

        public DirData(Path topTempDir) throws IOException
        {
            workingDir = Files.createTempDirectory(topTempDir, "ffront");
        }
    }

    final Configuration driverCfg;
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
        List<String> commonOpts = new ArrayList<String>(opts.translatorOptions());
        commonOpts.add("--in-memory-mode");
        commonOpts.add("-no-time");
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
    public static boolean run(Configuration cfg, AsciiArrayIOStream inputSrc, Path outputFilePath,
            OutputStream outputErr, Collection<Path> depXmodPaths, Path workingDir, Collection<String> options,
            Path originalInputFilepath, List<String> args) throws Exception
    {
        // TODO: rewrite this with mapped files (i.e. shared memory IPC)
        if (args == null)
        {
            args = new ArrayList<String>();
        } else
        {
            args.clear();
        }
        args.add(cfg.omniFrontEnd().toString());
        Path inputFilePath = workingDir.resolve("stdin.txt");
        dumpIntoFile(inputFilePath, inputSrc.getAsInputStreamUnsafe());
        args.add(inputFilePath.toString());
        for (Path depXmod : depXmodPaths)
        {
            args.add("-m");
            args.add(depXmod.toString());
        }
        args.add("-so");
        args.add(outputFilePath.toString());
        args.addAll(options);
        final AsciiArrayIOStream ppStdout = new AsciiArrayIOStream();
        final AsciiArrayIOStream ppStderr = new AsciiArrayIOStream();
        final int retCode = Subprocess.call(args, workingDir, ppStdout, ppStderr);
        if (ppStdout.size() > 0)
        {
            throw new Exception("stdout redirection didn't work");
        }
        if (retCode == 0)
        {
            final String inFileStr = originalInputFilepath != null ? originalInputFilepath.toString() : "&lt;stdin&gt;";
            AsciiArrayIOStream out = new AsciiArrayIOStream(outputFilePath);
            AsciiArrayIOStream outWithRep = new AsciiArrayIOStream();
            replaceInLines(out.getAsInputStreamUnsafe(), outWithRep, inputFilePath.toString(), inFileStr);
            dumpIntoFile(outputFilePath, outWithRep.getAsInputStreamUnsafe());
            return true;
        } else
        {
            final String inFileStr = originalInputFilepath != null ? originalInputFilepath.toString() : "<stdin>";
            try (InputStream pStderr = ppStderr.getAsInputStreamUnsafe())
            {
                replaceInLines(pStderr, outputErr, inputFilePath.toString(), inFileStr);
            }
            return false;
        }
    }

    Path getWorkingDir() throws IOException
    {

        DirData localTmp = dirData.get();
        if (localTmp == null)
        {
            localTmp = new DirData(driverTempDir);
            dirData.set(localTmp);
        }
        return localTmp.workingDir;
    }

    void run(Path inputFilepath, AsciiArrayIOStream inputSrc, AsciiArrayIOStream output, Collection<Path> depXmodPaths,
            Collection<String> options) throws Exception
    {
        AsciiArrayIOStream stderr = new AsciiArrayIOStream();
        List<String> args = new ArrayList<String>();
        final Path workingDir = getWorkingDir();
        final Path stdoutFilePath = workingDir.resolve("ffront_jni_stdout.txt");
        stdoutFilePath.toFile().deleteOnExit();
        try
        {
            boolean res = run(driverCfg, inputSrc, stdoutFilePath, stderr, depXmodPaths, workingDir, options,
                    inputFilepath, args);
            if (res)
            {
                try (InputStream f = Files.newInputStream(stdoutFilePath))
                {
                    copy(f, output);
                }
            } else
            {
                throw new Exception("Call to FortranFrontEnd failed");
            }
        } catch (Exception e)
        {
            inputSrc.reset();
            throw new Failed(args, inputSrc.getAsInputStreamUnsafe(), stderr.getAsInputStreamUnsafe(), e);
        } finally
        {
            Files.delete(stdoutFilePath);
        }
    }

    public void generateXmod(Path inputFilepath, AsciiArrayIOStream inputSrc, AsciiArrayIOStream output,
            Collection<Path> depXmodPaths) throws Exception
    {
        run(inputFilepath, inputSrc, output, depXmodPaths, xmodOpts);
    }

    public void generateAST(Path inputFilepath, AsciiArrayIOStream inputSrc, AsciiArrayIOStream output,
            Collection<Path> depXmodPaths) throws Exception
    {
        run(inputFilepath, inputSrc, output, depXmodPaths, astOpts);
    }
}
