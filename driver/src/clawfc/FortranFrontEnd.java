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
import static clawfc.Utils.sprintf;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
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
        public final Path debugDir;

        public DirData(Path topTempDir, Path topDebugDir, int threadID) throws IOException
        {
            final String dirName = sprintf("ffront_%d", threadID);
            workingDir = Files.createDirectory(topTempDir.resolve(dirName));
            debugDir = topDebugDir != null ? topDebugDir.resolve(dirName) : null;
        }
    }

    final Configuration driverCfg;
    final List<String> astOpts;
    final List<String> xmodOpts;
    final ThreadLocal<DirData> dirData;
    final Path outModDir;
    final Path driverTempDir;
    int threadsCounter;
    final Path debugDir;

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

    synchronized int getNewThreadID()
    {
        return ++threadsCounter;
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
        debugDir = opts.ffrontDebugDir();
        threadsCounter = 0;
    }

    /**
     * @param args Output parameter, only for error handling. Input contents
     *             ignored.
     * @throws Exception
     */
    public static boolean run(Configuration cfg, AsciiArrayIOStream inputSrc, Path outputFilePath,
            OutputStream outputErr, Collection<Path> depXmodPaths, Path workingDir, Collection<String> options,
            Path originalInputFilepath, List<String> args, Path debugDir) throws Exception
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
        if (retCode == 0)
        {
            if (ppStdout.size() > 0)
            {
                throw new Exception("stdout redirection didn't work");
            }
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
            if (ppStdout.size() > 0)
            {// For some reason java dumps JNI crash info into stdout
                copy(ppStdout.getAsInputStreamUnsafe(), outputErr);
            }
            if (debugDir != null)
            {// Collect all input and args in one place to make debugging easier
                createDebugDir(cfg, inputSrc, depXmodPaths, options, debugDir);
            }
            return false;
        }
    }

    static void createDebugDir(Configuration cfg, AsciiArrayIOStream inputSrc, Collection<Path> depXmodPaths,
            Collection<String> options, Path debugDir)
    {
        try
        {
            Utils.recreateDir(debugDir);
            Path inputFilePath = debugDir.resolve("stdin.txt");
            dumpIntoFile(inputFilePath, inputSrc.getAsInputStreamUnsafe());
            List<String> args = new ArrayList<String>();
            args.add(cfg.omniFrontEnd().toString());
            args.add("stdin.txt");
            args.add("-so");
            args.add("stdout.txt");
            for (Path depXmod : depXmodPaths)
            {
                Files.copy(depXmod, debugDir.resolve(depXmod.getFileName()));
                args.add("-m");
                args.add(depXmod.getFileName().toString());
            }
            args.addAll(options);
            Path debugScriptPath = debugDir.resolve("debug.sh");
            try (AsciiArrayIOStream debugScript = new AsciiArrayIOStream())
            {
                try (PrintWriter writer = new PrintWriter(debugScript))
                {
                    writer.println("#!/bin/bash");
                    writer.println("SCRIPT_DIR=\"$( cd \"$( dirname \"${BASH_SOURCE[0]}\" )\" &> /dev/null && pwd )\"");
                    writer.println("cd ${SCRIPT_DIR}");
                    writer.println(String.join(" ", args));
                }
                dumpIntoFile(debugScriptPath, debugScript.getAsInputStreamUnsafe());
            }
        } catch (Exception e)
        { // This method should not throw
        }

    }

    DirData getLocalDirData() throws IOException
    {

        DirData localTmp = dirData.get();
        if (localTmp == null)
        {
            localTmp = new DirData(driverTempDir, debugDir, getNewThreadID());
            dirData.set(localTmp);
        }
        return localTmp;
    }

    void run(Path inputFilepath, AsciiArrayIOStream inputSrc, AsciiArrayIOStream output, Collection<Path> depXmodPaths,
            Collection<String> options) throws Exception
    {
        AsciiArrayIOStream stderr = new AsciiArrayIOStream();
        List<String> args = new ArrayList<String>();
        final DirData localDirData = getLocalDirData();
        final Path stdoutFilePath = localDirData.workingDir.resolve("ffront_jni_stdout.txt");
        stdoutFilePath.toFile().deleteOnExit();
        try
        {
            boolean res = run(driverCfg, inputSrc, stdoutFilePath, stderr, depXmodPaths, localDirData.workingDir,
                    options, inputFilepath, args, localDirData.debugDir);
            if (res)
            {
                try (InputStream f = Files.newInputStream(stdoutFilePath))
                {
                    copy(f, output);
                }
            } else
            {
                String errMsg = "Call to FortranFrontEnd failed";
                if (localDirData.debugDir != null)
                {
                    errMsg += sprintf("\nuse %s/debug.sh for debugging");
                }
                throw new Exception(errMsg);
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
