/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2021, MeteoSwiss
 */
package claw.utils;

import static clawfc.Utils.collectIntoString;
import static clawfc.Utils.copy;
import static clawfc.Utils.sprintf;

import java.io.FileDescriptor;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

import claw.tests.Resources;
import clawfc.Driver;
import clawfc.utils.ByteArrayIOStream;
import clawfc.utils.SubprocessFailed;

public abstract class Utils
{
    static final Path RES_DIR = claw.tests.Resources.DIR;
    static final Path DRIVER_PATH = claw.tests.Resources.DRIVER_PATH();

    static CallResult runDiff(final List<String> args, Path workingDir) throws Exception
    {
        return runExecutable("diff", Paths.get("diff"), args, workingDir);
    }

    static void resetStdStreams()
    {
        System.setErr(new PrintStream(new FileOutputStream(FileDescriptor.err)));
        System.setOut(new PrintStream(new FileOutputStream(FileDescriptor.out)));
    }

    static CallResult runClawfc(final List<String> args, Path workingDir, boolean debug) throws Exception
    {
        if (!debug)
        {
            return runExecutable("clawfc", Resources.DRIVER_PATH(), args, workingDir);
        } else
        {
            CallResult res;
            ByteArrayIOStream stdErr = new ByteArrayIOStream();
            ByteArrayIOStream stdOut = new ByteArrayIOStream();
            try
            {
                System.setErr(new PrintStream(stdErr));
                System.setOut(new PrintStream(stdOut));
                Driver.run(args.stream().toArray(String[]::new));
                res = new CallResult(collectIntoString(stdErr.getAsInputStreamUnsafe()),
                        collectIntoString(stdOut.getAsInputStreamUnsafe()));
            } catch (Exception e)
            {
                copy(stdOut.getAsInputStreamUnsafe(), new FileOutputStream(FileDescriptor.out));
                copy(stdErr.getAsInputStreamUnsafe(), new FileOutputStream(FileDescriptor.err));
                throw e;
            } finally
            {
                resetStdStreams();
            }
            return res;
        }
    }

    static CallResult runFC(final List<String> args, Path workingDir) throws Exception
    {
        return runExecutable("fc", Resources.FC_PATH, args, workingDir);
    }

    static CallResult runExecutable(String name, Path exec, List<String> args, Path workingDir) throws Exception
    {
        try
        {
            List<String> execArgs = new ArrayList<String>(args.size() + 1);
            execArgs.add(exec.toString());
            execArgs.addAll(args);
            return runSubProcess(execArgs, workingDir);
        } catch (Exception e)
        {
            throw new Exception(sprintf("%s call failed", name), e);
        }

    }

    public static class CallResult
    {
        public final String stderr;
        public final String stdout;

        public CallResult(String stderr, String stdout)
        {
            this.stderr = stderr;
            this.stdout = stdout;
        }
    }

    public static CallResult runSubProcess(final List<String> args, Path workingDir)
            throws SubprocessFailed, IOException, InterruptedException
    {
        ProcessBuilder pb = new ProcessBuilder(args);
        pb.directory(workingDir.toFile());
        Process p = pb.start();
        final int retCode = p.waitFor();
        if (retCode == 0)
        {
            try (InputStream stdout = p.getInputStream(); InputStream stderr = p.getErrorStream())
            {
                CallResult res = new CallResult(collectIntoString(stderr), collectIntoString(stdout));
                return res;
            }
        } else
        {
            try (InputStream stdout = p.getInputStream(); InputStream stderr = p.getErrorStream())
            {
                throw new SubprocessFailed(args, stderr, stdout);
            }
        }
    }
}