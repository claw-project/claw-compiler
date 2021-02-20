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
import java.io.PrintStream;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

import claw.tests.Resources;
import clawfc.Driver;
import clawfc.utils.ByteArrayIOStream;
import clawfc.utils.Subprocess;
import clawfc.utils.Subprocess.CallResult;

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
                res = new CallResult(0, collectIntoString(stdErr.getAsInputStreamUnsafe()),
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
            CallResult res = Subprocess.callWithResult(execArgs, workingDir);
            if (res.retCode != 0)
            {
                if (!res.stderr.isEmpty())
                {
                    throw new Exception("stderr:\n" + res.stderr);
                } else if (!res.stdout.isEmpty())
                {
                    throw new Exception("stdout:\n" + res.stdout);
                } else
                {
                    throw new Exception("return value not zero");
                }
            }
            return res;
        } catch (Exception e)
        {
            throw new Exception(sprintf("%s call failed", name), e);
        }

    }
}