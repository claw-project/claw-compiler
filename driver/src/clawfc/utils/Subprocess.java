/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc.utils;

import static clawfc.Utils.USE_UNIX_SHELL_INSTEAD_OF_SUBPROCESS_BUILDER;
import static clawfc.Utils.collectIntoString;
import static clawfc.Utils.copy;
import static clawfc.Utils.skip;
import static clawfc.Utils.sprintf;

import java.io.Closeable;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.FileAttribute;
import java.util.List;
import java.util.concurrent.TimeUnit;

public class Subprocess
{
    final static int DEFAULT_TIMEOUT_S = 10;

    static class StreamGobbler extends Thread implements Closeable
    {
        final static int SLEEP_PERIOD_NS = 100000;

        final Process process;
        final byte buf[] = new byte[4 * 1024];
        final InputStream src;
        final OutputStream dst;
        Exception error;
        volatile boolean stopSignaled;

        public boolean isStopSignaled()
        {
            return stopSignaled;
        }

        public Exception getError()
        {
            return error;
        }

        public StreamGobbler(Process process, InputStream src, OutputStream dst)
        {
            this.process = process;
            this.src = src;
            this.dst = dst;
            this.error = null;
            this.stopSignaled = false;
            setDaemon(true);
            start();
        }

        void readAllData() throws IOException
        {
            int bytesAvailable;
            while ((bytesAvailable = src.available()) > 0)
            {
                if (dst != null)
                {
                    copy(src, dst, buf, bytesAvailable);
                } else
                {
                    skip(src, buf, bytesAvailable);
                }
            }
        }

        @Override
        public void run()
        {
            do
            {
                try
                {
                    readAllData();
                    if (isStopSignaled() || !process.isAlive())
                    {
                        readAllData();
                        return;
                    } else
                    {
                        sleep(0, SLEEP_PERIOD_NS);
                    }
                } catch (Exception e)
                {
                    error = e;
                    return;
                }
            } while (!Thread.interrupted());
        }

        public void shutdown() throws InterruptedException
        {
            stopSignaled = true;
            join();
        }

        @Override
        public void close() throws IOException
        {
            try
            {
                shutdown();
            } catch (InterruptedException e)
            {
                Thread.currentThread().interrupt();
            }
        }
    }

    static Path createTempFile(Path dir, String prefix, String suffix, FileAttribute<?>... attrs) throws IOException
    {
        Path path = Files.createTempFile(dir, prefix, suffix);
        File f = path.toFile();
        f.deleteOnExit();
        return path;
    }

    public static int call(final List<String> args, final Path workingDir, final OutputStream stdout,
            final OutputStream stderr, final InputStream input, Integer timeoutInSeconds,
            final boolean redirectErrStream, Path tmpDirPath, final boolean useUnixShell) throws Exception
    {
        if (tmpDirPath == null)
        {
            tmpDirPath = Paths.get(clawfc.Utils.DEFAULT_TOP_TEMP_DIR);
        }
        if (timeoutInSeconds == null)
        {
            timeoutInSeconds = Integer.valueOf(DEFAULT_TIMEOUT_S);
        }
        Path argsFilePath = null;
        Path inputFilePath = null;
        Path stdoutFilePath = null;
        Path stderrFilePath = null;
        Process p = null;
        try
        {
            if (input != null)
            {
                inputFilePath = createTempFile(tmpDirPath, "subprocess_input_", ".tmp");
                try (OutputStream pStdin = Files.newOutputStream(inputFilePath))
                {
                    copy(input, pStdin);
                }
            }
            stdoutFilePath = createTempFile(tmpDirPath, "subprocess_stdout_", ".tmp");
            stderrFilePath = createTempFile(tmpDirPath, "subprocess_stderr_", ".tmp");
            if (!useUnixShell)
            {
                ProcessBuilder pb = new ProcessBuilder(args);
                pb.redirectErrorStream(redirectErrStream);
                if (inputFilePath != null)
                {
                    pb.redirectInput(inputFilePath.toFile());
                }
                pb.redirectOutput(stdoutFilePath.toFile());
                pb.redirectError(stderrFilePath.toFile());
                pb.directory(workingDir.toFile());
                p = pb.start();
            } else
            {
                // TODO: get rid of this E V I L
                for (int i = 0, n = args.size(); i < n; ++i)
                {
                    args.set(i, "\"" + args.get(i) + "\"");
                }
                String argsStr = String.join(" ", args);
                if (inputFilePath != null)
                {
                    argsStr += " < " + inputFilePath.toString();
                }
                argsStr = "(" + argsStr + ")";
                if (redirectErrStream)
                {
                    argsStr = sprintf("%s &> %s", argsStr, stdoutFilePath);
                } else
                {
                    argsStr = sprintf("%s 2> %s 1> %s", argsStr, stderrFilePath, stdoutFilePath);
                }
                argsFilePath = createTempFile(tmpDirPath, "subprocess_args_", ".sh");
                clawfc.Utils.writeTextToFile(argsFilePath, argsStr);
                p = java.lang.Runtime.getRuntime().exec("bash " + argsFilePath.toString());
            }

            /*
             * try (final StreamGobbler stdoutReader = new StreamGobbler(p,
             * p.getInputStream(), stdout); final StreamGobbler stderrReader = new
             * StreamGobbler(p, p.getErrorStream(), stderr))
             */
            {
                /*- TODO:  Find out why this causes broken pipe when using Omni on daint!
                if (input != null)
                {                    
                    if (p.isAlive())
                    {
                        try (OutputStream pStdin = p.getOutputStream())
                        {
                            copy(input, pStdin);
                        }
                    } else
                    {
                        throw new Exception("Process exited before input could be written to it");
                    }
                }*/
                final boolean subProcCallRes = p.waitFor(timeoutInSeconds, TimeUnit.SECONDS);
                if (!subProcCallRes)
                {
                    throw new Exception("Subprocess call timed out");
                }
                final int retCode = p.exitValue();
                try (InputStream stdoutFileStrm = Files.newInputStream(stdoutFilePath))
                {
                    copy(stdoutFileStrm, stdout);
                }
                try (InputStream stderrFileStrm = Files.newInputStream(stderrFilePath))
                {
                    copy(stderrFileStrm, stderr);
                }
                return retCode;
            }
        } catch (Exception e)
        {
            if (argsFilePath != null)
            {
                Files.delete(argsFilePath);
            }
            if (inputFilePath != null)
            {
                Files.delete(inputFilePath);
            }
            if (stdoutFilePath != null)
            {
                Files.delete(stdoutFilePath);
            }
            if (stderrFilePath != null)
            {
                Files.delete(stderrFilePath);
            }
            if (p != null && p.isAlive())
            {
                p.destroyForcibly();
            }
            throw e;
        }
    }

    public static int call(final List<String> args, final Path workingDir, final OutputStream stdout,
            final OutputStream stderr, final InputStream input) throws Exception
    {
        return call(args, workingDir, stdout, stderr, input, null, false, null,
                USE_UNIX_SHELL_INSTEAD_OF_SUBPROCESS_BUILDER);
    }

    public static int call(final List<String> args, final Path workingDir, final OutputStream stdout,
            final OutputStream stderr) throws Exception
    {
        return call(args, workingDir, stdout, stderr, null, null, false, null,
                USE_UNIX_SHELL_INSTEAD_OF_SUBPROCESS_BUILDER);
    }

    public static class CallResult
    {
        public final int retCode;
        public final String stderr;
        public final String stdout;

        public CallResult(int retCode, String stderr, String stdout)
        {
            this.retCode = retCode;
            this.stderr = stderr;
            this.stdout = stdout;
        }
    }

    public static CallResult callWithResult(final List<String> args, Path workingDir) throws Exception
    {
        final AsciiArrayIOStream stdout = new AsciiArrayIOStream();
        final AsciiArrayIOStream stderr = new AsciiArrayIOStream();
        final int retCode = call(args, workingDir, stdout, stderr);
        CallResult res = new CallResult(retCode, collectIntoString(stderr.getAsInputStreamUnsafe()),
                collectIntoString(stdout.getAsInputStreamUnsafe()));
        return res;
    }
}
