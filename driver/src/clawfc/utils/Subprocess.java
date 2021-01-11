/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2021, MeteoSwiss
 */
package clawfc.utils;

import static clawfc.Utils.DEFAULT_TOP_TEMP_DIR;
import static clawfc.Utils.collectIntoString;
import static clawfc.Utils.copy;
import static clawfc.Utils.skip;

import java.io.Closeable;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
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
            }
        }
    }

    public static int call(final List<String> args, final Path workingDir, final OutputStream stdout,
            final OutputStream stderr, final InputStream input, Integer timeoutInSeconds,
            final boolean redirectErrStream) throws Exception
    {
        if (timeoutInSeconds == null)
        {
            timeoutInSeconds = Integer.valueOf(DEFAULT_TIMEOUT_S);
        }
        ProcessBuilder pb = new ProcessBuilder(args);
        pb.redirectErrorStream(redirectErrStream);
        pb.directory(workingDir.toFile());
        Process p = null;
        Path inputFilePath = null;
        Path stdoutFilePath = null;
        Path stderrFilePath = null;
        try
        {
            if (input != null)
            {
                inputFilePath = Files.createTempFile(Paths.get(DEFAULT_TOP_TEMP_DIR), "subprocess_input_", ".tmp");
                final File inputFile = inputFilePath.toFile();
                inputFile.deleteOnExit();
                try (OutputStream pStdin = Files.newOutputStream(inputFilePath))
                {
                    copy(input, pStdin);
                }
                pb.redirectInput(inputFile);
            }
            stdoutFilePath = Files.createTempFile(Paths.get(DEFAULT_TOP_TEMP_DIR), "subprocess_stdout_", ".tmp");
            {
                final File stdoutFile = stdoutFilePath.toFile();
                stdoutFile.deleteOnExit();
                pb.redirectOutput(stdoutFile);
            }
            stderrFilePath = Files.createTempFile(Paths.get(DEFAULT_TOP_TEMP_DIR), "subprocess_stderr_", ".tmp");
            {
                final File stderrFile = stderrFilePath.toFile();
                stderrFile.deleteOnExit();
                pb.redirectError(stderrFile);
            }
            p = pb.start();
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
                try (InputStream stdoutFile = Files.newInputStream(stdoutFilePath))
                {
                    copy(stdoutFile, stdout);
                }
                try (InputStream stderrFile = Files.newInputStream(stderrFilePath))
                {
                    copy(stderrFile, stderr);
                }
                return retCode;
            }
        } catch (Exception e)
        {
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
        return call(args, workingDir, stdout, stderr, input, null, false);
    }

    public static int call(final List<String> args, final Path workingDir, final OutputStream stdout,
            final OutputStream stderr) throws Exception
    {
        return call(args, workingDir, stdout, stderr, null, null, false);
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
