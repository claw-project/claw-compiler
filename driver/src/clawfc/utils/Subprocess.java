/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2021, MeteoSwiss
 */
package clawfc.utils;

import static clawfc.Utils.collectIntoString;
import static clawfc.Utils.copy;
import static clawfc.Utils.skip;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Path;
import java.util.List;
import java.util.concurrent.TimeUnit;

public class Subprocess
{
    final static int DEFAULT_TIMEOUT_S = 10;

    static class StreamGobbler extends Thread
    {
        final static int SLEEP_PERIOD_NS = 100000;

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

        public StreamGobbler(InputStream src, OutputStream dst)
        {
            this.src = src;
            this.dst = dst;
            this.error = null;
            this.stopSignaled = false;
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
                    if (isStopSignaled())
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
        Process p = pb.start();
        final StreamGobbler stdoutReader = new StreamGobbler(p.getInputStream(), stdout);
        final StreamGobbler stderrReader = new StreamGobbler(p.getErrorStream(), stderr);
        stdoutReader.start();
        stderrReader.start();
        if (input != null)
        {
            try (OutputStream pStdin = p.getOutputStream())
            {
                copy(input, pStdin);
            }
        }
        try
        {
            final boolean subProcCallRes = p.waitFor(timeoutInSeconds, TimeUnit.SECONDS);
            if (!subProcCallRes)
            {
                throw new Exception("Subprocess call timed out");
            }
        } finally
        {
            stdoutReader.shutdown();
            stderrReader.shutdown();

        }
        final int retCode = p.exitValue();
        return retCode;
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
