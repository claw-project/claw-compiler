/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc.utils;

import static clawfc.Utils.sprintf;

import java.io.Closeable;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.ClosedByInterruptException;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import clawfc.Utils;

/**
 * <code>GMakeJobServerConnection</code> encapsulates connection to GNU Make Job
 * server
 *
 */
public class GMakeJobServerConnection implements Closeable
{
    /**
     * <code>null</code> value means that GNU Make did not impose any limitations on
     * maximum number of jobs
     */
    final Integer maxJobs;
    /**
     * Timeout for acquiring free slot from GNU Make job server
     */
    final int TIMEOUT = 1;
    List<Byte> jobSlots;
    FileInputStream pipeRd;
    FileOutputStream pipeWr;
    final ExecutorService exec;

    public GMakeJobServerConnection() throws Exception
    {
        final String mkFlags = System.getenv("MAKEFLAGS");
        if (mkFlags == null)
        {
            throw new Exception("GNU Make not detected");
        }
        String[] flags = mkFlags.split(" ");
        Integer maxJobsVal = 1;
        String readFd = null, writeFd = null;
        for (String flag : flags)
        {
            if (flag.startsWith("-j"))
            {
                if (flag.contentEquals("-j"))
                {
                    maxJobsVal = null;
                } else
                {
                    maxJobsVal = Integer.valueOf(flag.substring("-j".length()));
                }
            } else if (flag.startsWith("--jobserver-auth"))
            {
                String[] fds = flag.split("=")[1].split(",");
                readFd = fds[0];
                writeFd = fds[1];
            }
        }
        maxJobs = maxJobsVal;
        jobSlots = new ArrayList<Byte>();
        if (readFd != null)
        {
            final String[] procStatusInfo = Utils.collectIntoString(Paths.get("/proc/self/status")).split("\\r?\\n");
            String parentPID = null;
            for (String line : procStatusInfo)
            {
                line = line.trim();
                if (line.startsWith("NSpgid:"))// Unclear why not PPid
                {
                    parentPID = line.split(":")[1].trim();
                    break;
                }
            }
            if (parentPID == null)
            {
                throw new Exception("Failed to get GNU Make pid");
            }
            pipeRd = new FileInputStream(sprintf("/proc/%s/fd/%s", parentPID, readFd));
            pipeWr = new FileOutputStream(sprintf("/proc/%s/fd/%s", parentPID, writeFd));
            exec = Executors.newSingleThreadExecutor();
        } else
        {
            exec = null;
        }
    }

    public int getSlots(final int maxNum) throws Exception
    {
        if (maxJobs == null)
        {
            return maxNum;
        }
        for (int num = 1; num < maxNum && num < maxJobs; ++num)
        {
            if (!getSlot())
            {
                break;
            }
        }
        return numAcquiredSlots() + 1;// 1 slot is already acquired by parent make
    }

    public int numAcquiredSlots()
    {
        return jobSlots.size();
    }

    public boolean getSlot() throws Exception
    {
        if (pipeRd == null)
        {
            throw new Exception("Connection to GNU Make server not established");
        }
        Callable<Byte> task = new Callable<Byte>()
        {
            @Override
            public Byte call() throws Exception
            {
                try
                {
                    ByteBuffer buf = ByteBuffer.allocate(1);
                    final int numReadBytes = pipeRd.getChannel().read(buf);
                    return buf.array()[0];
                } catch (ClosedByInterruptException e)
                {
                    return null;
                } catch (Exception e)
                {
                    return null;
                }
            }
        };
        Byte slot = null;
        try
        {
            slot = exec.invokeAll(Arrays.asList(task), TIMEOUT, TimeUnit.MILLISECONDS).get(0).get();
        } catch (CancellationException e)
        {
        }
        if (slot != null)
        {
            jobSlots.add(slot);
        }
        return slot != null;
    }

    @Override
    public void close() throws IOException
    {
        if (pipeRd != null)
        {
            pipeRd.close();
            pipeRd = null;
        }
        if (pipeWr != null)
        {
            for (Byte jobSlot : jobSlots)
            {
                pipeWr.write(jobSlot);
            }
            jobSlots.clear();
            pipeWr = null;
        }
    }

}
