/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.RandomAccessFile;
import java.nio.MappedByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.nio.file.attribute.FileTime;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Scanner;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.logging.Logger;
import java.util.stream.Stream;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;

import clawfc.utils.AsciiArrayIOStream;
import clawfc.utils.FileInfo;
import clawfc.utils.FileInfoImpl;
import clawfc.utils.Subprocess;

/**
 * <code>Utils</code> is a collection of static utility functions commonly used
 * in clawfc.
 *
 */
public class Utils
{
    public static final byte ASCII_NEWLINE_VALUE = (byte) '\n';
    public static final byte ASCII_CARRIAGE_RETURN = (byte) '\r';
    public static final byte ASCII_SPACE_VALUE = (byte) ' ';
    public static final String[] FORTRAN_FILE_EXTENSIONS = new String[] { "f", "F", "f90", "F90", "f95", "F95", "f03",
            "F03" };
    public static final String DEFAULT_TOP_TEMP_DIR = "/dev/shm";
    public static final Path STARTUP_DIR = Paths.get(System.getProperty("user.dir"));
    public static final Logger log = Logger.getLogger("CLAW");

    public static String getCmdOutput(String... args) throws Exception
    {
        AsciiArrayIOStream stdout = new AsciiArrayIOStream();
        final int retCode = Subprocess.call(Arrays.asList(args), STARTUP_DIR, stdout, null, null, null, true);
        if (retCode != 0)
        {
            throw new RuntimeException(
                    String.format("Cmd \"%s\" failed with return code ", String.join(" ", args), retCode));
        }
        String result = collectIntoString(stdout.getAsInputStreamUnsafe());
        return result;
    }

    public static String collectIntoString(InputStream istrm) throws IOException
    {
        String result = null;
        try (Scanner s = new Scanner(istrm).useDelimiter("\\A"))
        {
            result = s.hasNext() ? s.next() : "";
        } finally
        {
            istrm.close();
        }
        return result;
    }

    public static String collectIntoString(Path inFilePath) throws IOException
    {
        try (InputStream inStrm = Files.newInputStream(inFilePath))
        {
            return collectIntoString(inStrm);
        }
    }

    public static String getCmdOutput(List<String> args) throws Exception
    {
        return getCmdOutput(args.stream().toArray(String[]::new));
    }

    public static void copy(InputStream in, OutputStream out) throws IOException
    {
        byte[] buffer = new byte[10 * 1024];
        while (true)
        {
            int len = in.read(buffer);
            if (len != -1)
            {
                out.write(buffer, 0, len);
            } else
            {
                return;
            }
        }
    }

    public static void copy(InputStream in, OutputStream out, byte[] buffer, int numBytes) throws IOException
    {
        while (numBytes > 0)
        {
            int len = in.read(buffer, 0, Integer.min(buffer.length, numBytes));
            if (len != -1)
            {
                out.write(buffer, 0, len);
            } else
            {
                return;
            }
            numBytes -= len;
        }
    }

    public static void skip(InputStream in, byte[] buffer, int numBytes) throws IOException
    {
        while (numBytes > 0)
        {
            int len = in.read(buffer, 0, Integer.min(buffer.length, numBytes));
            if (len == -1)
            {
                return;
            }
            numBytes -= len;
        }
    }

    public static void replaceInLines(InputStream in, OutputStream out, CharSequence from, CharSequence to)
            throws IOException
    {
        try (BufferedReader inReader = new BufferedReader(new InputStreamReader(in));
                PrintWriter outWriter = new PrintWriter(out))
        {
            while (inReader.ready())
            {
                String inLine = inReader.readLine();
                String outLine = inLine.replace(from, to);
                outWriter.println(outLine);
            }
        }
    }

    public static void dumpIntoFile(Path filename, InputStream in) throws IOException
    {
        try (OutputStream out = Files.newOutputStream(filename))
        {
            copy(in, out);
        }
    }

    public static boolean dirExists(Path path)
    {
        final File f = path.toFile();
        return f.exists() && f.isDirectory();
    }

    public static boolean fileExists(Path path)
    {
        final File f = path.toFile();
        return f.exists() && !f.isDirectory();
    }

    public static Path dirPath(Path filePath)
    {
        return filePath.getParent();
    }

    public static void removeDir(Path path) throws IOException
    {
        try (Stream<Path> files = Files.walk(path))
        {
            files.map(Path::toFile).sorted(Collections.reverseOrder()).forEach(File::delete);
        }
    }

    public static void cleanDir(Path dirPath) throws IOException
    {
        try (Stream<Path> files = Files.walk(dirPath))
        {
            files.filter(Files::isRegularFile).map(Path::toFile).forEach(File::delete);
        }
    }

    public static void writeTextToFile(Path path, String text) throws IOException
    {
        try (PrintWriter out = new PrintWriter(path.toString()))
        {
            out.println(text);
        }
    }

    static ExecutorService createThreadPool(int maxNumMPJobs)
    {
        if (maxNumMPJobs > 1)
        {
            return Executors.newFixedThreadPool(maxNumMPJobs);
        } else
        {
            return Executors.newSingleThreadExecutor();
        }
    }

    static public class ExecuteTasks
    {
        final ExecutorService es;
        final List<Future<Void>> taskFutures;

        public ExecuteTasks(int maxNumMPJobs)
        {
            es = createThreadPool(maxNumMPJobs);
            taskFutures = Collections.synchronizedList(new ArrayList<Future<Void>>());
        }

        protected void submitTask(Callable<Void> task)
        {
            taskFutures.add(es.submit(task));
        }

        public void join() throws Exception
        {
            try
            {
                for (int i = 0; i < taskFutures.size(); ++i)
                {
                    Future<Void> taskFuture = taskFutures.get(i);
                    try
                    {
                        taskFuture.get();
                    } catch (InterruptedException e)
                    {// Exception means that THIS thread was interrupted
                        throw e;
                    } catch (ExecutionException e)
                    {
                        Throwable cause = e.getCause();
                        if (cause instanceof Exception)
                        {
                            throw (Exception) cause;
                        } else
                        {
                            throw e;
                        }
                    }
                }
            } finally
            {
                es.shutdownNow();
            }
        }
    }

    static void executeTasksUntilFirstError(List<Callable<Void>> tasks, int maxNumMPJobs) throws Exception
    {
        ExecuteTasks execTasks = new ExecuteTasks(maxNumMPJobs);
        for (Callable<Void> task : tasks)
        {
            execTasks.submitTask(task);
        }
        execTasks.join();
    }

    public static <T> int firstGreater(List<? extends Comparable<? super T>> list, T key)
    {
        int res = Collections.binarySearch(list, key);
        if (res >= 0)
        {
            res += 1;
        } else
        {
            res = -res - 1;
        }
        return res;
    }

    public static int firstGreater(int[] arr, int key)
    {
        int res = Arrays.binarySearch(arr, key);
        if (res >= 0)
        {
            res += 1;
        } else
        {
            res = -res - 1;
        }
        return res;
    }

    public static String sprintf(String format, Object... args)
    {
        return String.format(format, args);
    }

    public static Path getOrCreateDir(Path dirPath) throws Exception
    {
        if (!Utils.dirExists(dirPath))
        {
            try
            {
                Files.createDirectories(dirPath);
            } catch (Exception e)
            {
                throw new Exception(sprintf("Could not create directory %s", dirPath), e);
            }
        }
        return dirPath;
    }

    public static void recreateDir(Path dirPath) throws Exception
    {
        if (dirExists(dirPath))
        {
            removeDir(dirPath);
        }
        getOrCreateDir(dirPath);
    }

    public static FileTime max(FileTime ts1, FileTime ts2)
    {
        if (ts1.compareTo(ts2) > 0)
        {
            return ts1;
        } else
        {
            return ts2;
        }
    }

    public static String removeExtension(String filename)
    {
        return filename.substring(0, filename.lastIndexOf('.'));
    }

    public static String getExtension(String filename)
    {
        return filename.substring(filename.lastIndexOf('.') + 1);
    }

    public static void touch(Path path) throws IOException
    {
        Files.setLastModifiedTime(path, FileTime.from(Instant.now()));
    }

    public static FileInfo saveToFile(InputStream data, Path filePath) throws Exception
    {
        if (fileExists(filePath))
        {
            Files.delete(filePath);
        }
        Path outDirPath = filePath.getParent();
        Path outTmpFilePath = Files.createTempFile(outDirPath, null, null);
        outTmpFilePath.toFile().deleteOnExit();
        try (OutputStream out = Files.newOutputStream(outTmpFilePath))
        {
            copy(data, out);
        } catch (Exception e)
        {
            Files.deleteIfExists(outTmpFilePath);
            throw e;
        }
        Files.move(outTmpFilePath, filePath, StandardCopyOption.ATOMIC_MOVE);
        return new FileInfoImpl(filePath);
    }

    public static InputStream toInputStream(String str) throws IOException
    {
        InputStream inStrm = new ByteArrayInputStream(toAscii(str));
        return inStrm;
    }

    public static CharStream toCharStream(String str) throws IOException
    {
        InputStream inStrm = toInputStream(str);
        CharStream chrStrm = CharStreams.fromStream(inStrm, StandardCharsets.US_ASCII);
        return chrStrm;
    }

    public static byte[] toAscii(String str)
    {
        return str.getBytes(StandardCharsets.US_ASCII);
    }

    public static int getEolStartIndex(String str)
    {
        int res = str.lastIndexOf('\r');
        if (res < 0)
        {
            res = str.lastIndexOf('\n');
        }
        return res;
    }

    public class MappedFile extends RandomAccessFile
    {
        public MappedFile(Path path, boolean readOnly) throws IOException
        {
            super(path.toFile(), "r");
            this.path = path;
            channel = getChannel();
            final long size = channel.size();
            buf = channel.map(readOnly ? FileChannel.MapMode.READ_ONLY : FileChannel.MapMode.READ_WRITE, 0, size);
        }

        final FileChannel channel;
        MappedByteBuffer buf;
        public final Path path;
    }
}
