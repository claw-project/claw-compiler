/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintWriter;
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

public class Utils
{
    public static final byte ASCII_NEWLINE_VALUE = (byte) '\n';
    public static final byte ASCII_SPACE_VALUE = (byte) ' ';
    public static final String[] FORTRAN_FILE_EXTENSIONS = new String[] { "f", "F", "f90", "F90", "f95", "F95", "f03",
            "F03" };
    public static final String DEFAULT_TOP_TEMP_DIR = "/dev/shm";
    public static final Path STARTUP_DIR = Paths.get(System.getProperty("user.dir"));
    public static final Logger log = Logger.getLogger("CLAW");

    public static String getCmdOutput(String... args) throws Exception
    {
        ProcessBuilder pb = new ProcessBuilder(args);
        pb.redirectErrorStream(true);
        Process p = pb.start();
        final int retCode = p.waitFor();
        if (retCode != 0)
        {
            throw new RuntimeException(
                    String.format("Cmd \"%s\" failed with return code ", String.join(" ", args), retCode));
        }
        String result = collectIntoString(p.getInputStream());
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
        return Files.exists(path) && Files.isDirectory(path);
    }

    public static boolean fileExists(Path path)
    {
        return Files.exists(path) && !Files.isDirectory(path);
    }

    public static Path dirPath(Path filePath)
    {
        return filePath.getParent();
    }

    public static void removeDir(Path path) throws IOException
    {
        Files.walk(path).map(Path::toFile).sorted((o1, o2) -> -o1.compareTo(o2)).forEach(File::delete);
    }

    public static void cleanDir(Path dirPath) throws IOException
    {
        Files.walk(dirPath).filter(Files::isRegularFile).map(Path::toFile).forEach(File::delete);
    }

    public static void writeTextToFile(Path path, String text) throws IOException
    {
        try (PrintWriter out = new PrintWriter(path.toString()))
        {
            out.println(text);
        }
    }

    public static class NullOutputStream extends OutputStream
    {
        @Override
        public void write(int b) throws IOException
        {
        }
    }

    static ExecutorService createThreadPool(boolean useMultiProcessing)
    {
        if (useMultiProcessing)
        {
            int maxNumThreads = Runtime.getRuntime().availableProcessors();
            return Executors.newFixedThreadPool(maxNumThreads);
        } else
        {
            return Executors.newSingleThreadExecutor();
        }
    }

    static public class ExecuteTasks
    {
        final ExecutorService es;
        final List<Future<Void>> taskFutures;

        public ExecuteTasks(boolean useMultiProcessing)
        {
            es = createThreadPool(useMultiProcessing);
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
                    {
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

    static void executeTasksUntilFirstError(List<Callable<Void>> tasks, boolean useMultiProcessing) throws Exception
    {
        ExecuteTasks execTasks = new ExecuteTasks(useMultiProcessing);
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

    public static void touch(Path path) throws IOException
    {
        Files.setLastModifiedTime(path, FileTime.from(Instant.now()));
    }

    public static void saveToFile(InputStream data, Path filePath) throws IOException
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
    }
}