/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.Scanner;
import java.util.logging.Logger;

public class Utils
{
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
            result = s.hasNext() ? s.next() : null;
        } finally
        {
            istrm.close();
        }
        return result;
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

    public static void writeTextToFile(Path path, String text) throws IOException
    {
        try (PrintWriter out = new PrintWriter(path.toString()))
        {
            out.println(text);
        }
    }
}