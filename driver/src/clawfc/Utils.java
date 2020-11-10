/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Scanner;
import java.util.List;
import java.lang.ProcessBuilder;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class Utils
{
    public static final String[] FORTRAN_FILE_EXTENSIONS = new String[] {"f90", "F90", "f", "F", "f95", "f03"};
    public static final String DEFAULT_TOP_TEMP_DIR = "/dev/shm";
    public static final Path STARTUP_DIR = Paths.get(System.getProperty("user.dir"));
    
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
        String result = null;
        try (InputStream istrm = p.getInputStream(); Scanner s = new Scanner(istrm).useDelimiter("\\A"))
        {
            result = s.hasNext() ? s.next() : null;
        }
        return result;
    }
    
    public static String getCmdOutput(List<String> args) throws Exception
    {
        return getCmdOutput(args.stream().toArray(String[]::new));
    }
    
    public static class ByteArrayIOStream
        extends ByteArrayOutputStream
    {
        public ByteArrayIOStream() 
        { super(); }

        public ByteArrayIOStream(int size) 
        { super(size); }
        
        public ByteArrayInputStream getAsInputStream() 
        {
            ByteArrayInputStream res = new ByteArrayInputStream(this.buf, 0, this.count);
            this.buf = null;            
            return res;
        }
    }  
    
    public static void copy(InputStream in, OutputStream out) throws IOException
    {
        byte[] buffer = new byte[10 * 1024];        
        while(true)
        {
            int len = in.read(buffer);
            if(len != -1)
            { out.write(buffer, 0, len); }
            else
            { return; }
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
}