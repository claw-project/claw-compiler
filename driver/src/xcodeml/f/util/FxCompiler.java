/*
 * @author Mikhail Zhigun
 */
package xcodeml.f.util;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.RandomAccessFile;
import java.nio.MappedByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import xcodeml.f.util.NativeUtils.IOCache;
import xcodeml.f.util.NativeUtils.IOCacheOutputEntry;

public class FxCompiler
{

    static native int execute(CLIOptions opts, IOCache filesCache) throws Exception;

    public static void checkCLIArgs(final CLIOptions opts) throws Exception
    {
        if (opts.native_in_mem_mode_enabled)
        {
            if (!opts.inc_dir_paths.isEmpty())
            {
                throw new Exception("Include directories not allowed in the in-memory mode");
            }
            if (!opts.xmod_inc_dir_paths.isEmpty())
            {
                throw new Exception("Xmod include directories not allowed in the in-memory mode");
            }
        }
    }

    static Path readStdinIntoFile() throws IOException
    {
        ByteArrayOutputStream stdin = new ByteArrayOutputStream();
        {
            byte[] buf = new byte[1024];
            int bytesRead;
            while ((bytesRead = System.in.read(buf)) > 0)
            {
                stdin.write(buf, 0, bytesRead);
            }
        }
        final Path path = Files.createTempFile("stdin", null);
        try (RandomAccessFile raf = new RandomAccessFile(path.toFile(), "rw"))
        {
            FileChannel fileChannel = raf.getChannel();
            final byte[] data = stdin.toByteArray();
            MappedByteBuffer buf = fileChannel.map(FileChannel.MapMode.READ_WRITE, 0, data.length);
            buf.put(data);
        }
        return path;
    }

    static IOCache createCache(CLIOptions opts) throws IOException
    {
        final IOCache filesCache = new IOCache(false);
        if (opts.src_file_path != null)
        {
            filesCache.addInputFile(opts.src_file_path);
        } else
        {
            filesCache.addInputFile(readStdinIntoFile(), "stdin");
        }
        if (opts.stdout_file_path != null)
        {
            filesCache.addOutputFile(opts.stdout_file_path, "stdout");
        } else
        {
            filesCache.addOutputFile(Files.createTempFile("stdout", null), "stdout");
        }
        if (opts.out_file_path != null)
        {
            filesCache.addOutputFile(opts.out_file_path);
        }
        for (Path xmodPath : opts.xmod_inc_paths)
        {
            filesCache.addInputFile(xmodPath, xmodPath.getFileName().toString());
        }
        return filesCache;
    }

    static void printBuffer(MappedByteBuffer in, final int size, PrintStream out)
    {
        byte[] buf = new byte[4 * 1024];
        for (int pos = 0; pos < size;)
        {
            final int len = Math.min(size - pos, buf.length);
            in.get(buf, 0, len);
            pos += len;
            out.write(buf, 0, len);
        }
    }

    static void updateCache(IOCache cache, CLIOptions opts)
    {
        for (IOCacheOutputEntry entry : cache.getOutputFiles())
        {
            switch (entry.filename)
            {
            case "stdout":
                if (opts.stdout_file_path == null)
                {
                    printBuffer(entry.data, entry.getSize(), System.out);
                    continue;
                }
                break;
            default:
                break;
            }
            // Apparently data will be written to disk anyway, force() just makes the call
            // explicit
            if (opts.fsync_enabled)
            {
                entry.data.force();
            }
        }
    }

    public static void main(String[] args) throws Exception
    {
        final Path WORKING_DIR = Paths.get(System.getProperty("user.dir"));
        CLIOptions opts = CLIOptions.parseCmdlineArguments(args, WORKING_DIR);
        if (opts != null)
        {
            checkCLIArgs(opts);
            System.loadLibrary("ffront-jni");
            if (opts.native_in_mem_mode_enabled)
            {
                try (IOCache filesCache = createCache(opts))
                {
                    final int retCode = execute(opts, filesCache);
                    updateCache(filesCache, opts);
                    System.exit(retCode);
                }
            } else
            {
                System.exit(execute(opts, null));
            }
        } else
        {
            System.exit(0);
        }
    }
}
