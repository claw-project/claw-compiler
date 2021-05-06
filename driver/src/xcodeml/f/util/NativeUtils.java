/*
 * @author Mikhail Zhigun
 */
package xcodeml.f.util;

import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.MappedByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class NativeUtils
{
    public static class CppException extends Exception
    {
        public CppException(String msg)
        {
            super(msg);
        }
    }

    public static class Pointer
    {
        public long getAddress()
        {
            return address;
        }

        public final long address;

        public Pointer(long address)
        {
            this.address = address;
        }
    }

    static class IOCacheInputEntry
    {
        public IOCacheInputEntry(RandomAccessFile file, MappedByteBuffer data, String filename, int size)
        {
            this.file = file;
            this.data = data;
            this.filename = filename;
            this.size = size;
        }

        public final RandomAccessFile file;
        public final MappedByteBuffer data;
        public final String filename;
        public final int size;
    }

    static class IOCacheOutputEntry
    {
        public IOCacheOutputEntry(RandomAccessFile file, String filename)
        {
            this.file = file;
            this.data = null;
            this.filename = filename;
            this.size = 0;
        }

        public int getSize()
        {
            return size;
        }

        public MappedByteBuffer getData()
        {
            return data;
        }

        public void allocateData(int size) throws IOException
        {
            FileChannel fileChannel = file.getChannel();
            data = fileChannel.map(FileChannel.MapMode.READ_WRITE, 0, size);
            this.size = size;
        }

        public final RandomAccessFile file;
        MappedByteBuffer data;
        public final String filename;
        int size;
    }

    public static class IOCache implements AutoCloseable
    {
        public IOCache(boolean flushFilesToDisk)
        {
            this.flushFilesToDisk = flushFilesToDisk;
            inputFiles = new HashMap<Path, IOCacheInputEntry>();
            outputFiles = new HashMap<Path, IOCacheOutputEntry>();
        }

        public void addInputFile(Path path) throws IOException
        {
            addInputFile(path, path.toString());
        }

        public void addInputFile(Path path, String filename) throws IOException
        {
            if (!inputFiles.containsKey(path))
            {
                File f = path.toFile();
                RandomAccessFile raf = new RandomAccessFile(f, "r");
                FileChannel fileChannel = raf.getChannel();
                final int size = (int) fileChannel.size();
                MappedByteBuffer buf = fileChannel.map(FileChannel.MapMode.READ_ONLY, 0, size);
                inputFiles.put(path, new NativeUtils.IOCacheInputEntry(raf, buf, filename, size));
            }
        }

        public void addOutputFile(Path path) throws IOException
        {
            addOutputFile(path, path.toString());
        }

        public void addOutputFile(Path path, String filename) throws IOException
        {
            if (!outputFiles.containsKey(path))
            {
                File f = path.toFile();
                RandomAccessFile raf = new RandomAccessFile(f, "rw");
                outputFiles.put(path, new NativeUtils.IOCacheOutputEntry(raf, filename));
            }
        }

        public List<IOCacheInputEntry> getInputFiles()
        {
            return new ArrayList<NativeUtils.IOCacheInputEntry>(inputFiles.values());
        }

        public List<IOCacheOutputEntry> getOutputFiles()
        {
            return new ArrayList<NativeUtils.IOCacheOutputEntry>(outputFiles.values());
        }

        @Override
        public void close() throws Exception
        {
            for (IOCacheInputEntry e : inputFiles.values())
            {
                e.file.close();
            }
            for (IOCacheOutputEntry e : outputFiles.values())
            {
                if (flushFilesToDisk)
                {
                    e.data.force();
                }
                e.file.close();
            }
        }

        final boolean flushFilesToDisk;
        final Map<Path, IOCacheInputEntry> inputFiles;
        final Map<Path, IOCacheOutputEntry> outputFiles;
    }
}
