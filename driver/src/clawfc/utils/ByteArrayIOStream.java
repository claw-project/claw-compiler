/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.utils;

import static clawfc.Utils.copy;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;

public class ByteArrayIOStream extends ByteArrayOutputStream
{
    public int getSize()
    {
        return count;
    }

    public ByteArrayIOStream()
    {
        super();
    }

    public ByteArrayIOStream(Path filePath) throws IOException
    {
        super();
        try (InputStream f = Files.newInputStream(filePath))
        {
            copy(f, this);
        }
    }

    public ByteArrayIOStream(byte[] data) throws IOException
    {
        super();
        this.buf = data;
        this.count = data.length;
    }

    public ByteArrayIOStream(int size)
    {
        super(size);
    }

    public ByteArrayInputStream getAsInputStream()
    {
        ByteArrayInputStream res = getAsInputStreamUnsafe();
        this.buf = null;
        return res;
    }

    /*
     * "Unsafe" means that it is not safe to write to the stream as long as input
     * stream returned by this method is used.
     */
    public ByteArrayInputStream getAsInputStreamUnsafe()
    {
        return getAsInputStreamUnsafe(0, this.count);
    }

    public ByteArrayInputStream getAsInputStreamUnsafe(int offset, int count)
    {
        ByteArrayInputStream res = new ByteArrayInputStream(this.buf, offset, count);
        return res;
    }
}
