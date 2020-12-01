/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.utils;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;

public class ByteArrayIOStream extends ByteArrayOutputStream
{
    public ByteArrayIOStream()
    {
        super();
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

    public ByteArrayInputStream getAsInputStreamUnsafe()
    {
        return getAsInputStreamUnsafe(0, this.count);
    }

    public ByteArrayInputStream getAsInputStreamUnsafe(int offset, int count)
    {
        ByteArrayInputStream res = new ByteArrayInputStream(this.buf, offset, this.count);
        return res;
    }
}
