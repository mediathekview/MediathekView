package mSearch.tool;

import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;

/**
 * User: Christian F.
 * Date: 15.06.16
 * Time: 14:03
 */
public class ProgressMonitorInputStream extends FilterInputStream {
    /**
     * Creates a <code>FilterInputStream</code>
     * by assigning the  argument <code>in</code>
     * to the field <code>this.in</code> so as
     * to remember it for later use.
     *
     * @param in the underlying input stream, or <code>null</code> if
     *           this instance is to be created without an underlying stream.
     */
    public ProgressMonitorInputStream(InputStream in, long maxSize, InputStreamProgressMonitor mon) throws IOException {
        super(in);
        monitor = mon;
        this.size = maxSize;
        if (size == 0)
            throw new IOException("Size must be greater than zero!");
    }


    @Override
    public int read() throws IOException {
        final int read = super.read();
        if (read != -1) {
            bytesRead++;
            if (monitor != null)
                monitor.progress(bytesRead, size);
        }
        return read;
    }

    @Override
    public int read(byte[] b) throws IOException {
        final int read = super.read(b);
        if (read != -1) {
            bytesRead += read;
            if (monitor != null)
                monitor.progress(bytesRead, size);
        }
        return read;
    }

    @Override
    public int read(byte[] b, int off, int len) throws IOException {
        final int read = super.read(b, off, len);
        if (read != -1) {
            bytesRead += read;
            if (monitor != null)
                monitor.progress(bytesRead, size);
        }
        return read;
    }

    private InputStreamProgressMonitor monitor = null;
    /**
     * The number of bytes that can be read from the InputStream.
     */
    private long size = 0;
    /**
     * The number of bytes that have been read from the InputStream.
     */
    private long bytesRead = 0;
}
