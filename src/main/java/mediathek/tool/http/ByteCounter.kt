package mediathek.tool.http

import java.io.IOException
import java.io.InputStream
import java.io.OutputStream
import java.net.InetAddress
import java.net.Socket
import java.util.*
import java.util.concurrent.atomic.AtomicLong
import javax.net.SocketFactory

class ByteCounter {
    private val socketFactory: SocketFactory = CountSocketFactory()
    private val bytesWritten = AtomicLong()
    private val bytesRead = AtomicLong()

    fun resetCounters() {
        bytesWritten.set(0)
        bytesRead.set(0)
    }

    fun socketFactory(): SocketFactory {
        return socketFactory
    }

    fun bytesWritten(): Long {
        return bytesWritten.get()
    }

    fun bytesRead(): Long {
        return bytesRead.get()
    }

    fun bytesRead(length: Int) {
        while (true) {
            val old = bytesRead.get()
            val updated = old + length
            if (bytesRead.compareAndSet(old, updated)) break
        }
    }

    fun bytesWritten(length: Int) {
        while (true) {
            val old = bytesWritten.get()
            val updated = old + length
            if (bytesWritten.compareAndSet(old, updated)) break
        }
    }

    internal inner class CountSocketFactory : SocketFactory() {
        override fun createSocket(): Socket {
            return CountingSocket()
        }

        @Throws(IOException::class)
        override fun createSocket(s: String, i: Int): Socket {
            return CountingSocket(s, i)
        }

        @Throws(IOException::class)
        override fun createSocket(s: String, i: Int, inetAddress: InetAddress, i1: Int): Socket {
            return CountingSocket(s, i, inetAddress, i1)
        }

        @Throws(IOException::class)
        override fun createSocket(inetAddress: InetAddress, i: Int): Socket {
            return CountingSocket(inetAddress, i)
        }

        @Throws(IOException::class)
        override fun createSocket(
            inetAddress: InetAddress, i: Int, inetAddress1: InetAddress,
            i1: Int
        ): Socket {
            return CountingSocket(inetAddress, i, inetAddress1, i1)
        }
    }

    internal inner class CountingSocket : Socket {
        private val lock = Any()
        private var outputStream: OutputStream? = null
        private var inputStream: InputStream? = null

        constructor() : super()
        constructor(host: String?, port: Int) : super(host, port)
        constructor(address: InetAddress?, port: Int) : super(address, port)
        constructor(host: String?, port: Int, localAddr: InetAddress?, localPort: Int) : super(
            host,
            port,
            localAddr,
            localPort
        )

        constructor(address: InetAddress?, port: Int, localAddr: InetAddress?, localPort: Int) : super(
            address,
            port,
            localAddr,
            localPort
        )

        @Throws(IOException::class)
        override fun getInputStream(): InputStream {
            synchronized(lock) {
                if (inputStream == null) {
                    inputStream = CountingInputStream(super.getInputStream(), this@ByteCounter)
                }
            }
            return inputStream!!
        }

        @Throws(IOException::class)
        override fun getOutputStream(): OutputStream {
            synchronized(lock) {
                if (outputStream == null) {
                    outputStream = CountingOutputStream(super.getOutputStream(), this@ByteCounter)
                }
            }
            return outputStream!!
        }
    }

    internal class CountingOutputStream(private val delegate: OutputStream, private val byteCounter: ByteCounter) :
        OutputStream() {
        @Throws(IOException::class)
        override fun write(b: Int) {
            delegate.write(b)
            byteCounter.bytesWritten(1)
        }

        @Throws(IOException::class)
        override fun write(b: ByteArray) {
            delegate.write(b)
            byteCounter.bytesWritten(b.size)
        }

        @Throws(IOException::class)
        override fun write(b: ByteArray, off: Int, len: Int) {
            delegate.write(b, off, len)
            byteCounter.bytesWritten(len)
        }

        @Throws(IOException::class)
        override fun flush() {
            delegate.flush()
        }

        @Throws(IOException::class)
        override fun close() {
            delegate.close()
        }
    }

    internal class CountingInputStream(private val delegate: InputStream, private val byteCounter: ByteCounter) :
        InputStream() {
        @Throws(IOException::class)
        override fun read(): Int {
            val read = delegate.read()
            if (read > 0) byteCounter.bytesRead(1)
            return read
        }

        @Throws(IOException::class)
        override fun read(b: ByteArray): Int {
            val read = delegate.read(b)
            if (read > 0) byteCounter.bytesRead(read)
            return read
        }

        @Throws(IOException::class)
        override fun read(b: ByteArray, off: Int, len: Int): Int {
            val read = delegate.read(b, off, len)
            if (read > 0) byteCounter.bytesRead(read)
            return read
        }

        @Throws(IOException::class)
        override fun skip(n: Long): Long {
            return delegate.skip(n)
        }

        @Throws(IOException::class)
        override fun available(): Int {
            return delegate.available()
        }

        @Throws(IOException::class)
        override fun close() {
            delegate.close()
        }

        @Synchronized
        override fun mark(readlimit: Int) {
            delegate.mark(readlimit)
        }

        @Synchronized
        @Throws(IOException::class)
        override fun reset() {
            delegate.reset()
        }

        override fun markSupported(): Boolean {
            return delegate.markSupported()
        }
    }
}