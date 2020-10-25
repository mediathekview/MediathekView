package mediathek.tool.affinity

import com.sun.jna.*
import com.sun.jna.ptr.IntByReference
import com.sun.jna.ptr.PointerByReference
import org.apache.logging.log4j.LogManager
import java.util.*

class LinuxAffinity : IAffinity {
    override fun setDesiredCpuAffinity(numCpus: Int) {
        val bitSet = BitSet()
        bitSet[0] = numCpus
        setAffinity(bitSet)
    }

    private fun setAffinity(affinity: BitSet) {
        val instance = Native.loadLibrary("c", CLibrary::class.java)
        val buff = affinity.toByteArray()
        val cpuSetSizeInBytes = buff.size
        val cpusetArray = Memory(cpuSetSizeInBytes.toLong())
        try {
            cpusetArray.write(0, buff, 0, buff.size)
            val ret = instance.sched_setaffinity(0, cpuSetSizeInBytes, PointerByReference(cpusetArray))
            if (ret < 0)
                System.err.println("sched_setaffinity(($cpuSetSizeInBytes) , &($affinity) ) return $ret")
        } catch (e: LastErrorException) {
            if (e.errorCode != 22 || !Arrays.equals(buff, cpusetArray.getByteArray(0, cpuSetSizeInBytes))) {
                System.err.println("sched_setaffinity((" + cpuSetSizeInBytes + ") , &(" + affinity + ") ) errorNo=" + e.errorCode)
            }
        }

        val value = affinity.toLongArray()[0].toInt()
        val cpuset32 = IntByReference(0)
        cpuset32.value = value
        try {
            val ret = instance.sched_setaffinity(0, Integer.SIZE / 8, cpuset32)
            if (ret < 0)
                System.err.println("sched_setaffinity((" + Integer.SIZE / 8 + ") , &(" + Integer.toHexString(cpuset32.value) + ") ) return " + ret)
        } catch (e: LastErrorException) {
            System.err.println("sched_setaffinity((" + Integer.SIZE / 8 + ") , &(" + Integer.toHexString(cpuset32.value) + ") ) errorNo=" + e.errorCode)
        }
    }

    private interface CLibrary : Library {
        @Throws(LastErrorException::class)
        fun sched_setaffinity(pid: Int, cpusetsize: Int, cpuset: PointerType?): Int
    }

    companion object {
        private val logger = LogManager.getLogger()
    }
}