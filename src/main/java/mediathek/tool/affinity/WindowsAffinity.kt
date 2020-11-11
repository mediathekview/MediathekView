package mediathek.tool.affinity

import com.sun.jna.Native
import com.sun.jna.Pointer
import com.sun.jna.platform.win32.Kernel32
import com.sun.jna.platform.win32.WinNT.HANDLE
import org.apache.logging.log4j.LogManager
import java.util.*

class WindowsAffinity : IAffinity {
    override fun setDesiredCpuAffinity(numCpus: Int) {
        val pid = -1 // current process
        val bitSet = BitSet()
        bitSet[0] = numCpus
        val affinity = bitSet.stream()
                .takeWhile { i: Int -> i < java.lang.Long.SIZE }
                .mapToLong { i: Int -> 1L shl i }
                .reduce(0) { a: Long, b: Long -> a or b }
        val affinityMask = affinity.toInt()
        val instance = Native.load("Kernel32", AffinityKernel::class.java)
        val result = instance.SetProcessAffinityMask(HANDLE(Pointer(pid.toLong())), affinityMask)
        if (result) {
            logger.info("CPU affinity was set successfully.")
            logger.trace("Available processors: {}", Runtime.getRuntime().availableProcessors())
        } else logger.warn("Failed to set CPU affinity.")
    }

    private interface AffinityKernel : Kernel32 {
        fun SetProcessAffinityMask(hProcess: HANDLE?, dwProcessAffinityMask: Int): Boolean
    }

    companion object {
        private val logger = LogManager.getLogger()
    }
}