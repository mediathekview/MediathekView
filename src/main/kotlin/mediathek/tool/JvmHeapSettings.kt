package mediathek.tool

import com.sun.management.HotSpotDiagnosticMXBean
import com.sun.management.VMOption.Origin
import org.apache.logging.log4j.LogManager
import java.lang.management.ManagementFactory

internal object JvmHeapSettings {
    private const val DEFAULT_SOFT_HEAP_BYTES = 1536L * 1024 * 1024

    fun applyDefaultSoftHeapTarget() {
        if (DEFAULT_SOFT_HEAP_BYTES >= Runtime.getRuntime().maxMemory()) {
            return
        }

        try {
            val diagnostics = ManagementFactory.getPlatformMXBean(HotSpotDiagnosticMXBean::class.java) ?: return
            if (!diagnostics.getVMOption("UseShenandoahGC").value.toBoolean()) {
                return
            }

            val softHeap = diagnostics.getVMOption("SoftMaxHeapSize")
            if (softHeap.origin == Origin.DEFAULT || softHeap.origin == Origin.ERGONOMIC) {
                diagnostics.setVMOption(softHeap.name, DEFAULT_SOFT_HEAP_BYTES.toString())
            }
        } catch (ex: RuntimeException) {
            LogManager.getLogger(JvmHeapSettings::class.java).warn("Could not apply the default soft heap target", ex)
        }
    }
}
