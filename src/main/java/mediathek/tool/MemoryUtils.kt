package mediathek.tool

import mediathek.config.Konstanten

object MemoryUtils {
    @JvmStatic
    val isLowMemoryEnvironment: Boolean = Runtime.getRuntime().maxMemory() <= Konstanten.LOW_MEMORY_THRESHOLD
}