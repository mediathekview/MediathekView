package mediathek.tool.affinity

import org.apache.commons.lang3.SystemUtils

/**
 * Utility class to set OS-specific CPU affinity.
 */
object Affinity {
    @JvmStatic
    var affinityImpl: IAffinity = if (SystemUtils.IS_OS_WINDOWS)
        WindowsAffinity()
    else
        NullAffinity()

}