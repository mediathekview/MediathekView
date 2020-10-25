package mediathek.tool.affinity;

import org.apache.commons.lang3.SystemUtils;

/**
 * Utility class to set OS-specific CPU affinity.
 */
public class Affinity {
    private static final IAffinity AFFINITY_IMPL;

    static {
        if (SystemUtils.IS_OS_WINDOWS)
            AFFINITY_IMPL = new WindowsAffinity();
        else if (SystemUtils.IS_OS_LINUX)
            AFFINITY_IMPL = new LinuxAffinity();
        else
            AFFINITY_IMPL = new NullAffinity();
    }

    private Affinity() {
    }

    public static IAffinity getAffinityImpl() {
        return AFFINITY_IMPL;
    }
}
