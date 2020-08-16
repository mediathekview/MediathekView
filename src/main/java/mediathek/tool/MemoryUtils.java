package mediathek.tool;

import mediathek.config.Konstanten;

public class MemoryUtils {
    private static final boolean isLowMemory;

    static {
        isLowMemory = Runtime.getRuntime().maxMemory() <= Konstanten.LOW_MEMORY_THRESHOLD;
    }

    public static boolean isLowMemoryEnvironment() {
        return isLowMemory;
    }
}
