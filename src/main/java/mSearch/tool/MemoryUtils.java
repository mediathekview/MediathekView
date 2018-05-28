package mSearch.tool;

public class MemoryUtils {
    private static boolean isLowMemory;

    static {
        final long mem = Runtime.getRuntime().maxMemory();
        //we consider <640MB as low memory
        if (mem < 640 * 1024 * 1024)
            isLowMemory = true;
        else
            isLowMemory = false;
    }

    public static boolean isLowMemoryEnvironment() {
        return isLowMemory;
    }
}
