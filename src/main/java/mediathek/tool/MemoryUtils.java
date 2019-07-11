package mediathek.tool;

public class MemoryUtils {
    private static final boolean isLowMemory;

    static {
        final long mem = Runtime.getRuntime().maxMemory();
        //we consider <640MB as low memory
        isLowMemory = mem < 640 * 1024 * 1024;
    }

    public static boolean isLowMemoryEnvironment() {
        return isLowMemory;
    }
}
