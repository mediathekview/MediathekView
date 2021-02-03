package mediathek.tool;

import org.apache.commons.lang3.ArchUtils;
import org.apache.commons.lang3.SystemUtils;

import java.lang.management.ManagementFactory;
import java.lang.reflect.Field;

public class Functions {
    /**
     * Checks if the application has an debugger attached to it.
     * @return true if debugger was detected, false othewise.
     */
    public static boolean isDebuggerAttached() {
        return ManagementFactory.getRuntimeMXBean().getInputArguments().toString().indexOf("-agentlib:jdwp") > 0;
    }

    /**
     * Disable java unsafe console warning.
     */
    public static void disableAccessWarnings() {
        try {
            var unsafeClass = Class.forName("sun.misc.Unsafe");
            var field = unsafeClass.getDeclaredField("theUnsafe");
            field.setAccessible(true);
            var unsafe = field.get(null);

            var putObjectVolatile = unsafeClass.getDeclaredMethod("putObjectVolatile", Object.class, long.class, Object.class);
            var staticFieldOffset = unsafeClass.getDeclaredMethod("staticFieldOffset", Field.class);

            var loggerClass = Class.forName("jdk.internal.module.IllegalAccessLogger");
            var loggerField = loggerClass.getDeclaredField("logger");
            Long offset = (Long) staticFieldOffset.invoke(unsafe, loggerField);
            putObjectVolatile.invoke(unsafe, loggerClass, offset, null);
        } catch (Exception ignored) {
        }
    }

    public static String textLaenge(int max, String text, boolean mitte, boolean addVorne) {
        if (text.length() > max) {
            if (mitte) {
                text = text.substring(0, 25) + " .... " + text.substring(text.length() - (max - 31));
            } else {
                text = text.substring(0, max - 1);
            }
        }
        StringBuilder textBuilder = new StringBuilder(text);
        while (textBuilder.length() < max) {
            if (addVorne) {
                textBuilder.insert(0, ' ');
            } else {
                textBuilder.append(' ');
            }
        }
        text = textBuilder.toString();
        return text;
    }

    /**
     * Detect and return the currently used operating system.
     *
     * @return The enum for supported Operating Systems.
     */
    public static OperatingSystemType getOs() {
        OperatingSystemType os = OperatingSystemType.UNKNOWN;

        if (SystemUtils.IS_OS_WINDOWS) {
            if (ArchUtils.getProcessor().is64Bit())
                os = OperatingSystemType.WIN64;
            else
                os = OperatingSystemType.WIN32;
        } else if (SystemUtils.IS_OS_LINUX || SystemUtils.IS_OS_FREE_BSD)
            os = OperatingSystemType.LINUX; //This is a hack...
        else if (SystemUtils.IS_OS_MAC_OSX) {
            os = OperatingSystemType.MAC;
        }
        return os;
    }

    public static String getOsString() {
        return getOs().toString();
    }

    public enum OperatingSystemType {

        UNKNOWN(""), WIN32("Windows"), WIN64("Windows"), LINUX("Linux"), MAC("Mac");
        private final String name;

        OperatingSystemType(String name) {
            this.name = name;
        }

        @Override
        public String toString() {
            return name;
        }
    }
}
