package mediathek.tool;

import org.apache.commons.lang3.ArchUtils;
import org.apache.commons.lang3.SystemUtils;

import java.lang.management.ManagementFactory;

public class Functions {
    /**
     * Checks if the application has an debugger attached to it.
     * @return true if debugger was detected, false othewise.
     */
    public static boolean isDebuggerAttached() {
        return ManagementFactory.getRuntimeMXBean().getInputArguments().toString().indexOf("-agentlib:jdwp") > 0;
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

}
