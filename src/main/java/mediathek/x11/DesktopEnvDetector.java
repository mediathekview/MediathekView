package mediathek.x11;

import org.apache.commons.lang3.SystemUtils;

import java.io.IOException;
import java.util.Locale;
import java.util.Map;

public final class DesktopEnvDetector {

    public enum DesktopEnvironment {
        KDE, GNOME, UNITY, UNKNOWN
    }

    private DesktopEnvDetector() {
    }

    public static boolean trayIconSupported() {
        if (!SystemUtils.IS_OS_LINUX)
            return true;

        return (detect() != DesktopEnvironment.KDE);
    }

    public static DesktopEnvironment detect() {
        String envValue = getDesktopEnvFromVariables().toUpperCase(Locale.ROOT);

        if (envValue.contains("KDE"))
            return DesktopEnvironment.KDE;
        else if (envValue.contains("GNOME"))
            return DesktopEnvironment.GNOME;
        else if (envValue.contains("UNITY"))
            return DesktopEnvironment.UNITY;

        // Fallback
        if (isProcessRunning("plasmashell"))
            return DesktopEnvironment.KDE;
        else if (isProcessRunning("gnome-shell"))
            return DesktopEnvironment.GNOME;
        else if (isProcessRunning("unity-panel-service"))
            return DesktopEnvironment.UNITY;

        return DesktopEnvironment.UNKNOWN;
    }

    private static String getDesktopEnvFromVariables() {
        Map<String, String> env = System.getenv();

        if (env.containsKey("XDG_CURRENT_DESKTOP"))
            return env.get("XDG_CURRENT_DESKTOP");
        else if (env.containsKey("DESKTOP_SESSION"))
            return env.get("DESKTOP_SESSION");
        else if (env.containsKey("GDMSESSION"))
            return env.get("GDMSESSION");
        else if (env.containsKey("GNOME_DESKTOP_SESSION_ID"))
            return "GNOME";
        else
            return "unknown";
    }

    private static boolean isProcessRunning(String process) {
        try {
            Process p = new ProcessBuilder("pgrep", "-x", process)
                    .redirectErrorStream(true)
                    .start();
            int exitCode = p.waitFor();
            return exitCode == 0;
        } catch (IOException | InterruptedException e) {
            return false;
        }
    }
}
