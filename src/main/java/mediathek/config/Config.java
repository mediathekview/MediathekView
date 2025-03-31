/*
 * MediathekView
 * Copyright (C) 2008 W. Xaver
 * W.Xaver[at]googlemail.com
 * http://zdfmediathk.sourceforge.net/
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.config;

import mediathek.tool.dns.IPvPreferenceMode;
import picocli.CommandLine;

@CommandLine.Command(name = "MediathekView")
public class Config {
    @CommandLine.Parameters(index = "0", paramLabel = "<Pfad zum Verzeichnis>", description = "Pfad zum Einstellungsverzeichnis für Portablen Betrieb", arity = "0..1")
    public static String baseFilePath;
    /**
     * Specify the preferred DNS IP mode for name resolutions. Defaults to IPv4 only.
     */
    @CommandLine.Option(names = {"-dpm", "--dns-preference-mode"}, description = "Bevorzugtes IP-Protokoll für DNS festlegen")
    private static IPvPreferenceMode dnsIpPreferenceMode = IPvPreferenceMode.IPV4_ONLY;
    @CommandLine.Option(names = {"-d", "--debug"}, hidden = true, description = "Debug-Modus aktivieren (FÜR ENTWICKLER)")
    private static boolean debug; // Debugmodus
    @CommandLine.Option(names = {"-dfd", "--disable-flatlaf-decorations"}, description = "Deaktiviert unter Linux Window Manager Dekorationen")
    private static boolean disableFlatLafDecorations;
    /**
     * Limit the number of used CPUs on Windows.
     */
    @CommandLine.Option(names = {"-n", "--num-cpus"}, hidden = true, description = "Anzahl der genutzen CPU-Kerne festlegen (FÜR ENTWICKLER)")
    private static int numCpus;
    /**
     * For development use parameter to enable TRACE output to log env.
     */
    @CommandLine.Option(names = {"-e", "--enhanced-logging"}, description = "Erweiterten Log-Modus aktivieren")
    private static boolean enhancedLogging;
    /**
     * This will install a repaint manager which monitors Swing EDT violations.
     */
    @CommandLine.Option(names = {"-s", "--swing-thread-checker"}, description = "Swing EDT Thread Repaint Manager installieren (FÜR ENTWICKLER)", hidden = true)
    private static boolean installThreadCheckingRepaintManager;
    /**
     * Log HTTP traffic to console. By default HttpLoggingInterceptor.Level.BASIC will be used.
     * Configuration can be changed by ApplicationConfiguration.APPLICATION_DEBUG_HTTP_TRAFFIC_TRACE_LEVEL
     */
    @CommandLine.Option(names = {"-t", "--debug-http-traffic"}, hidden = true, description = "Logging für HTTP Traffic aktivieren (FÜR ENTWICKLER)")
    private static boolean debugHttpTraffic;
    private static boolean portableMode;
    @CommandLine.Option(names = {"-m", "--maximized"}, description = "Programmfenster beim Start maximieren")
    private static boolean startMaximized; // Fenster maximieren
    @CommandLine.Option(names = {"-h", "--help"}, usageHelp = true, description = "Hilfe anzeigen")
    private static boolean helpRequested;
    @CommandLine.Option(names = {"-f", "--disable-file-logging"}, description = "Speichern des Log output in Datei deaktivieren")
    private static boolean fileLoggingDisabled;
    /**
     * Disable JVM parameter checks on startup.
     */
    @CommandLine.Option(names = {"-nj", "--no-jvm-param-checks"}, description = "JVM Parameter-Prüfung deaktivieren")
    private static boolean disableJvmParameterChecks;
    @CommandLine.Option(names = {"-ns", "--no-splash"}, description = "Splash-Screen nicht anzeigen")
    private static boolean disableSplashScreen;
    @CommandLine.Option(names = {"-dq", "--download-quit"}, description = "Automatisch downloaden, dann beenden")
    private static boolean downloadAndQuit;

    public static boolean isDisableFlatLafDecorations() {
        return disableFlatLafDecorations;
    }

    public static IPvPreferenceMode getDnsIpPreferenceMode() {
        return dnsIpPreferenceMode;
    }

    public static void setDnsIpPreferenceMode(IPvPreferenceMode dnsIpPreferenceMode) {
        Config.dnsIpPreferenceMode = dnsIpPreferenceMode;
    }

    public static boolean shouldDownloadAndQuit() { return downloadAndQuit;}
    public static boolean isSplashScreenDisabled() { return disableSplashScreen;}

    public static boolean isDisableJvmParameterChecks() {
        return disableJvmParameterChecks;
    }

    public static boolean isInstallThreadCheckingRepaintManager() {
        return installThreadCheckingRepaintManager;
    }

    public static int getNumCpus() {
        return numCpus;
    }

    public static void setNumCpus(int num) {
        numCpus = num;
    }

    public static boolean isPortableMode() {
        return portableMode;
    }

    public static void setPortableMode(boolean portableMode) {
        Config.portableMode = portableMode;
    }

    public static boolean isEnhancedLoggingEnabled() {
        return enhancedLogging;
    }

    public static boolean isDebugModeEnabled() {
        return debug;
    }

    public static boolean isFileLoggingDisabled() {
        return fileLoggingDisabled;
    }

    public static boolean isStartMaximized() {
        return startMaximized;
    }

    public static boolean isHttpTrafficDebuggingEnabled() {
        return debugHttpTraffic;
    }
}
