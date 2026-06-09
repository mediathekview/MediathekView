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

package mediathek.config

import mediathek.tool.dns.IPvPreferenceMode
import picocli.CommandLine

@CommandLine.Command(name = "MediathekView")
object CommandLineOptions {
    @field:CommandLine.Parameters(
        index = "0",
        paramLabel = "<Pfad zum Verzeichnis>",
        description = ["Pfad zum Einstellungsverzeichnis für Portablen Betrieb"],
        arity = "0..1",
    )
    var baseFilePath: String? = null

    @field:CommandLine.Option(
        names = ["-dpm", "--dns-preference-mode"],
        description = ["Bevorzugtes IP-Protokoll für DNS festlegen"],
    )
    private var dnsIpPreferenceMode = IPvPreferenceMode.IPV4_ONLY

    @field:CommandLine.Option(
        names = ["-d", "--debug"],
        hidden = true,
        description = ["Debug-Modus aktivieren (FÜR ENTWICKLER)"],
    )
    private var debug = false

    @field:CommandLine.Option(
        names = ["-dfd", "--disable-flatlaf-decorations"],
        description = ["Deaktiviert unter Linux Window Manager Dekorationen"],
    )
    private var disableFlatLafDecorations = false

    @field:CommandLine.Option(
        names = ["-n", "--num-cpus"],
        hidden = true,
        description = ["Anzahl der genutzen CPU-Kerne festlegen (FÜR ENTWICKLER)"],
    )
    private var numCpus = 0

    @field:CommandLine.Option(
        names = ["-e", "--enhanced-logging"],
        description = ["Erweiterten Log-Modus aktivieren"],
    )
    private var enhancedLogging = false

    @field:CommandLine.Option(
        names = ["-s", "--swing-thread-checker"],
        description = ["Swing EDT Thread Repaint Manager installieren (FÜR ENTWICKLER)"],
        hidden = true,
    )
    private var installThreadCheckingRepaintManager = false

    @field:CommandLine.Option(
        names = ["-t", "--debug-http-traffic"],
        hidden = true,
        description = ["Logging für HTTP Traffic aktivieren (FÜR ENTWICKLER)"],
    )
    private var debugHttpTraffic = false

    private var portableMode = false

    @field:CommandLine.Option(
        names = ["-m", "--maximized"],
        description = ["Programmfenster beim Start maximieren"],
    )
    private var startMaximized = false

    @field:CommandLine.Option(
        names = ["-h", "--help"],
        usageHelp = true,
        description = ["Hilfe anzeigen"],
    )
    private var helpRequested = false

    @field:CommandLine.Option(
        names = ["-f", "--disable-file-logging"],
        description = ["Speichern des Log output in Datei deaktivieren"],
    )
    private var fileLoggingDisabled = false

    @field:CommandLine.Option(
        names = ["-nj", "--no-jvm-param-checks"],
        description = ["JVM Parameter-Prüfung deaktivieren"],
    )
    private var disableJvmParameterChecks = false

    @field:CommandLine.Option(
        names = ["-ns", "--no-splash"],
        description = ["Splash-Screen nicht anzeigen"],
    )
    private var disableSplashScreen = false

    @field:CommandLine.Option(
        names = ["-dq", "--download-quit"],
        description = ["Filmliste aktualisieren, Abo-Downloads starten und danach beenden"],
    )
    private var downloadAndQuit = false

    fun isDisableFlatLafDecorations(): Boolean = disableFlatLafDecorations

    fun getDnsIpPreferenceMode(): IPvPreferenceMode = dnsIpPreferenceMode

    fun setDnsIpPreferenceMode(dnsIpPreferenceMode: IPvPreferenceMode) {
        this.dnsIpPreferenceMode = dnsIpPreferenceMode
    }

    fun isSplashScreenDisabled(): Boolean = disableSplashScreen

    fun isDownloadAndQuit(): Boolean = downloadAndQuit

    fun isDisableJvmParameterChecks(): Boolean = disableJvmParameterChecks

    fun isInstallThreadCheckingRepaintManager(): Boolean = installThreadCheckingRepaintManager

    fun getNumCpus(): Int = numCpus

    fun setNumCpus(num: Int) {
        numCpus = num
    }

    fun isPortableMode(): Boolean = portableMode

    fun setPortableMode(portableMode: Boolean) {
        this.portableMode = portableMode
    }

    @JvmStatic
    fun isEnhancedLoggingEnabled(): Boolean = enhancedLogging

    @JvmStatic
    fun isDebugModeEnabled(): Boolean = debug

    fun isFileLoggingDisabled(): Boolean = fileLoggingDisabled

    @JvmStatic
    fun isStartMaximized(): Boolean = startMaximized

    fun isHttpTrafficDebuggingEnabled(): Boolean = debugHttpTraffic
}
