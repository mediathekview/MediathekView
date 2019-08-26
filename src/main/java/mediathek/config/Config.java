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

import picocli.CommandLine;

@CommandLine.Command(name = "MediathekView")
public class Config {

    @CommandLine.Option(names = {"-d", "--debug"}, hidden = true, description = "Debug-Modus aktivieren (FÜR ENTWICKLER)")
    private static boolean debug; // Debugmodus

    private static boolean portableMode;

    @CommandLine.Option(names = {"-m", "--maximized"}, description = "Programmfenster beim Start maximieren")
    private static boolean startMaximized; // Fenster maximieren

    @CommandLine.Parameters(index="0", paramLabel = "<Pfad zum Verzeichnis>",description = "Pfad zum Einstellungsverzeichnis für Portablen Betrieb", arity = "0..1")
    public static String baseFilePath;

    @CommandLine.Option(names = { "-h", "--help" }, usageHelp = true, description = "Hilfe anzeigen")
    private static boolean helpRequested;

    public static boolean isPortableMode() {
        return portableMode;
    }

    public static void setPortableMode(boolean portableMode) {
        Config.portableMode = portableMode;
    }

    public static boolean isDebugModeEnabled() {
        return debug;
    }

    public static boolean isStartMaximized() {
        return startMaximized;
    }
}
