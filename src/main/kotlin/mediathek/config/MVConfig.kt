package mediathek.config

import mediathek.tool.ApplicationConfiguration
import mediathek.tool.FilmListUpdateType
import mediathek.tool.GermanStringSorter
import mediathek.tool.GuiFunktionenProgramme
import org.apache.commons.lang3.SystemUtils
import org.apache.logging.log4j.LogManager
import javax.xml.stream.XMLStreamConstants
import javax.xml.stream.XMLStreamReader
import javax.xml.stream.XMLStreamWriter

object MVConfig {
    internal const val TRENNER = "#=#"
    private const val SYSTEM = "system"
    private val logger = LogManager.getLogger(MVConfig::class.java)
    private val values = HashMap<String, String>()
    private val knownConfigKeys by lazy { Configs.entries.mapTo(HashSet()) { it.configKey } }

    @Synchronized
    fun loadSystemParameter() {
        // einmal die leeren mit den inits füllen
        for (key in Configs.entries) {
            val value = values[key.configKey]
            if (value.isNullOrEmpty()) {
                add(key, key.defaultValue)
            }
        }

        if (CommandLineOptions.isDebugModeEnabled()) {
            logger.debug("Debug mode enabled - Setting FilmList import mode to MANUAL")
            FilmListUpdateType.MANUAL.writeToConfig()
        }

        logger.debug(
            "User-Agent: {}",
            ApplicationConfiguration.getConfiguration()
                .getString(ApplicationConfiguration.APPLICATION_USER_AGENT)
        )
    }

    @Synchronized
    internal fun add(key: String, value: String) {
        values[key] = value
    }

    @JvmStatic
    @Synchronized
    fun add(key: Configs, value: String) {
        values[key.configKey] = value
    }

    @JvmStatic
    @Synchronized
    fun setBoolean(key: Configs, value: Boolean) {
        add(key, value.toString())
    }

    @JvmStatic
    @Synchronized
    fun setInt(key: Configs, value: Int) {
        add(key, value.toString())
    }

    @Synchronized
    internal fun remove(key: String) {
        values.remove(key)
    }

    @JvmStatic
    @Synchronized
    operator fun get(key: Configs): String =
        values[key.configKey] ?: key.defaultValue

    @Synchronized
    internal operator fun get(key: String): String =
        values[key].orEmpty()

    @JvmStatic
    @Synchronized
    fun getBoolean(key: Configs): Boolean =
        get(key).toBoolean()

    @JvmStatic
    @Synchronized
    fun getInt(key: Configs, defaultValue: Int): Int =
        get(key).toIntOrNull() ?: defaultValue

    @JvmStatic
    @Synchronized
    fun getLong(key: Configs, defaultValue: Long): Long =
        get(key).toLongOrNull() ?: defaultValue

    fun isSystemElement(elementName: String): Boolean =
        SYSTEM == elementName

    fun readSystemConfiguration(parser: XMLStreamReader) {
        try {
            while (parser.hasNext()) {
                val event = parser.next()
                if (event == XMLStreamConstants.END_ELEMENT && isSystemElement(parser.localName)) {
                    break
                }
                if (event == XMLStreamConstants.START_ELEMENT) {
                    add(parser.localName, parser.elementText)
                }
            }
        } catch (ex: Exception) {
            logger.error("readSystemConfiguration", ex)
        }
    }

    fun writeSystemConfiguration(writer: XMLStreamWriter) {
        val configEntries = sortedKnownEntries()

        try {
            writer.writeStartElement(SYSTEM)
            writeNewLine(writer)
            for (entry in configEntries) {
                writer.writeCharacters("\t")
                writer.writeStartElement(entry.key)
                writer.writeCharacters(entry.value)
                writer.writeEndElement()
                writeNewLine(writer)
            }
            writer.writeEndElement()
            writeNewLine(writer)
        } catch (ex: Exception) {
            logger.error("writeSystemConfiguration", ex)
        }
    }

    @Synchronized
    private fun sortedKnownEntries(): List<ConfigEntry> =
        values
            .asSequence()
            .filter { (key, _) -> isKnownConfigKey(key) }
            .map { (key, value) -> ConfigEntry(key, value) }
            .sortedWith { first, second -> GermanStringSorter.compare(first.key, second.key) }
            .toList()

    private fun isKnownConfigKey(value: String): Boolean =
        value in knownConfigKeys

    private fun writeNewLine(writer: XMLStreamWriter) {
        writer.writeCharacters("\n")
    }

    private data class ConfigEntry(
        val key: String,
        val value: String,
    )

    enum class Configs(
        internal val configKey: String,
        internal val defaultValue: String = "",
    ) {
        // Programm-Configs, änderbar über Gui
        SYSTEM_ABOS_SOFORT_SUCHEN("Abos-sofort-suchen", true.toString()),
        SYSTEM_USE_REPLACETABLE(
            "Ersetzungstabelle-verwenden",
            (SystemUtils.IS_OS_LINUX || SystemUtils.IS_OS_MAC_OSX).toString()
        ), // wegen des Problems mit ext. Programmaufrufen und Leerzeichen
        SYSTEM_ONLY_ASCII("nur-ascii", false.toString()),
        SYSTEM_HINWEIS_NR_ANGEZEIGT("Hinweis-Nr-angezeigt"),
        SYSTEM_ORDNER_OEFFNEN("Download-Ordner-oeffnen"),
        SYSTEM_URL_OEFFNEN("Programm-Url-oeffnen"),
        SYSTEM_LINUX_SHUTDOWN("Programm-Linux-Shutdown"),
        SYSTEM_PLAYER_ABSPIELEN("Player-zum-Abspielen"),

        // Fenstereinstellungen
        SYSTEM_EIGENSCHAFTEN_TABELLE_FILME("Eigenschaften-Tabellen-Filme"),
        SYSTEM_EIGENSCHAFTEN_TABELLE_DOWNLOADS("Eigenschaften-Tabellen-Downloads"),
        SYSTEM_EIGENSCHAFTEN_TABELLE_ABOS("Eigenschaften-Tabellen-Abos"),
        SYSTEM_ANSICHT_SET_LANG("Ansicht-Set-lang"),
        SYSTEM_TAB_FILME_ICON_ANZEIGEN("system-tab-filme-icon-anzeigen", true.toString()),
        SYSTEM_TAB_FILME_LINEBREAK("system-tab-filme-linebreak", false.toString()),
        SYSTEM_TAB_FILME_ICON_KLEIN("system-tab-filme-icon-klein", true.toString()),
        SYSTEM_TAB_DOWNLOAD_ICON_ANZEIGEN("system-tab-download-icon-anzeigen", true.toString()),
        SYSTEM_TAB_DOWNLOAD_ICON_KLEIN("system-tab-download-icon-klein", true.toString()),
        SYSTEM_TAB_DOWNLOAD_FILTER_VIS("system-tab-download-filter-vis", true.toString()),
        SYSTEM_TAB_DOWNLOAD_LINEBREAK("system-tab-download-linebreak", false.toString()),
        SYSTEM_TAB_ABO_ICON_ANZEIGEN("system-tab-abo-icon-anzeigen", true.toString()),
        SYSTEM_TAB_ABO_ICON_KLEIN("system-tab-abo-icon-klein", true.toString()),

        // Einstellungen Filmliste
        SYSTEM_IMPORT_ART_FILME("update-filme"), // url automatisch suchen - oder nur manuell
        SYSTEM_IMPORT_URL_MANUELL("system-import-url-manuell"),

        // Programmpfade
        SYSTEM_PFAD_VLC("pfad-vlc", GuiFunktionenProgramme.getMusterPfadVlc()),
        SYSTEM_PFAD_FFMPEG("pfad-ffmpeg", GuiFunktionenProgramme.getMusterPfadFFmpeg()),
        SYSTEM_VERSION_PROGRAMMSET("Version-Programmset"),

        // Blacklist
        SYSTEM_BLACKLIST_ZUKUNFT_NICHT_ANZEIGEN("Blacklist-Zukunft-nicht-anzeigen"),
        SYSTEM_BLACKLIST_AUCH_ABO("Blacklist-auch-Abo"),
        SYSTEM_BLACKLIST_IST_WHITELIST("Blacklist-ist-Whitelist"),
        SYSTEM_BLACKLIST_FILMLAENGE("Blacklist-Filmlaenge", "0"),

        // Download
        SYSTEM_DOWNLOAD_SOFORT_STARTEN("Download-sofort-starten", false.toString()),
        SYSTEM_DOWNLOAD_ERRORMSG("download-error-msg", true.toString()),
        SYSTEM_DIALOG_DOWNLOAD__PFADE_ZUM_SPEICHERN("Pfade-zum-Speichern"), // gesammelten Downloadpfade im Downloaddialog

        // Abo
        SYSTEM_ABO_MIN_SIZE("Abo-Mindestdauer-Minuten")
    }
}
