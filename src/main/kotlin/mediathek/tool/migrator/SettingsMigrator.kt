package mediathek.tool.migrator

import mediathek.config.MVColor
import mediathek.config.application.ApplicationConfiguration
import org.apache.logging.log4j.LogManager
import org.w3c.dom.Element
import java.nio.file.Path
import javax.xml.XMLConstants
import javax.xml.parsers.DocumentBuilderFactory

class SettingsMigrator(
    private val settingsFile: Path,
) {
    private val legacyColorValues = LinkedHashMap<String, String>()

    fun migrate() {
        val document = documentBuilderFactory()
            .newDocumentBuilder()
            .parse(settingsFile.toFile())
        document.documentElement.normalize()

        val systemNodeList = document.documentElement.getElementsByTagName("system")
        if (systemNodeList.length == 0) {
            logger.trace("legacy settings file contains no system section, nothing to migrate")
            return
        }

        val systemChildNodeList = systemNodeList.item(0).childNodes
        for (index in 0 until systemChildNodeList.length) {
            val element = systemChildNodeList.item(index) as? Element ?: continue
            val nodeName = element.nodeName
            when (nodeName) {
                "Bandwidthmonitor-visible" -> migrateBandwidthMonitorVisibility(element)
                "Tray-anzeigen" -> migrateShowTray(element)
                "system-anz-tage-filmilste" -> migrateFilmListAnzTage(element) // kein Fehler!!!
                "maxDownload" -> migrateMaxNumDownloads(element)
                "system-panel-videoplayer-anzeigen" -> migrateSystemPanelVideoplayerAnzeigen(element)
                "Blacklist-Geo-nicht-anzeigen" -> migrateDoNotShowGeoFilms(element)
                "Groesse-Einstellungen" -> migrateSettingsDialogSize(element)
                "Blacklist-ausgeschaltet" -> migrateSystemBlacklistOn(element)
                LEGACY_DIRECTORY_OPEN_PROGRAM -> migrateDirectoryOpenProgram(element)
                LEGACY_VIDEO_PLAYER_PROGRAM -> migrateVideoPlayerProgram(element)
                LEGACY_WEB_BROWSER_PROGRAM -> migrateWebBrowserProgram(element)
                LEGACY_DOWNLOAD_ERROR_MESSAGE -> migrateDownloadErrorMessage(element)
                LEGACY_START_DOWNLOADS_IMMEDIATELY -> migrateStartDownloadsImmediately(element)
                LEGACY_PROGRAM_SET_SHOW_ALL_SETTINGS -> migrateProgramSetShowAllSettings(element)
                LEGACY_ABO_DEFAULT_MINIMUM_DURATION -> migrateAboDefaultMinimumDuration(element)
                LEGACY_FILM_LIST_UPDATE_TYPE -> migrateFilmListUpdateType(element)
                LEGACY_FILM_LIST_MANUAL_IMPORT_URL -> migrateFilmListManualImportUrl(element)
                LEGACY_FILM_TABLE_LINEBREAK -> migrateFilmTableLineBreak(element)
                LEGACY_DOWNLOAD_TABLE_LINEBREAK -> migrateDownloadTableLineBreak(element)
                LEGACY_FILM_TABLE_SHOW_SENDER_ICONS -> migrateFilmTableShowSenderIcons(element)
                LEGACY_FILM_TABLE_USE_SMALL_SENDER_ICONS -> migrateFilmTableUseSmallSenderIcons(element)
                LEGACY_DOWNLOAD_TABLE_SHOW_SENDER_ICONS -> migrateDownloadTableShowSenderIcons(element)
                LEGACY_DOWNLOAD_TABLE_USE_SMALL_SENDER_ICONS -> migrateDownloadTableUseSmallSenderIcons(element)
                LEGACY_ABO_TABLE_SHOW_SENDER_ICONS -> migrateAboTableShowSenderIcons(element)
                LEGACY_ABO_TABLE_USE_SMALL_SENDER_ICONS -> migrateAboTableUseSmallSenderIcons(element)
                LEGACY_FILM_TABLE_COLUMN_CONFIGURATION -> migrateFilmTableColumnConfiguration(element)
                LEGACY_DOWNLOAD_TABLE_COLUMN_CONFIGURATION -> migrateDownloadTableColumnConfiguration(element)
                LEGACY_ABO_TABLE_COLUMN_CONFIGURATION -> migrateAboTableColumnConfiguration(element)
                LEGACY_PROGRAM_INFORMATION_DISPLAYED_NUMBER -> migrateProgramInformationDisplayedNumber(element)
                LEGACY_BLACKLIST_DO_NOT_SHOW_FUTURE_FILMS -> migrateBlacklistDoNotShowFutureFilms(element)
                LEGACY_BLACKLIST_APPLY_TO_ABO -> migrateBlacklistApplyToAbo(element)
                LEGACY_BLACKLIST_WHITELIST_MODE -> migrateBlacklistWhitelistMode(element)
                LEGACY_BLACKLIST_MINIMUM_FILM_LENGTH -> migrateBlacklistMinimumFilmLength(element)
                LEGACY_SEARCH_ABOS_IMMEDIATELY -> migrateSearchAbosImmediately(element)
                LEGACY_FILENAME_USE_REPLACE_TABLE -> migrateFilenameUseReplaceTable(element)
                LEGACY_FILENAME_ONLY_ASCII -> migrateFilenameOnlyAscii(element)
                LEGACY_LINUX_SHUTDOWN_COMMAND -> migrateLinuxShutdownCommand(element)
                LEGACY_STANDARD_PROGRAM_SET_VERSION -> migrateStandardProgramSetVersion(element)
                LEGACY_STANDARD_VLC_PATH -> migrateStandardVlcPath(element)
                LEGACY_STANDARD_FFMPEG_PATH -> migrateStandardFFmpegPath(element)
                LEGACY_SAVED_DOWNLOAD_TARGET_PATHS -> migrateSavedDownloadTargetPaths(element)
                else -> if (MVColor.isLegacyColorKey(nodeName)) {
                    collectLegacyColor(element)
                }
            }
        }

        if (MVColor.migrateLegacyColors(legacyColorValues)) {
            logger.debug("migrateLegacyColors")
        }
    }

    private fun migrateDirectoryOpenProgram(element: Element) {
        ApplicationConfiguration.getInstance().directoryOpenProgram = element.textValue().orEmpty()
        logger.debug("migrateDirectoryOpenProgram")
    }

    private fun migrateVideoPlayerProgram(element: Element) {
        ApplicationConfiguration.getInstance().videoPlayerProgram = element.textValue().orEmpty()
        logger.debug("migrateVideoPlayerProgram")
    }

    private fun migrateWebBrowserProgram(element: Element) {
        ApplicationConfiguration.getInstance().webBrowserProgram = element.textValue().orEmpty()
        logger.debug("migrateWebBrowserProgram")
    }

    private fun migrateDownloadErrorMessage(element: Element) {
        val result = element.textValue()?.toBoolean() ?: return
        ApplicationConfiguration.getInstance().showDownloadErrorMessage = result
        logger.debug("migrateDownloadErrorMessage")
    }

    private fun migrateStartDownloadsImmediately(element: Element) {
        val result = element.textValue()?.toBoolean() ?: return
        ApplicationConfiguration.getInstance().startDownloadsImmediately = result
        logger.debug("migrateStartDownloadsImmediately")
    }

    private fun migrateProgramSetShowAllSettings(element: Element) {
        val result = element.textValue()?.toBoolean() ?: return
        ApplicationConfiguration.getInstance().programSetShowAllSettings = result
        logger.debug("migrateProgramSetShowAllSettings")
    }

    private fun migrateAboDefaultMinimumDuration(element: Element) {
        val result = element.textValue()?.toIntOrNull() ?: return
        ApplicationConfiguration.getInstance().defaultAboMinimumDurationMinutes = result
        logger.debug("migrateAboDefaultMinimumDuration")
    }

    private fun migrateFilmListUpdateType(element: Element) {
        val result = element.textValue()?.toIntOrNull() ?: return
        ApplicationConfiguration.getInstance().filmListUpdateType = result
        logger.debug("migrateFilmListUpdateType")
    }

    private fun migrateFilmListManualImportUrl(element: Element) {
        ApplicationConfiguration.getInstance().filmListManualImportUrl = element.textValue().orEmpty()
        logger.debug("migrateFilmListManualImportUrl")
    }

    private fun migrateFilmTableLineBreak(element: Element) {
        val result = element.textValue()?.toBoolean() ?: return
        ApplicationConfiguration.getInstance().filmTableLineBreak = result
        logger.debug("migrateFilmTableLineBreak")
    }

    private fun migrateDownloadTableLineBreak(element: Element) {
        val result = element.textValue()?.toBoolean() ?: return
        ApplicationConfiguration.getInstance().downloadTableLineBreak = result
        logger.debug("migrateDownloadTableLineBreak")
    }

    private fun migrateFilmTableShowSenderIcons(element: Element) {
        val result = element.textValue()?.toBoolean() ?: return
        ApplicationConfiguration.getInstance().filmTableShowSenderIcons = result
        logger.debug("migrateFilmTableShowSenderIcons")
    }

    private fun migrateFilmTableUseSmallSenderIcons(element: Element) {
        val result = element.textValue()?.toBoolean() ?: return
        ApplicationConfiguration.getInstance().filmTableUseSmallSenderIcons = result
        logger.debug("migrateFilmTableUseSmallSenderIcons")
    }

    private fun migrateDownloadTableShowSenderIcons(element: Element) {
        val result = element.textValue()?.toBoolean() ?: return
        ApplicationConfiguration.getInstance().downloadTableShowSenderIcons = result
        logger.debug("migrateDownloadTableShowSenderIcons")
    }

    private fun migrateDownloadTableUseSmallSenderIcons(element: Element) {
        val result = element.textValue()?.toBoolean() ?: return
        ApplicationConfiguration.getInstance().downloadTableUseSmallSenderIcons = result
        logger.debug("migrateDownloadTableUseSmallSenderIcons")
    }

    private fun migrateAboTableShowSenderIcons(element: Element) {
        val result = element.textValue()?.toBoolean() ?: return
        ApplicationConfiguration.getInstance().aboTableShowSenderIcons = result
        logger.debug("migrateAboTableShowSenderIcons")
    }

    private fun migrateAboTableUseSmallSenderIcons(element: Element) {
        val result = element.textValue()?.toBoolean() ?: return
        ApplicationConfiguration.getInstance().aboTableUseSmallSenderIcons = result
        logger.debug("migrateAboTableUseSmallSenderIcons")
    }

    private fun migrateFilmTableColumnConfiguration(element: Element) {
        ApplicationConfiguration.getInstance().filmTableColumnConfiguration = element.textValue().orEmpty()
        logger.debug("migrateFilmTableColumnConfiguration")
    }

    private fun migrateDownloadTableColumnConfiguration(element: Element) {
        ApplicationConfiguration.getInstance().downloadTableColumnConfiguration = element.textValue().orEmpty()
        logger.debug("migrateDownloadTableColumnConfiguration")
    }

    private fun migrateAboTableColumnConfiguration(element: Element) {
        ApplicationConfiguration.getInstance().aboTableColumnConfiguration = element.textValue().orEmpty()
        logger.debug("migrateAboTableColumnConfiguration")
    }

    private fun migrateProgramInformationDisplayedNumber(element: Element) {
        val result = element.textValue()?.toIntOrNull() ?: return
        ApplicationConfiguration.getInstance().programInformationDisplayedNumber = result
        logger.debug("migrateProgramInformationDisplayedNumber")
    }

    private fun migrateBlacklistDoNotShowFutureFilms(element: Element) {
        val result = element.textValue()?.toBoolean() ?: return
        ApplicationConfiguration.getInstance().blacklistDoNotShowFutureFilms = result
        logger.debug("migrateBlacklistDoNotShowFutureFilms")
    }

    private fun migrateBlacklistApplyToAbo(element: Element) {
        val result = element.textValue()?.toBoolean() ?: return
        ApplicationConfiguration.getInstance().blacklistApplyToAbo = result
        logger.debug("migrateBlacklistApplyToAbo")
    }

    private fun migrateBlacklistWhitelistMode(element: Element) {
        val result = element.textValue()?.toBoolean() ?: return
        ApplicationConfiguration.getInstance().blacklistWhitelistMode = result
        logger.debug("migrateBlacklistWhitelistMode")
    }

    private fun migrateBlacklistMinimumFilmLength(element: Element) {
        val result = element.textValue()?.toIntOrNull() ?: return
        ApplicationConfiguration.getInstance().blacklistMinimumFilmLengthMinutes = result
        logger.debug("migrateBlacklistMinimumFilmLength")
    }

    private fun migrateSearchAbosImmediately(element: Element) {
        val result = element.textValue()?.toBoolean() ?: return
        ApplicationConfiguration.getInstance().searchAbosImmediately = result
        logger.debug("migrateSearchAbosImmediately")
    }

    private fun migrateFilenameUseReplaceTable(element: Element) {
        val result = element.textValue()?.toBoolean() ?: return
        ApplicationConfiguration.getInstance().useFilenameReplaceTable = result
        logger.debug("migrateFilenameUseReplaceTable")
    }

    private fun migrateFilenameOnlyAscii(element: Element) {
        val result = element.textValue()?.toBoolean() ?: return
        ApplicationConfiguration.getInstance().onlyAsciiFilenames = result
        logger.debug("migrateFilenameOnlyAscii")
    }

    private fun migrateLinuxShutdownCommand(element: Element) {
        ApplicationConfiguration.getInstance().linuxShutdownCommand = element.textValue().orEmpty()
        logger.debug("migrateLinuxShutdownCommand")
    }

    private fun migrateStandardProgramSetVersion(element: Element) {
        ApplicationConfiguration.getInstance().standardProgramSetVersion = element.textValue().orEmpty()
        logger.debug("migrateStandardProgramSetVersion")
    }

    private fun migrateStandardVlcPath(element: Element) {
        ApplicationConfiguration.getInstance().standardVlcPath = element.textValue().orEmpty()
        logger.debug("migrateStandardVlcPath")
    }

    private fun migrateStandardFFmpegPath(element: Element) {
        ApplicationConfiguration.getInstance().standardFFmpegPath = element.textValue().orEmpty()
        logger.debug("migrateStandardFFmpegPath")
    }

    private fun migrateSavedDownloadTargetPaths(element: Element) {
        ApplicationConfiguration.getInstance().savedDownloadTargetPaths = element.textValue().orEmpty()
        logger.debug("migrateSavedDownloadTargetPaths")
    }

    private fun collectLegacyColor(element: Element) {
        val value = element.textValue().orEmpty()
        if (value.isNotEmpty()) {
            legacyColorValues[element.nodeName] = value
        }
    }

    private fun migrateSystemBlacklistOn(element: Element) {
        val result = element.textValue()?.toBoolean() ?: return
        ApplicationConfiguration.getInstance().isBlacklistEnabled = result
        logger.debug("migrateSystemBlacklistOn")
    }

    private fun migrateSettingsDialogSize(element: Element) {
        val size = element.textValue()?.let(::parseSettingsDialogSize) ?: return
        ApplicationConfiguration.getInstance()
            .setSettingsDialogBounds(size.x, size.y, size.width, size.height)
        logger.debug("migrateSettingsDialogSize")
    }

    private fun migrateDoNotShowGeoFilms(element: Element) {
        val result = element.textValue()?.toBoolean() ?: return
        ApplicationConfiguration.getInstance().blacklistDoNotShowGeoblockedFilms = result
        logger.debug("migrateDoNotShowGeoFilms")
    }

    private fun migrateSystemPanelVideoplayerAnzeigen(element: Element) {
        val result = element.textValue()?.toBoolean() ?: return
        ApplicationConfiguration.getInstance().buttonsPanelVisible = result
        logger.debug("migrateSystemPanelVideoplayerAnzeigen")
    }

    private fun migrateMaxNumDownloads(element: Element) {
        val text = element.textValue() ?: return
        val maxDownloads = text.toIntOrNull() ?: 1
        ApplicationConfiguration.getInstance().maxSimultaneousDownloads = maxDownloads
        logger.debug("migrateMaxNumDownloads")
    }

    private fun migrateFilmListAnzTage(element: Element) {
        val text = element.textValue() ?: return
        val anzahl = text.toIntOrNull() ?: 0
        ApplicationConfiguration.getInstance().filmListLoadNumDays = anzahl
        logger.debug("migrateFilmListAnzTage")
    }

    private fun migrateShowTray(element: Element) {
        val result = element.textValue()?.toBoolean() ?: return
        ApplicationConfiguration.getInstance().useTray = result
        logger.debug("migrateShowTray")
    }

    private fun migrateBandwidthMonitorVisibility(element: Element) {
        val result = element.textValue()?.toBoolean() ?: return
        ApplicationConfiguration.getInstance().bandwidthMonitorVisible = result
        logger.debug("migrateBandwidthMonitorVisibility")
    }

    private fun parseSettingsDialogSize(value: String): SettingsDialogSize {
        val parts = value.split(":")
        if (parts.size != SETTINGS_DIALOG_SIZE_PARTS) {
            return SettingsDialogSize()
        }

        val width = parts[0].toIntOrNull() ?: return SettingsDialogSize()
        val height = parts[1].toIntOrNull() ?: return SettingsDialogSize()
        val x = parts[2].toIntOrNull() ?: return SettingsDialogSize()
        val y = parts[3].toIntOrNull() ?: return SettingsDialogSize()
        return SettingsDialogSize(width, height, x, y)
    }

    private fun Element.textValue(): String? = firstChild?.nodeValue

    private data class SettingsDialogSize(
        val width: Int = 0,
        val height: Int = 0,
        val x: Int = 0,
        val y: Int = 0,
    )

    companion object {
        private const val SETTINGS_DIALOG_SIZE_PARTS = 4
        private const val LEGACY_DIRECTORY_OPEN_PROGRAM = "Download-Ordner-oeffnen"
        private const val LEGACY_VIDEO_PLAYER_PROGRAM = "Player-zum-Abspielen"
        private const val LEGACY_WEB_BROWSER_PROGRAM = "Programm-Url-oeffnen"
        private const val LEGACY_DOWNLOAD_ERROR_MESSAGE = "download-error-msg"
        private const val LEGACY_START_DOWNLOADS_IMMEDIATELY = "Download-sofort-starten"
        private const val LEGACY_PROGRAM_SET_SHOW_ALL_SETTINGS = "Ansicht-Set-lang"
        private const val LEGACY_ABO_DEFAULT_MINIMUM_DURATION = "Abo-Mindestdauer-Minuten"
        private const val LEGACY_FILM_LIST_UPDATE_TYPE = "update-filme"
        private const val LEGACY_FILM_LIST_MANUAL_IMPORT_URL = "system-import-url-manuell"
        private const val LEGACY_FILM_TABLE_LINEBREAK = "system-tab-filme-linebreak"
        private const val LEGACY_DOWNLOAD_TABLE_LINEBREAK = "system-tab-download-linebreak"
        private const val LEGACY_FILM_TABLE_SHOW_SENDER_ICONS = "system-tab-filme-icon-anzeigen"
        private const val LEGACY_FILM_TABLE_USE_SMALL_SENDER_ICONS = "system-tab-filme-icon-klein"
        private const val LEGACY_DOWNLOAD_TABLE_SHOW_SENDER_ICONS = "system-tab-download-icon-anzeigen"
        private const val LEGACY_DOWNLOAD_TABLE_USE_SMALL_SENDER_ICONS = "system-tab-download-icon-klein"
        private const val LEGACY_ABO_TABLE_SHOW_SENDER_ICONS = "system-tab-abo-icon-anzeigen"
        private const val LEGACY_ABO_TABLE_USE_SMALL_SENDER_ICONS = "system-tab-abo-icon-klein"
        private const val LEGACY_FILM_TABLE_COLUMN_CONFIGURATION = "Eigenschaften-Tabellen-Filme"
        private const val LEGACY_DOWNLOAD_TABLE_COLUMN_CONFIGURATION = "Eigenschaften-Tabellen-Downloads"
        private const val LEGACY_ABO_TABLE_COLUMN_CONFIGURATION = "Eigenschaften-Tabellen-Abos"
        private const val LEGACY_PROGRAM_INFORMATION_DISPLAYED_NUMBER = "Hinweis-Nr-angezeigt"
        private const val LEGACY_BLACKLIST_DO_NOT_SHOW_FUTURE_FILMS = "Blacklist-Zukunft-nicht-anzeigen"
        private const val LEGACY_BLACKLIST_APPLY_TO_ABO = "Blacklist-auch-Abo"
        private const val LEGACY_BLACKLIST_WHITELIST_MODE = "Blacklist-ist-Whitelist"
        private const val LEGACY_BLACKLIST_MINIMUM_FILM_LENGTH = "Blacklist-Filmlaenge"
        private const val LEGACY_SEARCH_ABOS_IMMEDIATELY = "Abos-sofort-suchen"
        private const val LEGACY_FILENAME_USE_REPLACE_TABLE = "Ersetzungstabelle-verwenden"
        private const val LEGACY_FILENAME_ONLY_ASCII = "nur-ascii"
        private const val LEGACY_LINUX_SHUTDOWN_COMMAND = "Programm-Linux-Shutdown"
        private const val LEGACY_STANDARD_PROGRAM_SET_VERSION = "Version-Programmset"
        private const val LEGACY_STANDARD_VLC_PATH = "pfad-vlc"
        private const val LEGACY_STANDARD_FFMPEG_PATH = "pfad-ffmpeg"
        private const val LEGACY_SAVED_DOWNLOAD_TARGET_PATHS = "Pfade-zum-Speichern"

        private val logger = LogManager.getLogger(SettingsMigrator::class.java)

        private fun documentBuilderFactory(): DocumentBuilderFactory =
            DocumentBuilderFactory.newInstance().apply {
                setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true)
                setFeature("http://apache.org/xml/features/disallow-doctype-decl", true)
                setFeature("http://xml.org/sax/features/external-general-entities", false)
                setFeature("http://xml.org/sax/features/external-parameter-entities", false)
                setAttribute(XMLConstants.ACCESS_EXTERNAL_DTD, "")
                setAttribute(XMLConstants.ACCESS_EXTERNAL_SCHEMA, "")
                isExpandEntityReferences = false
            }
    }
}
