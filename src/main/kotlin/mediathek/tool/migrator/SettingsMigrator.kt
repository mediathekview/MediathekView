package mediathek.tool.migrator

import mediathek.tool.ApplicationConfiguration
import mediathek.tool.withLock
import org.apache.commons.configuration2.Configuration
import org.apache.commons.configuration2.sync.LockMode
import org.apache.logging.log4j.LogManager
import org.w3c.dom.Element
import java.nio.file.Path
import javax.xml.XMLConstants
import javax.xml.parsers.DocumentBuilderFactory

class SettingsMigrator(
    private val settingsFile: Path,
) {
    private val config: Configuration = ApplicationConfiguration.getConfiguration()

    fun migrate() {
        val document = documentBuilderFactory()
            .newDocumentBuilder()
            .parse(settingsFile.toFile())
        document.documentElement.normalize()

        val systemNodeList = document.documentElement.getElementsByTagName("system")
        if (systemNodeList.length == 0) {
            logger.error("root element is empty")
            return
        }

        val systemChildNodeList = systemNodeList.item(0).childNodes
        for (index in 0 until systemChildNodeList.length) {
            val element = systemChildNodeList.item(index) as? Element ?: continue
            when (element.nodeName) {
                "Bandwidthmonitor-visible" -> migrateBandwidthMonitorVisibility(element)
                "Tray-anzeigen" -> migrateShowTray(element)
                "system-anz-tage-filmilste" -> migrateFilmListAnzTage(element) // kein Fehler!!!
                "maxDownload" -> migrateMaxNumDownloads(element)
                "system-panel-videoplayer-anzeigen" -> migrateSystemPanelVideoplayerAnzeigen(element)
                "Blacklist-Geo-nicht-anzeigen" -> migrateDoNotShowGeoFilms(element)
                "Groesse-Einstellungen" -> migrateSettingsDialogSize(element)
                "Blacklist-ausgeschaltet" -> migrateSystemBlacklistOn(element)
            }
        }
    }

    private fun migrateSystemBlacklistOn(element: Element) {
        val result = element.textValue()?.toBoolean() ?: return
        config.setProperty(ApplicationConfiguration.BLACKLIST_IS_ON, result)
        logger.debug("migrateSystemBlacklistOn")
    }

    private fun migrateSettingsDialogSize(element: Element) {
        val size = element.textValue()?.let(::parseSettingsDialogSize) ?: return
        config.withLock(LockMode.WRITE) {
            setProperty(ApplicationConfiguration.SettingsDialog.WIDTH, size.width)
            setProperty(ApplicationConfiguration.SettingsDialog.HEIGHT, size.height)
            setProperty(ApplicationConfiguration.SettingsDialog.X, size.x)
            setProperty(ApplicationConfiguration.SettingsDialog.Y, size.y)
        }
        logger.debug("migrateSettingsDialogSize")
    }

    private fun migrateDoNotShowGeoFilms(element: Element) {
        val result = element.textValue()?.toBoolean() ?: return
        ApplicationConfiguration.getInstance().setBlacklistDoNotShowGeoblockedFilms(result)
        logger.debug("migrateDoNotShowGeoFilms")
    }

    private fun migrateSystemPanelVideoplayerAnzeigen(element: Element) {
        val result = element.textValue()?.toBoolean() ?: return
        config.setProperty(ApplicationConfiguration.APPLICATION_BUTTONS_PANEL_VISIBLE, result)
        logger.debug("migrateSystemPanelVideoplayerAnzeigen")
    }

    private fun migrateMaxNumDownloads(element: Element) {
        val text = element.textValue() ?: return
        val maxDownloads = text.toIntOrNull() ?: 1
        config.setProperty(ApplicationConfiguration.DOWNLOAD_MAX_SIMULTANEOUS_NUM, maxDownloads)
        logger.debug("migrateMaxNumDownloads")
    }

    private fun migrateFilmListAnzTage(element: Element) {
        val text = element.textValue() ?: return
        val anzahl = text.toIntOrNull() ?: 0
        config.setProperty(ApplicationConfiguration.FilmList.LOAD_NUM_DAYS, anzahl)
        logger.debug("migrateFilmListAnzTage")
    }

    private fun migrateShowTray(element: Element) {
        val result = element.textValue()?.toBoolean() ?: return
        config.setProperty(ApplicationConfiguration.APPLICATION_UI_USE_TRAY, result)
        logger.debug("migrateShowTray")
    }

    private fun migrateBandwidthMonitorVisibility(element: Element) {
        val result = element.textValue()?.toBoolean() ?: return
        config.setProperty(ApplicationConfiguration.APPLICATION_UI_BANDWIDTH_MONITOR_VISIBLE, result)
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
