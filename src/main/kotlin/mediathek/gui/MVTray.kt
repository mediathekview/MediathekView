package mediathek.gui

import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.SupervisorJob
import kotlinx.coroutines.cancel
import kotlinx.coroutines.isActive
import kotlinx.coroutines.launch
import kotlinx.coroutines.swing.Swing
import mediathek.config.Daten
import mediathek.config.Konstanten
import mediathek.config.application.ApplicationConfiguration
import mediathek.gui.messages.TimerEvent
import mediathek.gui.messages.TrayIconEvent
import mediathek.mainwindow.TrayHost
import mediathek.tool.GetIcon
import mediathek.tool.MessageBus
import mediathek.tool.notification.MessageType
import mediathek.tool.notification.NotificationMessage
import mediathek.tool.notification.NotificationService
import net.engio.mbassy.listener.Handler
import org.apache.logging.log4j.LogManager
import java.awt.AWTException
import java.awt.MenuItem
import java.awt.PopupMenu
import java.awt.SystemTray
import java.awt.TrayIcon
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent

class MVTray(
    private val host: TrayHost,
) {
    private enum class TrayState {
        IDLE,
        DOWNLOADING,
        ERROR,
    }

    private var trayState = TrayState.IDLE
    private var tray: SystemTray? = null
    private var trayIcon: TrayIcon? = null
    private var count = 0
    private var subscribed = false
    private var uiScope = createUiScope()

    @Handler
    @Suppress("UNUSED_PARAMETER")
    private fun handleTimerEvent(msg: TimerEvent) {
        uiScope.launch {
            val currentTrayIcon = trayIcon ?: return@launch

            ++count
            if (count > 3) {
                // nur alle 3s ändern
                currentTrayIcon.toolTip = infoTextDownloads
                count = 0
            }

            // Anzahl, Anz-Abo, Anz-Down, nicht gestarted, laufen, fertig OK, fertig fehler
            val info = Daten.getInstance().listeDownloads.starts
            if (info.error > 0) {
                // es gibt welche mit Fehler
                if (trayState != TrayState.ERROR) {
                    trayState = TrayState.ERROR
                    currentTrayIcon.image = GetIcon.getProgramIcon("tray-fehler.png", 256, 256).image
                }
            } else if (info.running > 0) {
                // es laufen welche
                if (trayState != TrayState.DOWNLOADING) {
                    trayState = TrayState.DOWNLOADING
                    currentTrayIcon.image = GetIcon.getProgramIcon("tray-download.png", 256, 256).image
                }
            } else if (trayState != TrayState.IDLE) {
                trayState = TrayState.IDLE
                currentTrayIcon.image = Konstanten.ICON_TRAY
            }
        }
    }

    fun beenden() {
        val currentTray = tray
        val currentTrayIcon = trayIcon
        if (currentTray != null && currentTrayIcon != null) {
            currentTray.remove(currentTrayIcon)
        }
        if (subscribed) {
            MessageBus.messageBus.unsubscribe(this)
            subscribed = false
        }
        uiScope.cancel()
        tray = null
        trayIcon = null
    }

    fun systemTray(): MVTray? {
        if (!SystemTray.isSupported()) {
            logger.info("Tray wird nicht unterstützt")
            return null
        }

        if (!uiScope.isActive) {
            uiScope = createUiScope()
        }

        val newTray = SystemTray.getSystemTray()
        val newTrayIcon = TrayIcon(Konstanten.ICON_TRAY)
        tray = newTray
        trayIcon = newTrayIcon
        newTrayIcon.isImageAutoSize = true
        newTrayIcon.toolTip = infoTextDownloads
        addListener(newTrayIcon)

        val popup = PopupMenu()
        newTrayIcon.popupMenu = popup

        val itemInfo = MenuItem("Infos anzeigen")
        itemInfo.addActionListener { addNotification(textInfos) }
        popup.add(itemInfo)

        val itemRemoveTray = MenuItem("Trayicon ausblenden")
        itemRemoveTray.addActionListener {
            host.showMainWindow()
            ApplicationConfiguration.getInstance().useTray = false
            host.refreshSystemTray()
            MessageBus.messageBus.publishAsync(TrayIconEvent())
        }
        popup.add(itemRemoveTray)

        popup.addSeparator()
        val itemBeenden = MenuItem("Programm beenden")
        itemBeenden.addActionListener { host.quitApplication() }
        popup.add(itemBeenden)

        newTrayIcon.popupMenu = popup
        try {
            newTray.add(newTrayIcon)
            MessageBus.messageBus.subscribe(this)
            subscribed = true
            return this
        } catch (e: AWTException) {
            logger.error("Tray konnte nicht geladen werden", e)
            tray = null
            trayIcon = null
        }

        return null
    }

    private fun addListener(currentTrayIcon: TrayIcon) {
        currentTrayIcon.addMouseListener(object : MouseAdapter() {
            override fun mouseClicked(e: MouseEvent) {
                if (e.button == MouseEvent.BUTTON1 && e.clickCount == 1) {
                    host.toggleMainWindowVisibility()
                }
            }
        })
    }

    private val textInfos: String
        get() {
            val filmList = Daten.getInstance().listeFilme
            return buildString {
                append("Filmliste erstellt: ")
                append(filmList.metaData.generationDateTimeAsString)
                append(" Uhr  \n")
                append("Anz. Filme: ")
                append(filmList.size)
                append('\n')
                append(infoTextDownloads)
            }
        }

    private val infoTextDownloads: String
        get() {
            val daten = Daten.getInstance()
            val info = daten.listeDownloads.starts
            return buildString {
                append("Downloads: ")
                append(info.total_starts)

                if (info.hasValues()) {
                    append("   [ ")
                    append(if (info.running == 1) "1 läuft" else "${info.running} laufen")

                    if (info.running > 0) {
                        append(" (")
                        append(daten.downloadInfos.bandwidthStr)
                        append(')')
                    }

                    append(if (info.initialized == 1) ", 1 wartet" else ", ${info.initialized} warten")

                    if (info.finished > 0) {
                        append(if (info.finished == 1) ", 1 fertig" else ", ${info.finished} fertig")
                    }

                    if (info.error > 0) {
                        append(if (info.error == 1) ", 1 fehlerhaft" else ", ${info.error} fehlerhaft")
                    }

                    append(" ]")
                }
            }
        }

    private fun addNotification(meldung: String) {
        val msg = NotificationMessage()
        msg.title = "Programminfos"
        msg.message = meldung
        msg.type = MessageType.INFO
        NotificationService.displayNotification(msg)
    }

    companion object {
        private val logger = LogManager.getLogger()

        private fun createUiScope() = CoroutineScope(SupervisorJob() + Dispatchers.Swing)
    }
}
