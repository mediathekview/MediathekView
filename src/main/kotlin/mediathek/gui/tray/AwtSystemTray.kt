package mediathek.gui.tray

import kotlinx.coroutines.*
import kotlinx.coroutines.swing.Swing
import mediathek.config.Konstanten
import mediathek.controller.starter.DownloadServices
import mediathek.filmlisten.FilmCatalog
import mediathek.gui.messages.TimerEvent
import mediathek.tool.GetIcon
import mediathek.tool.MessageBus
import mediathek.tool.notification.MessageType
import mediathek.tool.notification.NotificationMessage
import mediathek.tool.notification.NotificationPublisher
import mediathek.tool.tray.SharedSystemTrayIcon
import net.engio.mbassy.listener.Handler
import org.apache.logging.log4j.LogManager
import java.awt.*
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent

internal class AwtSystemTray private constructor(
    private val filmCatalog: FilmCatalog,
    private val downloads: DownloadServices,
    private val notificationPublisher: NotificationPublisher,
    private val onToggleMainWindow: () -> Unit,
    private val onQuitApplication: () -> Unit,
) : SystemTraySession {
    private val uiScope = CoroutineScope(SupervisorJob() + Dispatchers.Swing)
    private val mouseListener = object : MouseAdapter() {
        override fun mouseClicked(event: MouseEvent) {
            if (event.button == MouseEvent.BUTTON1 && event.clickCount == 1) {
                onToggleMainWindow()
            }
        }
    }
    private var trayIcon: TrayIcon? = null
    private var iconState = TrayIconState.IDLE
    private var timerTicks = 0
    private var installed = false
    private var subscribed = false
    private var closed = false

    @Handler
    @Suppress("UNUSED_PARAMETER")
    private fun handleTimerEvent(msg: TimerEvent) {
        uiScope.launch {
            if (closed) {
                return@launch
            }
            val currentTrayIcon = trayIcon ?: return@launch

            timerTicks++
            val refreshTooltip = timerTicks >= TOOLTIP_REFRESH_INTERVAL_TICKS
            val snapshot = downloadSnapshot(includeBandwidth = refreshTooltip)
            if (refreshTooltip) {
                currentTrayIcon.toolTip = TrayPresentation.downloadText(snapshot)
                timerTicks = 0
            }
            updateIcon(currentTrayIcon, snapshot.iconState)
        }
    }

    override fun close() {
        if (closed) {
            return
        }
        closed = true

        if (subscribed) {
            MessageBus.messageBus.unsubscribe(this)
            subscribed = false
        }
        uiScope.cancel()

        trayIcon?.let { currentTrayIcon ->
            currentTrayIcon.removeMouseListener(mouseListener)
            currentTrayIcon.popupMenu = null
            currentTrayIcon.image = Konstanten.ICON_TRAY
            currentTrayIcon.toolTip = "MediathekView ${Konstanten.MVVERSION}"
        }
        trayIcon = null
        if (installed) {
            SharedSystemTrayIcon.release(this)
            installed = false
        }
    }

    private fun install(): Boolean = try {
        val icon = SharedSystemTrayIcon.acquire(this)
        trayIcon = icon
        installed = true
        icon.isImageAutoSize = true
        icon.toolTip = TrayPresentation.downloadText(downloadSnapshot(includeBandwidth = true))
        icon.popupMenu = createPopupMenu()
        icon.addMouseListener(mouseListener)
        MessageBus.messageBus.subscribe(this)
        subscribed = true
        true
    } catch (exception: AWTException) {
        logger.error("Tray konnte nicht geladen werden", exception)
        false
    }

    private fun createPopupMenu(): PopupMenu = PopupMenu().apply {
        add(MenuItem("Infos anzeigen").apply {
            addActionListener { showInformation() }
        })
        addSeparator()
        add(MenuItem("Programm beenden").apply {
            addActionListener { onQuitApplication() }
        })
    }

    private fun updateIcon(trayIcon: TrayIcon, newState: TrayIconState) {
        if (newState == iconState) {
            return
        }

        iconState = newState
        trayIcon.image = when (newState) {
            TrayIconState.IDLE -> Konstanten.ICON_TRAY
            TrayIconState.DOWNLOADING -> GetIcon.getProgramIcon("tray-download.png", 256, 256).image
            TrayIconState.ERROR -> GetIcon.getProgramIcon("tray-fehler.png", 256, 256).image
        }
    }

    private fun showInformation() {
        val filmList = filmCatalog.allFilms
        val text = TrayPresentation.informationText(
            filmListCreationTime = filmList.metaData.generationDateTimeAsString,
            filmCount = filmList.size,
            downloads = downloadSnapshot(includeBandwidth = true),
        )
        notificationPublisher.publish(NotificationMessage("Programminfos", text, MessageType.INFO))
    }

    private fun downloadSnapshot(includeBandwidth: Boolean): TrayDownloadSnapshot {
        val info = downloads.startInfo()
        return TrayDownloadSnapshot(
            totalStarts = info.totalStarts,
            initialized = info.initialized,
            running = info.running,
            finished = info.finished,
            error = info.error,
            bandwidthText = if (includeBandwidth && info.running > 0) {
                downloads.progressSnapshot().bandwidthText
            } else {
                ""
            },
            hasValues = info.hasValues(),
        )
    }

    companion object {
        private const val TOOLTIP_REFRESH_INTERVAL_TICKS = 4
        private val logger = LogManager.getLogger()

        fun create(
            filmCatalog: FilmCatalog,
            downloads: DownloadServices,
            notificationPublisher: NotificationPublisher,
            onToggleMainWindow: () -> Unit,
            onQuitApplication: () -> Unit,
        ): SystemTraySession? {
            if (!SystemTray.isSupported()) {
                logger.info("Tray wird nicht unterstützt")
                return null
            }

            val tray = AwtSystemTray(
                filmCatalog,
                downloads,
                notificationPublisher,
                onToggleMainWindow,
                onQuitApplication,
            )
            return if (tray.install()) {
                tray
            } else {
                tray.close()
                null
            }
        }
    }
}
