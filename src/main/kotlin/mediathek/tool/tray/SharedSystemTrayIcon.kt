package mediathek.tool.tray

import mediathek.config.Konstanten
import java.awt.SystemTray
import java.awt.TrayIcon
import java.util.*

/**
 * Shares MediathekView's single AWT tray icon between independent application features.
 */
internal object SharedSystemTrayIcon {
    private val lifecycleLock = Any()
    private val owners = Collections.newSetFromMap(IdentityHashMap<Any, Boolean>())
    private var trayIcon: TrayIcon? = null

    fun acquire(owner: Any): TrayIcon = synchronized(lifecycleLock) {
        check(SystemTray.isSupported()) { "System Tray is not supported" }

        val icon = trayIcon ?: TrayIcon(
            Konstanten.ICON_TRAY,
            "MediathekView ${Konstanten.MVVERSION}",
        ).also { newIcon ->
            newIcon.isImageAutoSize = true
            SystemTray.getSystemTray().add(newIcon)
            trayIcon = newIcon
        }
        owners.add(owner)
        icon
    }

    fun release(owner: Any) {
        synchronized(lifecycleLock) {
            if (!owners.remove(owner) || owners.isNotEmpty()) {
                return
            }

            trayIcon?.let(SystemTray.getSystemTray()::remove)
            trayIcon = null
        }
    }
}
