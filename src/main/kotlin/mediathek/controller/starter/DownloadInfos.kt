package mediathek.controller.starter

import mediathek.daten.DatenDownload
import mediathek.daten.DownloadSource
import mediathek.gui.messages.DownloadInfoUpdateAvailableEvent
import mediathek.gui.messages.TimerEvent
import mediathek.tool.BandwidthFormatter
import mediathek.tool.MessageBus
import net.engio.mbassy.listener.Handler

class DownloadInfos(
    private val unfinishedDownloadsProvider: (DownloadSource) -> List<DatenDownload>,
) {
    /**
     * Bandbreite: bytes per second
     */
    private var bandwidth: Long = 0

    /**
     * Restzeit aller gestarteten Downloads
     */
    private var timeRestAllDownloads: Long = 0

    /**
     * Restzeit fur die gerade ladenden/laufenden Downloads
     */
    private var timeRestAktDownloads: Long = 0

    /**
     * Anzahl Bytes bereits geladen fur die gerade ladenden/laufenden Downloads
     */
    var byteAktDownloads: Long = 0
        private set

    /**
     * Anzahl Bytes fur alle gestarteten Downloads
     */
    var byteAlleDownloads: Long = 0
        private set

    /**
     * Anzahl gestarteter Downloads
     */
    private var anzDownloadsRun: Int = 0

    var bandwidthStr: String = ""
        private set

    init {
        MessageBus.messageBus.subscribe(this)
    }

    @Suppress("UNUSED_PARAMETER")
    @Handler
    private fun handleTimerEvent(event: TimerEvent) {
        makeDownloadInfos()
    }

    private fun makeDownloadInfos() {
        resetData()

        val activeDownloads = unfinishedDownloadsProvider(DownloadSource.ALL)

        for (download in activeDownloads) {
            anzDownloadsRun++
            val start = download.runtime.runState
            if (start != null && start.status == StartStatus.RUNNING) {
                // die Downloads laufen gerade
                bandwidth += start.bandbreite // bytes per second
                byteAktDownloads += download.runtime.filmSize.aktSize.coerceAtLeast(0)
                byteAlleDownloads += download.runtime.filmSize.size.coerceAtLeast(0)
                if (start.restSekunden > timeRestAktDownloads) {
                    // der laengste gibt die aktuelle Restzeit vor
                    timeRestAktDownloads = start.restSekunden
                }
            }
        }

        bandwidth = bandwidth.coerceAtLeast(0)

        if (bandwidth > 0) {
            // sonst macht die Restzeit keinen Sinn
            val remainingBytes = byteAlleDownloads - byteAktDownloads
            timeRestAllDownloads = if (remainingBytes <= 0) {
                0
            } else {
                remainingBytes / bandwidth
            }
            if (timeRestAllDownloads < timeRestAktDownloads) {
                timeRestAllDownloads = timeRestAktDownloads // falsch geraten oder es gibt nur einen
            }

            if (anzDownloadsRun == 1) {
                timeRestAllDownloads = 0 // gibt ja nur noch einen
            }
        }

        bandwidthStr = BandwidthFormatter.format(bandwidth)

        // TODO put status values in Info Event message
        MessageBus.messageBus.publishAsync(DownloadInfoUpdateAvailableEvent())
    }

    private fun resetData() {
        anzDownloadsRun = 0
        byteAlleDownloads = 0
        byteAktDownloads = 0
        timeRestAktDownloads = 0
        timeRestAllDownloads = 0
        bandwidth = 0
    }
}
