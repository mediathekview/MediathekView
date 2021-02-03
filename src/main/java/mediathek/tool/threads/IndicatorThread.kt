package mediathek.tool.threads

import mediathek.config.Daten
import mediathek.controller.starter.Start
import mediathek.daten.DatenDownload

/**
 * Base class for platform-specific progress indicator threads
 */
open class IndicatorThread : Thread() {
    protected val daten: Daten
    protected fun calculateOverallPercentage(): Double {
        var numOfDownloadsActive = 0
        var accumPercentage = 0.0
        //only count running/active downloads and calc accumulated progress..
        val activeDownloadList = daten.listeDownloads.getListOfStartsNotFinished(DatenDownload.QUELLE_ALLE.toInt())
        for (download in activeDownloadList) {
            if (download.start.status == Start.STATUS_RUN) {
                numOfDownloadsActive++
                accumPercentage += download.start.percent / 10.0
            }
        }
        activeDownloadList.clear()
        return accumPercentage / numOfDownloadsActive
    }

    init {
        name = "IndicatorThread"
        daten = Daten.getInstance()
    }
}