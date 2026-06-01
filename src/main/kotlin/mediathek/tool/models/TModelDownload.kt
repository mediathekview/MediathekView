package mediathek.tool.models

import mediathek.daten.DatenDownload
import mediathek.daten.DownloadColumns
import mediathek.tool.MVFilmSize
import mediathek.tool.datum.Datum
import javax.swing.table.DefaultTableModel

class TModelDownload : DefaultTableModel() {
    override fun getColumnClass(columnIndex: Int): Class<*> =
        when (columnIndex) {
            DownloadColumns.NR,
            DownloadColumns.FILM_NR,
            -> Int::class.javaObjectType

            DownloadColumns.DATE -> Datum::class.java
            DownloadColumns.SIZE -> MVFilmSize::class.java

            DownloadColumns.HIGH_QUALITY,
            DownloadColumns.SUBTITLE_AVAILABLE,
            DownloadColumns.SPOTLIGHT,
            DownloadColumns.INTERRUPTED,
            DownloadColumns.PROGRAM_RESTART,
            DownloadColumns.INFO_FILE,
            DownloadColumns.SUBTITLE,
            DownloadColumns.DOWNLOAD_MANAGER,
            DownloadColumns.DEFERRED,
            -> Boolean::class.javaObjectType

            DownloadColumns.REF -> DatenDownload::class.java
            else -> String::class.java
        }

    override fun isCellEditable(row: Int, column: Int): Boolean = false

    override fun getColumnName(column: Int): String =
        when (column) {
            DownloadColumns.NR -> "DL Nr"
            DownloadColumns.FILM_NR -> "Film Nr."
            DownloadColumns.ABO -> "Abo"
            DownloadColumns.SENDER -> "Sender"
            DownloadColumns.TOPIC -> "Thema"
            DownloadColumns.TITLE -> "Titel"
            DownloadColumns.BUTTON_START,
            DownloadColumns.BUTTON_DELETE,
            -> ""

            DownloadColumns.PROGRESS -> "Fortschritt"
            DownloadColumns.REMAINING_TIME -> "Restzeit"
            DownloadColumns.BANDWIDTH -> "Geschwindigkeit"
            DownloadColumns.SIZE -> "Größe [MB]"
            DownloadColumns.DATE -> "Datum"
            DownloadColumns.TIME -> "Zeit"
            DownloadColumns.DURATION -> "Dauer"
            DownloadColumns.HIGH_QUALITY -> "HQ"
            DownloadColumns.SUBTITLE_AVAILABLE -> "UT"
            DownloadColumns.INTERRUPTED -> "Pause"
            DownloadColumns.GEO -> "Geo"
            DownloadColumns.FILM_URL -> "URL Film"
            DownloadColumns.HISTORY_URL -> "URL History"
            DownloadColumns.URL -> "URL"
            DownloadColumns.RTMP_URL -> "URL RTMP"
            DownloadColumns.SUBTITLE_URL -> "URL Untertitel"
            DownloadColumns.PROGRAM_SET -> "Programmset"
            DownloadColumns.PROGRAM -> "Programm"
            DownloadColumns.PROGRAM_INVOCATION -> "Programmaufruf"
            DownloadColumns.PROGRAM_INVOCATION_ARRAY -> "Programmaufruf Array"
            DownloadColumns.PROGRAM_RESTART -> "Restart"
            DownloadColumns.TARGET_FILE_NAME -> "Dateiname"
            DownloadColumns.TARGET_PATH -> "Pfad"
            DownloadColumns.TARGET_PATH_FILE_NAME -> "Pfad-Dateiname"
            DownloadColumns.TYPE -> "Art"
            DownloadColumns.SOURCE -> "Quelle"
            DownloadColumns.DEFERRED -> "Zurückgestellt"
            DownloadColumns.INFO_FILE -> "Infodatei"
            DownloadColumns.SPOTLIGHT -> "Spotlight"
            DownloadColumns.SUBTITLE -> "Untertitel"
            DownloadColumns.DOWNLOAD_MANAGER -> "Remote DL"
            DownloadColumns.REF -> "Ref"
            else -> throw IndexOutOfBoundsException("UNKNOWN COLUMN NAME: $column")
        }

    override fun getColumnCount(): Int = DownloadColumns.COUNT

    override fun getValueAt(row: Int, column: Int): Any? {
        val download = dataVector[row][DownloadColumns.REF] as DatenDownload

        return when (column) {
            DownloadColumns.HIGH_QUALITY -> download.film?.isHighQuality == true
            DownloadColumns.SUBTITLE_AVAILABLE -> download.film?.hasSubtitle() == true
            DownloadColumns.SPOTLIGHT -> download.isSpotlight
            DownloadColumns.INTERRUPTED -> download.isInterrupted
            DownloadColumns.PROGRAM_RESTART -> download.isRestart
            DownloadColumns.INFO_FILE -> download.isInfoFile
            DownloadColumns.SUBTITLE -> download.isSubtitle
            DownloadColumns.DOWNLOAD_MANAGER -> download.isDownloadManager
            DownloadColumns.DEFERRED -> download.isDeferred
            DownloadColumns.TYPE -> download.art.legacyId.toString()
            DownloadColumns.SOURCE -> download.quelle.legacyId.toString()
            DownloadColumns.GEO -> download.geo

            DownloadColumns.ABO -> download.aboName
            DownloadColumns.SENDER -> download.sender
            DownloadColumns.TOPIC -> download.topic
            DownloadColumns.TITLE -> download.title
            DownloadColumns.TIME -> download.time
            DownloadColumns.DURATION -> download.duration
            DownloadColumns.FILM_URL -> download.filmUrl
            DownloadColumns.HISTORY_URL -> download.historyUrl
            DownloadColumns.URL -> download.downloadUrl
            DownloadColumns.RTMP_URL -> download.rtmpUrl
            DownloadColumns.SUBTITLE_URL -> download.subtitleUrl
            DownloadColumns.PROGRAM_SET -> download.programSetName
            DownloadColumns.PROGRAM -> download.programName
            DownloadColumns.PROGRAM_INVOCATION -> download.programInvocation
            DownloadColumns.PROGRAM_INVOCATION_ARRAY -> download.programInvocationArray
            DownloadColumns.TARGET_FILE_NAME -> download.targetFileName
            DownloadColumns.TARGET_PATH -> download.targetPath
            DownloadColumns.TARGET_PATH_FILE_NAME -> download.targetPathFileName

            else -> super.getValueAt(row, column)
        }
    }
}
