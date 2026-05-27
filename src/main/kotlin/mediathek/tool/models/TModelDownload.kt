package mediathek.tool.models

import mediathek.daten.DatenDownload
import mediathek.tool.MVFilmSize
import mediathek.tool.datum.Datum
import javax.swing.table.DefaultTableModel

class TModelDownload : DefaultTableModel() {
    override fun getColumnClass(columnIndex: Int): Class<*> =
        when (columnIndex) {
            DatenDownload.DOWNLOAD_NR,
            DatenDownload.DOWNLOAD_FILM_NR,
            -> Int::class.javaObjectType

            DatenDownload.DOWNLOAD_DATUM -> Datum::class.java
            DatenDownload.DOWNLOAD_GROESSE -> MVFilmSize::class.java

            DatenDownload.DOWNLOAD_HD,
            DatenDownload.DOWNLOAD_UT,
            DatenDownload.DOWNLOAD_SPOTLIGHT,
            DatenDownload.DOWNLOAD_UNTERBROCHEN,
            DatenDownload.DOWNLOAD_PROGRAMM_RESTART,
            DatenDownload.DOWNLOAD_INFODATEI,
            DatenDownload.DOWNLOAD_SUBTITLE,
            DatenDownload.DOWNLOAD_PROGRAMM_DOWNLOADMANAGER,
            DatenDownload.DOWNLOAD_ZURUECKGESTELLT,
            -> Boolean::class.javaObjectType

            DatenDownload.DOWNLOAD_REF -> DatenDownload::class.java
            else -> String::class.java
        }

    override fun isCellEditable(row: Int, column: Int): Boolean = false

    override fun getColumnName(column: Int): String =
        when (column) {
            DatenDownload.DOWNLOAD_NR -> "DL Nr"
            DatenDownload.DOWNLOAD_FILM_NR -> "Film Nr."
            DatenDownload.DOWNLOAD_ABO -> "Abo"
            DatenDownload.DOWNLOAD_SENDER -> "Sender"
            DatenDownload.DOWNLOAD_THEMA -> "Thema"
            DatenDownload.DOWNLOAD_TITEL -> "Titel"
            DatenDownload.DOWNLOAD_BUTTON_START,
            DatenDownload.DOWNLOAD_BUTTON_DEL,
            -> ""

            DatenDownload.DOWNLOAD_PROGRESS -> "Fortschritt"
            DatenDownload.DOWNLOAD_RESTZEIT -> "Restzeit"
            DatenDownload.DOWNLOAD_BANDBREITE -> "Geschwindigkeit"
            DatenDownload.DOWNLOAD_GROESSE -> "Größe [MB]"
            DatenDownload.DOWNLOAD_DATUM -> "Datum"
            DatenDownload.DOWNLOAD_ZEIT -> "Zeit"
            DatenDownload.DOWNLOAD_DAUER -> "Dauer"
            DatenDownload.DOWNLOAD_HD -> "HQ"
            DatenDownload.DOWNLOAD_UT -> "UT"
            DatenDownload.DOWNLOAD_UNTERBROCHEN -> "Pause"
            DatenDownload.DOWNLOAD_GEO -> "Geo"
            DatenDownload.DOWNLOAD_FILM_URL -> "URL Film"
            DatenDownload.DOWNLOAD_HISTORY_URL -> "URL History"
            DatenDownload.DOWNLOAD_URL -> "URL"
            DatenDownload.DOWNLOAD_URL_RTMP -> "URL RTMP"
            DatenDownload.DOWNLOAD_URL_SUBTITLE -> "URL Untertitel"
            DatenDownload.DOWNLOAD_PROGRAMMSET -> "Programmset"
            DatenDownload.DOWNLOAD_PROGRAMM -> "Programm"
            DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF -> "Programmaufruf"
            DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY -> "Programmaufruf Array"
            DatenDownload.DOWNLOAD_PROGRAMM_RESTART -> "Restart"
            DatenDownload.DOWNLOAD_ZIEL_DATEINAME -> "Dateiname"
            DatenDownload.DOWNLOAD_ZIEL_PFAD -> "Pfad"
            DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME -> "Pfad-Dateiname"
            DatenDownload.DOWNLOAD_ART -> "Art"
            DatenDownload.DOWNLOAD_QUELLE -> "Quelle"
            DatenDownload.DOWNLOAD_ZURUECKGESTELLT -> "Zurückgestellt"
            DatenDownload.DOWNLOAD_INFODATEI -> "Infodatei"
            DatenDownload.DOWNLOAD_SPOTLIGHT -> "Spotlight"
            DatenDownload.DOWNLOAD_SUBTITLE -> "Untertitel"
            DatenDownload.DOWNLOAD_PROGRAMM_DOWNLOADMANAGER -> "Remote DL"
            DatenDownload.DOWNLOAD_REF -> "Ref"
            else -> throw IndexOutOfBoundsException("UNKNOWN COLUMN NAME: $column")
        }

    override fun getColumnCount(): Int = DatenDownload.MAX_ELEM

    override fun getValueAt(row: Int, column: Int): Any? {
        val download = dataVector[row][DatenDownload.DOWNLOAD_REF] as DatenDownload

        return when (column) {
            DatenDownload.DOWNLOAD_HD -> download.film != null && download.film.isHighQuality
            DatenDownload.DOWNLOAD_UT -> download.film != null && download.film.hasSubtitle()
            DatenDownload.DOWNLOAD_SPOTLIGHT -> download.isSpotlight
            DatenDownload.DOWNLOAD_UNTERBROCHEN -> download.isInterrupted
            DatenDownload.DOWNLOAD_PROGRAMM_RESTART -> download.isRestart
            DatenDownload.DOWNLOAD_INFODATEI -> download.isInfoFile
            DatenDownload.DOWNLOAD_SUBTITLE -> download.isSubtitle
            DatenDownload.DOWNLOAD_PROGRAMM_DOWNLOADMANAGER -> download.isDownloadManager
            DatenDownload.DOWNLOAD_ZURUECKGESTELLT -> download.istZurueckgestellt()
            DatenDownload.DOWNLOAD_ABO,
            DatenDownload.DOWNLOAD_SENDER,
            DatenDownload.DOWNLOAD_THEMA,
            DatenDownload.DOWNLOAD_TITEL,
            DatenDownload.DOWNLOAD_ZEIT,
            DatenDownload.DOWNLOAD_DAUER,
            DatenDownload.DOWNLOAD_GEO,
            DatenDownload.DOWNLOAD_FILM_URL,
            DatenDownload.DOWNLOAD_HISTORY_URL,
            DatenDownload.DOWNLOAD_URL,
            DatenDownload.DOWNLOAD_URL_RTMP,
            DatenDownload.DOWNLOAD_URL_SUBTITLE,
            DatenDownload.DOWNLOAD_PROGRAMMSET,
            DatenDownload.DOWNLOAD_PROGRAMM,
            DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF,
            DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY,
            DatenDownload.DOWNLOAD_ZIEL_DATEINAME,
            DatenDownload.DOWNLOAD_ZIEL_PFAD,
            DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME,
            DatenDownload.DOWNLOAD_ART,
            DatenDownload.DOWNLOAD_QUELLE,
            -> download.arr[column]

            else -> super.getValueAt(row, column)
        }
    }
}
