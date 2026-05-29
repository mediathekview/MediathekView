package mediathek.tool.cellrenderer

import com.formdev.flatlaf.extras.FlatSVGIcon
import mediathek.config.MVColor
import mediathek.controller.starter.DownloadProgressText
import mediathek.controller.starter.DownloadRunState
import mediathek.controller.starter.StartStatus
import mediathek.daten.DatenDownload
import mediathek.swing.IconUtils
import mediathek.tool.SVGIconUtilities
import mediathek.tool.table.MVTable
import org.apache.logging.log4j.LogManager
import org.kordamp.ikonli.fontawesome6.FontAwesomeRegular
import org.kordamp.ikonli.fontawesome6.FontAwesomeSolid
import org.kordamp.ikonli.swing.FontIcon
import java.awt.BorderLayout
import java.awt.Color
import java.awt.Component
import javax.swing.*
import javax.swing.border.Border

class CellRendererDownloads : CellRendererBaseWithStart() {
    private val filmStartIcons = rendererIconPair(
        normal = IconUtils.of(FontAwesomeSolid.PLAY),
        selected = FontIcon.of(FontAwesomeSolid.PLAY, IconUtils.DEFAULT_SIZE, Color.WHITE),
    )
    private val emptyBorder: Border = BorderFactory.createEmptyBorder(3, 2, 3, 2)
    private val largeBorder: Border = BorderFactory.createEmptyBorder(9, 2, 9, 2)
    private val progressBar = JProgressBar(0, 1000)
    private val panel = JPanel(BorderLayout()).apply {
        add(progressBar)
    }
    private val downloadStopIcons = rendererIconPair(
        normal = IconUtils.of(FontAwesomeSolid.STOP),
        selected = FontIcon.of(FontAwesomeSolid.STOP, IconUtils.DEFAULT_SIZE, Color.WHITE),
    )
    private val downloadStartIcons = rendererIconPair(
        normal = SVGIconUtilities.createSVGIcon("icons/fontawesome/caret-down.svg"),
        selected = SVGIconUtilities.createSVGIcon("icons/fontawesome/caret-down.svg").apply {
            colorFilter = FlatSVGIcon.ColorFilter { Color.WHITE }
        },
    )
    private val downloadClearIcons = rendererIconPair(
        normal = IconUtils.of(FontAwesomeSolid.ERASER),
        selected = FontIcon.of(FontAwesomeSolid.ERASER, IconUtils.DEFAULT_SIZE, Color.WHITE),
    )
    private val downloadDeleteIcons = rendererIconPair(
        normal = IconUtils.of(FontAwesomeRegular.TRASH_ALT),
        selected = FontIcon.of(FontAwesomeRegular.TRASH_ALT, IconUtils.DEFAULT_SIZE, Color.WHITE),
    )

    private fun applyHorizontalAlignment(colIndex: Int) {
        when (colIndex) {
            DatenDownload.DOWNLOAD_PROGRESS,
            DatenDownload.DOWNLOAD_FILM_NR,
            DatenDownload.DOWNLOAD_NR,
            DatenDownload.DOWNLOAD_DATUM,
            DatenDownload.DOWNLOAD_ZEIT,
            DatenDownload.DOWNLOAD_DAUER,
            DatenDownload.DOWNLOAD_BANDBREITE,
            DatenDownload.DOWNLOAD_RESTZEIT,
                -> horizontalAlignment = CENTER

            DatenDownload.DOWNLOAD_GROESSE -> horizontalAlignment = RIGHT
        }
    }

    private fun setBackgroundColor(c: Component, s: DownloadRunState?, isSelected: Boolean) {
        if (s != null) {
            val color = when (s.status) {
                StartStatus.INITIALIZED -> if (isSelected) MVColor.DOWNLOAD_WAIT_SEL.color else MVColor.DOWNLOAD_WAIT.color
                StartStatus.RUNNING -> if (isSelected) MVColor.DOWNLOAD_RUN_SEL.color else MVColor.DOWNLOAD_RUN.color
                StartStatus.FINISHED -> if (isSelected) MVColor.DOWNLOAD_FERTIG_SEL.color else MVColor.DOWNLOAD_FERTIG.color
                StartStatus.ERROR -> if (isSelected) MVColor.DOWNLOAD_FEHLER_SEL.color else MVColor.DOWNLOAD_FEHLER.color
            }
            c.background = color
        }
    }

    override fun getTableCellRendererComponent(
        table: JTable,
        value: Any?,
        isSelected: Boolean,
        hasFocus: Boolean,
        row: Int,
        column: Int,
    ): Component {
        try {
            resetComponent()
            super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column)

            val rowModelIndex = table.convertRowIndexToModel(row)
            val columnModelIndex = table.convertColumnIndexToModel(column)
            val datenDownload = table.model.getValueAt(rowModelIndex, DatenDownload.DOWNLOAD_REF) as DatenDownload
            val mvTable = table as MVTable

            if (mvTable.isLineBreak()) {
                horizontalAlignment = LEFT
                verticalAlignment = TOP

                when (columnModelIndex) {
                    DatenDownload.DOWNLOAD_TITEL,
                    DatenDownload.DOWNLOAD_THEMA,
                    DatenDownload.DOWNLOAD_URL,
                    DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF,
                    DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY,
                    DatenDownload.DOWNLOAD_FILM_URL,
                    DatenDownload.DOWNLOAD_URL_SUBTITLE,
                    DatenDownload.DOWNLOAD_ZIEL_DATEINAME,
                    DatenDownload.DOWNLOAD_ZIEL_PFAD,
                    DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME,
                    DatenDownload.DOWNLOAD_ABO,
                        -> return createTextArea(valueText(value), datenDownload, columnModelIndex, isSelected)
                }
            } else {
                applyHorizontalAlignment(columnModelIndex)
            }

            when (columnModelIndex) {
                DatenDownload.DOWNLOAD_PROGRESS -> renderProgressColumn(datenDownload, mvTable, isSelected)?.let { return it }
                DatenDownload.DOWNLOAD_FILM_NR -> hideZeroFilmNumber(table, rowModelIndex)
                DatenDownload.DOWNLOAD_ART -> renderDownloadType(datenDownload)
                DatenDownload.DOWNLOAD_QUELLE -> renderDownloadSource(datenDownload)
                DatenDownload.DOWNLOAD_BUTTON_START -> handleButtonStartColumn(datenDownload, isSelected)
                DatenDownload.DOWNLOAD_BUTTON_DEL -> handleButtonDeleteColumn(datenDownload, isSelected)
                DatenDownload.DOWNLOAD_ABO -> handleAboColumn(datenDownload)
                DatenDownload.DOWNLOAD_SENDER -> {
                    if (mvTable.showSenderIcons()) {
                        val targetDim = getSenderCellDimension(table, row, column)
                        setSenderIcon(valueText(value), targetDim, isSelected)
                    }
                }

                DatenDownload.DOWNLOAD_GEO -> drawGeolocationIcons(datenDownload.film, isSelected)
            }

            if (columnModelIndex == DatenDownload.DOWNLOAD_TITEL) {
                datenDownload.film?.let { film ->
                    setIndicatorIcons(table, film, isSelected)
                }
            }

            setBackgroundColor(this, datenDownload.start, isSelected)
        } catch (ex: Exception) {
            logger.error(ex)
        }
        return this
    }

    private fun renderProgressColumn(datenDownload: DatenDownload, mvTable: MVTable, isSelected: Boolean): Component? {
        progressBar.border = if (mvTable.showSenderIcons() && !mvTable.getUseSmallSenderIcons()) {
            largeBorder
        } else {
            emptyBorder
        }

        val start = datenDownload.start
        if (start == null) {
            text = ""
            return null
        }

        if (1 < start.percent && start.percent < DownloadRunState.PROGRESS_FERTIG) {
            setBackgroundColor(panel, start, isSelected)
            setBackgroundColor(progressBar, start, isSelected)

            progressBar.value = start.percent
            val progressValue = start.percent / 10.0
            progressBar.string = "$progressValue%"

            return panel
        }

        text = DownloadProgressText.getTextProgress(datenDownload.isDownloadManager, start)
        return null
    }

    private fun hideZeroFilmNumber(table: JTable, rowModelIndex: Int) {
        if (table.model.getValueAt(rowModelIndex, DatenDownload.DOWNLOAD_FILM_NR) as Int == 0) {
            text = ""
        }
    }

    private fun renderDownloadType(datenDownload: DatenDownload) {
        text = datenDownload.art.label
    }

    private fun renderDownloadSource(datenDownload: DatenDownload) {
        text = datenDownload.quelle.label
    }

    private fun createTextArea(
        value: String,
        datenDownload: DatenDownload,
        columnModelIndex: Int,
        isSelected: Boolean,
    ): JTextArea {
        val textArea = createWrappedTextArea(value)
        if (columnModelIndex == DatenDownload.DOWNLOAD_ABO) {
            handleAboColumn(textArea, datenDownload)
        }
        setBackgroundColor(textArea, datenDownload.start, isSelected)
        return textArea
    }

    private fun setIconsAndToolTips(datenDownload: DatenDownload, isSelected: Boolean) {
        val start = datenDownload.start
        if (start != null && !datenDownload.isDownloadManager) {
            when (start.status) {
                StartStatus.FINISHED -> {
                    icon = filmStartIcons.icon(isSelected)
                    toolTipText = PLAY_DOWNLOADED_FILM
                }

                StartStatus.ERROR -> {
                    icon = downloadStartIcons.icon(isSelected)
                    toolTipText = DOWNLOAD_STARTEN
                }

                else -> {
                    icon = downloadStopIcons.icon(isSelected)
                    toolTipText = DOWNLOAD_STOPPEN
                }
            }
        } else {
            icon = downloadStartIcons.icon(isSelected)
            toolTipText = DOWNLOAD_STARTEN
        }
    }

    private fun handleButtonStartColumn(datenDownload: DatenDownload, isSelected: Boolean) {
        horizontalAlignment = CENTER
        setIconsAndToolTips(datenDownload, isSelected)
    }

    private fun handleAboColumn(a: JTextArea, datenDownload: DatenDownload) {
        if (datenDownload.arr[DatenDownload.DOWNLOAD_ABO].isNotEmpty()) {
            a.foreground = MVColor.DOWNLOAD_IST_ABO.color
        } else {
            a.foreground = MVColor.DOWNLOAD_IST_DIREKTER_DOWNLOAD.color
            a.text = "Download"
        }
    }

    private fun handleAboColumn(datenDownload: DatenDownload) {
        horizontalAlignment = CENTER
        if (datenDownload.arr[DatenDownload.DOWNLOAD_ABO].isNotEmpty()) {
            foreground = MVColor.DOWNLOAD_IST_ABO.color
        } else {
            foreground = MVColor.DOWNLOAD_IST_DIREKTER_DOWNLOAD.color
            text = "Download"
        }
    }

    private fun handleButtonDeleteColumn(datenDownload: DatenDownload, isSelected: Boolean) {
        horizontalAlignment = CENTER
        val start = datenDownload.start
        if (start != null) {
            if (start.status >= StartStatus.FINISHED) {
                setIcon(downloadClearIcons, DOWNLOAD_ENTFERNEN, isSelected)
            } else {
                setupDownloadLoeschen(isSelected)
            }
        } else {
            setupDownloadLoeschen(isSelected)
        }
    }

    private fun setIcon(icons: RendererIconPair, tooltip: String, isSelected: Boolean) {
        icon = icons.icon(isSelected)
        toolTipText = tooltip
    }

    private fun setupDownloadLoeschen(isSelected: Boolean) {
        setIcon(downloadDeleteIcons, DOWNLOAD_LOESCHEN, isSelected)
    }

    private companion object {
        private const val DOWNLOAD_STARTEN = "Download starten"
        private const val DOWNLOAD_LOESCHEN = "Download aus Liste entfernen"
        private const val DOWNLOAD_STOPPEN = "Download stoppen"
        private const val DOWNLOAD_ENTFERNEN = "Download entfernen"
        private const val PLAY_DOWNLOADED_FILM = "gespeicherten Film abspielen"
        private val logger = LogManager.getLogger(CellRendererDownloads::class.java)
    }
}
