/*
 * Copyright (c) 2026 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.gui.dialog.edit_download

import com.github.kokorin.jaffree.process.JaffreeAbnormalExitException
import kotlinx.coroutines.*
import kotlinx.coroutines.swing.Swing
import mediathek.config.Konstanten
import mediathek.config.application.ApplicationConfiguration
import mediathek.controller.DownloadColumn
import mediathek.controller.starter.DownloadProgressText
import mediathek.daten.DatenDownload
import mediathek.daten.DatenProg
import mediathek.daten.DownloadType
import mediathek.daten.FilmResolution
import mediathek.gui.dialog.HelpTextDialog
import mediathek.gui.dialog.MVPanelDownloadZiel
import mediathek.gui.dialog.download.DownloadQualityLiveInfoText
import mediathek.gui.dialog.download.DownloadQualityResolutionSizes
import mediathek.gui.dialog.download.DownloadQualitySupport
import mediathek.swing.IconUtils
import mediathek.swing.MultilineLabel
import mediathek.tool.*
import net.miginfocom.layout.AC
import net.miginfocom.layout.CC
import net.miginfocom.layout.LC
import net.miginfocom.swing.MigLayout
import org.apache.logging.log4j.LogManager
import org.kordamp.ikonli.fontawesome6.FontAwesomeSolid
import org.kordamp.ikonli.materialdesign2.MaterialDesignM
import java.awt.Color
import java.awt.Component
import java.awt.Dimension
import java.awt.Point
import java.awt.event.ComponentAdapter
import java.awt.event.ComponentEvent
import java.io.File
import java.nio.file.Path
import javax.swing.*
import javax.swing.event.DocumentEvent
import javax.swing.event.DocumentListener

class DialogEditDownload(
    parent: JFrame,
    private val datenDownload: DatenDownload,
    private val gestartet: Boolean
) : DialogEditDownloadBase(parent) {
    private val uiScope = CoroutineScope(SupervisorJob() + Dispatchers.Swing)
    private val logger = LogManager.getLogger(javaClass)
    private val jCheckBoxRestart = JCheckBox()
    private val jCheckBoxInfodatei = JCheckBox()
    private val jCheckBoxSubtitle = JCheckBox()
    private val jCheckBoxSpotlight = JCheckBox()
    private val mVPanelDownloadZiel = MVPanelDownloadZiel(parent, datenDownload, false)
    private val orgProgArray = datenDownload.programInvocationArray
    private val cbHighQuality = JCheckBox().apply { isEnabled = false }
    private val cbSubtitleAvailable = JCheckBox().apply { isEnabled = false }

    private var confirmed = false
    private var resolutionLabelHigh = ""
    private var resolutionLabelNormal = ""
    private var resolutionLabelLow = ""
    private var dateiGroesseHD = ""
    private var dateiGroesseHoch = ""
    private var dateiGroesseKlein = ""
    private var resolution = FilmResolution.Enum.NORMAL
    private var ffprobePath: Path? = null
    private var resolutionSizesJob: Job? = null
    private var liveInfoJob: Job? = null
    private var urlField: JTextField? = null
    private var programmAufrufField: JTextField? = null
    private var programmAufrufArrayField: JTextField? = null
    private var btnQuerCodecDetailsForLocalUrl: JButton? = null

    init {
        mVPanelDownloadZiel.border = BorderFactory.createLineBorder(Color(204, 204, 204))

        setupButtonBar()
        setupQualityPanel()
        setupResolutionButtons()
        setupResolutionButtonListeners()
        EscapeKeyHandler.installHandler(this) { dispose() }
        buildLayout()
        restoreLocation()
        setupComponentListeners()
        loadQualityPanelData()
    }

    override fun dispose() {
        uiScope.cancel()
        super.dispose()
    }

    private fun setupButtonBar() {
        jButtonOk.addActionListener {
            if (check()) {
                dispose()
            }
        }
        jButtonAbbrechen.addActionListener { dispose() }
        rootPane.defaultButton = jButtonOk
    }

    private fun setupQualityPanel() {
        resolutionLabelHigh = jRadioButtonResHd.text
        resolutionLabelNormal = jRadioButtonResHi.text
        resolutionLabelLow = jRadioButtonResLo.text
        setupBusyIndicator()
        stabilizeLiveInfoArea()
        detectFfprobeExecutable()
        jButtonRequestLiveInfo.addActionListener { requestLiveInfo() }
        mVPanelDownloadZiel.addPathDocumentListener(object : DocumentListener {
            override fun insertUpdate(e: DocumentEvent) = updateDiskSpaceIndicatorsAsync()
            override fun removeUpdate(e: DocumentEvent) = updateDiskSpaceIndicatorsAsync()
            override fun changedUpdate(e: DocumentEvent) = updateDiskSpaceIndicatorsAsync()
        })
    }

    private fun setupBusyIndicator() {
        jLabelBusyIndicator.text = ""
        jLabelBusyIndicator.isBusy = false
        jLabelBusyIndicator.isVisible = false
        showLiveInfo(DownloadQualityLiveInfoText())
    }

    private fun stabilizeLiveInfoArea() {
        val originalText = DownloadQualityLiveInfoText(jLabelVideoInfo.text, jLabelAudioInfo.text)
        showLiveInfo(LIVE_INFO_PLACEHOLDER)
        jLabelVideoInfo.minimumSize = jLabelVideoInfo.preferredSize
        jLabelAudioInfo.minimumSize = jLabelAudioInfo.preferredSize
        showLiveInfo(originalText)
    }

    private fun detectFfprobeExecutable() {
        ffprobePath = DownloadQualitySupport.findFfprobeExecutableDirectory()
        if (ffprobePath != null) {
            return
        }

        jLabelBusyIndicator.isVisible = true
        jLabelBusyIndicator.isBusy = false
        jLabelBusyIndicator.text = "Hilfsprogramm nicht gefunden!"
        jLabelBusyIndicator.foreground = Color.RED
        jButtonRequestLiveInfo.isEnabled = false
    }

    private fun loadQualityPanelData() {
        loadResolutionSizesAsync()
        updateDiskSpaceIndicatorsAsync()
    }

    private fun setupResolutionButtons() {
        disableResolutionButtons()
        if (datenDownload.art != DownloadType.DIRECT && datenDownload.pSet == null) {
            jPanelRes.isVisible = false
            return
        }
        if (datenDownload.film != null) {
            configureResolutionButton(jRadioButtonResHi, FilmResolution.Enum.NORMAL)
            configureResolutionButton(jRadioButtonResHd, FilmResolution.Enum.HIGH_QUALITY)
            configureResolutionButton(jRadioButtonResLo, FilmResolution.Enum.LOW)
        }
        resolution = selectedResolution()
    }

    private fun disableResolutionButtons() {
        jRadioButtonResHd.isEnabled = false
        jRadioButtonResHi.isEnabled = false
        jRadioButtonResLo.isEnabled = false
    }

    private fun configureResolutionButton(button: JRadioButton, filmResolution: FilmResolution.Enum) {
        val film = datenDownload.film ?: return
        val url = film.getUrlFuerAufloesung(filmResolution)
        if (url.isEmpty()) {
            return
        }

        button.isEnabled = !gestartet
        button.isSelected = datenDownload.selectedResolution == filmResolution
    }

    private fun setupResolutionButtonListeners() {
        jRadioButtonResHd.addActionListener { handleResolutionChange() }
        jRadioButtonResHi.addActionListener { handleResolutionChange() }
        jRadioButtonResLo.addActionListener { handleResolutionChange() }
    }

    private fun handleResolutionChange() {
        changeRes()
        resetLiveInfoDisplay()
    }

    private fun selectedResolution(): FilmResolution.Enum = when {
        jRadioButtonResHd.isSelected -> FilmResolution.Enum.HIGH_QUALITY
        jRadioButtonResLo.isSelected -> FilmResolution.Enum.LOW
        else -> FilmResolution.Enum.NORMAL
    }

    private fun changeRes() {
        val film = datenDownload.film ?: return
        val selectedResolution = selectedResolution()
        datenDownload.selectedResolution = selectedResolution
        datenDownload.downloadUrl = film.getUrlFuerAufloesung(selectedResolution)
        urlField?.text = datenDownload.downloadUrl

        val size = when (selectedResolution) {
            FilmResolution.Enum.HIGH_QUALITY -> dateiGroesseHD
            FilmResolution.Enum.NORMAL -> dateiGroesseHoch
            FilmResolution.Enum.LOW -> dateiGroesseKlein
        }
        if (datenDownload.art == DownloadType.PROGRAM && datenDownload.pSet != null) {
            updateProgramCallFields(selectedResolution)
        }
        datenDownload.setGroesse(size)
    }

    private fun updateProgramCallFields(selectedResolution: FilmResolution.Enum) {
        val pSet = datenDownload.pSet ?: return
        val film = datenDownload.film ?: return
        val newDownload = DatenDownload(
            pSet,
            film,
            datenDownload.quelle,
            datenDownload.abo,
            datenDownload.targetFileName,
            datenDownload.targetPath,
            selectedResolution.toString()
        )

        datenDownload.programInvocation =
            newDownload.programInvocation
        datenDownload.programInvocationArray =
            newDownload.programInvocationArray
        programmAufrufField?.text = datenDownload.programInvocation
        programmAufrufArrayField?.text = datenDownload.programInvocationArray
    }

    private fun loadResolutionSizesAsync() {
        resolutionSizesJob?.cancel()
        resolutionSizesJob = uiScope.launch {
            val sizes = try {
                val film = datenDownload.film
                if (film == null) {
                    DownloadQualityResolutionSizes()
                } else {
                    withContext(Dispatchers.IO) { DownloadQualitySupport.loadResolutionSizes(film) }
                }
            } catch (ex: Exception) {
                logger.error("Error occurred while fetching file sizes", ex)
                DownloadQualityResolutionSizes()
            }

            applyResolutionSizes(sizes)
            updateDiskSpaceIndicators(currentPathText())
        }
    }

    private fun applyResolutionSizes(sizes: DownloadQualityResolutionSizes) {
        dateiGroesseHD = sizes.high
        dateiGroesseHoch = sizes.normal
        dateiGroesseKlein = sizes.low
        jRadioButtonResHd.text = DownloadQualitySupport.formatResolutionLabel(
            resolutionLabelHigh,
            if (jRadioButtonResHd.isEnabled) sizes.high else null
        )
        jRadioButtonResHi.text = DownloadQualitySupport.formatResolutionLabel(resolutionLabelNormal, sizes.normal)
        jRadioButtonResLo.text = DownloadQualitySupport.formatResolutionLabel(
            resolutionLabelLow,
            if (jRadioButtonResLo.isEnabled) sizes.low else null
        )
    }

    private fun requestLiveInfo() {
        val executablePath = ffprobePath ?: return
        val film = datenDownload.film ?: return

        val selectedResolution = selectedResolution()
        requestLiveInfo {
            DownloadQualitySupport.fetchLiveInfo(executablePath, film, selectedResolution)
        }
    }

    private fun requestLiveInfoForUrl(url: String) {
        val executablePath = ffprobePath ?: return
        if (url.isBlank()) {
            showLiveInfoErrorDialog("Keine URL vorhanden.")
            return
        }

        requestLiveInfoForUrlDialog(executablePath, url)
    }

    private fun requestLiveInfo(loadLiveInfo: () -> DownloadQualityLiveInfoText) {
        liveInfoJob?.cancel()
        setLiveInfoButtonsEnabled(false)
        jLabelBusyIndicator.isVisible = true
        jLabelBusyIndicator.isBusy = true
        showLiveInfo(DownloadQualityLiveInfoText())

        liveInfoJob = uiScope.launch {
            try {
                val liveInfo = runInterruptible(Dispatchers.IO) {
                    loadLiveInfo()
                }
                showLiveInfo(liveInfo)
            } catch (_: CancellationException) {
                showLiveInfo(DownloadQualityLiveInfoText())
            } catch (ex: JaffreeAbnormalExitException) {
                showLiveInfoError(DownloadQualitySupport.getLiveInfoErrorString(ex))
            } catch (_: Exception) {
                showLiveInfoError("Unbekannter Fehler aufgetreten.")
            } finally {
                resetBusyIndicator()
                setLiveInfoButtonsEnabled(ffprobePath != null)
            }
        }
    }

    private fun requestLiveInfoForUrlDialog(executablePath: Path, url: String) {
        liveInfoJob?.cancel()
        setLiveInfoButtonsEnabled(false)
        jLabelBusyIndicator.isVisible = true
        jLabelBusyIndicator.isBusy = true
        showLiveInfo(DownloadQualityLiveInfoText())

        liveInfoJob = uiScope.launch {
            try {
                val (liveInfo, fileSizeInMegabytes) = runInterruptible(Dispatchers.IO) {
                    DownloadQualitySupport.fetchLiveInfo(executablePath, url) to loadFileSizeInMegabytes(url)
                }
                resetBusyIndicator()
                showLiveInfoDialog(liveInfo, fileSizeInMegabytes)
            } catch (_: CancellationException) {
                showLiveInfo(DownloadQualityLiveInfoText())
            } catch (ex: JaffreeAbnormalExitException) {
                resetBusyIndicator()
                showLiveInfoErrorDialog(DownloadQualitySupport.getLiveInfoErrorString(ex))
            } catch (_: Exception) {
                resetBusyIndicator()
                showLiveInfoErrorDialog("Unbekannter Fehler aufgetreten.")
            } finally {
                resetBusyIndicator()
                setLiveInfoButtonsEnabled(ffprobePath != null)
            }
        }
    }

    private fun loadFileSizeInMegabytes(url: String): String {
        return runCatching {
            FileSize.getFileLengthFromUrl(url, true)
        }.onFailure {
            logger.error("Error occurred while fetching file size for URL", it)
        }.getOrDefault("")
    }

    private fun setLiveInfoButtonsEnabled(enabled: Boolean) {
        jButtonRequestLiveInfo.isEnabled = enabled
        btnQuerCodecDetailsForLocalUrl?.isEnabled = enabled
    }

    private fun resetBusyIndicator() {
        jLabelBusyIndicator.isVisible = false
        jLabelBusyIndicator.isBusy = false
    }

    private fun resetLiveInfoDisplay() {
        liveInfoJob?.cancel()
        resetBusyIndicator()
        showLiveInfo(DownloadQualityLiveInfoText())
        setLiveInfoButtonsEnabled(ffprobePath != null)
    }

    private fun showLiveInfo(liveInfoText: DownloadQualityLiveInfoText) {
        val labelForeground = UIManager.getColor(LABEL_FOREGROUND_KEY)
        jLabelVideoInfo.foreground = labelForeground
        jLabelAudioInfo.foreground = labelForeground
        jLabelVideoInfo.text = liveInfoText.video
        jLabelAudioInfo.text = liveInfoText.audio
    }

    private fun showLiveInfoError(message: String) {
        jLabelVideoInfo.foreground = Color.RED
        jLabelVideoInfo.text = message
        jLabelAudioInfo.foreground = UIManager.getColor(LABEL_FOREGROUND_KEY)
        jLabelAudioInfo.text = ""
    }

    private fun showLiveInfoDialog(liveInfoText: DownloadQualityLiveInfoText, fileSizeInMegabytes: String = "") {
        val message = buildString {
            append("<html>")
            append("<b>Video:</b> ")
            append(liveInfoText.video.removePrefix("Video:").trimStart())
            append("<br><b>Audio:</b> ")
            append(liveInfoText.audio.removePrefix("Audio:").trimStart())

            if (fileSizeInMegabytes.isNotBlank()) {
                append("<br><b>Dateigröße:</b> ")
                append(fileSizeInMegabytes)
                append(" MB")
            }
            append("</html>")
        }

        JOptionPane.showMessageDialog(
            this,
            message,
            "Codec-Details",
            JOptionPane.INFORMATION_MESSAGE
        )
    }

    private fun showLiveInfoErrorDialog(message: String) {
        JOptionPane.showMessageDialog(
            this,
            message,
            "Codec-Details",
            JOptionPane.ERROR_MESSAGE
        )
    }

    private fun updateDiskSpaceIndicatorsAsync() {
        uiScope.launch {
            val pathText = currentPathText()
            val usableSpace = withContext(Dispatchers.IO) { DownloadQualitySupport.getFreeDiskSpace(pathText) }
            updateDiskSpaceIndicators(usableSpace)
        }
    }

    private fun updateDiskSpaceIndicators(pathText: String) {
        uiScope.launch {
            val usableSpace = withContext(Dispatchers.IO) { DownloadQualitySupport.getFreeDiskSpace(pathText) }
            updateDiskSpaceIndicators(usableSpace)
        }
    }

    private fun updateDiskSpaceIndicators(usableSpace: Long) {
        resetResolutionButtonForegrounds()
        val border = jPanelRes.border as javax.swing.border.TitledBorder
        border.title = DownloadQualitySupport.qualityPanelTitle(QUALITY_PANEL_TITLE, usableSpace)
        jPanelRes.repaint()

        val usableSpaceInMiB = usableSpace / FileSize.ONE_MIB
        if (usableSpaceInMiB <= 0) {
            return
        }

        markResolutionExceedingDiskSpace(jRadioButtonResHd, dateiGroesseHD, usableSpaceInMiB)
        markResolutionExceedingDiskSpace(jRadioButtonResHi, dateiGroesseHoch, usableSpaceInMiB)
        markResolutionExceedingDiskSpace(jRadioButtonResLo, dateiGroesseKlein, usableSpaceInMiB)
    }

    private fun resetResolutionButtonForegrounds() {
        val foreground = UIManager.getColor(LABEL_FOREGROUND_KEY)
        jRadioButtonResHd.foreground = foreground
        jRadioButtonResHi.foreground = foreground
        jRadioButtonResLo.foreground = foreground
    }

    private fun markResolutionExceedingDiskSpace(button: JRadioButton, sizeText: String, usableSpaceInMiB: Long) {
        val size = sizeText.toIntOrNull() ?: return
        if (size > usableSpaceInMiB) {
            button.foreground = Color.RED
        }
    }

    private fun currentPathText(): String = mVPanelDownloadZiel.currentPath

    private fun buildLayout() {
        jPanelExtra.removeAll()
        addRow(DownloadColumn.ABO)
        addRow(DownloadColumn.SENDER)
        addRow(DownloadColumn.TOPIC)
        addRow(DownloadColumn.TITLE)
        addRow(DownloadColumn.SIZE)
        addRow(DownloadColumn.DATE)
        addRow(DownloadColumn.TIME)
        addRow(DownloadColumn.DURATION)
        addRow(DownloadColumn.HIGH_QUALITY)
        addRow(DownloadColumn.SUBTITLE_AVAILABLE)
        addRow(DownloadColumn.GEO)
        addRow(DownloadColumn.FILM_URL)
        addRow(DownloadColumn.URL)
        addRow(DownloadColumn.SUBTITLE_URL)
        addRow(DownloadColumn.PROGRAM_SET)
        addRow(DownloadColumn.PROGRAM)
        addRow(DownloadColumn.PROGRAM_INVOCATION)
        addRow(DownloadColumn.PROGRAM_INVOCATION_ARRAY)
        addRow(DownloadColumn.PROGRAM_RESTART)
        addRow(DownloadColumn.TARGET_FILE_NAME)
        addRow(DownloadColumn.TARGET_PATH)
        addRow(DownloadColumn.TARGET_PATH_FILE_NAME)
        addRow(DownloadColumn.TYPE)
        addRow(DownloadColumn.SOURCE)
        addRow(DownloadColumn.INFO_FILE)
        addRow(DownloadColumn.SPOTLIGHT)
        addRow(DownloadColumn.SUBTITLE)
        jPanelExtra.validate()
    }

    private fun addRow(column: DownloadColumn) {
        if (isDirectDownloadProgram() && (column == DownloadColumn.PROGRAM_INVOCATION
                    || column == DownloadColumn.PROGRAM_INVOCATION_ARRAY)
        ) {
            return
        }
        if (isEmptyOptionalRow(column)) {
            return
        }

        val label = createLabel(column)
        val textField = createTextField(column)
        addExtraField(column, label, textField)
    }

    private fun isEmptyOptionalRow(column: DownloadColumn): Boolean = when (column) {
        DownloadColumn.ABO -> datenDownload.aboName.isBlank()
        DownloadColumn.DURATION -> datenDownload.duration.isBlank()
        else -> false
    }

    private fun createTextField(column: DownloadColumn): JTextField {
        val textField = createReadOnlyTextField(textFieldValue(column))
        when (column) {
            DownloadColumn.URL -> urlField = textField
            DownloadColumn.PROGRAM_INVOCATION -> programmAufrufField = textField
            DownloadColumn.PROGRAM_INVOCATION_ARRAY -> programmAufrufArrayField = textField
            else -> Unit
        }
        return textField
    }

    private fun textFieldValue(column: DownloadColumn): String =
        when (column) {
            DownloadColumn.ABO -> datenDownload.aboName
            DownloadColumn.SENDER -> datenDownload.sender
            DownloadColumn.TOPIC -> datenDownload.topic
            DownloadColumn.TITLE -> datenDownload.title
            DownloadColumn.DATE -> datenDownload.date
            DownloadColumn.TIME -> datenDownload.time
            DownloadColumn.DURATION -> datenDownload.duration
            DownloadColumn.FILM_URL -> datenDownload.filmUrl
            DownloadColumn.HISTORY_URL -> datenDownload.historyUrl
            DownloadColumn.URL -> datenDownload.downloadUrl
            DownloadColumn.RTMP_URL -> datenDownload.rtmpUrl
            DownloadColumn.SUBTITLE_URL -> datenDownload.subtitleUrl
            DownloadColumn.PROGRAM_SET -> datenDownload.programSetName
            DownloadColumn.PROGRAM -> datenDownload.programName
            DownloadColumn.PROGRAM_INVOCATION -> datenDownload.programInvocation
            DownloadColumn.PROGRAM_INVOCATION_ARRAY -> datenDownload.programInvocationArray
            DownloadColumn.TARGET_FILE_NAME -> datenDownload.targetFileName
            DownloadColumn.TARGET_PATH -> datenDownload.targetPath
            DownloadColumn.TARGET_PATH_FILE_NAME -> datenDownload.targetPathFileName
            DownloadColumn.NUMBER -> datenDownload.nr.toString()
            DownloadColumn.FILM_NUMBER -> datenDownload.film?.filmNr?.toString().orEmpty()
            DownloadColumn.BUTTON_START,
            DownloadColumn.BUTTON_DELETE,
            -> ""

            DownloadColumn.PROGRESS ->
                DownloadProgressText.getTextProgress(datenDownload.isDownloadManager, datenDownload.runtime.runState)

            DownloadColumn.REMAINING_TIME -> datenDownload.textRestzeit
            DownloadColumn.BANDWIDTH -> datenDownload.textBandbreite
            DownloadColumn.SIZE -> datenDownload.runtime.filmSize.toString()
            DownloadColumn.HIGH_QUALITY -> (datenDownload.film?.isHighQuality == true).toString()
            DownloadColumn.SUBTITLE_AVAILABLE -> (datenDownload.film?.hasSubtitle() == true).toString()
            DownloadColumn.INTERRUPTED -> datenDownload.isInterrupted.toString()
            DownloadColumn.GEO -> datenDownload.geo
            DownloadColumn.PROGRAM_RESTART -> datenDownload.isRestart.toString()
            DownloadColumn.TYPE -> datenDownload.art.label
            DownloadColumn.SOURCE -> datenDownload.quelle.label
            DownloadColumn.DEFERRED -> datenDownload.isDeferred.toString()
            DownloadColumn.INFO_FILE -> datenDownload.isInfoFile.toString()
            DownloadColumn.SPOTLIGHT -> datenDownload.isSpotlight.toString()
            DownloadColumn.SUBTITLE -> datenDownload.isSubtitle.toString()
            DownloadColumn.DOWNLOAD_MANAGER -> datenDownload.isDownloadManager.toString()
            DownloadColumn.REF -> ""
        }

    private fun createReadOnlyTextField(text: String) = JTextField().apply {
        isEditable = false
        this.text = text
    }

    private fun isDirectDownloadProgram(): Boolean =
        DownloadType.DIRECT.label == datenDownload.programName

    private fun createLabel(column: DownloadColumn) = JLabel("${labelText(column)}: ").apply {
        font = font.deriveFont(java.awt.Font.BOLD)
    }

    private fun labelText(column: DownloadColumn): String = when (column) {
        DownloadColumn.ABO -> "Abo"
        DownloadColumn.SENDER -> "Sender"
        DownloadColumn.TOPIC -> "Thema"
        DownloadColumn.TITLE -> "Titel"
        DownloadColumn.SIZE -> "Größe"
        DownloadColumn.DATE -> "Datum"
        DownloadColumn.TIME -> "Zeit"
        DownloadColumn.DURATION -> "Dauer"
        DownloadColumn.HIGH_QUALITY -> "HD"
        DownloadColumn.SUBTITLE_AVAILABLE -> "UT"
        DownloadColumn.GEO -> "Geo"
        DownloadColumn.FILM_URL -> "Film-URL"
        DownloadColumn.URL -> "URL"
        DownloadColumn.SUBTITLE_URL -> "URL-Untertitel"
        DownloadColumn.PROGRAM_SET -> "Programmset"
        DownloadColumn.PROGRAM -> "Programm"
        DownloadColumn.PROGRAM_INVOCATION -> "Programmaufruf_"
        DownloadColumn.PROGRAM_INVOCATION_ARRAY -> "Programmaufruf"
        DownloadColumn.PROGRAM_RESTART -> "Restart"
        DownloadColumn.TARGET_FILE_NAME -> "Dateiname"
        DownloadColumn.TARGET_PATH -> "Pfad"
        DownloadColumn.TARGET_PATH_FILE_NAME -> "Pfad-Dateiname"
        DownloadColumn.TYPE -> "Art"
        DownloadColumn.SOURCE -> "Quelle"
        DownloadColumn.INFO_FILE -> "Infodatei"
        DownloadColumn.SPOTLIGHT -> "Spotlight"
        DownloadColumn.SUBTITLE -> "Untertitel"
        else -> error("Unknown download label column: $column")
    }

    private fun addExtraField(column: DownloadColumn, label: JLabel, textField: JTextField) {
        if (showDownloadTargetEditor(column, label)
            || addCheckboxField(column, label)
            || addAvailabilityField(column, label)
            || addProgramCallField(column, label, textField)
        ) {
            return
        }

        addDefaultField(column, label, textField)
    }

    private fun showDownloadTargetEditor(column: DownloadColumn, label: JLabel): Boolean {
        if (datenDownload.art != DownloadType.DIRECT || gestartet) {
            return false
        }
        if (column != DownloadColumn.TARGET_FILE_NAME
            && column != DownloadColumn.TARGET_PATH_FILE_NAME
            && column != DownloadColumn.TARGET_PATH
        ) {
            return false
        }

        if (column == DownloadColumn.TARGET_FILE_NAME) {
            addValueComponent(label, mVPanelDownloadZiel)
        }
        return true
    }

    private fun addCheckboxField(column: DownloadColumn, label: JLabel): Boolean = when (column) {
        DownloadColumn.PROGRAM_RESTART -> {
            configureCheckbox(
                label,
                jCheckBoxRestart,
                datenDownload.isRestart,
                !gestartet && !datenDownload.isDownloadManager
            )
            true
        }

        DownloadColumn.INFO_FILE -> {
            configureCheckbox(
                label,
                jCheckBoxInfodatei,
                datenDownload.isInfoFile,
                !gestartet
            )
            true
        }

        DownloadColumn.SUBTITLE -> {
            configureCheckbox(
                label,
                jCheckBoxSubtitle,
                datenDownload.isSubtitle,
                !gestartet
            )
            true
        }

        DownloadColumn.SPOTLIGHT -> {
            configureCheckbox(
                label,
                jCheckBoxSpotlight,
                datenDownload.isSpotlight,
                !gestartet
            )
            true
        }

        else -> false
    }

    private fun configureCheckbox(label: JLabel, checkBox: JCheckBox, selected: Boolean, enabled: Boolean) {
        label.foreground = hyperlinkColor()
        checkBox.isSelected = selected
        checkBox.actionListeners.forEach(checkBox::removeActionListener)
        checkBox.addActionListener { updateCheckboxValues() }
        checkBox.isEnabled = enabled
        addValueComponent(label, checkBox)
    }

    private fun addAvailabilityField(column: DownloadColumn, label: JLabel): Boolean = when (column) {
        DownloadColumn.HIGH_QUALITY -> {
            addAvailabilityComponent(label, cbHighQuality, datenDownload.film?.isHighQuality == true)
            true
        }

        DownloadColumn.SUBTITLE_AVAILABLE -> {
            addAvailabilityComponent(label, cbSubtitleAvailable, datenDownload.film?.hasSubtitle() == true)
            true
        }

        else -> false
    }

    private fun addAvailabilityComponent(label: JLabel, checkBox: JCheckBox, visible: Boolean) {
        checkBox.isSelected = true
        checkBox.isVisible = visible
        addValueComponent(label, checkBox)
    }

    private fun addProgramCallField(column: DownloadColumn, label: JLabel, textField: JTextField): Boolean {
        if (column == DownloadColumn.PROGRAM_INVOCATION) {
            return true
        }
        if (column != DownloadColumn.PROGRAM_INVOCATION_ARRAY || datenDownload.art != DownloadType.PROGRAM) {
            return false
        }

        if (datenDownload.programInvocationArray.isEmpty()) {
            label.foreground = hyperlinkColor()
            programmAufrufField?.let {
                makeEditable(it, DownloadColumn.PROGRAM_INVOCATION)
                addValueComponent(label, it)
            }
            return true
        }

        label.foreground = hyperlinkColor()
        makeEditable(textField, column)
        addValueComponent(label, createProgramCallPanel())
        return true
    }

    private fun createProgramCallPanel() = JPanel().apply {
        border = BorderFactory.createTitledBorder("")
        layout = MigLayout(
            LC().insets("2").fillX(),
            AC().count(2).index(1).grow().fill(),
            AC()
        )
        val programmField = requireNotNull(programmAufrufField)
        val programmArrayField = requireNotNull(programmAufrufArrayField)

        val resetButton = JButton("").apply {
            toolTipText = "Reset"
            icon = IconUtils.of(FontAwesomeSolid.REDO_ALT)
            addActionListener { programmArrayField.text = orgProgArray }
        }
        val helpButton = JButton("").apply {
            icon = SVGIconUtilities.createSVGIcon("icons/fontawesome/circle-question.svg")
            toolTipText = "Hilfe anzeigen"
            addActionListener {
                HelpTextDialog.show(
                    this@DialogEditDownload,
                    GetFile.getHilfeSuchen(Konstanten.PFAD_HILFETEXT_EDIT_DOWNLOAD_PROG),
                )
            }
        }

        add(helpButton)
        add(programmField, CC().growX().pushX().minWidth("0").wrap())
        add(resetButton)
        add(programmArrayField, CC().growX().pushX().minWidth("0").wrap())
    }

    private fun addDefaultField(column: DownloadColumn, label: JLabel, textField: JTextField) {
        when (column) {
            DownloadColumn.ABO -> addValueComponent(label, createValueLabel(datenDownload.aboName))
            DownloadColumn.SENDER -> addValueComponent(label, createValueLabel(datenDownload.sender))
            DownloadColumn.PROGRAM_SET -> addValueComponent(label, createValueLabel(datenDownload.programSetName))
            DownloadColumn.PROGRAM -> addValueComponent(label, createValueLabel(datenDownload.programName))
            DownloadColumn.SUBTITLE_URL -> addValueComponent(label, createValueLabel(datenDownload.subtitleUrl))
            DownloadColumn.DATE -> addValueComponent(label, createValueLabel(datenDownload.date))
            DownloadColumn.TIME -> addValueComponent(label, createValueLabel(datenDownload.time))
            DownloadColumn.TYPE -> addValueComponent(label, createValueLabel(downloadArtText()))
            DownloadColumn.SOURCE -> addValueComponent(label, createValueLabel(downloadQuelleText()))
            DownloadColumn.DURATION -> {
                val durationText = DurationFormatter.fromOrNull(datenDownload.duration)?.toDisplayText() ?: ""
                addValueComponent(label, createValueLabel(durationText))
            }
            DownloadColumn.GEO -> addValueComponent(label, createGeoLabel())
            DownloadColumn.SIZE -> addValueComponent(label, createValueLabel("${datenDownload.runtime.filmSize} MB"))
            DownloadColumn.TOPIC -> addValueComponent(label, createMultilineLabel(datenDownload.topic))
            DownloadColumn.TITLE -> addValueComponent(label, createMultilineLabel(datenDownload.title))

            else -> addTextFieldValue(column, label, textField)
        }
    }

    private fun addTextFieldValue(column: DownloadColumn, label: JLabel, textField: JTextField) {
        when (column) {
            DownloadColumn.NUMBER -> textField.text = datenDownload.nr.toString()
            DownloadColumn.FILM_NUMBER -> datenDownload.film?.let { film ->
                textField.text = film.filmNr.toString()
            }

            DownloadColumn.URL -> if (datenDownload.art == DownloadType.DIRECT) {
                label.foreground = hyperlinkColor()
                makeEditable(textField, column)
            }

            DownloadColumn.PROGRESS -> textField.text =
                DownloadProgressText.getTextProgress(datenDownload.isDownloadManager, datenDownload.runtime.runState)

            DownloadColumn.REMAINING_TIME -> textField.text = datenDownload.textRestzeit
            else -> Unit
        }
        if (column == DownloadColumn.URL) {
            addValueComponent(label, createDownloadUrlPanel(textField))
            return
        }
        addValueComponent(label, textField)
    }

    private fun createDownloadUrlPanel(textField: JTextField) = JPanel().apply {
        layout = MigLayout(
            LC().insets("0").fillX(),
            AC().count(2).grow().fill(),
            AC()
        )
        makeShrinkable(textField)

        val queryCodecDetailsButton = JButton("").apply {
            name = "btnQuerCodecDetailsForLocalUrl"
            icon = IconUtils.of(MaterialDesignM.MOVIE_SEARCH)
            toolTipText = "Codec-Details für URL abfragen"
            isEnabled = ffprobePath != null
            addActionListener { requestLiveInfoForUrl(textField.text.trim()) }
        }
        btnQuerCodecDetailsForLocalUrl = queryCodecDetailsButton

        add(textField, CC().growX().pushX().minWidth("0"))
        add(queryCodecDetailsButton, CC().gapLeft("5"))
    }

    private fun createValueLabel(text: String) = JLabel(text).apply(::makeShrinkable)

    private fun createMultilineLabel(text: String) = MultilineLabel().apply {
        this.text = text
        makeShrinkable(this)
    }

    private fun createGeoLabel() = JLabel().apply {
        horizontalAlignment = SwingConstants.LEFT
        val film = datenDownload.film
        val currentLocation = ApplicationConfiguration.getInstance().geographicLocation
        if (film == null) {
            toolTipText = "Keine Geoinformationen vorhanden"
            icon = IconUtils.of(FontAwesomeSolid.LOCK_OPEN)
            return@apply
        }

        val unlocked = !film.isGeoBlockedForLocation(currentLocation)
        toolTipText = when {
            !unlocked && !film.hasCountries() -> "Gesperrt für $currentLocation"
            film.hasCountries() -> film.countriesAsString
            else -> "Keine Geoinformationen vorhanden"
        }
        icon = IconUtils.of(if (unlocked) FontAwesomeSolid.LOCK_OPEN else FontAwesomeSolid.LOCK)
        makeShrinkable(this)
    }

    private fun makeEditable(textField: JTextField, column: DownloadColumn) {
        textField.isEditable = !gestartet
        textField.document.addDocumentListener(createDocumentListener(textField, column))
    }

    private fun createDocumentListener(textField: JTextField, column: DownloadColumn) = object : DocumentListener {
        override fun insertUpdate(e: DocumentEvent) = updateDownloadValue(textField, column)
        override fun removeUpdate(e: DocumentEvent) = updateDownloadValue(textField, column)
        override fun changedUpdate(e: DocumentEvent) = updateDownloadValue(textField, column)
    }

    private fun updateDownloadValue(textField: JTextField, column: DownloadColumn) {
        val value = textField.text.trim()
        when (column) {
            DownloadColumn.URL -> datenDownload.downloadUrl = value
            DownloadColumn.PROGRAM_INVOCATION -> datenDownload.programInvocation = value
            DownloadColumn.PROGRAM_INVOCATION_ARRAY -> datenDownload.programInvocationArray = value
            else -> {
                logger.warn("Ignoring unsupported editable download column: {}", column)
                return
            }
        }
        if (column == DownloadColumn.PROGRAM_INVOCATION_ARRAY) {
            datenDownload.programInvocation =
                DatenProg.makeProgAufrufArray(datenDownload.programInvocationArray)
            programmAufrufField?.text = datenDownload.programInvocation
        }
    }

    private fun downloadArtText(): String = datenDownload.art.label

    private fun downloadQuelleText(): String = datenDownload.quelle.label

    private fun addValueComponent(label: JLabel, component: Component) {
        (component as? JComponent)?.let(::makeShrinkable)
        jPanelExtra.add(label)
        jPanelExtra.add(component, CC().growX().pushX().minWidth("0").wrap())
    }

    private fun makeShrinkable(component: JComponent) {
        val preferredSize = component.preferredSize
        component.minimumSize = Dimension(0, preferredSize.height)
    }

    private fun hyperlinkColor(): Color? = UIManager.getColor("Hyperlink.linkColor")

    private fun updateCheckboxValues() {
        datenDownload.isRestart = jCheckBoxRestart.isSelected
        datenDownload.isInfoFile = jCheckBoxInfodatei.isSelected
        datenDownload.isSubtitle = jCheckBoxSubtitle.isSelected
        datenDownload.isSpotlight = jCheckBoxSpotlight.isSelected
    }

    private fun setupComponentListeners() {
        addComponentListener(object : ComponentAdapter() {
            override fun componentResized(e: ComponentEvent) = saveLocation()
            override fun componentMoved(e: ComponentEvent) = saveLocation()
        })
    }

    private fun restoreLocation() {
        val state = ApplicationConfiguration.getInstance().editDownloadDialogState
        if (!state.hasStoredLocation()) {
            return
        }

        location = Point(state.x, state.y)

        if (state.hasStoredSize()) {
            size = Dimension(state.width, state.height)
        }
    }

    private fun saveLocation() {
        if (!isVisible) {
            return
        }

        val location = locationOnScreen
        ApplicationConfiguration.getInstance()
            .setEditDownloadDialogBounds(location.x, location.y, width, height)
    }

    private fun downloadDateiLoeschen(download: DatenDownload): Boolean {
        return try {
            val file = File(download.targetPathFileName)
            if (!file.exists()) {
                return true
            }

            val ret = JOptionPane.showConfirmDialog(
                this,
                "Die Auflösung wurde geändert, der Film kann nicht weitergeführt werden.\nDatei muss zuerst gelöscht werden.",
                "Film Löschen?",
                JOptionPane.YES_NO_OPTION
            )
            if (ret != JOptionPane.YES_OPTION) {
                return false
            }

            logger.info("Datei löschen: {}", file.absolutePath)
            check(file.delete()) { "delete failed" }
            true
        } catch (_: Exception) {
            JOptionPane.showMessageDialog(this, "Konnte die Datei nicht löschen!", "Film löschen", JOptionPane.ERROR_MESSAGE)
            logger.error("Fehler beim löschen: {}", download.targetPathFileName)
            true
        }
    }

    private fun check(): Boolean {
        mVPanelDownloadZiel.applyTargetSelection()
        val resolutionChanged = when (resolution) {
            FilmResolution.Enum.HIGH_QUALITY -> !jRadioButtonResHd.isSelected
            FilmResolution.Enum.NORMAL -> !jRadioButtonResHi.isSelected
            FilmResolution.Enum.LOW -> !jRadioButtonResLo.isSelected
        }
        confirmed = !resolutionChanged || downloadDateiLoeschen(datenDownload)
        return confirmed
    }

    fun isConfirmed(): Boolean = confirmed

    companion object {
        private const val QUALITY_PANEL_TITLE = "Download-Qualität"
        private const val LABEL_FOREGROUND_KEY = "Label.foreground"
        private val LIVE_INFO_PLACEHOLDER = DownloadQualityLiveInfoText(
            video = "Video: 1920x1080, 2220 kBit/s, 50 fps (avg), H.264",
            audio = "Audio: 48000 Hz, 128 kBit/s, AAC (Advanced Audio Coding)"
        )
    }
}
