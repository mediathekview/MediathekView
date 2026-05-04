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
import mediathek.controller.starter.Start
import mediathek.daten.DatenDownload
import mediathek.daten.DatenProg
import mediathek.daten.FilmResolution
import mediathek.gui.dialog.DialogHilfe
import mediathek.gui.dialog.MVPanelDownloadZiel
import mediathek.gui.dialog.download.DownloadQualityLiveInfoText
import mediathek.gui.dialog.download.DownloadQualityResolutionSizes
import mediathek.gui.dialog.download.DownloadQualitySupport
import mediathek.swing.IconUtils
import mediathek.swing.MultilineLabel
import mediathek.tool.*
import net.miginfocom.swing.MigLayout
import org.apache.commons.configuration2.sync.LockMode
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
) : DialogEditDownloadView(parent) {
    private val uiScope = CoroutineScope(SupervisorJob() + Dispatchers.Swing)
    private val logger = LogManager.getLogger(javaClass)
    private val jCheckBoxRestart = JCheckBox()
    private val jCheckBoxInfodatei = JCheckBox()
    private val jCheckBoxSubtitle = JCheckBox()
    private val jCheckBoxSpotlight = JCheckBox()
    private val mVPanelDownloadZiel = MVPanelDownloadZiel(parent, datenDownload, false)
    private val orgProgArray = datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY]
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
        if (datenDownload.art != DatenDownload.ART_DOWNLOAD && datenDownload.pSet == null) {
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
        button.isSelected = datenDownload.arr[DatenDownload.DOWNLOAD_URL] == url
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
        datenDownload.arr[DatenDownload.DOWNLOAD_URL] = film.getUrlFuerAufloesung(selectedResolution)
        urlField?.text = datenDownload.arr[DatenDownload.DOWNLOAD_URL]

        val size = when (selectedResolution) {
            FilmResolution.Enum.HIGH_QUALITY -> dateiGroesseHD
            FilmResolution.Enum.NORMAL -> dateiGroesseHoch
            FilmResolution.Enum.LOW -> dateiGroesseKlein
        }
        if (datenDownload.art == DatenDownload.ART_PROGRAMM && datenDownload.pSet != null) {
            updateProgramCallFields(selectedResolution)
        }
        datenDownload.setGroesse(size)
    }

    private fun updateProgramCallFields(selectedResolution: FilmResolution.Enum) {
        val newDownload = DatenDownload(
            datenDownload.pSet,
            datenDownload.film,
            datenDownload.quelle,
            datenDownload.abo,
            datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_DATEINAME],
            datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD],
            selectedResolution.toString()
        )

        datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF] =
            newDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF]
        datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY] =
            newDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY]
        programmAufrufField?.text = datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF]
        programmAufrufArrayField?.text = datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY]
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

        val usableSpaceInMiB = usableSpace / FileSize.ONE_MiB
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

    private fun currentPathText(): String = mVPanelDownloadZiel.currentPath ?: datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD]

    private fun buildLayout() {
        jPanelExtra.removeAll()
        addRow(DatenDownload.DOWNLOAD_ABO)
        addRow(DatenDownload.DOWNLOAD_SENDER)
        addRow(DatenDownload.DOWNLOAD_THEMA)
        addRow(DatenDownload.DOWNLOAD_TITEL)
        addRow(DatenDownload.DOWNLOAD_GROESSE)
        addRow(DatenDownload.DOWNLOAD_DATUM)
        addRow(DatenDownload.DOWNLOAD_ZEIT)
        addRow(DatenDownload.DOWNLOAD_DAUER)
        addRow(DatenDownload.DOWNLOAD_HD)
        addRow(DatenDownload.DOWNLOAD_UT)
        addRow(DatenDownload.DOWNLOAD_GEO)
        addRow(DatenDownload.DOWNLOAD_FILM_URL)
        addRow(DatenDownload.DOWNLOAD_URL)
        addRow(DatenDownload.DOWNLOAD_URL_SUBTITLE)
        addRow(DatenDownload.DOWNLOAD_PROGRAMMSET)
        addRow(DatenDownload.DOWNLOAD_PROGRAMM)
        addRow(DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF)
        addRow(DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY)
        addRow(DatenDownload.DOWNLOAD_PROGRAMM_RESTART)
        addRow(DatenDownload.DOWNLOAD_ZIEL_DATEINAME)
        addRow(DatenDownload.DOWNLOAD_ZIEL_PFAD)
        addRow(DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME)
        addRow(DatenDownload.DOWNLOAD_ART)
        addRow(DatenDownload.DOWNLOAD_QUELLE)
        addRow(DatenDownload.DOWNLOAD_INFODATEI)
        addRow(DatenDownload.DOWNLOAD_SPOTLIGHT)
        addRow(DatenDownload.DOWNLOAD_SUBTITLE)
        jPanelExtra.validate()
    }

    private fun addRow(index: Int) {
        if (isDirectDownloadProgram() && (index == DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF
                    || index == DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY)
        ) {
            return
        }
        if (isEmptyOptionalRow(index)) {
            return
        }

        val label = createLabel(index)
        val textField = createTextField(index)
        addExtraField(index, label, textField)
    }

    private fun isEmptyOptionalRow(index: Int): Boolean = when (index) {
        DatenDownload.DOWNLOAD_ABO, DatenDownload.DOWNLOAD_DAUER -> datenDownload.arr[index].isBlank()
        else -> false
    }

    private fun createTextField(index: Int): JTextField {
        val textField = createReadOnlyTextField(datenDownload.arr[index])
        when (index) {
            DatenDownload.DOWNLOAD_URL -> urlField = textField
            DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF -> programmAufrufField = textField
            DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY -> programmAufrufArrayField = textField
        }
        return textField
    }

    private fun createReadOnlyTextField(text: String) = JTextField().apply {
        isEditable = false
        setText(text)
    }

    private fun isDirectDownloadProgram(): Boolean =
        DatenDownload.ART_DOWNLOAD_TXT == datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM]

    private fun createLabel(index: Int) = JLabel("${labelText(index)}: ").apply {
        font = font.deriveFont(java.awt.Font.BOLD)
    }

    private fun labelText(index: Int): String = when (index) {
        DatenDownload.DOWNLOAD_ABO -> "Abo"
        DatenDownload.DOWNLOAD_SENDER -> "Sender"
        DatenDownload.DOWNLOAD_THEMA -> "Thema"
        DatenDownload.DOWNLOAD_TITEL -> "Titel"
        DatenDownload.DOWNLOAD_GROESSE -> "Größe"
        DatenDownload.DOWNLOAD_DATUM -> "Datum"
        DatenDownload.DOWNLOAD_ZEIT -> "Zeit"
        DatenDownload.DOWNLOAD_DAUER -> "Dauer"
        DatenDownload.DOWNLOAD_HD -> "HD"
        DatenDownload.DOWNLOAD_UT -> "UT"
        DatenDownload.DOWNLOAD_GEO -> "Geo"
        DatenDownload.DOWNLOAD_FILM_URL -> "Film-URL"
        DatenDownload.DOWNLOAD_URL -> "URL"
        DatenDownload.DOWNLOAD_URL_SUBTITLE -> "URL-Untertitel"
        DatenDownload.DOWNLOAD_PROGRAMMSET -> "Programmset"
        DatenDownload.DOWNLOAD_PROGRAMM -> "Programm"
        DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF -> "Programmaufruf_"
        DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY -> "Programmaufruf"
        DatenDownload.DOWNLOAD_PROGRAMM_RESTART -> "Restart"
        DatenDownload.DOWNLOAD_ZIEL_DATEINAME -> "Dateiname"
        DatenDownload.DOWNLOAD_ZIEL_PFAD -> "Pfad"
        DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME -> "Pfad-Dateiname"
        DatenDownload.DOWNLOAD_ART -> "Art"
        DatenDownload.DOWNLOAD_QUELLE -> "Quelle"
        DatenDownload.DOWNLOAD_INFODATEI -> "Infodatei"
        DatenDownload.DOWNLOAD_SPOTLIGHT -> "Spotlight"
        DatenDownload.DOWNLOAD_SUBTITLE -> "Untertitel"
        else -> error("Unknown download label index: $index")
    }

    private fun addExtraField(index: Int, label: JLabel, textField: JTextField) {
        if (showDownloadTargetEditor(index, label)
            || addCheckboxField(index, label)
            || addAvailabilityField(index, label)
            || addProgramCallField(index, label, textField)
        ) {
            return
        }

        addDefaultField(index, label, textField)
    }

    private fun showDownloadTargetEditor(index: Int, label: JLabel): Boolean {
        if (datenDownload.art != DatenDownload.ART_DOWNLOAD || gestartet) {
            return false
        }
        if (index != DatenDownload.DOWNLOAD_ZIEL_DATEINAME
            && index != DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME
            && index != DatenDownload.DOWNLOAD_ZIEL_PFAD
        ) {
            return false
        }

        if (index == DatenDownload.DOWNLOAD_ZIEL_DATEINAME) {
            addValueComponent(label, mVPanelDownloadZiel)
        }
        return true
    }

    private fun addCheckboxField(index: Int, label: JLabel): Boolean = when (index) {
        DatenDownload.DOWNLOAD_PROGRAMM_RESTART -> {
            configureCheckbox(
                label,
                jCheckBoxRestart,
                datenDownload.isRestart,
                !gestartet && !datenDownload.isDownloadManager
            )
            true
        }

        DatenDownload.DOWNLOAD_INFODATEI -> {
            configureCheckbox(
                label,
                jCheckBoxInfodatei,
                datenDownload.arr[DatenDownload.DOWNLOAD_INFODATEI].toBoolean(),
                !gestartet
            )
            true
        }

        DatenDownload.DOWNLOAD_SUBTITLE -> {
            configureCheckbox(
                label,
                jCheckBoxSubtitle,
                datenDownload.arr[DatenDownload.DOWNLOAD_SUBTITLE].toBoolean(),
                !gestartet
            )
            true
        }

        DatenDownload.DOWNLOAD_SPOTLIGHT -> {
            configureCheckbox(
                label,
                jCheckBoxSpotlight,
                datenDownload.arr[DatenDownload.DOWNLOAD_SPOTLIGHT].toBoolean(),
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

    private fun addAvailabilityField(index: Int, label: JLabel): Boolean = when (index) {
        DatenDownload.DOWNLOAD_HD -> {
            addAvailabilityComponent(label, cbHighQuality, datenDownload.film?.isHighQuality == true)
            true
        }

        DatenDownload.DOWNLOAD_UT -> {
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

    private fun addProgramCallField(index: Int, label: JLabel, textField: JTextField): Boolean {
        if (index == DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF) {
            return true
        }
        if (index != DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY || datenDownload.art != DatenDownload.ART_PROGRAMM) {
            return false
        }

        if (datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY].isEmpty()) {
            label.foreground = hyperlinkColor()
            programmAufrufField?.let {
                makeEditable(it, DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF)
                addValueComponent(label, it)
            }
            return true
        }

        label.foreground = hyperlinkColor()
        makeEditable(textField, index)
        addValueComponent(label, createProgramCallPanel())
        return true
    }

    private fun createProgramCallPanel() = JPanel().apply {
        border = BorderFactory.createTitledBorder("")
        layout = MigLayout("insets 2, fillx", "[][grow,fill]", "")
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
                DialogHilfe(
                    this@DialogEditDownload,
                    true,
                    GetFile.getHilfeSuchen(Konstanten.PFAD_HILFETEXT_EDIT_DOWNLOAD_PROG)
                ).isVisible = true
            }
        }

        add(helpButton)
        add(programmField, "growx, pushx, wmin 0, wrap")
        add(resetButton)
        add(programmArrayField, "growx, pushx, wmin 0, wrap")
    }

    private fun addDefaultField(index: Int, label: JLabel, textField: JTextField) {
        when (index) {
            DatenDownload.DOWNLOAD_ABO,
            DatenDownload.DOWNLOAD_SENDER,
            DatenDownload.DOWNLOAD_ZEIT,
            DatenDownload.DOWNLOAD_URL_SUBTITLE,
            DatenDownload.DOWNLOAD_PROGRAMMSET,
            DatenDownload.DOWNLOAD_PROGRAMM,
            DatenDownload.DOWNLOAD_DATUM -> addValueComponent(label, createValueLabel(datenDownload.arr[index]))
            DatenDownload.DOWNLOAD_ART -> addValueComponent(label, createValueLabel(downloadArtText()))
            DatenDownload.DOWNLOAD_QUELLE -> addValueComponent(label, createValueLabel(downloadQuelleText()))
            DatenDownload.DOWNLOAD_DAUER -> {
                val durationText = DurationFormatter.fromOrNull(datenDownload.arr[index])?.toDisplayText() ?: ""
                addValueComponent(label, createValueLabel(durationText))
            }
            DatenDownload.DOWNLOAD_GEO -> addValueComponent(label, createGeoLabel())
            DatenDownload.DOWNLOAD_GROESSE -> addValueComponent(label, createValueLabel("${datenDownload.mVFilmSize} MB"))
            DatenDownload.DOWNLOAD_THEMA,
            DatenDownload.DOWNLOAD_TITEL -> addValueComponent(label, createMultilineLabel(datenDownload.arr[index]))

            else -> addTextFieldValue(index, label, textField)
        }
    }

    private fun addTextFieldValue(index: Int, label: JLabel, textField: JTextField) {
        when (index) {
            DatenDownload.DOWNLOAD_NR -> textField.text = datenDownload.nr.toString()
            DatenDownload.DOWNLOAD_FILM_NR -> if (datenDownload.film != null) {
                textField.text = datenDownload.film.filmNr.toString()
            }

            DatenDownload.DOWNLOAD_URL -> if (datenDownload.art == DatenDownload.ART_DOWNLOAD) {
                label.foreground = hyperlinkColor()
                makeEditable(textField, index)
            }

            DatenDownload.DOWNLOAD_PROGRESS -> textField.text =
                Start.getTextProgress(datenDownload.isDownloadManager, datenDownload.start)

            DatenDownload.DOWNLOAD_RESTZEIT -> textField.text = datenDownload.textRestzeit
        }
        if (index == DatenDownload.DOWNLOAD_URL) {
            addValueComponent(label, createDownloadUrlPanel(textField))
            return
        }
        addValueComponent(label, textField)
    }

    private fun createDownloadUrlPanel(textField: JTextField) = JPanel().apply {
        layout = MigLayout("insets 0, fillx", "[grow,fill][]", "")
        makeShrinkable(textField)

        val queryCodecDetailsButton = JButton("").apply {
            name = "btnQuerCodecDetailsForLocalUrl"
            icon = IconUtils.of(MaterialDesignM.MOVIE_SEARCH)
            toolTipText = "Codec-Details für URL abfragen"
            isEnabled = ffprobePath != null
            addActionListener { requestLiveInfoForUrl(textField.text.trim()) }
        }
        btnQuerCodecDetailsForLocalUrl = queryCodecDetailsButton

        add(textField, "growx, pushx, wmin 0")
        add(queryCodecDetailsButton, "gapleft 5")
    }

    private fun createValueLabel(text: String) = JLabel(text).apply(::makeShrinkable)

    private fun createMultilineLabel(text: String) = MultilineLabel().apply {
        setText(text)
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

    private fun makeEditable(textField: JTextField, index: Int) {
        textField.isEditable = !gestartet
        textField.document.addDocumentListener(createDocumentListener(textField, index))
    }

    private fun createDocumentListener(textField: JTextField, index: Int) = object : DocumentListener {
        override fun insertUpdate(e: DocumentEvent) = updateDownloadValue(textField, index)
        override fun removeUpdate(e: DocumentEvent) = updateDownloadValue(textField, index)
        override fun changedUpdate(e: DocumentEvent) = updateDownloadValue(textField, index)
    }

    private fun updateDownloadValue(textField: JTextField, index: Int) {
        datenDownload.arr[index] = textField.text.trim()
        if (index == DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY) {
            datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF] =
                DatenProg.makeProgAufrufArray(datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY])
            programmAufrufField?.text = datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF]
        }
    }

    private fun downloadArtText(): String = when (datenDownload.art) {
        DatenDownload.ART_DOWNLOAD -> DatenDownload.ART_DOWNLOAD_TXT
        DatenDownload.ART_PROGRAMM -> DatenDownload.ART_PROGRAMM_TXT
        else -> datenDownload.arr[DatenDownload.DOWNLOAD_ART]
    }

    private fun downloadQuelleText(): String = when (datenDownload.quelle) {
        DatenDownload.QUELLE_ALLE -> DatenDownload.QUELLE_ALLE_TXT
        DatenDownload.QUELLE_ABO -> DatenDownload.QUELLE_ABO_TXT
        DatenDownload.QUELLE_BUTTON -> DatenDownload.QUELLE_BUTTON_TXT
        DatenDownload.QUELLE_DOWNLOAD -> DatenDownload.QUELLE_DOWNLOAD_TXT
        else -> datenDownload.arr[DatenDownload.DOWNLOAD_QUELLE]
    }

    private fun addValueComponent(label: JLabel, component: Component) {
        (component as? JComponent)?.let(::makeShrinkable)
        jPanelExtra.add(label)
        jPanelExtra.add(component, "growx, pushx, wmin 0, wrap")
    }

    private fun makeShrinkable(component: JComponent) {
        val preferredSize = component.preferredSize
        component.minimumSize = Dimension(0, preferredSize.height)
    }

    private fun hyperlinkColor(): Color? = UIManager.getColor("Hyperlink.linkColor")

    private fun updateCheckboxValues() {
        datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_RESTART] = jCheckBoxRestart.isSelected.toString()
        datenDownload.arr[DatenDownload.DOWNLOAD_INFODATEI] = jCheckBoxInfodatei.isSelected.toString()
        datenDownload.arr[DatenDownload.DOWNLOAD_SUBTITLE] = jCheckBoxSubtitle.isSelected.toString()
        datenDownload.arr[DatenDownload.DOWNLOAD_SPOTLIGHT] = jCheckBoxSpotlight.isSelected.toString()
    }

    private fun setupComponentListeners() {
        addComponentListener(object : ComponentAdapter() {
            override fun componentResized(e: ComponentEvent) = saveLocation()
            override fun componentMoved(e: ComponentEvent) = saveLocation()
        })
    }

    private fun restoreLocation() {
        val config = ApplicationConfiguration.getConfiguration()
        config.withLock(LockMode.READ) {
            try {
                location = Point(
                    config.getInt(ApplicationConfiguration.EditDownloadDialog.X),
                    config.getInt(ApplicationConfiguration.EditDownloadDialog.Y)
                )

                val width = config.getInt(ApplicationConfiguration.EditDownloadDialog.WIDTH, -1)
                val height = config.getInt(ApplicationConfiguration.EditDownloadDialog.HEIGHT, -1)
                if (width != -1 && height != -1) {
                    size = Dimension(width, height)
                }
            } catch (_: NoSuchElementException) {
            }
        }
    }

    private fun saveLocation() {
        if (!isVisible) {
            return
        }

        val config = ApplicationConfiguration.getConfiguration()
        config.withLock(LockMode.WRITE) {
            val location = locationOnScreen
            config.setProperty(ApplicationConfiguration.EditDownloadDialog.X, location.x)
            config.setProperty(ApplicationConfiguration.EditDownloadDialog.Y, location.y)
            config.setProperty(ApplicationConfiguration.EditDownloadDialog.WIDTH, width)
            config.setProperty(ApplicationConfiguration.EditDownloadDialog.HEIGHT, height)
        }
    }

    private fun downloadDateiLoeschen(download: DatenDownload): Boolean {
        return try {
            val file = File(download.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME])
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
            logger.error("Fehler beim löschen: {}", download.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME])
            true
        }
    }

    private fun check(): Boolean {
        mVPanelDownloadZiel.setPfadName_geaendert()
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
