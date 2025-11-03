/*
 * Copyright (c) 2025 derreisende77.
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

package mediathek.gui.dialog.add_download

import com.github.kokorin.jaffree.StreamType
import com.github.kokorin.jaffree.ffprobe.FFprobe
import com.github.kokorin.jaffree.ffprobe.FFprobeResult
import com.github.kokorin.jaffree.ffprobe.Stream
import com.github.kokorin.jaffree.process.JaffreeAbnormalExitException
import kotlinx.coroutines.*
import kotlinx.coroutines.swing.Swing
import mediathek.config.Daten
import mediathek.config.MVColor
import mediathek.config.MVConfig
import mediathek.daten.*
import mediathek.gui.messages.DownloadListChangedEvent
import mediathek.mainwindow.MediathekGui
import mediathek.tool.*
import mediathek.tool.MessageBus.messageBus
import org.apache.commons.configuration2.sync.LockMode
import org.apache.commons.lang3.SystemUtils
import org.apache.logging.log4j.LogManager
import java.awt.Color
import java.awt.Dimension
import java.awt.Frame
import java.awt.event.ActionListener
import java.awt.event.ComponentAdapter
import java.awt.event.ComponentEvent
import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.util.*
import javax.swing.*
import javax.swing.event.DocumentEvent
import javax.swing.event.DocumentListener
import javax.swing.text.JTextComponent
import kotlin.coroutines.cancellation.CancellationException
import kotlin.math.max

class DialogAddDownloadWithCoroutines(
    parent: Frame,
    private val film: DatenFilm,
    /**
     * The currently selected pSet or null when no selection.
     */
    private var activeProgramSet: DatenPset,
    private val requestedResolution: Optional<FilmResolution.Enum>
) : DialogAddDownload(parent) {
    private val coroutineScope = CoroutineScope(SupervisorJob() + Dispatchers.Swing)
    private var liveInfoJob: Job? = null
    private var highQualityMandated: Boolean = false
    private var stopBeob = false
    private var nameGeaendert = false
    private var ffprobePath: Path? = null
    private var orgPfad = ""
    private var minimumDialogWidth: Int = 720
    private var minimumDialogHeight: Int = 600
    private val listeSpeichern: ListePset = Daten.listePset.listeSpeichern
    private var fileSizeJob: Job? = null
    private var dateiGroesseHighQuality: String = ""
    private var dateiGroesseNormalQuality: String = ""
    private var dateiGroesseLowQuality: String = ""
    private lateinit var cbPathTextComponent: JTextComponent
    private lateinit var datenDownload: DatenDownload

    companion object {
        private val logger = LogManager.getLogger()
        private const val NO_DATA_AVAILABLE = "Keine Daten verfügbar."
        private const val TITLED_BORDER_STRING = "Download-Qualität"
        private const val KEY_LABEL_FOREGROUND: String = "Label.foreground"
        private const val KEY_TEXTFIELD_BACKGROUND: String = "TextField.background"

        @JvmStatic
        fun saveComboPfad(jcb: JComboBox<String>, orgPath: String) {
            val pfade = mutableListOf<String>()
            val s = jcb.selectedItem?.toString().orEmpty()

            if (s != orgPath || ApplicationConfiguration.getConfiguration()
                    .getBoolean(ApplicationConfiguration.DOWNLOAD_SHOW_LAST_USED_PATH, true)
            ) {
                pfade.add(s)
            }

            for (i in 0 until jcb.itemCount) {
                val item = jcb.getItemAt(i)
                if (item != orgPath && item !in pfade) {
                    pfade.add(item)
                }
            }

            if (pfade.isNotEmpty()) {
                val joined = pfade
                    .filter { it.isNotEmpty() }
                    .take(mediathek.config.Konstanten.MAX_PFADE_DIALOG_DOWNLOAD)
                    .joinToString("<>")
                MVConfig.add(MVConfig.Configs.SYSTEM_DIALOG_DOWNLOAD__PFADE_ZUM_SPEICHERN, joined)
            }
        }

        @JvmStatic
        fun setModelPfad(pfad: String, jcb: JComboBox<String>) {
            val pfade = mutableListOf<String>()
            val showLastUsedPath = ApplicationConfiguration.getConfiguration()
                .getBoolean(ApplicationConfiguration.DOWNLOAD_SHOW_LAST_USED_PATH, true)

            // Wenn gewünscht, den letzten verwendeten Pfad an den Anfang setzen
            if (!showLastUsedPath && pfad.isNotEmpty()) {
                pfade.add(pfad)
            }

            val gespeichertePfade = MVConfig.get(MVConfig.Configs.SYSTEM_DIALOG_DOWNLOAD__PFADE_ZUM_SPEICHERN)
            if (gespeichertePfade.isNotEmpty()) {
                val p = gespeichertePfade.split("<>")
                for (s in p) {
                    if (s !in pfade) {
                        pfade.add(s)
                    }
                }
            }

            // aktueller Pfad ans Ende setzen
            if (showLastUsedPath && pfad.isNotEmpty() && pfad !in pfade) {
                pfade.add(pfad)
            }

            jcb.model = DefaultComboBoxModel(pfade.toTypedArray())
        }

    }

    init {
        getRootPane().setDefaultButton(btnDownloadImmediately)
        EscapeKeyHandler.installHandler(this) { this.dispose() }

        setupUI()

        setupMinimumSizeForOs()
        restoreWindowSizeFromConfig() //only install on windows and linux, macOS works...
        installMinResizePreventer()

        setLocationRelativeTo(parent)

        addComponentListener(DialogPositionComponentListener())

        btnRequestLiveInfo.addActionListener {
            liveInfoJob?.cancel()
            liveInfoJob = coroutineScope.launch {
                fetchLiveFilmInfo()
            }
        }

        btnDownloadImmediately.requestFocus()
    }

    override fun dispose() {
        coroutineScope.cancel()
        super.dispose()
    }

    private fun setupUI() {
        setupBusyIndicator()
        detectFfprobeExecutable()

        fileSizeJob = launchResolutionFutures()

        coroutineScope.launch {
            waitForFileSizeJob()
            calculateAndCheckDiskSpace()
        }

        setupZielButton()

        btnDownloadImmediately.addActionListener { prepareDownload(true) }
        btnQueueDownload.addActionListener { prepareDownload(false) }
        jButtonAbbrechen.addActionListener { dispose() }

        setupPSetComboBox()
        setupSenderTextField()
        setupNameTextField()
        setupPathTextComponent()

        setupFilmQualityRadioButtons()

        setupDeleteHistoryButton()
        setupPfadSpeichernCheckBox()

        setupResolutionButtons()
        setupInfoFileCreationCheckBox()

        nameGeaendert = false
    }

    private fun prepareDownload(startAutomatically: Boolean) {
        if (check()) {
            saveComboPfad(jComboBoxPfad, orgPfad)
            saveDownload(startAutomatically)
        }
    }

    /**
     * Store download in list and start immediately if requested.
     */
    private fun saveDownload(startAutomatically: Boolean) {
        datenDownload = DatenDownload(
            activeProgramSet,
            film,
            DatenDownload.QUELLE_DOWNLOAD,
            null,
            jTextFieldName.text,
            jComboBoxPfad.selectedItem?.toString() ?: "",
            getFilmResolution().toString()
        ).apply {
            setGroesse(getFilmSize())
            arr[DatenDownload.DOWNLOAD_INFODATEI] = jCheckBoxInfodatei.isSelected.toString()
            arr[DatenDownload.DOWNLOAD_SUBTITLE] = jCheckBoxSubtitle.isSelected.toString()
        }

        addDownloadToQueue(startAutomatically)
        dispose()
    }

    /**
     * Setup the resolution radio buttons based on available download URLs.
     */
    private fun setupResolutionButtons() {
        activeProgramSet = listeSpeichern[jComboBoxPset.getSelectedIndex()]

        prepareResolutionButtons()
        prepareSubtitleCheckbox()
        setNameFilm()
    }

    private fun prepareResolutionButtons() {
        requestedResolution.ifPresent { highQualityMandated = it == FilmResolution.Enum.HIGH_QUALITY }

        when {
            highQualityMandated || isHighQualityRequested() -> jRadioButtonAufloesungHd.isSelected = true
            isLowQualityRequested() -> jRadioButtonAufloesungKlein.isSelected = true
            else -> jRadioButtonAufloesungHoch.isSelected = true
        }
    }

    private fun setupInfoFileCreationCheckBox() {
        jCheckBoxInfodatei.apply {
            if (film.isLivestream) {
                //disable for Livestreams as they do not contain useful data, even if pset wants it...
                isEnabled = false
                isSelected = false
            } else {
                isEnabled = true
                isSelected = activeProgramSet.shouldCreateInfofile()
            }
        }
    }

    private fun isLowQualityRequested(): Boolean {
        return activeProgramSet.arr[DatenPset.PROGRAMMSET_AUFLOESUNG] == FilmResolution.Enum.LOW.toString() &&
                film.lowQualityUrl.isNotEmpty()
    }

    private fun isHighQualityRequested(): Boolean {
        return activeProgramSet.arr[DatenPset.PROGRAMMSET_AUFLOESUNG] == FilmResolution.Enum.HIGH_QUALITY.toString()
                && film.isHighQuality
    }

    /**
     * Return the resolution based on selected RadioButton.
     */
    private fun getFilmResolution(): FilmResolution.Enum {
        return when {
            jRadioButtonAufloesungHd.isSelected -> FilmResolution.Enum.HIGH_QUALITY
            jRadioButtonAufloesungKlein.isSelected -> FilmResolution.Enum.LOW
            else -> FilmResolution.Enum.NORMAL
        }
    }

    private fun prepareSubtitleCheckbox() {
        if (!film.hasSubtitle()) {
            jCheckBoxSubtitle.setEnabled(false)
        } else {
            jCheckBoxSubtitle.setSelected(activeProgramSet.shouldDownloadSubtitle())
        }
    }

    private fun setNameFilm() {
        // beim ersten Mal werden die Standardpfade gesucht
        if (!nameGeaendert) {
            // nur wenn vom Benutzer noch nicht geändert!
            stopBeob = true

            datenDownload = DatenDownload(
                activeProgramSet,
                film,
                DatenDownload.QUELLE_DOWNLOAD,
                null,
                "",
                "",
                getFilmResolution().toString()
            )

            if (datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_DATEINAME].isEmpty()) {
                // dann wird nicht gespeichert → eigentlich falsche Seteinstellungen?
                jTextFieldName.apply {
                    isEnabled = false
                    text = ""
                }
                jComboBoxPfad.apply {
                    isEnabled = false
                    model = DefaultComboBoxModel(arrayOf(""))
                }
                jButtonZiel.isEnabled = false
            } else {
                jTextFieldName.apply {
                    isEnabled = true
                    text = datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_DATEINAME]
                }
                jComboBoxPfad.isEnabled = true
                jButtonZiel.isEnabled = true
                setModelPfad(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD], jComboBoxPfad)
                orgPfad = datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD]
            }

            stopBeob = false
        }
    }

    private fun addDownloadToQueue(startAutomatically: Boolean) {
        Daten.getInstance().listeDownloads.addMitNummer(datenDownload)
        messageBus.publishAsync(DownloadListChangedEvent())

        if (startAutomatically) {
            datenDownload.startDownload()
        }
    }

    private fun getFilmSize(): String {
        return when {
            jRadioButtonAufloesungHd.isSelected -> dateiGroesseHighQuality
            jRadioButtonAufloesungKlein.isSelected -> dateiGroesseLowQuality
            else -> dateiGroesseNormalQuality
        }
    }


    private fun check(): Boolean {
        val pfadRaw = jComboBoxPfad.selectedItem?.toString() ?: return false
        val name = jTextFieldName.text

        //if (datenDownload == null) return false

        if (pfadRaw.isEmpty() || name.isEmpty()) {
            MVMessageDialog.showMessageDialog(
                this,
                "Pfad oder Name ist leer",
                "Fehlerhafter Pfad/Name!",
                JOptionPane.ERROR_MESSAGE
            )
            return false
        }

        val pfad = if (pfadRaw.endsWith(File.separator)) pfadRaw else pfadRaw + File.separator

        return if (GuiFunktionenProgramme.checkPathWriteable(pfad)) {
            true
        } else {
            MVMessageDialog.showMessageDialog(
                this,
                "Pfad ist nicht beschreibbar",
                "Fehlerhafter Pfad!",
                JOptionPane.ERROR_MESSAGE
            )
            false
        }
    }

    private fun setupFilmQualityRadioButtons() {
        val listener = ActionListener {
            setNameFilm()
            lblStatus.setText("")
            lblAudioInfo.setText("")
            lblBusyIndicator.setBusy(false)
            lblBusyIndicator.isVisible = false
            liveInfoJob?.cancel()
        }
        jRadioButtonAufloesungHd.addActionListener(listener)
        jRadioButtonAufloesungHd.setEnabled(!film.highQualityUrl.isEmpty())

        jRadioButtonAufloesungKlein.addActionListener(listener)
        jRadioButtonAufloesungKlein.setEnabled(!film.lowQualityUrl.isEmpty())

        jRadioButtonAufloesungHoch.addActionListener(listener)
        jRadioButtonAufloesungHoch.setSelected(true)
    }

    private fun setupPSetComboBox() {
        val model = DefaultComboBoxModel(listeSpeichern.getObjectDataCombo())
        jComboBoxPset.apply {
            // disable when only one entry...
            setEnabled(listeSpeichern.size > 1)
            setModel(model)
            setSelectedItem(activeProgramSet.name)
            addActionListener { setupResolutionButtons() }
        }
    }

    private fun setupSenderTextField() {
        jTextFieldSender.apply {
            text = "${film.sender}: ${film.title}"
            setBackground(UIManager.getColor("Label.background"))
        }
    }

    private fun setupDeleteHistoryButton() {
        jButtonDelHistory.apply {
            setText("")
            setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/trash-can.svg"))
            addActionListener {
                MVConfig.add(MVConfig.Configs.SYSTEM_DIALOG_DOWNLOAD__PFADE_ZUM_SPEICHERN, "")
                jComboBoxPfad.setModel(DefaultComboBoxModel(arrayOf<String?>(orgPfad)))
            }
        }
    }

    private fun setupPfadSpeichernCheckBox() {
        val config = ApplicationConfiguration.getConfiguration()
        jCheckBoxPfadSpeichern.apply {
            setSelected(config.getBoolean(ApplicationConfiguration.DOWNLOAD_SHOW_LAST_USED_PATH, true))
            addActionListener {
                config.setProperty(
                    ApplicationConfiguration.DOWNLOAD_SHOW_LAST_USED_PATH,
                    jCheckBoxPfadSpeichern.isSelected
                )
            }
        }
    }

    private fun detectFfprobeExecutable() {
        try {
            ffprobePath = GuiFunktionenProgramme.findExecutableOnPath("ffprobe").parent
        } catch (_: Exception) {
            logger.error("ffprobe not found on system.")
            lblBusyIndicator.apply {
                isVisible = true
                isBusy = false
                setText("Hilfsprogramm nicht gefunden!")
                setForeground(Color.RED)
            }
            btnRequestLiveInfo.setEnabled(false)
        }
    }

    /** Prevents that a dialog can be resized smaller than its minimum dimensions.
     * Needed on Windows, but not macOS and Linux. */
    private fun installMinResizePreventer() {
        if (!SystemUtils.IS_OS_WINDOWS) return

        addComponentListener(object : ComponentAdapter() {
            override fun componentResized(e: ComponentEvent?) {
                val size = getSize()
                val w = max(size.width, minimumSize.width)
                val h = max(size.height, minimumSize.height)
                if (w != size.width || h != size.height) {
                    setSize(w, h)
                }
            }
        })
    }

    private fun restoreWindowSizeFromConfig() {
        val config = ApplicationConfiguration.getConfiguration()
        try {
            config.lock(LockMode.READ)
            val width = max(config.getInt(ApplicationConfiguration.AddDownloadDialog.WIDTH), minimumDialogWidth)
            val height = max(config.getInt(ApplicationConfiguration.AddDownloadDialog.HEIGHT), minimumDialogHeight)
            val x = config.getInt(ApplicationConfiguration.AddDownloadDialog.X)
            val y = config.getInt(ApplicationConfiguration.AddDownloadDialog.Y)

            setBounds(x, y, width, height)
        } catch (_: NoSuchElementException) {
            //do not restore anything
        } finally {
            config.unlock(LockMode.READ)
        }
    }

    private fun setupBusyIndicator() {
        lblBusyIndicator.apply {
            setText("")
            isBusy = false
            isVisible = false
        }
        lblStatus.setText("")
        lblAudioInfo.setText("")
    }

    private fun setupMinimumSizeForOs() {
        if (SystemUtils.IS_OS_WINDOWS)
            minimumDialogHeight -= 150
        else if (SystemUtils.IS_OS_LINUX) {
            minimumDialogHeight -= 50
        }
        else if (SystemUtils.IS_OS_MAC_OSX) {
            minimumDialogHeight -= 150
        }
        minimumSize = Dimension(minimumDialogWidth, minimumDialogHeight)
    }

    private suspend fun fetchLiveFilmInfo() {
        btnRequestLiveInfo.isEnabled = false
        lblBusyIndicator.apply {
            isVisible = true
            isBusy = true
        }
        lblStatus.text = ""
        lblAudioInfo.text = ""

        try {
            val url = film.getUrlFuerAufloesung(getFilmResolution())

            val result = withContext(Dispatchers.IO) {
                FFprobe.atPath(ffprobePath)
                    .setShowStreams(true)
                    .setInput(url)
                    .execute()
            }

            processFFprobeResult(result)
        } catch (_: CancellationException) {
            clearInfo()
        } catch (ex: JaffreeAbnormalExitException) {
            setupLabels(getJaffreeErrorString(ex))
        } catch (_: Exception) {
            setupLabels("Unbekannter Fehler aufgetreten.")
        } finally {
            resetBusyLabelAndButton()
        }
    }

    private fun processFFprobeResult(result: FFprobeResult) {
        val audioStream = result.streams.find { it.codecType == StreamType.AUDIO }
        val videoStream = result.streams.find { it.codecType == StreamType.VIDEO }

        lblAudioInfo.foreground = UIManager.getColor(KEY_LABEL_FOREGROUND)
        lblAudioInfo.text = audioStream?.let { getAudioInfo(it, it.sampleRate) } ?: NO_DATA_AVAILABLE

        lblStatus.foreground = UIManager.getColor(KEY_LABEL_FOREGROUND)
        lblStatus.text = videoStream?.let {
            val frameRate = it.avgFrameRate.toInt()
            val codecName = getVideoCodecName(it)
            getVideoInfoString(it, frameRate, codecName)
        } ?: NO_DATA_AVAILABLE
    }

    private fun resetBusyLabelAndButton() {
        lblBusyIndicator.apply {
            isVisible = false
            isBusy = false
        }
        btnRequestLiveInfo.setEnabled(true)
    }

    private fun clearInfo() {
        lblStatus.text = ""
        lblAudioInfo.text = ""
    }

    private fun setupLabels(inText: String) {
        lblStatus.apply {
            text = inText
            foreground = Color.RED
        }
        lblAudioInfo.text = ""
    }

    /**
     * Return only the first part of the long codec name.
     *
     * @param stream The video stream from ffprobe.
     * @return First entry of long codec name.
     */
    private fun getVideoCodecName(stream: Stream): String {
        logger.trace("video codec long name: ${stream.codecLongName}")
        return stream.codecLongName.split("/")
            .firstOrNull()
            ?.trim()
            ?: stream.codecLongName
    }

    private fun getAudioInfo(stream: Stream, sampleRate: Int?): String {
        val bitRate = safeProcessBitRate(stream.bitRate)
        return if (bitRate == 0) {
            "Audio: ${sampleRate ?: "?"} Hz, ${stream.codecLongName}"
        } else {
            "Audio: ${sampleRate ?: "?"} Hz, $bitRate kBit/s, ${stream.codecLongName}"
        }
    }

    private fun getVideoInfoString(stream: Stream, frameRate: Int, codecName: String?): String {
        val bitRate = safeProcessBitRate(stream.bitRate)
        return if (bitRate == 0) {
            "Video: ${stream.width}x${stream.height}, $frameRate fps (avg), $codecName"
        } else {
            "Video: ${stream.width}x${stream.height}, $bitRate kBit/s, $frameRate fps (avg), $codecName"
        }
    }

    private fun safeProcessBitRate(inBitRate: Int?): Int {
        return try {
            (inBitRate ?: 0) / 1000
        } catch (_: Exception) {
            0
        }
    }

    private fun getJaffreeErrorString(ex: JaffreeAbnormalExitException): String {
        return try {
            val msg = ex.processErrorLogMessages.first().message.split(":")
            val errMsg = msg.last().trim()
            if (errMsg.startsWith("Server returned ")) {
                errMsg.removePrefix("Server returned ").trim()
            } else {
                "Unbekannter Fehler aufgetreten."
            }
        } catch (_: Exception) {
            "Unbekannter Fehler aufgetreten."
        }
    }

    /**
     * Calculate free disk space on volume and check if the movies can be safely downloaded.
     */
    fun calculateAndCheckDiskSpace() {
        UIManager.getColor(KEY_LABEL_FOREGROUND)?.let { fgColor ->
            jRadioButtonAufloesungHd.foreground = fgColor
            jRadioButtonAufloesungHoch.foreground = fgColor
            jRadioButtonAufloesungKlein.foreground = fgColor
        }

        try {
            val filmBorder = jPanelSize.border as? javax.swing.border.TitledBorder ?: return
            var usableSpace = getFreeDiskSpace(cbPathTextComponent.text)

            filmBorder.title = if (usableSpace > 0) {
                "$TITLED_BORDER_STRING [ Freier Speicherplatz: ${FileUtils.humanReadableByteCountBinary(usableSpace)} ]"
            } else {
                TITLED_BORDER_STRING
            }

            // Border needs to be repainted after update...
            jPanelSize.repaint()

            // jetzt noch prüfen, obs auf die Platte passt
            usableSpace /= FileSize.ONE_MiB
            if (usableSpace > 0) {
                if (dateiGroesseHighQuality.isNotEmpty()) {
                    val size = dateiGroesseHighQuality.toIntOrNull() ?: 0
                    if (size > usableSpace) {
                        jRadioButtonAufloesungHd.foreground = Color.RED
                    }
                }
                if (dateiGroesseNormalQuality.isNotEmpty()) {
                    val size = dateiGroesseNormalQuality.toIntOrNull() ?: 0
                    if (size > usableSpace) {
                        jRadioButtonAufloesungHoch.foreground = Color.RED
                    }
                }
                if (dateiGroesseLowQuality.isNotEmpty()) {
                    val size = dateiGroesseLowQuality.toIntOrNull() ?: 0
                    if (size > usableSpace) {
                        jRadioButtonAufloesungKlein.foreground = Color.RED
                    }
                }
            }
        } catch (ex: Exception) {
            logger.error("calculateAndCheckDiskSpace()", ex)
        }
    }

    private fun setupPathTextComponent() {
        cbPathTextComponent = jComboBoxPfad.editor.editorComponent as JTextComponent
        cbPathTextComponent.isOpaque = true

        cbPathTextComponent.document.addDocumentListener(object : DocumentListener {
            override fun insertUpdate(e: DocumentEvent?) = tus()
            override fun removeUpdate(e: DocumentEvent?) = tus()
            override fun changedUpdate(e: DocumentEvent?) = tus()

            private fun fileNameCheck(filePath: String) {
                val editor = jComboBoxPfad.editor.editorComponent
                if (filePath != FilenameUtils.checkFilenameForIllegalCharacters(filePath, true)) {
                    editor.background = MVColor.DOWNLOAD_FEHLER.color
                } else {
                    editor.background = UIManager.getColor(KEY_TEXTFIELD_BACKGROUND)
                }
            }

            private fun tus() {
                if (!stopBeob) {
                    nameGeaendert = true
                    // do not perform check on Windows
                    if (!SystemUtils.IS_OS_WINDOWS) {
                        (jComboBoxPfad.selectedItem as? String)?.let { fileNameCheck(it) }
                    }
                    calculateAndCheckDiskSpace()
                }
            }
        })
    }

    private fun setupNameTextField() {
        jTextFieldName.document.addDocumentListener(object : DocumentListener {

            override fun insertUpdate(e: DocumentEvent?) = tus()
            override fun removeUpdate(e: DocumentEvent?) = tus()
            override fun changedUpdate(e: DocumentEvent?) = tus()

            private fun tus() {
                if (!stopBeob) {
                    nameGeaendert = true
                    if (jTextFieldName.text != FilenameUtils.checkFilenameForIllegalCharacters(
                            jTextFieldName.text,
                            false
                        )
                    ) {
                        jTextFieldName.background = MVColor.DOWNLOAD_FEHLER.color
                    } else {
                        jTextFieldName.background = UIManager.getDefaults().getColor(KEY_TEXTFIELD_BACKGROUND)
                    }
                }
            }
        })
    }

    /**
     * Get the free disk space for a selected path.
     *
     * @return Free disk space in bytes.
     */
    private fun getFreeDiskSpace(strPath: String): Long {
        if (strPath.isEmpty()) return 0L

        return try {
            var path = Paths.get(strPath)
            while (path != null && Files.notExists(path)) {
                path = path.parent
            }

            if (path == null) {
                0L
            } else {
                Files.getFileStore(path).usableSpace
            }
        } catch (ex: Exception) {
            logger.error("getFreeDiskSpace Failed", ex)
            0L
        }
    }

    private fun setupZielButton() {
        jButtonZiel.apply {
            icon = SVGIconUtilities.createSVGIcon("icons/fontawesome/folder-open.svg")
            text = ""
            addActionListener {
                val initialDirectory = (jComboBoxPfad.selectedItem as? String).orEmpty()
                FileDialogs.chooseDirectoryLocation(
                    MediathekGui.ui(),
                    "Film speichern",
                    initialDirectory
                )?.absolutePath?.let { selectedDirectory ->
                    SwingUtilities.invokeLater {
                        jComboBoxPfad.apply {
                            addItem(selectedDirectory)
                            selectedItem = selectedDirectory
                        }
                    }
                }
            }
        }
    }

    private fun fetchFileSizeForQuality(resolution: FilmResolution.Enum): String {
        return runCatching {
            val url = film.getUrlFuerAufloesung(resolution)
            film.getFileSizeForUrl(url)
        }.onFailure { logger.error("Failed to retrieve file size for $resolution", it) }
            .getOrDefault("")
    }

    private fun fetchFileSizeForNormalQuality(): String {
        return runCatching {
            film.getFileSizeForUrl(film.urlNormalQuality)
        }.onFailure { logger.error("Failed to retrieve normal quality size", it) }
            .getOrDefault("")
    }

    private fun launchResolutionFutures(): Job = coroutineScope.launch {
        val fetchSizeBackup = ApplicationConfiguration.getConfiguration()
            .getBoolean(ApplicationConfiguration.DOWNLOAD_FETCH_FILE_SIZE, true)
        ApplicationConfiguration.getConfiguration()
            .setProperty(ApplicationConfiguration.DOWNLOAD_FETCH_FILE_SIZE, true)

        try {
            val hqDeferred = async(Dispatchers.IO) { fetchFileSizeForQuality(FilmResolution.Enum.HIGH_QUALITY) }
            val hochDeferred = async(Dispatchers.IO) { fetchFileSizeForNormalQuality() }
            val kleinDeferred = async(Dispatchers.IO) { fetchFileSizeForQuality(FilmResolution.Enum.LOW) }

            val hqSize = hqDeferred.await()
            val hochSize = hochDeferred.await()
            val kleinSize = kleinDeferred.await()

            withContext(Dispatchers.Swing) {
                if (jRadioButtonAufloesungHd.isEnabled) {
                    dateiGroesseHighQuality = hqSize
                    if (dateiGroesseHighQuality.isNotEmpty()) {
                        jRadioButtonAufloesungHd.text += "   [ $dateiGroesseHighQuality MB ]"
                    }
                }

                dateiGroesseNormalQuality = hochSize
                if (dateiGroesseNormalQuality.isNotEmpty()) {
                    jRadioButtonAufloesungHoch.text += "   [ $dateiGroesseNormalQuality MB ]"
                }

                if (jRadioButtonAufloesungKlein.isEnabled) {
                    dateiGroesseLowQuality = kleinSize
                    if (dateiGroesseLowQuality.isNotEmpty()) {
                        jRadioButtonAufloesungKlein.text += "   [ $dateiGroesseLowQuality MB ]"
                    }
                }
            }
        } catch (ex: Exception) {
            logger.error("Error occurred while fetching file sizes", ex)
        } finally {
            ApplicationConfiguration.getConfiguration()
                .setProperty(ApplicationConfiguration.DOWNLOAD_FETCH_FILE_SIZE, fetchSizeBackup)
        }
    }

    private suspend fun waitForFileSizeJob() {
        try {
            fileSizeJob?.join()
        } catch (ex: CancellationException) {
            logger.warn("File size calculation was cancelled", ex)
        } catch (ex: Exception) {
            logger.error("Error while waiting for file size calculation", ex)
        }
    }

}

private class DialogPositionComponentListener : ComponentAdapter() {

    override fun componentResized(e: ComponentEvent) {
        storeWindowPosition(e)
    }

    override fun componentMoved(e: ComponentEvent) {
        storeWindowPosition(e)
    }

    private fun storeWindowPosition(e: ComponentEvent) {
        val config = ApplicationConfiguration.getConfiguration()
        val component = e.component

        val dims = component.size
        val loc = component.location

        try {
            config.lock(LockMode.WRITE)
            config.setProperty(ApplicationConfiguration.AddDownloadDialog.WIDTH, dims.width)
            config.setProperty(ApplicationConfiguration.AddDownloadDialog.HEIGHT, dims.height)
            config.setProperty(ApplicationConfiguration.AddDownloadDialog.X, loc.x)
            config.setProperty(ApplicationConfiguration.AddDownloadDialog.Y, loc.y)
        } finally {
            config.unlock(LockMode.WRITE)
        }
    }

}