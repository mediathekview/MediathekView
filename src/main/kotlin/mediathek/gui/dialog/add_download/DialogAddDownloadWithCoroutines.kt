/*
 * Copyright (c) 2025-2026 derreisende77.
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

import com.github.kokorin.jaffree.process.JaffreeAbnormalExitException
import kotlinx.coroutines.*
import kotlinx.coroutines.channels.BufferOverflow
import kotlinx.coroutines.channels.awaitClose
import kotlinx.coroutines.flow.*
import kotlinx.coroutines.swing.Swing
import mediathek.config.Daten
import mediathek.config.MVColor
import mediathek.config.MVConfig
import mediathek.daten.*
import mediathek.gui.dialog.download.DownloadQualityLiveInfoText
import mediathek.gui.dialog.download.DownloadQualityResolutionSizeLoadResult
import mediathek.gui.dialog.download.DownloadQualityResolutionSizes
import mediathek.gui.dialog.download.DownloadQualitySupport
import mediathek.gui.messages.DownloadListChangedEvent
import mediathek.mainwindow.MediathekGui
import mediathek.tool.*
import mediathek.tool.MessageBus.messageBus
import org.apache.commons.configuration2.Configuration
import org.apache.commons.configuration2.sync.LockMode
import org.apache.commons.lang3.SystemUtils
import org.apache.logging.log4j.LogManager
import java.awt.*
import java.awt.event.ActionListener
import java.awt.event.ComponentAdapter
import java.awt.event.ComponentEvent
import java.io.File
import java.nio.file.Path
import java.util.*
import javax.swing.*
import javax.swing.event.DocumentEvent
import javax.swing.event.DocumentListener
import javax.swing.text.JTextComponent
import kotlin.coroutines.cancellation.CancellationException
import kotlin.time.Duration.Companion.milliseconds
import kotlin.time.Duration.Companion.seconds

class DialogAddDownloadWithCoroutines(
    parent: Frame,
    private val film: DatenFilm,
    /**
     * The currently selected pSet or null when no selection.
     */
    private var activeProgramSet: DatenPset,
    private val requestedResolution: Optional<FilmResolution.Enum>
) : DialogAddDownload(parent) {
    private sealed interface LiveInfoCommand {
        data object Cancel : LiveInfoCommand
        data class Fetch(val resolution: FilmResolution.Enum) : LiveInfoCommand
    }

    private data class ResolutionButtonLabels(
        val high: String,
        val normal: String,
        val low: String
    )

    private data class StoredDialogPosition(
        val x: Int,
        val y: Int
    )

    private val uiScopeDelegate = lazy(LazyThreadSafetyMode.NONE) {
        CoroutineScope(SupervisorJob() + Dispatchers.Swing)
    }
    private val uiScope by uiScopeDelegate
    private val liveInfoRequests = MutableSharedFlow<LiveInfoCommand>(
        extraBufferCapacity = 1,
        onBufferOverflow = BufferOverflow.DROP_OLDEST
    )
    private var highQualityMandated: Boolean = false
    private var stopBeob = false
    private var nameGeaendert = false
    private var isInitialSizeLookupRunning: Boolean = false
    private var isLiveInfoLookupRunning: Boolean = false
    private var initialSizeLookupStatusMessage: String? = null
    private var initialSizeLookupStatusIsError: Boolean = false
    private var ffprobePath: Path? = null
    private var orgPfad = ""
    private val appConfig get() = ApplicationConfiguration.getConfiguration()
    private val listeSpeichern: ListePset = Daten.getInstance().listePset.listeSpeichern
    private lateinit var resolutionButtonLabels: ResolutionButtonLabels
    private lateinit var cbPathTextComponent: JTextComponent
    private lateinit var datenDownload: DatenDownload
    private var resolutionSizes = DownloadQualityResolutionSizes()

    companion object {
        private val logger = LogManager.getLogger()
        private const val MINIMUM_DIALOG_WIDTH = 720
        private const val TITLED_BORDER_STRING = "Download-Qualität"
        private const val KEY_LABEL_FOREGROUND: String = "Label.foreground"
        private const val KEY_TEXTFIELD_BACKGROUND: String = "TextField.background"
        private val INITIAL_SIZE_LOOKUP_TIMEOUT = 20.seconds
        private val LIVE_INFO_PLACEHOLDER = DownloadQualityLiveInfoText(
            video = "Video: 1920x1080, 2220 kBit/s, 50 fps (avg), H.264",
            audio = "Audio: 48000 Hz, 128 kBit/s, AAC (Advanced Audio Coding)"
        )

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
        configureWindowDefaults()
        initializeUi()
        bindUi()
        initializeDialogSize(parent)
        registerWindowPositionTracking()
        startCoroutineBindings()
        btnDownloadImmediately.requestFocus()
    }

    override fun dispose() {
        if (uiScopeDelegate.isInitialized()) {
            uiScope.cancel()
        }
        super.dispose()
    }

    private fun configureWindowDefaults() {
        rootPane.defaultButton = btnDownloadImmediately
        EscapeKeyHandler.installHandler(this) { dispose() }
    }

    private fun initializeUi() {
        setupBusyIndicator()
        stabilizeLiveInfoArea()
        detectFfprobeExecutable()
        setupZielButton()
        setupPSetComboBox()
        resolutionButtonLabels = ResolutionButtonLabels(
            high = jRadioButtonAufloesungHd.text,
            normal = jRadioButtonAufloesungHoch.text,
            low = jRadioButtonAufloesungKlein.text
        )
        setupFilmHeader()
        setupFilmQualityRadioButtons()
        setupDeleteHistoryButton()
        setupPfadSpeichernCheckBox()
        applySelectedProgramSet()
        resetNameFieldValidationColor()
        setupPathEditor()
        constrainEditableFieldWidthsForPacking()
        nameGeaendert = false
    }

    private fun bindUi() {
        btnDownloadImmediately.addActionListener { prepareDownload(true) }
        btnQueueDownload.addActionListener { prepareDownload(false) }
        jButtonAbbrechen.addActionListener { dispose() }
        btnRequestLiveInfo.addActionListener {
            liveInfoRequests.tryEmit(LiveInfoCommand.Fetch(getFilmResolution()))
        }
    }

    private fun initializeDialogSize(parent: Frame) {
        updateMinimumSizeFromPackedLayout()
        constrainPackedSizeToScreen()
        removeStoredWindowSizeFromConfig()
        restoreWindowPositionFromConfig(parent)
    }

    private fun registerWindowPositionTracking() {
        addComponentListener(DialogPositionComponentListener(appConfig))
    }

    private fun startCoroutineBindings() {
        startLiveInfoCollector()
        startPathObservation()
        startNameObservation()
        loadInitialBackgroundData()
    }

    private fun startLiveInfoCollector() {
        uiScope.launch {
            liveInfoRequests.collectLatest { command ->
                when (command) {
                    LiveInfoCommand.Cancel -> resetLiveInfoDisplay()
                    is LiveInfoCommand.Fetch -> fetchLiveFilmInfo(command.resolution)
                }
            }
        }
    }

    private fun loadInitialBackgroundData() {
        uiScope.launch {
            isInitialSizeLookupRunning = true
            updateBusyIndicator()
            btnRequestLiveInfo.isEnabled = false

            try {
                val sizeLoadResult = withTimeoutOrNull(INITIAL_SIZE_LOOKUP_TIMEOUT) {
                    loadResolutionSizes()
                }

                if (sizeLoadResult == null) {
                    showInitialSizeLookupMessage("Dateigröße konnte nicht rechtzeitig ermittelt werden.")
                } else {
                    applyResolutionSizes(sizeLoadResult.sizes)
                    if (sizeLoadResult.sizes.high.isEmpty() &&
                        sizeLoadResult.sizes.normal.isEmpty() &&
                        sizeLoadResult.sizes.low.isEmpty()
                    ) {
                        if (sizeLoadResult.httpStatusCode == 403 || sizeLoadResult.httpStatusCode == 404) {
                            showInitialSizeLookupError("HTTP ${sizeLoadResult.httpStatusCode} beim Ermitteln der Dateigröße.")
                        } else {
                            showInitialSizeLookupMessage("Dateigröße konnte nicht ermittelt werden.")
                        }
                    } else {
                        clearInitialSizeLookupMessage()
                    }
                }

                calculateAndCheckDiskSpaceAsync()
            } finally {
                isInitialSizeLookupRunning = false
                updateBusyIndicator()
                btnRequestLiveInfo.isEnabled = ffprobePath != null
            }
        }
    }

    private fun stabilizeLiveInfoArea() {
        val originalText = DownloadQualityLiveInfoText(lblStatus.text, lblAudioInfo.text)
        showLiveInfo(LIVE_INFO_PLACEHOLDER)

        lblStatus.preferredSize = lblStatus.preferredSize
        lblStatus.minimumSize = lblStatus.preferredSize
        lblAudioInfo.preferredSize = lblAudioInfo.preferredSize
        lblAudioInfo.minimumSize = lblAudioInfo.preferredSize

        showLiveInfo(originalText)
    }

    private fun updateMinimumSizeFromPackedLayout() {
        pack()
        minimumSize = Dimension(
            MINIMUM_DIALOG_WIDTH.coerceAtLeast(size.width),
            size.height
        )
    }

    private fun constrainPackedSizeToScreen() {
        val usableBounds = getUsableScreenBounds()
        val boundedWidth = size.width.coerceAtMost(usableBounds.width)
        val boundedHeight = size.height.coerceAtMost(usableBounds.height)
        if (boundedWidth != size.width || boundedHeight != size.height) {
            size = Dimension(boundedWidth, boundedHeight)
        }
        minimumSize = Dimension(
            minimumSize.width.coerceAtMost(usableBounds.width),
            minimumSize.height.coerceAtMost(usableBounds.height)
        )
    }

    private fun restoreWindowPositionFromConfig(parent: Frame) {
        val storedPosition = readStoredDialogPosition()
        if (storedPosition == null) {
            setLocationRelativeTo(parent)
        } else {
            applyStoredPosition(storedPosition)
        }
    }

    private fun removeStoredWindowSizeFromConfig() {
        appConfig.withLock(LockMode.WRITE) {
            clearProperty(ApplicationConfiguration.AddDownloadDialog.WIDTH)
            clearProperty(ApplicationConfiguration.AddDownloadDialog.HEIGHT)
        }
    }

    private fun readStoredDialogPosition(): StoredDialogPosition? {
        return try {
            appConfig.withLock(LockMode.READ) {
                StoredDialogPosition(
                    x = getInt(ApplicationConfiguration.AddDownloadDialog.X),
                    y = getInt(ApplicationConfiguration.AddDownloadDialog.Y)
                )
            }
        } catch (_: NoSuchElementException) {
            null
        }
    }

    private fun applyStoredPosition(position: StoredDialogPosition) {
        val usableBounds = getUsableScreenBounds()
        val boundedWidth = width.coerceAtMost(usableBounds.width)
        val boundedHeight = height.coerceAtMost(usableBounds.height)
        val maxX = usableBounds.x + usableBounds.width - boundedWidth
        val maxY = usableBounds.y + usableBounds.height - boundedHeight
        val boundedX = position.x.coerceIn(usableBounds.x, maxX)
        val boundedY = position.y.coerceIn(usableBounds.y, maxY)
        setLocation(boundedX, boundedY)
    }

    private fun getUsableScreenBounds(): Rectangle {
        val maximumWindowBounds = GraphicsEnvironment.getLocalGraphicsEnvironment().maximumWindowBounds
        return graphicsConfiguration?.bounds?.intersection(maximumWindowBounds) ?: maximumWindowBounds
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
    private fun applySelectedProgramSet() {
        activeProgramSet = listeSpeichern[jComboBoxPset.getSelectedIndex()]
        selectResolution()
        updateSubtitleCheckbox()
        updateInfoFileCreationCheckBox()
        setNameFilm()
    }

    private fun selectResolution() {
        requestedResolution.ifPresent { highQualityMandated = it == FilmResolution.Enum.HIGH_QUALITY }

        when {
            highQualityMandated || isHighQualityRequested() -> jRadioButtonAufloesungHd.isSelected = true
            isLowQualityRequested() -> jRadioButtonAufloesungKlein.isSelected = true
            else -> jRadioButtonAufloesungHoch.isSelected = true
        }
    }

    private fun updateInfoFileCreationCheckBox() {
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
        return activeProgramSet.aufloesung == FilmResolution.Enum.LOW &&
                film.lowQualityUrl.isNotEmpty()
    }

    private fun isHighQualityRequested(): Boolean {
        return activeProgramSet.aufloesung == FilmResolution.Enum.HIGH_QUALITY
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

    private fun updateSubtitleCheckbox() {
        if (!film.hasSubtitle()) {
            jCheckBoxSubtitle.setEnabled(false)
        } else {
            jCheckBoxSubtitle.setSelected(activeProgramSet.shouldDownloadSubtitle())
        }
    }

    private fun setNameFilm() {
        pausePathObservation {
            datenDownload = DatenDownload(
                activeProgramSet,
                film,
                DatenDownload.QUELLE_DOWNLOAD,
                null,
                "",
                "",
                getFilmResolution().toString()
            )

            applyDownloadTargetFields(datenDownload)
        }
    }

    private fun applyDownloadTargetFields(download: DatenDownload) {
        val generatedName = download.arr[DatenDownload.DOWNLOAD_ZIEL_DATEINAME]
        if (generatedName.isEmpty()) {
            // dann wird nicht gespeichert → eigentlich falsche Seteinstellungen?
            setTargetInputsEnabled(false)
            jTextFieldName.text = ""
            jComboBoxPfad.model = DefaultComboBoxModel(arrayOf(""))
            return
        }

        setTargetInputsEnabled(true)
        val targetPath = download.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD]
        setModelPfad(targetPath, jComboBoxPfad)
        orgPfad = targetPath
        if (!nameGeaendert) {
            jTextFieldName.text = generatedName
        }
    }

    private fun setTargetInputsEnabled(enabled: Boolean) {
        jTextFieldName.isEnabled = enabled
        jComboBoxPfad.isEnabled = enabled
        jButtonZiel.isEnabled = enabled
    }

    private fun addDownloadToQueue(startAutomatically: Boolean) {
        Daten.getInstance().listeDownloads.addMitNummer(datenDownload)
        messageBus.publishAsync(DownloadListChangedEvent())

        if (startAutomatically) {
            datenDownload.startDownload()
        }
    }

    private fun getFilmSize(): String {
        return resolutionSizes.forResolution(getFilmResolution())
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

        return if (GuiFunktionenProgramme.checkPathWriteableWithoutCreating(pfad)) {
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
            resetLiveInfoDisplay()
            liveInfoRequests.tryEmit(LiveInfoCommand.Cancel)
        }
        configureResolutionButton(
            button = jRadioButtonAufloesungHd,
            enabled = film.highQualityUrl.isNotEmpty(),
            listener = listener
        )
        configureResolutionButton(
            button = jRadioButtonAufloesungKlein,
            enabled = film.lowQualityUrl.isNotEmpty(),
            listener = listener
        )
        configureResolutionButton(
            button = jRadioButtonAufloesungHoch,
            listener = listener
        )
        jRadioButtonAufloesungHoch.isSelected = true
    }

    private fun setupPSetComboBox() {
        val model = DefaultComboBoxModel(listeSpeichern.getObjectDataCombo())
        jComboBoxPset.apply {
            // disable when only one entry...
            setEnabled(listeSpeichern.size > 1)
            setModel(model)
            setSelectedItem(activeProgramSet.name)
            addActionListener { applySelectedProgramSet() }
        }
    }

    private fun setupFilmHeader() {
        lblSenderIcon.setMaxIconSize(Dimension(64, 64))
        lblSenderIcon.setSender(film.sender)
        updateSenderLabelSizing()
        lblFilmTitle.text = film.title
        lblFilmTitle.toolTipText = film.title
        lblFilmThema.text = film.thema
        lblFilmThema.toolTipText = film.thema
        constrainHeaderLabelWidthForPacking(lblFilmTitle)
        constrainHeaderLabelWidthForPacking(lblFilmThema)
    }

    private fun updateSenderLabelSizing() {
        if (lblSenderIcon.icon != null) {
            val iconSize = lblSenderIcon.preferredSize
            lblSenderIcon.minimumSize = iconSize
            lblSenderIcon.preferredSize = iconSize
            lblSenderIcon.maximumSize = iconSize
            return
        }

        val preferredSize = lblSenderIcon.preferredSize
        val preferredHeight = preferredSize.height
        lblSenderIcon.minimumSize = Dimension(0, preferredHeight)
        lblSenderIcon.preferredSize = preferredSize
        lblSenderIcon.maximumSize = Dimension(Int.MAX_VALUE, preferredHeight)
    }

    private fun constrainHeaderLabelWidthForPacking(label: JLabel) {
        val preferredHeight = label.preferredSize.height
        val constrainedSize = Dimension(0, preferredHeight)
        label.minimumSize = constrainedSize
        label.preferredSize = constrainedSize
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
        jCheckBoxPfadSpeichern.apply {
            setSelected(appConfig.getBoolean(ApplicationConfiguration.DOWNLOAD_SHOW_LAST_USED_PATH, true))
            addActionListener {
                appConfig.setProperty(
                    ApplicationConfiguration.DOWNLOAD_SHOW_LAST_USED_PATH,
                    jCheckBoxPfadSpeichern.isSelected
                )
            }
        }
    }

    private fun detectFfprobeExecutable() {
        ffprobePath = DownloadQualitySupport.findFfprobeExecutableDirectory()
        if (ffprobePath == null) {
            lblBusyIndicator.apply {
                isVisible = true
                isBusy = false
                setText("Hilfsprogramm nicht gefunden!")
                setForeground(Color.RED)
            }
            btnRequestLiveInfo.setEnabled(false)
        }
    }

    private fun setupBusyIndicator() {
        lblBusyIndicator.apply {
            text = ""
            isBusy = false
            isVisible = false
        }
        restoreStatusArea()
    }

    private suspend fun fetchLiveFilmInfo(resolution: FilmResolution.Enum) {
        val executablePath = ffprobePath ?: return

        isLiveInfoLookupRunning = true
        btnRequestLiveInfo.isEnabled = false
        updateBusyIndicator()
        showLiveInfo(DownloadQualityLiveInfoText())

        try {
            val result = runInterruptible(Dispatchers.IO) {
                DownloadQualitySupport.fetchLiveInfo(executablePath, film, resolution)
            }

            showLiveInfo(result)
        } catch (_: CancellationException) {
            showLiveInfo(DownloadQualityLiveInfoText())
        } catch (ex: JaffreeAbnormalExitException) {
            showLiveInfoError(DownloadQualitySupport.getLiveInfoErrorString(ex))
        } catch (_: Exception) {
            showLiveInfoError("Unbekannter Fehler aufgetreten.")
        } finally {
            isLiveInfoLookupRunning = false
            if (isInitialSizeLookupRunning) {
                updateBusyIndicator()
            } else {
                hideBusyIndicator()
            }
            btnRequestLiveInfo.isEnabled = ffprobePath != null && !isInitialSizeLookupRunning
        }
    }

    private fun updateBusyIndicator() {
        lblBusyIndicator.apply {
            when {
                isLiveInfoLookupRunning -> {
                    isVisible = true
                    isBusy = true
                    text = ""
                    showBusyStatusMessage("Codec-Details werden geladen...")
                }

                isInitialSizeLookupRunning -> {
                    isVisible = true
                    isBusy = true
                    text = ""
                    showBusyStatusMessage("Dateigrößen werden geladen...")
                }

                else -> {
                    text = ""
                    isVisible = false
                    isBusy = false
                    restoreStatusArea()
                }
            }
        }
    }

    private fun resetLiveInfoDisplay() {
        if (!isInitialSizeLookupRunning) {
            isLiveInfoLookupRunning = false
        }
        updateBusyIndicator()
        showLiveInfo(DownloadQualityLiveInfoText())
    }

    private fun hideBusyIndicator() {
        lblBusyIndicator.apply {
            text = ""
            isVisible = false
            isBusy = false
        }
    }

    private fun showLiveInfo(liveInfoText: DownloadQualityLiveInfoText) {
        val labelForeground = UIManager.getColor(KEY_LABEL_FOREGROUND)
        lblStatus.foreground = labelForeground
        lblAudioInfo.foreground = labelForeground
        lblStatus.text = liveInfoText.video
        lblAudioInfo.text = liveInfoText.audio
    }

    private fun showLiveInfoError(message: String) {
        lblStatus.foreground = Color.RED
        lblStatus.text = message
        lblAudioInfo.foreground = UIManager.getColor(KEY_LABEL_FOREGROUND)
        lblAudioInfo.text = ""
    }

    private fun showInitialSizeLookupMessage(message: String) {
        initialSizeLookupStatusMessage = message
        initialSizeLookupStatusIsError = false
        restoreStatusArea()
    }

    private fun showInitialSizeLookupError(message: String) {
        initialSizeLookupStatusMessage = message
        initialSizeLookupStatusIsError = true
        restoreStatusArea()
    }

    private fun clearInitialSizeLookupMessage() {
        initialSizeLookupStatusMessage = null
        initialSizeLookupStatusIsError = false
        restoreStatusArea()
    }

    private fun showBusyStatusMessage(message: String) {
        lblStatus.foreground = UIManager.getColor(KEY_LABEL_FOREGROUND)
        lblStatus.text = message
        lblAudioInfo.foreground = UIManager.getColor(KEY_LABEL_FOREGROUND)
        lblAudioInfo.text = ""
    }

    private fun restoreStatusArea() {
        when {
            isLiveInfoLookupRunning -> return
            initialSizeLookupStatusMessage != null -> {
                lblStatus.foreground = if (initialSizeLookupStatusIsError) Color.RED else Color(180, 110, 0)
                lblStatus.text = initialSizeLookupStatusMessage
                lblAudioInfo.foreground = UIManager.getColor(KEY_LABEL_FOREGROUND)
                lblAudioInfo.text = ""
            }

            else -> {
                showLiveInfo(DownloadQualityLiveInfoText())
            }
        }
    }

    /**
     * Calculate free disk space on volume and check if the movies can be safely downloaded.
     */
    private fun calculateAndCheckDiskSpace(usableSpaceBytes: Long) {
        resetResolutionButtonForegrounds()

        try {
            val filmBorder = jPanelSize.border as? javax.swing.border.TitledBorder ?: return
            var usableSpace = usableSpaceBytes

            filmBorder.title = DownloadQualitySupport.qualityPanelTitle(TITLED_BORDER_STRING, usableSpace)

            // Border needs to be repainted after update...
            jPanelSize.repaint()

            // jetzt noch prüfen, obs auf die Platte passt
            usableSpace /= FileSize.ONE_MiB
            if (usableSpace > 0) {
                markResolutionsExceedingDiskSpace(usableSpace)
            }
        } catch (ex: Exception) {
            logger.error("calculateAndCheckDiskSpace()", ex)
        }
    }

    private fun resetResolutionButtonForegrounds() {
        UIManager.getColor(KEY_LABEL_FOREGROUND)?.let { fgColor ->
            jRadioButtonAufloesungHd.foreground = fgColor
            jRadioButtonAufloesungHoch.foreground = fgColor
            jRadioButtonAufloesungKlein.foreground = fgColor
        }
    }

    private fun markResolutionsExceedingDiskSpace(usableSpaceInMiB: Long) {
        val radioButtonsWithSizes = listOf(
            jRadioButtonAufloesungHd to resolutionSizes.high,
            jRadioButtonAufloesungHoch to resolutionSizes.normal,
            jRadioButtonAufloesungKlein to resolutionSizes.low
        )
        for ((button, sizeText) in radioButtonsWithSizes) {
            val size = sizeText.toIntOrNull() ?: continue
            if (size > usableSpaceInMiB) {
                button.foreground = Color.RED
            }
        }
    }

    private suspend fun calculateAndCheckDiskSpaceAsync(currentPath: String = cbPathTextComponent.text) {
        val usableSpace = withContext(Dispatchers.IO) {
            DownloadQualitySupport.getFreeDiskSpace(currentPath)
        }
        calculateAndCheckDiskSpace(usableSpace)
    }

    private fun setupPathEditor() {
        cbPathTextComponent = jComboBoxPfad.editor.editorComponent as JTextComponent
        cbPathTextComponent.isOpaque = true
    }

    private fun constrainEditableFieldWidthsForPacking() {
        constrainComponentWidthForPacking(jTextFieldName)
        constrainComponentWidthForPacking(jComboBoxPfad)
        constrainComponentWidthForPacking(cbPathTextComponent)
    }

    private fun constrainComponentWidthForPacking(component: JComponent) {
        val preferredHeight = component.preferredSize.height
        val constrainedSize = Dimension(0, preferredHeight)
        component.minimumSize = constrainedSize
        component.preferredSize = constrainedSize
    }

    @OptIn(FlowPreview::class)
    private fun startPathObservation() {
        uiScope.launch {
            cbPathTextComponent.textChanges()
                .drop(1)
                .debounce(250.milliseconds)
                .distinctUntilChanged()
                .collectLatest(::handlePathChange)
        }
    }

    @OptIn(FlowPreview::class)
    private fun startNameObservation() {
        uiScope.launch {
            jTextFieldName.textChanges()
                .drop(1)
                .debounce(100.milliseconds)
                .distinctUntilChanged()
                .collect(::handleNameChange)
        }
    }

    private suspend fun handlePathChange(currentPath: String) {
        if (stopBeob) {
            return
        }
        if (!SystemUtils.IS_OS_WINDOWS) {
            updatePathValidationColor(currentPath)
        }
        calculateAndCheckDiskSpaceAsync(currentPath)
    }

    private fun handleNameChange(currentName: String) {
        if (stopBeob) {
            return
        }
        nameGeaendert = true
        updateValidationBackground(
            component = jTextFieldName,
            isValid = currentName == FilenameUtils.checkFilenameForIllegalCharacters(currentName, false)
        )
    }

    private fun JTextComponent.textChanges(): Flow<String> = callbackFlow {
        val currentDocument = document
        val listener = object : DocumentListener {
            override fun insertUpdate(e: DocumentEvent?) {
                trySend(text)
            }

            override fun removeUpdate(e: DocumentEvent?) {
                trySend(text)
            }

            override fun changedUpdate(e: DocumentEvent?) {
                trySend(text)
            }
        }

        currentDocument.addDocumentListener(listener)
        trySend(text)
        awaitClose { currentDocument.removeDocumentListener(listener) }
    }

    private fun updatePathValidationColor(filePath: String) {
        val editor = jComboBoxPfad.editor.editorComponent
        updateValidationBackground(
            component = editor as JComponent,
            isValid = filePath == FilenameUtils.checkFilenameForIllegalCharacters(filePath, true)
        )
    }

    private fun resetNameFieldValidationColor() {
        jTextFieldName.background = UIManager.getDefaults().getColor(KEY_TEXTFIELD_BACKGROUND)
    }

    private fun updateValidationBackground(component: JComponent, isValid: Boolean) {
        component.background = if (isValid) {
            UIManager.getColor(KEY_TEXTFIELD_BACKGROUND)
        } else {
            MVColor.DOWNLOAD_FEHLER.color
        }
    }

    private fun pausePathObservation(block: () -> Unit) {
        stopBeob = true
        try {
            block()
        } finally {
            stopBeob = false
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

    private suspend fun loadResolutionSizes(): DownloadQualityResolutionSizeLoadResult {
        return try {
            withContext(Dispatchers.IO) { DownloadQualitySupport.loadResolutionSizeResult(film) }
        } catch (ex: CancellationException) {
            throw ex
        } catch (ex: Exception) {
            logger.error("Error occurred while fetching file sizes", ex)
            DownloadQualityResolutionSizeLoadResult()
        }
    }

    private fun applyResolutionSizes(sizes: DownloadQualityResolutionSizes) {
        resolutionSizes = sizes
        jRadioButtonAufloesungHd.text = formatResolutionLabel(
            resolutionButtonLabels.high,
            sizes.high.takeIf { jRadioButtonAufloesungHd.isEnabled }
        )
        jRadioButtonAufloesungHoch.text = formatResolutionLabel(
            resolutionButtonLabels.normal,
            sizes.normal
        )
        jRadioButtonAufloesungKlein.text = formatResolutionLabel(
            resolutionButtonLabels.low,
            sizes.low.takeIf { jRadioButtonAufloesungKlein.isEnabled }
        )
    }

    private fun formatResolutionLabel(baseLabel: String, sizeInMb: String?): String {
        return DownloadQualitySupport.formatResolutionLabel(baseLabel, sizeInMb)
    }

    private fun configureResolutionButton(
        button: JRadioButton,
        enabled: Boolean = true,
        listener: ActionListener
    ) {
        button.isEnabled = enabled
        button.addActionListener(listener)
    }
}

private class DialogPositionComponentListener(private val config: Configuration) : ComponentAdapter() {
    override fun componentMoved(e: ComponentEvent) {
        config.withLock(LockMode.WRITE) {
            val location = e.component.location
            setProperty(ApplicationConfiguration.AddDownloadDialog.X, location.x)
            setProperty(ApplicationConfiguration.AddDownloadDialog.Y, location.y)
        }
    }
}
