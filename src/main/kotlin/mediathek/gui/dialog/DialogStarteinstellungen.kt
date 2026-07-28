package mediathek.gui.dialog

import kotlinx.coroutines.*
import kotlinx.coroutines.swing.Swing
import mediathek.config.application.ApplicationConfiguration
import mediathek.daten.DatenPset
import mediathek.daten.ListePsetVorlagen
import mediathek.daten.ProgramSetRepository
import mediathek.daten.ProgramSetTemplateResolver
import mediathek.daten.blacklist.BlacklistServices
import mediathek.gui.dialogEinstellungen.PanelEinstellungenGeo
import mediathek.gui.dialogEinstellungen.PanelProgrammPfade
import mediathek.gui.dialogEinstellungen.pset.PanelPsetKurz
import mediathek.gui.dialogEinstellungen.pset.PanelPsetLang
import mediathek.tool.GuiFunktionenProgramme
import org.apache.commons.lang3.SystemUtils
import org.pushingpixels.radiance.swing.ktx.addDelayedWindowListener
import java.awt.BorderLayout
import java.awt.Component
import java.util.function.BiConsumer
import javax.swing.JFrame
import kotlin.coroutines.CoroutineContext

class DialogStarteinstellungen(
    parent: JFrame?,
    private val programSets: ProgramSetRepository,
    private val blacklist: BlacklistServices,
    private val programSetExporter: BiConsumer<Array<DatenPset>, String>,
) : DialogStarteinstellungenBase(parent), CoroutineScope {
    private enum class State { START, PFAD, PSET, FERTIG }

    enum class ResultCode {
        SUCCESS,
        CANCELLED,
    }

    override val coroutineContext: CoroutineContext = SupervisorJob() + Dispatchers.Swing

    private var status = State.START
    private val parentComponent = parent
    private var anpassen = false
    private var transitionJob: Job? = null

    init {
        initializeDialog()
    }

    fun showDialog(): ResultCode {
        isVisible = true
        return if (status == State.FERTIG) ResultCode.SUCCESS else ResultCode.CANCELLED
    }

    override fun dispose() {
        transitionJob?.cancel()
        cancel()
        super.dispose()
    }

    private fun initializeDialog() {
        installActions()
        configureInitialState()
        installDefaultProgramPaths()
        createLayout()
        updateInitialAvailability()
        addDelayedWindowListener(onWindowOpened = { toFront() })
    }

    private fun installActions() {
        jButtonStandard.addActionListener { launchAdvance() }
        jButtonAnpassen.addActionListener {
            anpassen = true
            launchAdvance()
        }
        jCheckBoxAlleEinstellungen.addActionListener {
            status = State.PSET
            launchAdvance()
        }
    }

    private fun configureInitialState() {
        jButtonStandard.text = START_WITH_DEFAULTS_TEXT
        jCheckBoxAlleEinstellungen.isVisible = false
    }

    private fun installDefaultProgramPaths() {
        ApplicationConfiguration.getInstance().standardVlcPath = GuiFunktionenProgramme.getMusterPfadVlc()
        ApplicationConfiguration.getInstance().standardFFmpegPath = GuiFunktionenProgramme.getMusterPfadFFmpeg()
    }

    private fun createLayout() {
        val panelEinstellungenGeo = PanelEinstellungenGeo(parentComponent, true, blacklist)
        jPanelExtra.layout = BorderLayout()
        jPanelExtra.add(panelEinstellungenGeo, BorderLayout.CENTER)
    }

    private fun updateInitialAvailability() {
        if (hasMissingProgramPath()) {
            jButtonStandard.isEnabled = false
            anpassen = true
        }
    }

    private fun launchAdvance() {
        if (transitionJob?.isActive == true) {
            return
        }

        transitionJob = launch {
            try {
                weiter()
            } catch (_: CancellationException) {
                // Dialog disposal is part of the normal lifecycle.
            } finally {
                transitionJob = null
            }
        }
    }

    private suspend fun weiter() {
        jButtonStandard.isEnabled = true
        when (status) {
            State.START -> statusStart()
            State.PFAD -> statusPfade()
            State.PSET -> statusPset()
            State.FERTIG -> beenden()
        }
    }

    private suspend fun statusStart() {
        setContinueButtonText()

        status = when {
            hasMissingProgramPath() -> State.PFAD
            anpassen -> State.PFAD
            addStandardSetWithNavigationLock(parentComponent) -> State.FERTIG
            else -> State.PSET
        }

        weiter()
    }

    private fun statusPfade() {
        hideAdjustmentControls()
        val searchFfmpeg = !SystemUtils.IS_OS_MAC_OSX && !SystemUtils.IS_OS_WINDOWS
        setMainContent(PanelProgrammPfade(parentComponent, true, searchFfmpeg))

        status = State.PSET
        setContinueButtonText()
    }

    private suspend fun statusPset() {
        jButtonAnpassen.isVisible = false
        jCheckBoxAlleEinstellungen.isVisible = true
        if (programSets.list.isEmpty()) {
            addStandardSetWithNavigationLock(parentComponent)
        }

        if (jCheckBoxAlleEinstellungen.isSelected) {
            setMainContent(PanelPsetLang(parentComponent, programSets, programSets.list, programSetExporter))
        } else {
            setMainContent(PanelPsetKurz(parentComponent, programSets.list))
        }
        status = State.FERTIG
        setContinueButtonText()
    }

    private suspend fun addStandardSet(parent: JFrame?): Boolean {
        val pSet = withContext(Dispatchers.IO) {
            ListePsetVorlagen.getStandarset(parent, false)
        } ?: return false

        ProgramSetTemplateResolver.replaceTemplates(parent, pSet)
        programSets.addProgramSets(pSet)
        ApplicationConfiguration.getInstance().standardProgramSetVersion = pSet.version
        return true
    }

    private suspend fun addStandardSetWithNavigationLock(parent: JFrame?): Boolean {
        setNavigationEnabled(false)
        return try {
            addStandardSet(parent)
        } finally {
            setNavigationEnabled(true)
        }
    }

    private fun beenden() {
        dispose()
    }

    private fun hasMissingProgramPath(): Boolean =
        ApplicationConfiguration.getInstance().standardVlcPath.isEmpty() ||
            ApplicationConfiguration.getInstance().standardFFmpegPath.isEmpty()

    private fun setContinueButtonText() {
        jButtonStandard.text = CONTINUE_TEXT
    }

    private fun hideAdjustmentControls() {
        jButtonAnpassen.isVisible = false
        jCheckBoxAlleEinstellungen.isVisible = false
    }

    private fun setMainContent(component: Component) {
        jScrollPane1.setViewportView(component)
    }

    private fun setNavigationEnabled(enabled: Boolean) {
        jButtonStandard.isEnabled = enabled
        jButtonAnpassen.isEnabled = enabled
        jCheckBoxAlleEinstellungen.isEnabled = enabled
    }

    companion object {
        private const val CONTINUE_TEXT = "Weiter"
        private const val START_WITH_DEFAULTS_TEXT = "Mit Standardeinstellungen starten"
    }
}
