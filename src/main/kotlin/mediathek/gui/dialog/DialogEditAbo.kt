package mediathek.gui.dialog

import mediathek.config.Daten
import mediathek.config.Konstanten
import mediathek.config.MVColor
import mediathek.daten.abo.AboTags
import mediathek.daten.abo.DatenAbo
import mediathek.daten.abo.FilmLengthState
import mediathek.tool.*
import mediathek.tool.datum.DateUtil
import java.awt.Color
import javax.swing.*
import javax.swing.border.CompoundBorder
import javax.swing.border.EmptyBorder
import javax.swing.event.DocumentEvent
import javax.swing.event.DocumentListener
import javax.swing.text.JTextComponent

class DialogEditAbo(
    parent: JFrame,
    private val aktAbo: DatenAbo,
    private val isMultiEditMode: Boolean,
) : DialogEditAboView(parent) {
    private val emptyBorder = EmptyBorder(5, 5, 5, 5)

    /**
     * This determines in multi edit mode, which fields should be applied to all selected abos...
     */
    val multiEditCbIndices = BooleanArray(AboTags.entries.size)

    /**
     * Determines whether the whole edit operation was "ok" -> successful or not.
     */
    private var ok = false

    init {
        val daten = Daten.getInstance()
        configureFilmLengthButtons()
        configureComboBoxes(daten)
        configurePathValidation(daten)
        configureActions(parent)

        initializeExtraPanel()
        pack()

        GuiFunktionen.centerOnScreen(this, false)
    }

    fun successful(): Boolean = ok

    private fun configureFilmLengthButtons() {
        ButtonGroup().apply {
            add(rbMin)
            add(rbMax)
        }
    }

    private fun configureComboBoxes(daten: Daten) {
        jScrollPane1.verticalScrollBar.unitIncrement = 16
        comboboxPSet.model = DefaultComboBoxModel(daten.listePset.listeAbo.objectDataCombo)
        comboboxSender.model = SenderListComboBoxModel()
    }

    private fun configurePathValidation(daten: Daten) {
        val pfade = daten.listeAbo.pfade
        if (!pfade.contains(aktAbo.zielpfad)) {
            pfade.add(0, aktAbo.zielpfad)
        }
        comboboxPfad.model = DefaultComboBoxModel(pfade.toTypedArray())
        comboboxPfad.isEditable = true
        checkPfad()

        val editorComp = comboboxPfad.editor.editorComponent as JTextComponent
        editorComp.isOpaque = true
        editorComp.document.addDocumentListener(CheckPathDocListener())
    }

    private fun configureActions(parent: JFrame) {
        jButtonBeenden.addActionListener {
            if (applyIfValid()) {
                dispose()
            } else {
                MVMessageDialog.showMessageDialog(parent, "Filter angeben!", "Leeres Abo", JOptionPane.ERROR_MESSAGE)
            }
        }
        jButtonAbbrechen.addActionListener { dispose() }
        rootPane.defaultButton = jButtonBeenden

        EscapeKeyHandler.installHandler(this, this::dispose)

        jButtonHelp.addActionListener {
            val msg = GetFile.getHilfeSuchen(Konstanten.PFAD_HILFETEXT_DIALOG_ADD_ABO).trim()
            DialogHilfe(this, true, msg).isVisible = true
        }
    }

    private fun checkPfad() {
        val s = (comboboxPfad.editor.editorComponent as JTextComponent).text
        val editor = comboboxPfad.editor.editorComponent
        if (s != FilenameUtils.checkFilenameForIllegalCharacters(s, false)) {
            editor.background = MVColor.DOWNLOAD_FEHLER.color
        } else {
            editor.background = UIManager.getColor(TEXTFIELD_BACKGROUND)
        }
    }

    private fun initializeExtraPanel() {
        initializeExtraComponents()
    }

    private fun initializeExtraComponents() {
        configureMultiEditControls()
        styleSearchLabels()
        bindAboToFields()
    }

    private fun configureMultiEditControls() {
        val border = CompoundBorder(BorderFactory.createLineBorder(Color(204, 204, 255), 4, true), emptyBorder)
        labelMultiEditHeader.border = border
        labelMultiEditHeader.isVisible = isMultiEditMode

        initializeMultiEditCheckBox(checkBoxMultiEditEingeschaltet, AboTags.EINGESCHALTET)
        initializeMultiEditCheckBox(checkBoxMultiEditMin, AboTags.MIN)
        initializeMultiEditCheckBox(checkBoxMultiEditMindestdauer, AboTags.MINDESTDAUER)
        initializeMultiEditCheckBox(checkBoxMultiEditZielpfad, AboTags.ZIELPFAD)
        initializeMultiEditCheckBox(checkBoxMultiEditPSet, AboTags.PSET)
        initializeMultiEditCheckBox(checkBoxMultiEditDoNotStartAutomatically, AboTags.DO_NOT_START_AUTOMATICALLY)
    }

    private fun styleSearchLabels() {
        labelSender.foreground = MVColor.getBlueColor()
        labelThema.foreground = MVColor.getBlueColor()
        labelTitel.foreground = MVColor.getBlueColor()
        labelThemaTitel.foreground = MVColor.getBlueColor()
        labelIrgendwo.foreground = MVColor.getBlueColor()
    }

    private fun bindAboToFields() {
        val nr = aktAbo.nr
        labelNrValue.text = if (nr > 0) {
            aktAbo.nr.toString()
        } else {
            "noch nicht vergeben"
        }

        checkBoxEingeschaltet.isSelected = aktAbo.isActive
        textFieldName.text = aktAbo.name
        textFieldName.document.addDocumentListener(EmptyTextDocListener(textFieldName))
        comboboxSender.selectedItem = aktAbo.sender
        textFieldThema.text = aktAbo.thema
        textFieldTitel.text = aktAbo.title
        textFieldThemaTitel.text = aktAbo.themaTitel
        textFieldIrgendwo.text = aktAbo.irgendwo

        bindDurationControls()
        bindPathAndProgramSetFields()
        checkBoxDoNotStartAutomatically.isSelected = aktAbo.isDoNotStartAutomatically
    }

    private fun bindDurationControls() {
        val minDauer = aktAbo.mindestDauerMinuten
        sliderDauer.value = minDauer
        labelDauer.text = if (minDauer == 0) " $DAUER_TEXT_ALL" else minDauer.toString()
        sliderDauer.addChangeListener { updateDurationLabel() }

        val isMin = aktAbo.filmLengthState == FilmLengthState.MINIMUM
        rbMin.isSelected = isMin
        rbMax.isSelected = !isMin
    }

    private fun updateDurationLabel() {
        labelDauer.text = "  ${durationLabelText(sliderDauer.value)}"
    }

    private fun bindPathAndProgramSetFields() {
        comboboxPfad.selectedItem = aktAbo.zielpfad
        labelDownDatumValue.text = aktAbo.downDatum?.format(DateUtil.FORMATTER).orEmpty()
        comboboxPSet.selectedItem = aktAbo.psetName
        // falls das Feld leer war, wird es jetzt auf den ersten Eintrag gesetzt
        aktAbo.psetName = selectedComboBoxValue(comboboxPSet)
    }

    private fun initializeMultiEditCheckBox(checkBox: JCheckBox, tag: AboTags) {
        checkBox.border = emptyBorder
        checkBox.horizontalTextPosition = JCheckBox.CENTER
        checkBox.addActionListener { multiEditCbIndices[tag.index] = checkBox.isSelected }
        checkBox.isVisible = isMultiEditMode
    }

    private fun applyIfValid(): Boolean {
        val test = aktAbo.copy
        writeFieldsToAbo(test)
        ok = if (test.isInvalid) {
            false
        } else {
            writeFieldsToAbo(aktAbo)
            true
        }
        return ok
    }

    private fun writeFieldsToAbo(abo: DatenAbo) {
        // no ABO_NR
        abo.isActive = checkBoxEingeschaltet.isSelected
        abo.isDoNotStartAutomatically = checkBoxDoNotStartAutomatically.isSelected
        abo.name = textFieldName.text.trim()
        abo.sender = selectedComboBoxValue(comboboxSender)
        abo.thema = textFieldThema.text.trim()
        abo.title = textFieldTitel.text.trim()
        abo.themaTitel = textFieldThemaTitel.text.trim()
        abo.irgendwo = textFieldIrgendwo.text.trim()
        abo.mindestDauerMinuten = sliderDauer.value
        abo.filmLengthState = if (rbMin.isSelected) FilmLengthState.MINIMUM else FilmLengthState.MAXIMUM
        abo.zielpfad = selectedComboBoxValue(comboboxPfad)
        // no ABO_DOWN_DATUM
        abo.psetName = selectedComboBoxValue(comboboxPSet)
    }

    private inner class CheckPathDocListener : DocumentListener {
        override fun insertUpdate(e: DocumentEvent) {
            checkPfad()
        }

        override fun removeUpdate(e: DocumentEvent) {
            checkPfad()
        }

        override fun changedUpdate(e: DocumentEvent) {
            checkPfad()
        }
    }

    private inner class EmptyTextDocListener(private val tf: JTextField) : DocumentListener {
        private fun doWork() {
            val isEmpty = tf.text.isBlank()
            tf.background = if (isEmpty) Color.red else UIManager.getColor(TEXTFIELD_BACKGROUND)
            jButtonBeenden.isEnabled = !isEmpty
        }

        override fun insertUpdate(e: DocumentEvent) {
            doWork()
        }

        override fun removeUpdate(e: DocumentEvent) {
            doWork()
        }

        override fun changedUpdate(e: DocumentEvent) {
            doWork()
        }
    }

    private companion object {
        private const val TEXTFIELD_BACKGROUND = "TextField.background"
        private const val DAUER_TEXT_ALL = "Alles"

        private fun durationLabelText(duration: Int): String = if (duration == 0) DAUER_TEXT_ALL else duration.toString()

        private fun selectedComboBoxValue(comboBox: JComboBox<String>): String =
            requireNotNull(comboBox.selectedItem).toString()
    }
}
