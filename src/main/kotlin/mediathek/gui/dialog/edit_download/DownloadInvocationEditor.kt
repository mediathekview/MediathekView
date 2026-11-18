package mediathek.gui.dialog.edit_download

import mediathek.controller.starter.DownloadInvocation
import mediathek.daten.DatenDownload
import mediathek.daten.DatenProg
import javax.swing.JTextField
import javax.swing.event.DocumentEvent
import javax.swing.event.DocumentListener

/** Keeps command ownership and Swing document updates together; confined to the EDT. */
internal class DownloadInvocationEditor(
    private val download: DatenDownload,
    private val commandField: JTextField,
    private val arrayField: JTextField,
) {
    private val original = download.invocation()
    private var updatingFields = false

    init {
        commandField.onEdit {
            download.programInvocation = it
        }
        arrayField.onEdit {
            download.programInvocationArray = it
            download.programInvocation = DatenProg.makeProgAufrufArray(it)
            updateFields { commandField.text = download.programInvocation }
        }
    }

    fun applyGenerated(source: DatenDownload) = apply(source.invocation())

    fun reset() = apply(original)

    private fun apply(invocation: DownloadInvocation) = updateFields {
        download.programInvocation = invocation.command
        download.programInvocationArray = invocation.commandArray
        download.automaticArteHlsProgramMap = invocation.automaticArteHlsProgramMap
        commandField.text = invocation.command
        arrayField.text = invocation.commandArray
    }

    private fun updateFields(update: () -> Unit) {
        updatingFields = true
        try {
            update()
        } finally {
            updatingFields = false
        }
    }

    private fun JTextField.onEdit(edit: (String) -> Unit) {
        document.addDocumentListener(object : DocumentListener {
            private fun changed() {
                if (!updatingFields) {
                    download.automaticArteHlsProgramMap = false
                    edit(text.trim())
                }
            }
            override fun insertUpdate(e: DocumentEvent) = changed()
            override fun removeUpdate(e: DocumentEvent) = changed()
            override fun changedUpdate(e: DocumentEvent) = changed()
        })
    }

    private fun DatenDownload.invocation() = DownloadInvocation(
        programInvocation, programInvocationArray, automaticArteHlsProgramMap,
    )
}
