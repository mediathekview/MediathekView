package mediathek.gui.dialogEinstellungen

import com.formdev.flatlaf.FlatDarkLaf
import com.formdev.flatlaf.FlatLightLaf
import mediathek.daten.DatenPset
import mediathek.daten.ProgramSetRepository
import mediathek.gui.dialogEinstellungen.pset.PanelPsetLang
import mediathek.gui.messages.ProgramSetChangedEvent
import mediathek.swing.SwingDispatch
import mediathek.tool.MessageBus
import mediathek.tool.table.MVPsetTable
import net.engio.mbassy.listener.Handler
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.ValueSource
import java.awt.Component
import java.awt.Container
import javax.swing.*
import javax.swing.border.TitledBorder

internal class PanelPsetLangTest {
    @ParameterizedTest
    @ValueSource(strings = ["existing", "new", "duplicate"])
    fun editingNameKeepsEditorStateAndNotifiesOtherConsumers(kind: String) {
        val repository = ProgramSetRepository()
        repository.list.add(DatenPset("Speichern"))
        val notifications = ChangeCounter()
        MessageBus.messageBus.subscribe(notifications)
        val panel = SwingDispatch.callAndWait {
            PanelPsetLang(null, repository, repository.list, { _, _ -> })
        }
        try {
            SwingDispatch.runAndWait {
                when (kind) {
                    "new" -> panel.button("jButtonGruppeNeu").doClick()
                    "duplicate" -> panel.button("jButtonGruppeDuplizieren").doClick()
                }
            }
            flushEdt()
            val table = SwingDispatch.callAndWait {
                panel.descendants().filterIsInstance<MVPsetTable>().single().also {
                    it.setRowSelectionInterval(repository.list.lastIndex, repository.list.lastIndex)
                }
            }
            val model = SwingDispatch.callAndWait { table.model }
            val nameField = panel.field("jTextFieldSetName")
            val oldName = SwingDispatch.callAndWait { nameField.text }
            val beforeNotifications = notifications.count
            SwingDispatch.runAndWait {
                nameField.caretPosition = oldName.length
                nameField.replaceSelection("X")
            }
            flushEdt()
            SwingDispatch.runAndWait {
                assertSame(model, table.model, "Typing must not rebuild the table and request table focus")
                assertEquals(oldName + "X", nameField.text)
                assertEquals(oldName.length + 1, nameField.caretPosition)
                assertEquals(repository.list.lastIndex, table.selectedRow)
                panel.assertProgramHeading(oldName + "X")
                nameField.document.remove(oldName.length, 1)
            }
            flushEdt()
            SwingDispatch.runAndWait {
                assertSame(model, table.model, "Deleting must not rebuild the table either")
                assertEquals(oldName, repository.list.last().name)
                assertEquals(oldName, nameField.text)
                assertEquals(oldName, table.model.getValueAt(table.selectedRow, DatenPset.PROGRAMMSET_NAME))
                assertEquals(oldName.length, nameField.caretPosition)
                assertEquals(beforeNotifications + 2, notifications.count)
                panel.assertProgramHeading(oldName)
            }
        } finally {
            SwingDispatch.runAndWait { panel.removeNotify() }
            MessageBus.messageBus.unsubscribe(notifications)
        }
    }

    @Test
    fun externalChangesStillRefreshThePanel() {
        val repository = ProgramSetRepository()
        repository.list.add(DatenPset("Speichern"))
        val panel = SwingDispatch.callAndWait {
            PanelPsetLang(null, repository, repository.list, { _, _ -> })
        }
        try {
            repository.addProgramSet(DatenPset("Extern"))
            flushEdt()
            SwingDispatch.runAndWait {
                val table = panel.descendants().filterIsInstance<MVPsetTable>().single()
                assertEquals(2, table.rowCount)
                assertEquals("Extern", table.model.getValueAt(1, DatenPset.PROGRAMMSET_NAME))
            }
        } finally {
            SwingDispatch.runAndWait { panel.removeNotify() }
        }
    }

    @Test
    fun downloadHelpUsesThemeColorsInitiallyAndAfterThemeChanges() {
        SwingDispatch.runAndWait {
            val previousLookAndFeel = UIManager.getLookAndFeel()
            var panel: PanelPsetLang? = null
            try {
                UIManager.setLookAndFeel(FlatDarkLaf())
                val repository = ProgramSetRepository()
                repository.list.add(DatenPset("Speichern"))
                val settings = PanelPsetLang(null, repository, repository.list, { _, _ -> })
                panel = settings
                val help = settings.descendants().filterIsInstance<JTextArea>()
                    .single { it.text.startsWith("Filme, deren URL") }
                assertHelpColors(help)
                UIManager.setLookAndFeel(FlatLightLaf())
                SwingUtilities.updateComponentTreeUI(settings)
                assertHelpColors(help)
                UIManager.setLookAndFeel(FlatDarkLaf())
                SwingUtilities.updateComponentTreeUI(settings)
                assertHelpColors(help)
            } finally {
                panel?.removeNotify()
                UIManager.setLookAndFeel(previousLookAndFeel)
            }
        }
    }

    private fun assertHelpColors(help: JTextArea) {
        val reference = JTextArea().apply { isEditable = false }
        assertEquals(reference.background, help.background)
        assertEquals(reference.foreground, help.foreground)
        assertNotEquals(help.background, help.foreground)
        assertFalse(help.isEditable)
    }

    private fun flushEdt() = SwingDispatch.runAndWait { }

    private fun PanelPsetLang.assertProgramHeading(name: String) {
        val programTable = descendants().filterIsInstance<JTable>().single { it !is MVPsetTable }
        val scrollPane = SwingUtilities.getAncestorOfClass(JScrollPane::class.java, programTable) as JScrollPane
        assertEquals("Set Name: $name", (scrollPane.border as TitledBorder).title)
    }

    private fun PanelPsetLang.field(name: String): JTextField =
        PanelPsetLang::class.java.superclass.getDeclaredField(name).apply { isAccessible = true }[this] as JTextField

    private fun PanelPsetLang.button(name: String): JButton =
        PanelPsetLang::class.java.superclass.getDeclaredField(name).apply { isAccessible = true }[this] as JButton

    private fun Component.descendants(): Sequence<Component> = sequence {
        yield(this@descendants)
        if (this@descendants is Container) {
            components.forEach { yieldAll(it.descendants()) }
        }
    }

    class ChangeCounter {
        var count = 0
            private set

        @Handler
        fun changed(@Suppress("UNUSED_PARAMETER") event: ProgramSetChangedEvent) {
            count++
        }
    }
}
