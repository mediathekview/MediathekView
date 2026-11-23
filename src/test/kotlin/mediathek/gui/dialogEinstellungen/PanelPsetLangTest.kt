package mediathek.gui.dialogEinstellungen

import com.formdev.flatlaf.FlatDarkLaf
import com.formdev.flatlaf.FlatLightLaf
import mediathek.daten.DatenPset
import mediathek.daten.ProgramSetRepository
import mediathek.gui.dialogEinstellungen.pset.PanelPsetLang
import mediathek.gui.messages.ProgramSetChangedEvent
import mediathek.swing.SwingDispatch
import mediathek.tool.MessageBus
import net.engio.mbassy.listener.Handler
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.ValueSource
import java.awt.Color
import java.awt.Component
import java.awt.Container
import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.StandardCopyOption.COPY_ATTRIBUTES
import java.util.concurrent.TimeUnit
import javax.swing.*
import javax.swing.border.TitledBorder
import javax.swing.event.TableModelEvent

internal class PanelPsetLangTest {
    @Test
    fun malformedProgramPathIsRejected() {
        assertFalse(PanelPsetLang.isProgramPathValid("malformed\u0000program"))
    }

    @Test
    fun directoryIsRejectedAsProgramPath(@TempDir directory: Path) {
        assertFalse(PanelPsetLang.isProgramPathValid(directory.toString()))
    }

    @Test
    fun bareProgramNameUsesPathWhenLocalDirectoryHasSameName(@TempDir directory: Path) {
        Files.createDirectory(directory.resolve("java"))
        val java = javaExecutable()
        val log = directory.resolve("program-path-check.log")
        val builder = ProcessBuilder(
            java.toString(), "-cp", System.getProperty("java.class.path"), ProgramPathCheckProbe::class.java.name
        ).directory(directory.toFile()).redirectErrorStream(true).redirectOutput(log.toFile())
        val environment = builder.environment()
        val pathKey = environment.keys.firstOrNull { it.equals("PATH", ignoreCase = true) } ?: "PATH"
        environment[pathKey] = java.parent.toString()
        val process = builder.start()
        try {
            assertTrue(process.waitFor(20, TimeUnit.SECONDS), "Program path check timed out")
            assertEquals(0, process.exitValue()) { Files.readString(log) }
        } finally {
            process.destroyForcibly()
        }
    }

    @Test
    fun executablePathContainingSpacesIsAccepted(@TempDir directory: Path) {
        val java = javaExecutable()
        val spacedDirectory = Files.createDirectory(directory.resolve("Program Files"))
        val executable = Files.copy(java, spacedDirectory.resolve(java.fileName), COPY_ATTRIBUTES)

        assertTrue(PanelPsetLang.isProgramPathValid(executable.toString()))
    }

    private fun javaExecutable(): Path {
        val name = if (File.separatorChar == '\\') "java.exe" else "java"
        return Path.of(System.getProperty("java.home"), "bin", name)
    }

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
                panel.programSetTable().also {
                    it.setRowSelectionInterval(repository.list.lastIndex, repository.list.lastIndex)
                }
            }
            val modelEvents = mutableListOf<TableModelEvent>()
            val model = SwingDispatch.callAndWait {
                table.model.also { it.addTableModelListener { event -> modelEvents.add(event) } }
            }
            val nameField = panel.field("jTextFieldSetName")
            val oldName = SwingDispatch.callAndWait { nameField.text }
            val beforeNotifications = notifications.count
            SwingDispatch.runAndWait {
                nameField.caretPosition = oldName.length
                nameField.replaceSelection("X")
            }
            flushEdt()
            SwingDispatch.runAndWait {
                assertSame(model, table.model)
                assertEquals(DatenPset.PROGRAMMSET_NAME, modelEvents.single().column)
                assertEquals(table.selectedRow, modelEvents.single().firstRow)
                assertEquals(oldName + "X", nameField.text)
                assertEquals(oldName.length + 1, nameField.caretPosition)
                assertEquals(repository.list.lastIndex, table.selectedRow)
                panel.assertProgramHeading(oldName + "X")
                nameField.document.remove(oldName.length, 1)
            }
            flushEdt()
            SwingDispatch.runAndWait {
                assertSame(model, table.model)
                assertEquals(2, modelEvents.size)
                assertTrue(modelEvents.all { it.column == DatenPset.PROGRAMMSET_NAME })
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

    @ParameterizedTest
    @ValueSource(strings = [
        "jButtonGruppeNeu",
        "jButtonGruppeDuplizieren",
        "jButtonGruppeAb",
        "jButtonAbspielen",
        "jCheckBoxSpeichern",
        "jButtonGruppeStandardfarbe",
    ])
    fun localChangesRefreshTheSameModelOnceAndNotifyOtherConsumers(controlName: String) {
        val repository = ProgramSetRepository()
        val first = DatenPset("First").apply { farbe = Color.BLUE }
        repository.list.add(first)
        repository.list.add(DatenPset("Second"))
        val notifications = ChangeCounter()
        MessageBus.messageBus.subscribe(notifications)
        val panel = SwingDispatch.callAndWait {
            PanelPsetLang(null, repository, repository.list, { _, _ -> })
        }
        try {
            var refreshes = 0
            val model = SwingDispatch.callAndWait {
                val table = panel.programSetTable()
                table.model.also {
                    it.addTableModelListener { refreshes++ }
                    panel.button(controlName).doClick()
                }
            }
            flushEdt()
            SwingDispatch.runAndWait {
                val table = panel.programSetTable()
                assertSame(model, table.model)
                assertEquals(1, refreshes)
                assertEquals(1, notifications.count)
                assertEquals(repository.list.map { it.name }, List(table.rowCount) { row ->
                    model.getValueAt(row, DatenPset.PROGRAMMSET_NAME)
                })
                val selectedRow = if (controlName == "jButtonGruppeAb") 1 else 0
                assertArrayEquals(intArrayOf(selectedRow), table.selectedRows)
                assertSame(first, repository.list[selectedRow])
                assertEquals(first.name, panel.field("jTextFieldSetName").text)
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
            val (model, nameColumn) = SwingDispatch.callAndWait {
                val table = panel.programSetTable()
                assertEquals(JTable::class.java, table.javaClass)
                assertNull(table.rowSorter)
                assertEquals(
                    listOf(DatenPset.PROGRAMMSET_NAME, DatenPset.PROGRAMMSET_IST_ABSPIELEN, DatenPset.PROGRAMMSET_IST_SPEICHERN),
                    table.columnModel.columns.toList().map { it.modelIndex },
                )
                val nameIndex = table.convertColumnIndexToView(DatenPset.PROGRAMMSET_NAME)
                val playbackIndex = table.convertColumnIndexToView(DatenPset.PROGRAMMSET_IST_ABSPIELEN)
                table.moveColumn(nameIndex, playbackIndex)
                table.model to table.columnModel.getColumn(playbackIndex).apply { width = 180 }
            }
            repository.addProgramSet(DatenPset("Extern").apply {
                farbe = Color.BLUE
                setSpeichern(true)
            })
            flushEdt()
            SwingDispatch.runAndWait {
                val table = panel.programSetTable()
                assertSame(model, table.model)
                assertEquals(2, table.rowCount)
                assertArrayEquals(intArrayOf(0), table.selectedRows)
                val nameIndex = table.convertColumnIndexToView(DatenPset.PROGRAMMSET_NAME)
                val playbackIndex = table.convertColumnIndexToView(DatenPset.PROGRAMMSET_IST_ABSPIELEN)
                val saveIndex = table.convertColumnIndexToView(DatenPset.PROGRAMMSET_IST_SPEICHERN)
                assertEquals("Extern", table.getValueAt(1, nameIndex))
                assertEquals(Boolean::class.javaObjectType, table.getColumnClass(playbackIndex))
                assertEquals(false, table.getValueAt(1, playbackIndex))
                assertEquals(Boolean::class.javaObjectType, table.getColumnClass(saveIndex))
                assertEquals(true, table.getValueAt(1, saveIndex))
                assertSame(nameColumn, table.columnModel.getColumn(nameIndex))
                assertEquals(180, nameColumn.width)
                assertTrue(nameIndex > playbackIndex)
                assertEquals(-1, table.convertColumnIndexToView(DatenPset.PROGRAMMSET_FARBE))
                val renderedName = table.prepareRenderer(table.getCellRenderer(1, nameIndex), 1, nameIndex)
                assertEquals(Color.BLUE, renderedName.foreground)
            }
        } finally {
            SwingDispatch.runAndWait { panel.removeNotify() }
        }
    }

    @ParameterizedTest
    @ValueSource(ints = [1, 3])
    fun removingSelectedLastProgramSetClearsTheSelectionAndEditor(setCount: Int) {
        val repository = ProgramSetRepository()
        repeat(setCount) { index -> repository.list.add(DatenPset("Set $index")) }
        val panel = SwingDispatch.callAndWait {
            PanelPsetLang(null, repository, repository.list, { _, _ -> })
        }
        try {
            val table = SwingDispatch.callAndWait {
                panel.programSetTable().also {
                    it.setRowSelectionInterval(setCount - 1, setCount - 1)
                }
            }
            val nameField = panel.field("jTextFieldSetName")
            SwingDispatch.runAndWait {
                assertEquals("Set ${setCount - 1}", nameField.text)
                repository.removeAtIndexes(intArrayOf(setCount - 1))
            }
            flushEdt()
            SwingDispatch.runAndWait {
                assertEquals(setCount - 1, table.rowCount)
                assertEquals(-1, table.selectedRow)
                assertEquals("", nameField.text)
                assertFalse(nameField.isEnabled)
                if (table.rowCount > 0) {
                    table.setRowSelectionInterval(0, 0)
                    assertEquals("Set 0", nameField.text)
                    panel.assertProgramHeading("Set 0")
                }
            }
        } finally {
            SwingDispatch.runAndWait { panel.removeNotify() }
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = [false, true])
    fun removingMultipleProgramSetsRestoresOnlyRowsStillInRange(removeAll: Boolean) {
        val repository = ProgramSetRepository()
        repeat(4) { index -> repository.list.add(DatenPset("Set $index")) }
        val panel = SwingDispatch.callAndWait {
            PanelPsetLang(null, repository, repository.list, { _, _ -> })
        }
        try {
            val table = SwingDispatch.callAndWait {
                panel.programSetTable().also {
                    if (removeAll) {
                        it.setRowSelectionInterval(0, 3)
                    } else {
                        it.setRowSelectionInterval(1, 1)
                        it.addRowSelectionInterval(3, 3)
                    }
                    repository.removeAtIndexes(it.selectedRows)
                }
            }
            flushEdt()
            SwingDispatch.runAndWait {
                val nameField = panel.field("jTextFieldSetName")
                if (removeAll) {
                    assertEquals(0, table.rowCount)
                    assertEquals(-1, table.selectedRow)
                    assertFalse(nameField.isEnabled)
                    assertEquals("", nameField.text)
                } else {
                    assertEquals(listOf("Set 0", "Set 2"), repository.list.map { it.name })
                    assertEquals(2, table.rowCount)
                    assertArrayEquals(intArrayOf(1), table.selectedRows)
                    assertEquals("Set 2", nameField.text)
                    panel.assertProgramHeading("Set 2")
                }
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
        val programTable = helperProgramTable()
        val scrollPane = SwingUtilities.getAncestorOfClass(JScrollPane::class.java, programTable) as JScrollPane
        assertEquals("Set Name: $name", (scrollPane.border as TitledBorder).title)
    }

    private fun PanelPsetLang.field(name: String): JTextField =
        PanelPsetLang::class.java.superclass.getDeclaredField(name).apply { isAccessible = true }[this] as JTextField

    private fun PanelPsetLang.button(name: String): AbstractButton =
        PanelPsetLang::class.java.superclass.getDeclaredField(name).apply { isAccessible = true }[this] as AbstractButton

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

internal object ProgramPathCheckProbe {
    @JvmStatic
    fun main(args: Array<String>) {
        check(Files.isDirectory(Path.of("java"))) { "Expected a directory named java in the working directory" }
        check(PanelPsetLang.isProgramPathValid("java")) { "Expected java to resolve through PATH" }
        check(!PanelPsetLang.isProgramPathValid(".${File.separator}java")) { "Explicit directory was accepted" }
        check(!PanelPsetLang.isProgramPathValid("java${File.separator}")) { "Directory with trailing separator was accepted" }
    }
}

internal fun PanelPsetLang.programSetTable(): JTable = tableInScrollPane("jScrollPane3")

internal fun PanelPsetLang.helperProgramTable(): JTable = tableInScrollPane("jScrollPane1")

private fun PanelPsetLang.tableInScrollPane(name: String): JTable {
    val scrollPane = PanelPsetLang::class.java.superclass.getDeclaredField(name)
        .apply { isAccessible = true }[this] as JScrollPane
    return scrollPane.viewport.view as JTable
}
