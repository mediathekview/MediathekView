package mediathek.gui.tabs.tab_film

import mediathek.config.Daten
import mediathek.gui.messages.ProgramSetChangedEvent
import mediathek.tool.MessageBus.messageBus
import net.engio.mbassy.listener.Handler
import org.jdesktop.swingx.WrapLayout
import java.awt.BorderLayout
import java.awt.Dimension
import java.awt.FlowLayout
import javax.swing.*

class PsetButtonsPanel(private val guiFilme: GuiFilme) : JPanel() {
    private val btnPanel = JPanel()

    init {
        layout = BorderLayout()
        preferredSize = Dimension(Int.MAX_VALUE, DEFAULT_HEIGHT)
        minimumSize = Dimension(100, DEFAULT_HEIGHT)

        btnPanel.layout = WrapLayout(FlowLayout.LEFT, 5, 5)

        val scrollPane = JScrollPane()
        add(scrollPane, BorderLayout.CENTER)
        scrollPane.viewport.view = btnPanel

        setupButtonLayout()

        messageBus.subscribe(this)
    }

    fun install(tabbedPane: JTabbedPane) {
        tabbedPane.add("Buttons", this)
    }

    @Suppress("UNUSED_PARAMETER")
    @Handler
    private fun handleProgramSetChangedEvent(event: ProgramSetChangedEvent) {
        SwingUtilities.invokeLater(this::setupButtonLayout)
    }

    protected fun setupButtonLayout() {
        btnPanel.removeAll()

        for (pset in Daten.getInstance().listePset.listeButton) {
            if (!pset.isFreeLine) {
                val component: JComponent = if (pset.isLabel) {
                    JLabel(pset.name)
                } else {
                    JButton(pset.name).apply {
                        addActionListener { guiFilme.playerStarten(pset) }
                    }
                }

                pset.foregroundColor.ifPresent(component::setForeground)
                btnPanel.add(component)
            } else {
                btnPanel.add(JLabel(""))
            }
        }

        validate()
        repaint()
    }

    companion object {
        private const val DEFAULT_HEIGHT = 90
    }
}
