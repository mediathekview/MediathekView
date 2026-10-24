package mediathek.gui.dialogEinstellungen.allgemein

import mediathek.config.application.ApplicationConfiguration
import net.miginfocom.layout.AC
import net.miginfocom.layout.CC
import net.miginfocom.layout.LC
import net.miginfocom.swing.MigLayout
import org.jdesktop.swingx.JXTitledSeparator
import javax.swing.JCheckBox
import javax.swing.JPanel

class SearchConfigPanel : JPanel(MigLayout(LC().insets("5").hideMode(3), AC().grow().fill())) {
    init {
        add(JXTitledSeparator("Moderne Suche"), CC().wrap())
        add(ModernSearchConfigPanel(), CC().growX().wrap())

        add(JXTitledSeparator("Suchverlauf"), CC().gapTop("12").wrap())
        val applicationConfiguration = ApplicationConfiguration.getInstance()
        val lruHistory = JCheckBox("Zuletzt verwendete Suchbegriffe im Suchverlauf nach oben verschieben").apply {
            name = "useLruSearchHistory"
            toolTipText = "Gilt für die Filmsuche und die Audiothek. Deaktiviert bleibt die manuell festgelegte Reihenfolge erhalten."
            isSelected = applicationConfiguration.useLruSearchHistory
            addActionListener {
                applicationConfiguration.useLruSearchHistory = isSelected
            }
        }
        add(lruHistory, CC().wrap())
    }
}
