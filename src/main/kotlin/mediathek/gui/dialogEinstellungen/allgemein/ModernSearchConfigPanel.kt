package mediathek.gui.dialogEinstellungen.allgemein

import mediathek.config.application.ApplicationConfiguration

class ModernSearchConfigPanel : ModernSearchConfigPanelBase() {
    init {
        val applicationConfiguration = ApplicationConfiguration.getInstance()

        cbActivateModernSearch.isSelected = applicationConfiguration.useModernSearch
        cbActivateModernSearch.addActionListener {
            applicationConfiguration.useModernSearch = cbActivateModernSearch.isSelected
        }
    }
}
