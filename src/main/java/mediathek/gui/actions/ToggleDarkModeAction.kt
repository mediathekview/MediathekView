package mediathek.gui.actions

import com.formdev.flatlaf.FlatDarkLaf
import com.formdev.flatlaf.FlatLaf
import com.formdev.flatlaf.FlatLightLaf
import com.formdev.flatlaf.extras.FlatAnimatedLafChange
import mediathek.mainwindow.MediathekGui
import mediathek.tool.ApplicationConfiguration
import mediathek.tool.SVGIconUtilities
import java.awt.event.ActionEvent
import javax.swing.AbstractAction

class ToggleDarkModeAction : AbstractAction() {
    init {
        putValue(SHORT_DESCRIPTION, "Dunkelmodus ein-/ausschalten")
        putValue(SMALL_ICON, SVGIconUtilities.createSVGIcon("icons/fontawesome/circle-half-stroke.svg"))
    }

    override fun actionPerformed(e: ActionEvent) {
        FlatAnimatedLafChange.showSnapshot()
        if (FlatLaf.isLafDark())
            FlatLightLaf.setup()
        else
            FlatDarkLaf.setup()

        MediathekGui.ui().setupAlternatingRowColors()

        // update all components
        FlatLaf.updateUI()
        FlatAnimatedLafChange.hideSnapshotWithAnimation()
        ApplicationConfiguration.getConfiguration()
            .setProperty(ApplicationConfiguration.APPLICATION_DARK_MODE, FlatLaf.isLafDark())
    }
}