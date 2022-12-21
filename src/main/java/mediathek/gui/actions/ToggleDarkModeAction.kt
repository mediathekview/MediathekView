package mediathek.gui.actions

import com.formdev.flatlaf.FlatLaf
import com.formdev.flatlaf.extras.FlatAnimatedLafChange
import mediathek.mainwindow.MediathekGui
import mediathek.tool.ApplicationConfiguration
import mediathek.tool.DarkModeFactory
import mediathek.tool.LightModeFactory
import mediathek.tool.SVGIconUtilities
import java.awt.event.ActionEvent
import javax.swing.AbstractAction
import javax.swing.LookAndFeel

class ToggleDarkModeAction : AbstractAction() {
    init {
        putValue(SHORT_DESCRIPTION, "Dunkelmodus ein-/ausschalten")
        putValue(SMALL_ICON, SVGIconUtilities.createSVGIcon("icons/fontawesome/circle-half-stroke.svg"))
    }

    override fun actionPerformed(e: ActionEvent) {
        FlatAnimatedLafChange.showSnapshot()

        val laf: LookAndFeel = if (!FlatLaf.isLafDark()) {
            DarkModeFactory.lookAndFeel
        } else {
            LightModeFactory.lookAndFeel
        }
        FlatLaf.setup(laf)
        MediathekGui.ui().setupAlternatingRowColors()
        // update all components
        FlatLaf.updateUI()

        FlatAnimatedLafChange.hideSnapshotWithAnimation()
        ApplicationConfiguration.getConfiguration()
            .setProperty(ApplicationConfiguration.APPLICATION_DARK_MODE, FlatLaf.isLafDark())
    }
}