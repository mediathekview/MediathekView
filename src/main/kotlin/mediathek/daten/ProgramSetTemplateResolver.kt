package mediathek.daten

import mediathek.config.StandardLocations
import mediathek.config.application.ApplicationConfiguration
import mediathek.gui.dialogEinstellungen.PanelProgrammPfade
import javax.swing.JFrame
import javax.swing.JOptionPane

object ProgramSetTemplateResolver {
    const val MUSTER_PFAD_ZIEL = "ZIELPFAD"
    const val MUSTER_PFAD_VLC = "PFAD_VLC"
    const val MUSTER_PFAD_FFMPEG = "PFAD_FFMPEG"

    @JvmStatic
    fun replaceTemplates(parent: JFrame?, programSets: ListePset) {
        for (programSet in programSets) {
            replaceTemplates(parent, programSet)
        }
    }

    private fun replaceTemplates(parent: JFrame?, programSet: DatenPset) {
        programSet.zielPfad = programSet.zielPfad.replace(MUSTER_PFAD_ZIEL, StandardLocations.getStandardDownloadPath())
        var vlc = ""
        var ffmpeg = ""

        // damit nur die Variablen abgefragt werden, die auch verwendet werden
        for (program in programSet.listeProg) {
            if (program.programPath.contains(MUSTER_PFAD_VLC) || program.switches.contains(MUSTER_PFAD_VLC)) {
                vlc = getPfadVlc(parent)
                break
            }
        }

        for (program in programSet.listeProg) {
            if (program.programPath.contains(MUSTER_PFAD_FFMPEG) || program.switches.contains(MUSTER_PFAD_FFMPEG)) {
                ffmpeg = getPfadFFmpeg(parent)
                break
            }
        }

        for (program in programSet.listeProg) {
            program.programPath = program.programPath.replace(MUSTER_PFAD_VLC, vlc)
            program.switches = program.switches.replace(MUSTER_PFAD_VLC, vlc)
            program.programPath = program.programPath.replace(MUSTER_PFAD_FFMPEG, ffmpeg)
            program.switches = program.switches.replace(MUSTER_PFAD_FFMPEG, ffmpeg)
        }
    }

    private fun getPfadVlc(parent: JFrame?): String {
        // liefert den Pfad wenn vorhanden, wenn nicht wird er in einem Dialog abgefragt
        if (ApplicationConfiguration.getInstance().standardVlcPath.isEmpty()) {
            showProgramPathDialog(parent, PanelProgrammPfade(parent, true, false))
        }
        return ApplicationConfiguration.getInstance().standardVlcPath
    }

    private fun getPfadFFmpeg(parent: JFrame?): String {
        // liefert den Pfad wenn vorhanden, wenn nicht wird er in einem Dialog abgefragt
        if (ApplicationConfiguration.getInstance().standardFFmpegPath.isEmpty()) {
            showProgramPathDialog(parent, PanelProgrammPfade(parent, false, true))
        }
        return ApplicationConfiguration.getInstance().standardFFmpegPath
    }

    private fun showProgramPathDialog(parent: JFrame?, panel: PanelProgrammPfade) {
        JOptionPane.showMessageDialog(parent, panel, "Pfade Standardprogramme", JOptionPane.PLAIN_MESSAGE)
    }
}
