package mediathek.daten

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.ValueSource

internal class ProgramSetTemplateResolverTest {
    @ParameterizedTest
    @ValueSource(strings = ["", "/usr/bin/vlc"])
    fun usesTheExplicitVlcPathInBothProgramSets(vlcPath: String) {
        val playback = DatenProg().apply { programPath = "PFAD_VLC" }
        val download = DatenProg().apply { switches = "/c \"PFAD_VLC\" %f" }
        val programSets = ListePset().apply {
            add(DatenPset().apply { addProg(playback) })
            add(DatenPset().apply { addProg(download) })
        }

        ProgramSetTemplateResolver.replaceTemplates(null, programSets, vlcPath = vlcPath)

        assertEquals(vlcPath, playback.programPath)
        assertEquals("/c \"$vlcPath\" %f", download.switches)
    }
}
