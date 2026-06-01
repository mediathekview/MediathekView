package mediathek.daten

import org.junit.jupiter.api.Assertions.assertArrayEquals
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertFalse
import org.junit.jupiter.api.Assertions.assertNotSame
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test

class DatenProgTest {
    @Test
    fun toArrayReturnsCopy() {
        val prog = DatenProg()
        prog.name = "VLC"

        val values = prog.toArray()
        values[DatenProg.PROGRAMM_NAME] = "changed"

        assertEquals("VLC", prog.name)
        assertNotSame(values, prog.toArray())
    }

    @Test
    fun copyFromPreservesProvidedBooleanDefaults() {
        val values = DatenProg().toArray()
        values[DatenProg.PROGRAMM_NAME] = "Program"

        val prog = DatenProg()
        prog.isRestart = true
        prog.isDownloadManager = true
        prog.copyFrom(values)

        assertEquals("Program", prog.name)
        assertFalse(prog.isRestart)
        assertFalse(prog.isDownloadManager)
    }

    @Test
    fun copyFromResetsMissingBooleanValuesToDefault() {
        val prog = DatenProg()
        prog.isRestart = true
        prog.isDownloadManager = true

        prog.copyFrom(arrayOf("Program"))

        assertEquals("Program", prog.name)
        assertFalse(prog.isRestart)
        assertFalse(prog.isDownloadManager)
    }

    @Test
    fun copyCreatesIndependentProgram() {
        val prog = DatenProg("Name", "/bin/echo", "%f", "true", "false")

        val copy = prog.copy()
        copy.name = "Other"

        assertEquals("Name", prog.name)
        assertEquals("Other", copy.name)
        assertTrue(copy.isRestart)
        assertFalse(copy.isDownloadManager)
    }

    @Test
    fun makeProgramInvocationArrayMatchesLegacySeparatorFormat() {
        val prog = DatenProg("Name", "/bin/echo", "%f --flag", "", "")

        assertEquals("/bin/echo<>%f<>--flag", prog.programmAufrufArray)
        assertEquals("/bin/echo %f --flag", DatenProg.makeProgAufrufArray(prog.programmAufrufArray))
    }

    @Test
    fun columnSnapshotKeepsLegacyOrder() {
        val prog = DatenProg("Name", "/bin/echo", "%f", "true", "false")

        assertArrayEquals(
            arrayOf("Name", "", "/bin/echo", "%f", "", "", "true", "false"),
            prog.toArray(),
        )
    }
}
