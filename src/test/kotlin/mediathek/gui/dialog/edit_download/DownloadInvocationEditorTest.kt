package mediathek.gui.dialog.edit_download

import mediathek.controller.DownloadStorage
import mediathek.controller.starter.ArteHlsDownloadPreparer
import mediathek.controller.starter.DownloadInvocationRequest
import mediathek.controller.starter.DownloadProgramInvocationBuilder
import mediathek.controller.starter.RuntimeExec
import mediathek.daten.*
import mediathek.tool.ArteHlsQualitySelector
import mediathek.tool.ArteQualityFixture
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertFalse
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.ValueSource
import java.nio.file.Path
import javax.swing.JTextField
import javax.swing.SwingUtilities

internal class DownloadInvocationEditorTest {
    @TempDir
    lateinit var directory: Path

    private fun download(explicit: Boolean = false, outputOptions: String = ""): DatenDownload {
        val switches = "-i %f " + (if (explicit) "-map p:1 -map -0:s " else "") + "-c copy -bsf:a aac_adtstoasc $outputOptions**"
        val program = DatenProg("ffmpeg", "/opt/bin/ffmpeg", switches, "false", "false")
        val invocation = DownloadProgramInvocationBuilder.build(
            DownloadType.PROGRAM, program,
            DownloadInvocationRequest(ArteQualityFixture.URL, "", "/tmp", "film.mp4", "/tmp/film.mp4", "", "", FilmResolution.Enum.HIGH_QUALITY),
        )
        return DatenDownload().apply {
            sender = "ARTE"
            art = DownloadType.PROGRAM
            quelle = DownloadSource.DOWNLOAD
            downloadUrl = ArteQualityFixture.URL
            selectedResolution = FilmResolution.Enum.HIGH_QUALITY
            targetPathFileName = "/tmp/film.mp4"
            programInvocation = invocation.command
            programInvocationArray = invocation.commandArray
            automaticArteHlsProgramMap = invocation.automaticArteHlsProgramMap
            pSet = DatenPset().apply { listeProg.add(program) }
        }
    }

    private fun restore(download: DatenDownload): DatenDownload {
        val path = directory.resolve("downloads.json")
        DownloadStorage.write(path, listOf(download))
        return DownloadStorage.read(path).single()
    }

    private fun selectedVideo(download: DatenDownload): String {
        val prepared = ArteHlsDownloadPreparer.prepare(download, { true }) {
            ArteHlsQualitySelector.Playlist(it, ArteQualityFixture.master())
        } ?: download.programInvocationArray.split(RuntimeExec.TRENNER_PROG_ARRAY)
        return prepared[prepared.indexOf("-map") + 1]
    }

    @Test
    fun `manual array edits clear ownership and retain the explicit stream after JSON restoration`() = SwingUtilities.invokeAndWait {
        val download = download()
        val program = requireNotNull(download.pSet).listeProg.single()
        val switches = program.switches
        val display = JTextField(download.programInvocation)
        val array = JTextField(download.programInvocationArray)
        DownloadInvocationEditor(download, display, array)
        val edited = download.programInvocationArray.replace("<>/tmp/film.mp4", "<>-movflags<>+faststart<>/tmp/film.mp4")

        array.text = edited

        assertFalse(download.automaticArteHlsProgramMap)
        assertEquals(edited, download.programInvocationArray)
        assertEquals(DatenProg.makeProgAufrufArray(edited), display.text)
        assertEquals(display.text, download.programInvocation)
        assertEquals("p:1", selectedVideo(download))
        val restored = restore(download)
        assertFalse(restored.automaticArteHlsProgramMap)
        assertEquals(edited, restored.programInvocationArray)
        assertEquals(download.programInvocation, restored.programInvocation)
        assertEquals("p:1", selectedVideo(restored))
        assertEquals(switches, program.switches)
    }

    @ParameterizedTest
    @ValueSource(booleans = [false, true])
    fun `regenerated commands replace ownership in either direction despite document events and JSON restoration`(explicit: Boolean) = SwingUtilities.invokeAndWait {
        val download = download(!explicit, "-threads 1 ")
        val source = download(explicit)
        val display = JTextField(download.programInvocation)
        val array = JTextField(download.programInvocationArray)
        val editor = DownloadInvocationEditor(download, display, array)
        // Different generated options force remove/insert events without a preceding manual edit.
        assertEquals(explicit, download.automaticArteHlsProgramMap)
        editor.applyGenerated(source)

        assertEquals(!explicit, download.automaticArteHlsProgramMap)
        assertEquals(source.programInvocation, display.text)
        assertEquals(source.programInvocationArray, array.text)
        assertEquals(source.programInvocation, download.programInvocation)
        assertEquals(source.programInvocationArray, download.programInvocationArray)
        val expected = if (explicit) "0:p:1:v:0" else "0:p:4:v:0"
        assertEquals(expected, selectedVideo(download))
        val restored = restore(download)
        assertEquals(!explicit, restored.automaticArteHlsProgramMap)
        assertEquals(source.programInvocationArray, restored.programInvocationArray)
        assertEquals(source.programInvocation, restored.programInvocation)
        assertEquals(expected, selectedVideo(restored))
        // Subsequent user changes must still clear ownership after the refresh guard is released.
        array.text += " "
        assertFalse(download.automaticArteHlsProgramMap)
        assertEquals(!explicit, source.automaticArteHlsProgramMap)
    }

    @ParameterizedTest
    @ValueSource(booleans = [false, true])
    fun `reset restores original commands and ownership through JSON restoration`(explicit: Boolean) = SwingUtilities.invokeAndWait {
        val download = download(explicit)
        val originalArray = download.programInvocationArray
        val originalCommand = download.programInvocation
        val display = JTextField(originalCommand)
        val array = JTextField(originalArray)
        val editor = DownloadInvocationEditor(download, display, array)
        editor.applyGenerated(download(!explicit))
        array.text += "<>-modified"

        editor.reset()

        assertEquals(!explicit, download.automaticArteHlsProgramMap)
        assertEquals(originalArray, array.text)
        assertEquals(originalCommand, display.text)
        val restored = restore(download)
        assertEquals(!explicit, restored.automaticArteHlsProgramMap)
        assertEquals(originalArray, restored.programInvocationArray)
        assertEquals(originalCommand, restored.programInvocation)
    }

    @Test
    fun `manual display command edits also clear ownership`() = SwingUtilities.invokeAndWait {
        val download = download()
        val display = JTextField(download.programInvocation)
        DownloadInvocationEditor(download, display, JTextField(download.programInvocationArray))
        display.text += " -custom"
        assertFalse(download.automaticArteHlsProgramMap)
        assertEquals(display.text, download.programInvocation)
        assertFalse(restore(download).automaticArteHlsProgramMap)
    }
}
