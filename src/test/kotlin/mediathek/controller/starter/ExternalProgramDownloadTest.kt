package mediathek.controller.starter

import mediathek.config.CommandLineOptions
import mediathek.config.Konstanten
import mediathek.controller.history.AboHistoryController
import mediathek.controller.history.AboHistoryEntry
import mediathek.daten.DatenDownload
import mediathek.daten.DownloadSource
import mediathek.daten.DownloadType
import mediathek.tool.notification.DisabledNotificationBackend
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.CsvSource
import org.junit.jupiter.params.provider.ValueSource
import org.mockito.Mockito.*
import picocli.CommandLine
import java.io.IOException
import java.nio.file.Files
import java.nio.file.Path
import java.time.LocalDateTime
import java.util.concurrent.CompletableFuture
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit.MILLISECONDS
import java.util.concurrent.TimeUnit.SECONDS
import java.util.concurrent.TimeoutException

internal class ExternalProgramDownloadTest {
    @TempDir
    lateinit var directory: Path

    @ParameterizedTest
    @ValueSource(booleans = [false, true])
    fun `cancellation during process startup destroys the returned process before finalization`(downloadManager: Boolean) {
        val download = download().apply { isDownloadManager = downloadManager }
        val state = requireNotNull(download.runtime.runState)
        val process = mock(Process::class.java)
        doAnswer {
            assertSame(state, download.runtime.runState, "Destroy the process before finalization resets the run state")
            assertTrue(state.isRunning)
            null
        }.`when`(process).destroy()
        mockConstruction(RuntimeExec::class.java) { runtime, _ ->
            `when`(runtime.exec(true)).thenAnswer {
                // The user stops the download while ProcessBuilder.start() is returning.
                state.requestStop()
                process
            }
        }.use { construction ->
            runner(download).run()
            assertEquals(1, construction.constructed().size)
        }
        assertSame(process, state.process)
        verify(process).destroy()
        assertNull(download.runtime.runState)
        assertFalse(state.isError)
    }

    @Test
    fun `cancellation before startup never launches a process`() {
        val download = download()
        val state = requireNotNull(download.runtime.runState).apply { requestStop() }
        mockConstruction(RuntimeExec::class.java).use { construction ->
            runner(download).run()
            assertTrue(construction.constructed().isEmpty())
        }
        assertNull(state.process)
        assertNull(download.runtime.runState)
        assertFalse(state.isError)
    }

    @Test
    fun `cancellation during failed startup finishes without retrying or marking an error`() {
        val download = download()
        val state = requireNotNull(download.runtime.runState)
        mockConstruction(RuntimeExec::class.java) { runtime, _ ->
            `when`(runtime.exec(true)).thenAnswer {
                state.requestStop()
                null
            }
        }.use { construction ->
            runner(download).run()
            assertEquals(1, construction.constructed().size)
        }
        assertNull(state.process)
        assertNull(download.runtime.runState)
        assertFalse(state.isError)
    }

    @Test
    fun `normal successful process is monitored and is not destroyed`() {
        val download = download()
        val state = requireNotNull(download.runtime.runState)
        val process = mock(Process::class.java)
        `when`(process.exitValue()).thenReturn(0)
        mockConstruction(RuntimeExec::class.java) { runtime, context ->
            assertNull(context.arguments()[4], "A custom command retains its original invocation")
            `when`(runtime.exec(true)).thenReturn(process)
        }.use {
            runner(download).run()
        }
        verify(process).exitValue()
        verify(process, never()).destroy()
        assertTrue(state.isFinished)
        assertSame(state, download.runtime.runState)
    }

    @ParameterizedTest
    @ValueSource(strings = ["vlc", "custom-downloader"])
    fun `non-FFmpeg programs write directly to the final filename`(executable: String) {
        val download = standardFfmpegDownload()
        val final = Path.of(download.targetPathFileName)
        val part = final.resolveSibling("${final.fileName}.part")
        val args = if (executable == "vlc") {
            listOf(executable, download.downloadUrl, ":sout=#standard{access=file,mux=mp4,dst=$final}", "--play-and-exit")
        } else {
            listOf(executable, "-i", download.downloadUrl, "-c", "copy", "-bsf:a", "aac_adtstoasc", final.toString())
        }
        download.programInvocation = args.joinToString(" ")
        download.programInvocationArray = args.joinToString(RuntimeExec.TRENNER_PROG_ARRAY)
        val content = ByteArray(Konstanten.MIN_FILM_FILE_SIZE_KB.toInt()) { 42 }
        val process = mock(Process::class.java)
        `when`(process.exitValue()).thenAnswer {
            assertArrayEquals(content, Files.readAllBytes(final))
            assertFalse(Files.exists(part))
            0
        }

        mockConstruction(RuntimeExec::class.java) { runtime, context ->
            assertEquals(download.programInvocation, context.arguments()[2])
            assertNull(context.arguments()[4], "Keep the external program's original output argument")
            val launchedArgs = (context.arguments()[3] as String).split(RuntimeExec.TRENNER_PROG_ARRAY)
            assertEquals(args, launchedArgs)
            val output = if (executable == "vlc") {
                launchedArgs[2].substringAfter(",dst=").removeSuffix("}")
            } else {
                launchedArgs.last()
            }
            `when`(runtime.exec(true)).thenAnswer {
                assertFalse(Files.exists(part))
                Files.write(Path.of(output), content)
                process
            }
        }.use {
            runner(download).run()
            assertEquals(1, it.constructed().size)
        }

        verify(process).exitValue()
        assertTrue(requireNotNull(download.runtime.runState).isFinished)
        assertArrayEquals(content, Files.readAllBytes(final))
        assertFalse(Files.exists(part))
    }

    @ParameterizedTest
    @CsvSource("false,false", "true,false", "false,true")
    fun `standard FFmpeg publishes its part file after exiting successfully`(windows: Boolean, automaticMap: Boolean) {
        val download = standardFfmpegDownload(windows, automaticMap).apply {
            quelle = DownloadSource.ABO
            aboName = "Subscription"
        }
        val originalInvocation = download.programInvocationArray
        val final = Path.of(download.targetPathFileName)
        val part = final.resolveSibling("Film mit Leerzeichen.mp4.part")
        Files.writeString(part, "stale output from an earlier run")
        val content = ByteArray(Konstanten.MIN_FILM_FILE_SIZE_KB.toInt()) { 42 }
        val state = requireNotNull(download.runtime.runState)
        val historyEntries = mutableListOf<AboHistoryEntry>()
        val history = mock(AboHistoryController::class.java) { invocation ->
            val entry = invocation.arguments.singleOrNull() as? AboHistoryEntry
                ?: return@mock RETURNS_DEFAULTS.answer(invocation)
            assertTrue(Files.exists(final), "Record history after publishing the MP4")
            assertFalse(Files.exists(part))
            historyEntries.add(entry)
            null
        }
        val process = mock(Process::class.java)
        `when`(process.exitValue()).thenAnswer {
            assertTrue(Files.exists(part))
            assertFalse(Files.exists(final))
            0
        }
        mockConstruction(RuntimeExec::class.java) { runtime, context ->
            val baseArguments = originalInvocation.split(RuntimeExec.TRENNER_PROG_ARRAY).dropLast(1)
            assertEquals(baseArguments + listOf("-f", "mp4", part.toString()), context.arguments()[4])
            `when`(runtime.exec(true)).thenAnswer {
                assertFalse(Files.exists(part), "FFmpeg must start with a fresh output")
                Files.write(part, content)
                process
            }
        }.use {
            runner(download) { history }.run()
            assertEquals(1, it.constructed().size)
        }
        assertEquals(download.historyUrl, historyEntries.single().url)
        assertArrayEquals(content, Files.readAllBytes(final))
        assertFalse(Files.exists(part))
        assertEquals(final.toString(), download.targetPathFileName)
        assertEquals(originalInvocation, download.programInvocationArray)
        assertTrue(state.isFinished)
    }

    @ParameterizedTest
    @ValueSource(ints = [0, 1])
    fun `cancellation at FFmpeg exit preserves the part without retrying`(exitCode: Int) {
        val download = standardFfmpegDownload().apply { isRestart = true }
        val final = Path.of(download.targetPathFileName)
        val part = final.resolveSibling("Film mit Leerzeichen.mp4.part")
        val state = requireNotNull(download.runtime.runState)
        val process = mock(Process::class.java)
        `when`(process.exitValue()).thenAnswer {
            state.requestStop()
            exitCode
        }
        mockConstruction(RuntimeExec::class.java) { runtime, _ ->
            `when`(runtime.exec(true)).thenAnswer {
                Files.write(part, ByteArray(Konstanten.MIN_FILM_FILE_SIZE_KB.toInt()))
                process
            }
        }.use {
            runner(download).run()
            assertEquals(1, it.constructed().size)
        }
        assertFalse(Files.exists(final))
        assertTrue(Files.exists(part))
        assertNull(download.runtime.runState)
        assertFalse(state.isError)
        assertFalse(state.isFinished)
    }

    @ParameterizedTest
    @ValueSource(booleans = [false, true])
    fun `failed FFmpeg download does not publish a final file`(processFailed: Boolean) {
        val download = standardFfmpegDownload()
        val final = Path.of(download.targetPathFileName)
        val part = final.resolveSibling("Film mit Leerzeichen.mp4.part")
        val state = requireNotNull(download.runtime.runState)
        val process = mock(Process::class.java)
        `when`(process.exitValue()).thenReturn(if (processFailed) 1 else 0)
        mockConstruction(RuntimeExec::class.java) { runtime, _ ->
            `when`(runtime.exec(true)).thenAnswer {
                if (processFailed) Files.write(part, ByteArray(Konstanten.MIN_FILM_FILE_SIZE_KB.toInt()))
                process
            }
        }.use {
            runner(download).run()
        }
        assertFalse(Files.exists(final))
        assertEquals(processFailed, Files.exists(part))
        assertTrue(state.isError)
    }

    @ParameterizedTest
    @ValueSource(booleans = [false, true])
    fun `FFmpeg retries independent outputs up to the attempt limit`(lastAttemptSucceeds: Boolean) {
        val download = standardFfmpegDownload().apply { isRestart = true }
        val final = Path.of(download.targetPathFileName)
        val part = final.resolveSibling("Film mit Leerzeichen.mp4.part")
        val state = requireNotNull(download.runtime.runState)
        mockConstruction(RuntimeExec::class.java) { runtime, context ->
            val attempt = context.count
            val process = mock(Process::class.java)
            val succeeds = lastAttemptSucceeds && attempt == Konstanten.MAX_EXTERNAL_STARTS
            `when`(process.exitValue()).thenReturn(if (succeeds) 0 else 1)
            doAnswer {
                state.updateProgress(530)
                null
            }.`when`(runtime).awaitProgress()
            `when`(runtime.exec(true)).thenAnswer {
                assertFalse(Files.exists(part))
                assertEquals(DownloadRunState.PROGRESS_NICHT_GESTARTET, state.percent)
                val size = Konstanten.MIN_FILM_FILE_SIZE_KB.toInt() + if (lastAttemptSucceeds) 0 else attempt
                Files.write(part, ByteArray(size) { attempt.toByte() })
                process
            }
        }.use {
            runner(download).run()
            assertEquals(Konstanten.MAX_EXTERNAL_STARTS, it.constructed().size)
        }
        assertEquals(lastAttemptSucceeds, state.isFinished)
        assertEquals(!lastAttemptSucceeds, state.isError)
        val result = if (lastAttemptSucceeds) final else part
        assertEquals(Konstanten.MAX_EXTERNAL_STARTS.toByte(), Files.readAllBytes(result).first())
        assertFalse(Files.exists(if (lastAttemptSucceeds) part else final))
    }

    @ParameterizedTest
    @CsvSource("0,false", "1,false", "0,true")
    fun `existing MP4 is replaced only after an approved download succeeds`(exitCode: Int, stop: Boolean) {
        val download = standardFfmpegDownload()
        val final = Path.of(download.targetPathFileName)
        val part = final.resolveSibling("Film mit Leerzeichen.mp4.part")
        val original = "existing small MP4"
        val replacement = ByteArray(Konstanten.MIN_FILM_FILE_SIZE_KB.toInt()) { 34 }
        Files.writeString(final, original)
        val state = requireNotNull(download.runtime.runState)
        val process = mock(Process::class.java)
        `when`(process.exitValue()).thenAnswer {
            assertEquals(original, Files.readString(final))
            if (stop) state.requestStop()
            exitCode
        }
        inCliDownloadMode {
            mockConstruction(RuntimeExec::class.java) { runtime, _ ->
                `when`(runtime.exec(true)).thenAnswer {
                    Files.write(part, replacement)
                    process
                }
            }.use {
                runner(download).run()
                assertEquals(1, it.constructed().size)
            }
        }
        if (exitCode == 0 && !stop) {
            assertArrayEquals(replacement, Files.readAllBytes(final))
            assertFalse(Files.exists(part))
        } else {
            assertEquals(original, Files.readString(final))
            assertTrue(Files.exists(part))
        }
    }

    @Test
    fun `a target created during FFmpeg output requires overwrite approval`() {
        val download = standardFfmpegDownload()
        val final = Path.of(download.targetPathFileName)
        val part = final.resolveSibling("Film mit Leerzeichen.mp4.part")
        val state = requireNotNull(download.runtime.runState)
        val process = mock(Process::class.java)
        `when`(process.exitValue()).thenReturn(0)
        inCliDownloadMode {
            mockConstruction(RuntimeExec::class.java) { runtime, _ ->
                `when`(runtime.exec(true)).thenAnswer {
                    Files.write(part, ByteArray(Konstanten.MIN_FILM_FILE_SIZE_KB.toInt()))
                    Files.writeString(final, "created while FFmpeg was running")
                    process
                }
            }.use { runner(download).run() }
        }
        assertEquals("created while FFmpeg was running", Files.readString(final))
        assertTrue(Files.exists(part))
        assertTrue(state.isError)
    }

    @Test
    fun `cancelling an older FFmpeg run retains the newer run state`() {
        val download = standardFfmpegDownload()
        val state = requireNotNull(download.runtime.runState)
        val nextState = DownloadRunState()
        val process = mock(Process::class.java)
        mockConstruction(RuntimeExec::class.java) { runtime, _ ->
            `when`(runtime.exec(true)).thenAnswer {
                state.requestStop()
                download.runtime.runState = nextState
                process
            }
        }.use { runner(download).run() }
        verify(process).destroy()
        assertSame(nextState, download.runtime.runState)
    }

    @Test
    fun `cancelling FFmpeg forces termination when the process does not exit gracefully`() {
        val download = standardFfmpegDownload()
        val state = requireNotNull(download.runtime.runState)
        val process = mock(Process::class.java)
        `when`(process.isAlive).thenReturn(true, false)
        `when`(process.waitFor(1, SECONDS)).thenReturn(false, true)
        mockConstruction(RuntimeExec::class.java) { runtime, _ ->
            `when`(runtime.exec(true)).thenAnswer {
                state.requestStop()
                process
            }
        }.use { runner(download).run() }

        val order = inOrder(process)
        order.verify(process).destroy()
        order.verify(process).waitFor(1, SECONDS)
        order.verify(process).destroyForcibly()
        order.verify(process).waitFor(1, SECONDS)
        assertNull(download.runtime.runState)
        assertFalse(state.isError)
    }

    @Test
    fun `a restart waits for the cancelled FFmpeg process before reusing its part file`() {
        val download = standardFfmpegDownload()
        val originalRun = requireNotNull(download.runtime.runState)
        val original = requireNotNull(FfmpegMp4Output.forDownload(download, originalRun))
        val restart = requireNotNull(FfmpegMp4Output.forDownload(download, DownloadRunState()))
        val process = mock(Process::class.java)
        val exit = CompletableFuture<Process>()
        `when`(process.isAlive).thenReturn(true)
        `when`(process.onExit()).thenReturn(exit)

        try {
            assertNotNull(original.prepareAttempt(null))
            val part = original.partFile.toPath()
            Files.writeString(part, "still in use")
            assertThrows(IOException::class.java) { restart.prepareAttempt(null) }

            originalRun.requestStop()
            original.releaseAfter(process)
            Executors.newVirtualThreadPerTaskExecutor().use { executor ->
                val attempt = executor.submit<List<String>?> { restart.prepareAttempt(null) }
                try {
                    assertThrows(TimeoutException::class.java) { attempt.get(100, MILLISECONDS) }
                    assertEquals("still in use", Files.readString(part))
                } finally {
                    exit.complete(process)
                }
                assertNotNull(attempt.get(2, SECONDS))
                assertFalse(Files.exists(part))
            }
        } finally {
            original.releaseAfter(null)
            restart.releaseAfter(null)
        }
    }

    private fun inCliDownloadMode(block: () -> Unit) {
        val cli = CommandLine(CommandLineOptions)
        cli.parseArgs("--download-quit")
        try {
            block()
        } finally {
            cli.parseArgs()
        }
    }

    private fun standardFfmpegDownload(windows: Boolean = false, automaticMap: Boolean = false) = download().apply {
        sender = "ZDF"
        downloadUrl = "https://example.invalid/film.m3u8"
        targetPathFileName = directory.resolve("Film mit Leerzeichen.mp4").toString()
        val executable = if (windows) "bin\\ffmpeg.exe" else "/path with spaces/ffmpeg"
        val target = if (windows) "\"$targetPathFileName\"" else targetPathFileName
        automaticArteHlsProgramMap = automaticMap
        val arguments = buildList {
            add(executable)
            if (automaticMap) addAll(listOf("-http_seekable", "0"))
            addAll(listOf("-i", downloadUrl))
            if (automaticMap) addAll(listOf("-map", "p:3", "-map", "-0:s"))
            addAll(listOf("-c", "copy", "-bsf:a", "aac_adtstoasc", target))
        }
        programInvocation = arguments.joinToString(" ")
        programInvocationArray = arguments.joinToString(RuntimeExec.TRENNER_PROG_ARRAY)
    }

    private fun runner(
        download: DatenDownload,
        history: () -> AboHistoryController = { error("Unexpected subscription history access") },
    ) = ExternalProgramDownload(
        aboHistoryControllerProvider = history,
        datenDownload = download,
        notificationPublisher = DisabledNotificationBackend,
    )

    private fun download() = DatenDownload().apply {
        title = "Startup cancellation"
        sender = "ARTE.DE"
        quelle = DownloadSource.BUTTON
        art = DownloadType.PROGRAM
        // Non-HLS URL keeps these lifecycle tests independent of HTTP and playlist parsing.
        downloadUrl = "https://example.invalid/film.mp4"
        targetPath = directory.toString()
        targetPathFileName = directory.resolve("film.mp4").toString()
        runtime.runState = DownloadRunState().apply { startTime = LocalDateTime.now() }
        programInvocation = "ffmpeg -i $downloadUrl $targetPathFileName"
    }
}
