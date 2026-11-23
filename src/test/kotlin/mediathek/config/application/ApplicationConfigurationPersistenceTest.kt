package mediathek.config.application

import org.apache.commons.configuration2.XMLConfiguration
import org.apache.commons.configuration2.io.FileHandler
import org.apache.commons.configuration2.sync.LockMode
import org.apache.commons.configuration2.sync.ReadWriteSynchronizer
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.condition.EnabledOnOs
import org.junit.jupiter.api.condition.OS
import org.junit.jupiter.api.io.TempDir
import org.mockito.Mockito.mock
import java.io.IOException
import java.io.Writer
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.attribute.PosixFilePermissions
import java.util.concurrent.CountDownLatch
import java.util.concurrent.FutureTask
import java.util.concurrent.ScheduledFuture
import java.util.concurrent.TimeUnit.SECONDS

class ApplicationConfigurationPersistenceTest {
    @TempDir
    lateinit var directory: Path

    @Test
    fun `updating dialog bounds completes while autosave is waiting for configuration`() {
        val autosaveWaiting = CountDownLatch(1)
        val xml = XMLConfiguration().apply {
            synchronizer = object : ReadWriteSynchronizer() {
                override fun beginWrite() {
                    if (Thread.currentThread().name == "test-autosave") {
                        autosaveWaiting.countDown()
                    }
                    super.beginWrite()
                }
            }
        }
        val settingsPath = directory.resolve("settings.xml")
        val scheduler = ManualAutosave()
        val configuration = ApplicationConfiguration(settingsPath, xml, scheduler::schedule)
        val previousSettings = Files.readAllBytes(settingsPath)
        val originalAutosave = FutureTask(scheduler.task, Unit)
        val update = FutureTask {
            xml.withLock(LockMode.WRITE) {
                Thread.ofVirtual().name("test-autosave").start(originalAutosave)
                assertTrue(autosaveWaiting.await(5, SECONDS), "Autosave did not attempt to lock configuration")
                configuration.setSettingsDialogBounds(10, 20, 640, 480)
            }
        }
        // Virtual threads allow the test to time out even if the two workers deadlock.
        Thread.startVirtualThread(update)

        update.get(5, SECONDS)
        originalAutosave.get(5, SECONDS)
        assertArrayEquals(previousSettings, Files.readAllBytes(settingsPath), "Superseded autosave ran")

        scheduler.task.run()
        assertEquals(640, loadSettings(settingsPath).getInt("application.ui.settings_dialog.width"))
    }

    @Test
    fun `failed autosave preserves previous XML and the next autosave replaces it`() {
        val settingsPath = directory.resolve("settings.xml")
        val scheduler = ManualAutosave()
        var failWrite = false
        var previousSettings = byteArrayOf()
        val xml = object : XMLConfiguration() {
            override fun write(writer: Writer) {
                if (failWrite) {
                    writer.write("<settings><partial>")
                    writer.flush()
                    assertArrayEquals(previousSettings, Files.readAllBytes(settingsPath))
                    throw IOException("Simulated partial settings write")
                }
                super.write(writer)
            }
        }.apply { synchronizer = ReadWriteSynchronizer() }
        val configuration = ApplicationConfiguration(settingsPath, xml, scheduler::schedule)
        configuration.setUserAgent("Original")
        scheduler.task.run()
        previousSettings = Files.readAllBytes(settingsPath)

        failWrite = true
        configuration.setUserAgent("Updated ä")
        scheduler.task.run()

        assertArrayEquals(previousSettings, Files.readAllBytes(settingsPath))
        Files.list(directory).use { assertEquals(listOf(settingsPath), it.toList()) }

        failWrite = false
        configuration.setUserAgent("Recovered ä")
        scheduler.task.run()

        assertEquals("Recovered ä", loadSettings(settingsPath).getString("application.user_agent"))
        Files.list(directory).use { assertEquals(listOf(settingsPath), it.toList()) }
    }

    @Test
    @EnabledOnOs(OS.MAC, OS.LINUX)
    fun `explicit save replaces the symlink target and retains its permissions`() {
        val targetDirectory = Files.createDirectory(directory.resolve("profile"))
        val target = targetDirectory.resolve("actual-settings.xml")
        Files.writeString(target, "<settings/>")
        val permissions = PosixFilePermissions.fromString("rw-r-----")
        Files.setPosixFilePermissions(target, permissions)
        val settingsPath = directory.resolve("settings.xml")
        val linkTarget = directory.relativize(target)
        Files.createSymbolicLink(settingsPath, linkTarget)
        val scheduler = ManualAutosave()
        val configuration = ApplicationConfiguration(settingsPath, scheduleAutosave = scheduler::schedule)

        configuration.setUserAgent("Updated through the symlink")
        configuration.writeConfiguration()

        assertEquals(linkTarget, Files.readSymbolicLink(settingsPath))
        assertEquals(permissions, Files.getPosixFilePermissions(target))
        assertEquals("Updated through the symlink", loadSettings(target).getString("application.user_agent"))
    }

    @Test
    fun `dry run keeps pending autosave and cleanup backs up the previous settings`() {
        val settingsPath = directory.resolve("settings.xml")
        val scheduler = ManualAutosave()
        val xml = XMLConfiguration().apply { synchronizer = ReadWriteSynchronizer() }
        val configuration = ApplicationConfiguration(settingsPath, xml, scheduler::schedule)
        xml.setProperty("obsolete.setting", "value")

        val dryRun = configuration.cleanupConfiguration(dryRun = true)
        assertEquals(listOf("obsolete.setting"), dryRun.removedKeys)
        assertNull(dryRun.backupPath)
        scheduler.task.run()
        val previousSettings = Files.readAllBytes(settingsPath)
        assertEquals("value", loadSettings(settingsPath).getString("obsolete.setting"))

        val cleanup = configuration.cleanupConfiguration(dryRun = false)
        assertArrayEquals(previousSettings, Files.readAllBytes(checkNotNull(cleanup.backupPath)))
        assertFalse(loadSettings(settingsPath).containsKey("obsolete.setting"))

        configuration.setUserAgent("Saved after cleanup")
        scheduler.task.run()
        assertEquals("Saved after cleanup", loadSettings(settingsPath).getString("application.user_agent"))
    }

    private fun loadSettings(path: Path): XMLConfiguration = XMLConfiguration().also {
        FileHandler(it).load(path.toFile())
    }

    private class ManualAutosave {
        lateinit var task: Runnable
            private set

        fun schedule(task: Runnable): ScheduledFuture<*> {
            this.task = task
            return mock(ScheduledFuture::class.java)
        }
    }
}
