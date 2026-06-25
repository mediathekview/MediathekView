package mediathek.mainwindow

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

class MainWindowQuitControllerTest {
    @Test
    fun `declined quit resets in-progress guard`() {
        var confirmationRequests = 0
        val shutdownRequests = mutableListOf<Pair<Boolean, Boolean>>()
        val controller = testController(
            quitConfirmer = {
                confirmationRequests++
                if (confirmationRequests == 1) {
                    MainWindowQuitController.QuitConfirmation.declined()
                } else {
                    MainWindowQuitController.QuitConfirmation(canQuit = true, shutdownComputer = false)
                }
            },
            shutdownStarter = { shutdownComputer, resetSettingsOnQuit ->
                shutdownRequests += shutdownComputer to resetSettingsOnQuit
            },
        )

        assertFalse(controller.quitApplication())
        assertTrue(controller.quitApplication())

        assertEquals(2, confirmationRequests)
        assertEquals(listOf(false to false), shutdownRequests)
    }

    @Test
    fun `additional quit requests are ignored after shutdown starts`() {
        var confirmationRequests = 0
        val shutdownRequests = mutableListOf<Pair<Boolean, Boolean>>()
        val controller = testController(
            quitConfirmer = {
                confirmationRequests++
                MainWindowQuitController.QuitConfirmation(canQuit = true, shutdownComputer = false)
            },
            shutdownStarter = { shutdownComputer, resetSettingsOnQuit ->
                shutdownRequests += shutdownComputer to resetSettingsOnQuit
            },
        )

        assertTrue(controller.quitApplication())
        assertTrue(controller.quitApplication())

        assertEquals(1, confirmationRequests)
        assertEquals(listOf(false to false), shutdownRequests)
    }

    @Test
    fun `settings reset flag is passed to shutdown starter`() {
        val shutdownRequests = mutableListOf<Pair<Boolean, Boolean>>()
        val controller = testController(
            quitConfirmer = { requestedShutdown ->
                MainWindowQuitController.QuitConfirmation(canQuit = true, shutdownComputer = requestedShutdown)
            },
            shutdownStarter = { shutdownComputer, resetSettingsOnQuit ->
                shutdownRequests += shutdownComputer to resetSettingsOnQuit
            },
        )

        controller.requestSettingsResetOnQuit()

        assertTrue(controller.quitApplication(shutdownComputer = true))
        assertEquals(listOf(true to true), shutdownRequests)
    }

    @Test
    fun `non EDT quit confirmation runs through EDT runner`() {
        val runnerDescriptions = mutableListOf<String>()
        val controller = testController(
            edtRunner = { description, action ->
                runnerDescriptions += description
                action.run()
            },
            quitConfirmer = {
                MainWindowQuitController.QuitConfirmation(canQuit = true, shutdownComputer = false)
            },
            shutdownStarter = { _, _ -> },
        )

        assertTrue(controller.quitApplication())
        assertEquals(listOf("Confirm application quit"), runnerDescriptions)
    }

    private fun testController(
        edtRunner: ShutdownEdtRunner = ShutdownEdtRunner { _, action -> action.run() },
        quitConfirmer: (Boolean) -> MainWindowQuitController.QuitConfirmation,
        shutdownStarter: (shutdownComputer: Boolean, resetSettingsOnQuit: Boolean) -> Unit,
    ): MainWindowQuitController = MainWindowQuitController(
        edtRunner = edtRunner,
        quitConfirmer = quitConfirmer,
        shutdownStarter = shutdownStarter,
    )
}
