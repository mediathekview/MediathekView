package mediathek.mainwindow

import org.apache.logging.log4j.LogManager

fun interface ShutdownEdtRunner {
    fun run(description: String, action: Runnable)
}

enum class ShutdownThread {
    EDT,
    BACKGROUND,
}

data class ShutdownStep(
    val description: String,
    val thread: ShutdownThread,
    val action: Runnable,
)

class ShutdownCoordinator(
    private val edtRunner: ShutdownEdtRunner,
) {
    private val steps = mutableListOf<ShutdownStep>()

    fun register(step: ShutdownStep): ShutdownCoordinator {
        steps += step
        return this
    }

    fun shutdown() {
        steps.forEach(::runStep)
    }

    private fun runStep(step: ShutdownStep) {
        logger.trace(step.description)
        try {
            when (step.thread) {
                ShutdownThread.EDT -> edtRunner.run(step.description, step.action)
                ShutdownThread.BACKGROUND -> step.action.run()
            }
        } catch (ex: RuntimeException) {
            logger.error("Shutdown step failed: {}", step.description, ex)
        }
    }

    companion object {
        private val logger = LogManager.getLogger(ShutdownCoordinator::class.java)
    }
}
