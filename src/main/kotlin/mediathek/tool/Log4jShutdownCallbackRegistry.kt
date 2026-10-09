package mediathek.tool

import org.apache.logging.log4j.Logger
import org.apache.logging.log4j.core.LifeCycle
import org.apache.logging.log4j.core.util.Cancellable
import org.apache.logging.log4j.core.util.ShutdownCallbackRegistry
import org.apache.logging.log4j.status.StatusLogger
import java.io.Serializable
import java.lang.ref.Reference
import java.lang.ref.SoftReference
import java.util.concurrent.CopyOnWriteArrayList
import java.util.concurrent.atomic.AtomicReference

/**
 * Properly shutdown Log4j logging infrastructure.
 * Regular shutdown() does not seem to work.
 */
class Log4jShutdownCallbackRegistry : ShutdownCallbackRegistry, LifeCycle, Runnable, Serializable {
    private val state = AtomicReference(LifeCycle.State.INITIALIZED)
    private val hooks: MutableCollection<Cancellable> = CopyOnWriteArrayList()
    override fun run() {
        if (state.compareAndSet(LifeCycle.State.STARTED, LifeCycle.State.STOPPING)) {
            for (hook in hooks) {
                try {
                    hook.run()
                } catch (t: Throwable) {
                    LOGGER.error(ShutdownCallbackRegistry.SHUTDOWN_HOOK_MARKER, "Caught exception executing shutdown hook {}", hook, t)
                }
            }
            state.set(LifeCycle.State.STOPPED)
        }
    }

    override fun addShutdownCallback(callback: Runnable): Cancellable {
        if (isStarted) {
            val receipt: Cancellable = object : Cancellable {
                // use a reference to prevent memory leaks
                private val hook: Reference<Runnable> = SoftReference(callback)
                override fun cancel() {
                    hook.clear()
                    hooks.remove(this)
                }

                override fun run() {
                    val hook = hook.get()
                    if (hook != null) {
                        hook.run()
                        this.hook.clear()
                    }
                }

                override fun toString(): String {
                    return hook.get().toString()
                }
            }
            hooks.add(receipt)
            return receipt
        }
        throw IllegalStateException("Can´t add shutdown hook as this is one not started. State: " +
                state.get().name)
    }

    override fun start() {
        state.set(LifeCycle.State.STARTED)
    }

    override fun stop() {
        state.set(LifeCycle.State.STOPPED)
    }

    override fun getState(): LifeCycle.State {
        return state.get()
    }

    override fun initialize() {
        // needs to be implemented but never gets called...
    }

    override fun isStarted(): Boolean {
        return state.get() == LifeCycle.State.STARTED
    }

    override fun isStopped(): Boolean {
        return state.get() == LifeCycle.State.STOPPED
    }

    companion object {
        private val LOGGER: Logger = StatusLogger.getLogger()
        private val INSTANCES: MutableCollection<Log4jShutdownCallbackRegistry> = CopyOnWriteArrayList()
        fun execute() {
            for (instance in INSTANCES) {
                try {
                    instance.run()
                } catch (t: Throwable) {
                    LOGGER.error(ShutdownCallbackRegistry.SHUTDOWN_HOOK_MARKER, "Caught exception executing shutdown hook {}", instance, t)
                }
            }
        }
    }

    init {
        INSTANCES.add(this)
    }
}