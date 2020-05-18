package mediathek.tool;

import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.core.LifeCycle;
import org.apache.logging.log4j.core.util.Cancellable;
import org.apache.logging.log4j.core.util.ShutdownCallbackRegistry;
import org.apache.logging.log4j.status.StatusLogger;

import java.io.Serializable;
import java.lang.ref.Reference;
import java.lang.ref.SoftReference;
import java.util.Collection;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Properly shutdown Log4j logging infrastructure.
 * Regular shutdown() does not seem to work.
 */
public class Log4jShutdownCallbackRegistry implements ShutdownCallbackRegistry, LifeCycle, Runnable, Serializable {

    protected static final Logger LOGGER = StatusLogger.getLogger();
    private static final Collection<Log4jShutdownCallbackRegistry> INSTANCES = new CopyOnWriteArrayList<>();
    private final AtomicReference<State> state = new AtomicReference<>(State.INITIALIZED);
    private final Collection<Cancellable> hooks = new CopyOnWriteArrayList<>();

    public Log4jShutdownCallbackRegistry() {
        INSTANCES.add(this);
    }

    public static void execute() {
        for (var instance : INSTANCES) {
            try {
                instance.run();
            } catch (final Throwable t) {
                LOGGER.error(SHUTDOWN_HOOK_MARKER, "Caught exception executing shutdown hook {}", instance, t);
            }
        }
    }

    @Override
    public void run() {
        if (state.compareAndSet(State.STARTED, State.STOPPING)) {
            for (var hook : hooks) {
                try {
                    hook.run();
                } catch (final Throwable t) {
                    LOGGER.error(SHUTDOWN_HOOK_MARKER, "Caught exception executing shutdown hook {}", hook, t);
                }
            }
            state.set(State.STOPPED);
        }
    }

    @Override
    public Cancellable addShutdownCallback(final Runnable callback) {
        if (isStarted()) {
            final Cancellable receipt = new Cancellable() {
                // use a reference to prevent memory leaks
                private final Reference<Runnable> hook = new SoftReference<>(callback);

                @Override
                public void cancel() {
                    hook.clear();
                    hooks.remove(this);
                }

                @Override
                public void run() {
                    final Runnable hook = this.hook.get();
                    if (hook != null) {
                        hook.run();
                        this.hook.clear();
                    }
                }

                @Override
                public String toString() {
                    return String.valueOf(hook.get());
                }
            };
            hooks.add(receipt);
            return receipt;
        }
        throw new IllegalStateException("Can´t add shutdown hook as this is one not started. State: " +
                state.get().name());
    }

    @Override
    public void start() {
        state.set(State.STARTED);
    }

    @Override
    public void stop() {
        state.set(State.STOPPED);
    }

    public State getState() {
        return state.get();
    }

    @Override
    public void initialize() {
        // needs to be implemented but never gets called...
    }

    @Override
    public boolean isStarted() {
        return state.get() == State.STARTED;
    }

    @Override
    public boolean isStopped() {
        return state.get() == State.STOPPED;
    }

}