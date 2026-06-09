package mediathek.config.application

import org.apache.commons.configuration2.Configuration
import org.apache.commons.configuration2.sync.LockMode

inline fun <T> Configuration.withLock(lockMode: LockMode, action: Configuration.() -> T): T {
    try {
        lock(lockMode)
        return action()
    } finally {
        unlock(lockMode)
    }
}