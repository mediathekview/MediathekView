/*
 * Copyright (c) 2026 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.swing

import kotlinx.coroutines.suspendCancellableCoroutine
import java.lang.reflect.InvocationTargetException
import javax.swing.SwingUtilities
import kotlin.coroutines.resume
import kotlin.coroutines.resumeWithException

fun interface SwingDispatcher {
    fun dispatch(action: Runnable)
}

object SwingDispatch : SwingDispatcher {
    override fun dispatch(action: Runnable) {
        if (SwingUtilities.isEventDispatchThread()) {
            action.run()
        } else {
            SwingUtilities.invokeLater(action)
        }
    }

    fun dispatch(action: () -> Unit) {
        dispatch(Runnable(action))
    }

    fun runAndWait(description: String = "EDT action", action: Runnable) {
        callAndWait(description) {
            action.run()
        }
    }

    fun <T> callAndWait(description: String = "EDT action", action: () -> T): T {
        if (SwingUtilities.isEventDispatchThread()) {
            return action()
        }

        var result: Result<T>? = null
        try {
            SwingUtilities.invokeAndWait {
                result = runCatching(action)
            }
        } catch (exception: InterruptedException) {
            Thread.currentThread().interrupt()
            throw IllegalStateException("$description interrupted", exception)
        } catch (exception: InvocationTargetException) {
            throw IllegalStateException("$description failed", exception.cause)
        }

        return checkNotNull(result) { "$description did not produce a result" }.getOrThrow()
    }

    suspend fun <T> call(action: () -> T): T {
        if (SwingUtilities.isEventDispatchThread()) {
            return action()
        }

        return suspendCancellableCoroutine { continuation ->
            SwingUtilities.invokeLater {
                if (!continuation.isActive) {
                    return@invokeLater
                }

                try {
                    continuation.resume(action())
                } catch (exception: Throwable) {
                    continuation.resumeWithException(exception)
                }
            }
        }
    }
}
