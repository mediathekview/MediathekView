/*
 * Copyright (c) 2025-2026 derreisende77.
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

package mediathek.tool

import org.apache.commons.lang3.SystemUtils
import java.lang.foreign.Arena
import java.lang.foreign.FunctionDescriptor
import java.lang.foreign.Linker
import java.lang.foreign.SymbolLookup
import java.lang.foreign.ValueLayout.ADDRESS
import java.lang.foreign.ValueLayout.JAVA_INT
import java.lang.foreign.ValueLayout.JAVA_LONG
import java.util.concurrent.TimeUnit

internal object SleepAwareElapsedClock {
    private const val MAC_CLOCK_MONOTONIC_RAW = 4
    private const val LINUX_CLOCK_BOOTTIME = 7

    private val readMillis by lazy { selectClock() }

    fun millis(): Long? = readMillis?.invoke()

    private fun selectClock(): (() -> Long)? {
        return try {
            val clock = when {
                SystemUtils.IS_OS_MAC_OSX -> macClock()
                SystemUtils.IS_OS_LINUX -> linuxClock()
                SystemUtils.IS_OS_WINDOWS -> windowsClock()
                else -> return null
            }
            clock()
            clock
        } catch (_: RuntimeException) {
            null
        } catch (_: LinkageError) {
            null
        }
    }

    private fun macClock(): () -> Long {
        val linker = Linker.nativeLinker()
        val clockGettimeNanos = linker.downcallHandle(
            linker.defaultLookup().find("clock_gettime_nsec_np").orElseThrow(),
            FunctionDescriptor.of(JAVA_LONG, JAVA_INT),
        )
        return {
            val nanos = clockGettimeNanos.invokeExact(MAC_CLOCK_MONOTONIC_RAW) as Long
            check(nanos > 0) { "Could not read CLOCK_MONOTONIC_RAW" }
            TimeUnit.NANOSECONDS.toMillis(nanos)
        }
    }

    private fun linuxClock(): () -> Long {
        val linker = Linker.nativeLinker()
        val clockGettime = linker.downcallHandle(
            linker.defaultLookup().find("clock_gettime").orElseThrow(),
            FunctionDescriptor.of(JAVA_INT, JAVA_INT, ADDRESS),
        )
        // The supported 64-bit Linux targets use two 64-bit fields in struct timespec.
        return {
            Arena.ofConfined().use { arena ->
                val timespec = arena.allocate(JAVA_LONG, 2)
                val result = clockGettime.invokeExact(LINUX_CLOCK_BOOTTIME, timespec) as Int
                check(result == 0) { "Could not read CLOCK_BOOTTIME" }
                TimeUnit.SECONDS.toMillis(timespec[JAVA_LONG, 0L]) +
                    TimeUnit.NANOSECONDS.toMillis(timespec[JAVA_LONG, JAVA_LONG.byteSize()])
            }
        }
    }

    private fun windowsClock(): () -> Long {
        val linker = Linker.nativeLinker()
        val kernel32 = SymbolLookup.libraryLookup("kernel32.dll", Arena.global())
        val getTickCount64 = linker.downcallHandle(
            kernel32.find("GetTickCount64").orElseThrow(),
            FunctionDescriptor.of(JAVA_LONG),
        )
        return { getTickCount64.invokeExact() as Long }
    }
}
