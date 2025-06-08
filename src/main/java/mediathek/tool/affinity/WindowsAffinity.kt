/*
 * Copyright (c) 2025 derreisende77.
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

package mediathek.tool.affinity

import org.apache.logging.log4j.LogManager
import java.lang.foreign.*
import java.lang.invoke.MethodHandle

class WindowsAffinity : IAffinity {
    override fun setDesiredCpuAffinity(numCpus: Int) {
        require(numCpus in 1..64) { "numCpus must be between 1 and 64" }

        val affinityMask = (0 until numCpus).fold(0L) { acc, i -> acc or (1L shl i) }

        Linker.nativeLinker().let { linker ->
            val kernel32 = SymbolLookup.libraryLookup("kernel32.dll", Arena.global())

            val getCurrentProcess: MethodHandle = linker.downcallHandle(
                kernel32.find("GetCurrentProcess").orElseThrow(),
                FunctionDescriptor.of(ValueLayout.ADDRESS)
            )

            val setAffinity: MethodHandle = linker.downcallHandle(
                kernel32.find("SetProcessAffinityMask").orElseThrow(),
                FunctionDescriptor.of(ValueLayout.JAVA_INT, ValueLayout.ADDRESS, ValueLayout.JAVA_LONG)
            )

            val processHandle = getCurrentProcess.invoke() as MemorySegment
            val result = setAffinity.invoke(processHandle, affinityMask) as Int

            if (result != 0) {
                logger.info("CPU affinity was set successfully to mask: 0x${affinityMask.toString(16)}")
            } else {
                logger.warn("Failed to set CPU affinity.")
            }
        }
    }

    companion object {
        private val logger = LogManager.getLogger()
    }
}
