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

package mediathek.mac

import org.apache.commons.lang3.SystemUtils
import org.apache.logging.log4j.LogManager
import java.lang.foreign.*
import java.lang.invoke.MethodHandle

object MacAccessibilityPermission {
    private val log = LogManager.getLogger()
    private val axIsProcessTrusted: MethodHandle? = initializeAxIsProcessTrusted()

    fun isTrusted(): Boolean {
        if (!SystemUtils.IS_OS_MAC_OSX || axIsProcessTrusted == null) {
            return false
        }

        return try {
            axIsProcessTrusted.invokeExact() as Boolean
        } catch (throwable: Throwable) {
            log.error("Failed to query macOS Accessibility permission.", throwable)
            false
        }
    }

    private fun initializeAxIsProcessTrusted(): MethodHandle? {
        if (!SystemUtils.IS_OS_MAC_OSX) {
            return null
        }

        val linker = Linker.nativeLinker()
        val applicationServices = SymbolLookup.libraryLookup(
            "/System/Library/Frameworks/ApplicationServices.framework/ApplicationServices",
            Arena.global(),
        )

        return try {
            linker.downcallHandle(
                applicationServices.find("AXIsProcessTrusted").orElseThrow(),
                FunctionDescriptor.of(ValueLayout.JAVA_BOOLEAN),
            )
        } catch (throwable: Throwable) {
            log.error("Failed to initialize AXIsProcessTrusted access.", throwable)
            null
        }
    }
}
