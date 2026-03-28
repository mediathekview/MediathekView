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

package mediathek.mac;

import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.lang.foreign.Arena;
import java.lang.foreign.FunctionDescriptor;
import java.lang.foreign.Linker;
import java.lang.foreign.SymbolLookup;
import java.lang.invoke.MethodHandle;

public final class MacAccessibilityPermission {
    private static final Logger LOG = LogManager.getLogger();
    private static final MethodHandle AX_IS_PROCESS_TRUSTED;

    static {
        MethodHandle axIsProcessTrusted = null;

        if (SystemUtils.IS_OS_MAC_OSX) {
            var linker = Linker.nativeLinker();
            var applicationServices = SymbolLookup.libraryLookup(
                    "/System/Library/Frameworks/ApplicationServices.framework/ApplicationServices",
                    Arena.global()
            );

            try {
                axIsProcessTrusted = linker.downcallHandle(
                        applicationServices.find("AXIsProcessTrusted").orElseThrow(),
                        FunctionDescriptor.of(java.lang.foreign.ValueLayout.JAVA_BOOLEAN)
                );
            }
            catch (Throwable t) {
                LOG.error("Failed to initialize AXIsProcessTrusted access.", t);
            }
        }

        AX_IS_PROCESS_TRUSTED = axIsProcessTrusted;
    }

    private MacAccessibilityPermission() {
    }

    public static boolean isTrusted() {
        if (!SystemUtils.IS_OS_MAC_OSX || AX_IS_PROCESS_TRUSTED == null) {
            return false;
        }

        try {
            return (boolean) AX_IS_PROCESS_TRUSTED.invokeExact();
        }
        catch (Throwable t) {
            LOG.error("Failed to query macOS Accessibility permission.", t);
            return false;
        }
    }
}
