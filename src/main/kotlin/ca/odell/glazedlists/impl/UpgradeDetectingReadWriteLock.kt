/*
 * Copyright (c) 2026 derreisende77.
 *
 * This file is part of the MediathekView project:
 * https://github.com/mediathekview/MediathekView
 *
 * MediathekView is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * MediathekView is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package ca.odell.glazedlists.impl

import java.util.concurrent.TimeUnit
import java.util.concurrent.locks.ReentrantReadWriteLock

/**
 * A reentrant read/write lock that fails fast for unsupported read-to-write upgrades.
 */
internal class UpgradeDetectingReadWriteLock : ReentrantReadWriteLock() {
    private val guardedWriteLock = UpgradeDetectingWriteLock(this)

    override fun writeLock(): WriteLock = guardedWriteLock

    private class UpgradeDetectingWriteLock(
        private val owner: UpgradeDetectingReadWriteLock,
    ) : WriteLock(owner) {
        override fun lock() {
            owner.checkWriteAcquisition()
            super.lock()
        }

        override fun lockInterruptibly() {
            owner.checkWriteAcquisition()
            super.lockInterruptibly()
        }

        override fun tryLock(): Boolean {
            owner.checkWriteAcquisition()
            return super.tryLock()
        }

        override fun tryLock(timeout: Long, unit: TimeUnit): Boolean {
            owner.checkWriteAcquisition()
            return super.tryLock(timeout, unit)
        }
    }

    private fun checkWriteAcquisition() {
        check(readHoldCount == 0 || isWriteLockedByCurrentThread) {
            "Read-to-write lock upgrades are unsupported because they deadlock"
        }
    }
}
