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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.gui.bookmark

import ca.odell.glazedlists.ObservableElementChangeHandler
import ca.odell.glazedlists.ObservableElementList
import java.beans.PropertyChangeListener
import java.util.*

internal class BookmarkObservableConnector : ObservableElementList.Connector<BookmarkData> {
    private var changeHandler: ObservableElementChangeHandler<out BookmarkData>? = null

    override fun installListener(element: BookmarkData): EventListener =
        PropertyChangeListener { changeHandler?.elementChanged(element) }
            .also(element::addPropertyChangeListener)

    override fun uninstallListener(element: BookmarkData, listener: EventListener) {
        element.removePropertyChangeListener(listener as PropertyChangeListener)
    }

    override fun setObservableElementList(list: ObservableElementChangeHandler<out BookmarkData>?) {
        changeHandler = list
    }
}
