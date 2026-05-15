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

package mediathek.mainwindow

import mediathek.gui.tabs.tab_livestreams.LivestreamPanel
import mediathek.tool.ApplicationConfiguration
import javax.swing.JTabbedPane

private const val ACTION_TITLE = "Zapp Livestreams Tab ein-/ausblenden"
private const val TAB_TITLE = "zapp Livestreams"
private const val PREFERRED_INSERT_INDEX = 2

class ToggleZappLivestreamsTabAction(
    tabbedPane: JTabbedPane,
    livestreamPanel: LivestreamPanel,
) : ToggleOptionalTabAction(
    tabbedPane,
    livestreamPanel,
    ACTION_TITLE,
    TAB_TITLE,
    ApplicationConfiguration.APPLICATION_UI_SHOW_ZAPP_LIVESTREAMS,
    PREFERRED_INSERT_INDEX,
)
