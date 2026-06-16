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

import mediathek.audiothek.ui.main.AudiothekPanel
import mediathek.config.application.ApplicationConfiguration
import javax.swing.JTabbedPane

private const val ACTION_TITLE = "Audiothek Tab ein-/ausblenden"
private const val TAB_TITLE = "Audiothek"
private const val PREFERRED_INSERT_INDEX = 4

class ToggleAudiothekTabAction(
    tabbedPane: JTabbedPane,
    audiothekPanel: AudiothekPanel,
) : ToggleOptionalTabAction(
    tabbedPane,
    audiothekPanel,
    ACTION_TITLE,
    TAB_TITLE,
    { ApplicationConfiguration.getInstance().audiothekTabVisible = it },
    PREFERRED_INSERT_INDEX,
)
