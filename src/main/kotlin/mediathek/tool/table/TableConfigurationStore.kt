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

package mediathek.tool.table

import mediathek.config.application.ApplicationConfiguration

data class BooleanConfigurationProperty(
    val read: () -> Boolean,
    val write: (Boolean) -> Unit,
)

data class StringConfigurationProperty(
    val read: () -> String,
    val write: (String) -> Unit,
)

data class TableConfigurationStore(
    val showSenderIcons: BooleanConfigurationProperty?,
    val smallSenderIcons: BooleanConfigurationProperty?,
    val columnConfiguration: StringConfigurationProperty,
)

object TableConfigurationStores {
    val FILM = TableConfigurationStore(
        showSenderIcons = BooleanConfigurationProperty(
            read = { ApplicationConfiguration.getInstance().filmTableShowSenderIcons },
            write = { ApplicationConfiguration.getInstance().filmTableShowSenderIcons = it },
        ),
        smallSenderIcons = BooleanConfigurationProperty(
            read = { ApplicationConfiguration.getInstance().filmTableUseSmallSenderIcons },
            write = { ApplicationConfiguration.getInstance().filmTableUseSmallSenderIcons = it },
        ),
        columnConfiguration = StringConfigurationProperty(
            read = { ApplicationConfiguration.getInstance().filmTableColumnConfiguration },
            write = { ApplicationConfiguration.getInstance().filmTableColumnConfiguration = it },
        ),
    )

    val DOWNLOAD = TableConfigurationStore(
        showSenderIcons = BooleanConfigurationProperty(
            read = { ApplicationConfiguration.getInstance().downloadTableShowSenderIcons },
            write = { ApplicationConfiguration.getInstance().downloadTableShowSenderIcons = it },
        ),
        smallSenderIcons = BooleanConfigurationProperty(
            read = { ApplicationConfiguration.getInstance().downloadTableUseSmallSenderIcons },
            write = { ApplicationConfiguration.getInstance().downloadTableUseSmallSenderIcons = it },
        ),
        columnConfiguration = StringConfigurationProperty(
            read = { ApplicationConfiguration.getInstance().downloadTableColumnConfiguration },
            write = { ApplicationConfiguration.getInstance().downloadTableColumnConfiguration = it },
        ),
    )

    val ABO = TableConfigurationStore(
        showSenderIcons = BooleanConfigurationProperty(
            read = { ApplicationConfiguration.getInstance().aboTableShowSenderIcons },
            write = { ApplicationConfiguration.getInstance().aboTableShowSenderIcons = it },
        ),
        smallSenderIcons = BooleanConfigurationProperty(
            read = { ApplicationConfiguration.getInstance().aboTableUseSmallSenderIcons },
            write = { ApplicationConfiguration.getInstance().aboTableUseSmallSenderIcons = it },
        ),
        columnConfiguration = StringConfigurationProperty(
            read = { ApplicationConfiguration.getInstance().aboTableColumnConfiguration },
            write = { ApplicationConfiguration.getInstance().aboTableColumnConfiguration = it },
        ),
    )
}
