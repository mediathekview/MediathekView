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

package mediathek.config.application

import org.apache.commons.configuration2.XMLConfiguration
import org.apache.commons.configuration2.sync.LockMode
import org.apache.commons.lang3.SystemUtils

class ApplicationMainWindowConfiguration(
    private val config: XMLConfiguration,
) {
    var senderListVerticalWrap: Boolean
        get() = config.getBoolean(SENDER_LIST_VERTICAL_WRAP, true)
        set(value) {
            config.setProperty(SENDER_LIST_VERTICAL_WRAP, value)
        }

    var toolbarBlacklistIconWithText: Boolean
        get() = config.getBoolean(TOOLBAR_BLACKLIST_ICON_WITH_TEXT, false)
        set(value) {
            config.setProperty(TOOLBAR_BLACKLIST_ICON_WITH_TEXT, value)
        }

    var filmTimeUseLongFormat: Boolean
        get() = config.getBoolean(UI_TAB_FILME_TIME_USE_LONG_FORMAT, false)
        set(value) {
            config.setProperty(UI_TAB_FILME_TIME_USE_LONG_FORMAT, value)
        }

    var installTabSwitchListener: Boolean
        get() = config.getBoolean(APPLICATION_INSTALL_TAB_SWITCH_LISTENER, true)
        set(value) {
            config.setProperty(APPLICATION_INSTALL_TAB_SWITCH_LISTENER, value)
        }

    fun ensureInstallTabSwitchListenerDefault() {
        if (!config.containsKey(APPLICATION_INSTALL_TAB_SWITCH_LISTENER)) {
            config.setProperty(APPLICATION_INSTALL_TAB_SWITCH_LISTENER, !SystemUtils.IS_OS_MAC_OSX)
        }
    }

    var restoreSelectedTab: Boolean
        get() = config.getBoolean(APPLICATION_RESTORE_SELECTED_TAB, false)
        set(value) {
            config.setProperty(APPLICATION_RESTORE_SELECTED_TAB, value)
        }

    var selectedMainWindowTabIndex: Int
        get() = config.getInt(APPLICATION_UI_SELECTED_TAB, -1)
        set(value) {
            config.setProperty(APPLICATION_UI_SELECTED_TAB, value)
        }

    var tabPositionTop: Boolean
        get() = config.getBoolean(APPLICATION_UI_TAB_POSITION_TOP, true)
        set(value) {
            config.setProperty(APPLICATION_UI_TAB_POSITION_TOP, value)
        }

    var mainWindowTabIcons: Boolean
        get() = config.getBoolean(APPLICATION_UI_MAINWINDOW_TAB_ICONS, false)
        set(value) {
            config.setProperty(APPLICATION_UI_MAINWINDOW_TAB_ICONS, value)
        }

    var localSenderIcons: Boolean
        get() = config.getBoolean(APPLICATION_UI_LOCAL_SENDER_ICONS, false)
        set(value) {
            config.setProperty(APPLICATION_UI_LOCAL_SENDER_ICONS, value)
        }

    var listIconPositionRight: Boolean
        get() = config.getBoolean(APPLICATION_UI_LIST_ICON_POSITION_RIGHT, false)
        set(value) {
            config.setProperty(APPLICATION_UI_LIST_ICON_POSITION_RIGHT, value)
        }

    var programSetShowAllSettings: Boolean
        get() = config.getBoolean(APPLICATION_UI_PROGRAM_SET_SHOW_ALL_SETTINGS, false)
        set(value) {
            config.setProperty(APPLICATION_UI_PROGRAM_SET_SHOW_ALL_SETTINGS, value)
        }

    var standardProgramSetVersion: String
        get() = config.getString(PROGRAM_SET_STANDARD_VERSION, "")
        set(value) {
            config.setProperty(PROGRAM_SET_STANDARD_VERSION, value)
        }

    val defaultFontState: ApplicationConfiguration.DefaultFontState
        get() =
            config.withLock(LockMode.READ) {
                try {
                    ApplicationConfiguration.DefaultFontState(
                        getString(APPLICATION_UI_DEFAULT_FONT_FAMILY),
                        getInt(APPLICATION_UI_DEFAULT_FONT_SIZE),
                    )
                } catch (_: NoSuchElementException) {
                    ApplicationConfiguration.DefaultFontState.empty()
                }
            }

    fun setDefaultFontState(
        family: String?,
        size: Int,
    ) {
        config.withLock(LockMode.WRITE) {
            setProperty(APPLICATION_UI_DEFAULT_FONT_FAMILY, family)
            setProperty(APPLICATION_UI_DEFAULT_FONT_SIZE, size)
        }
    }

    fun clearDefaultFontState() {
        config.withLock(LockMode.WRITE) {
            clearProperty(APPLICATION_UI_DEFAULT_FONT_FAMILY)
            clearProperty(APPLICATION_UI_DEFAULT_FONT_SIZE)
        }
    }

    var useTray: Boolean
        get() = config.getBoolean(APPLICATION_UI_USE_TRAY, false)
        set(value) {
            config.setProperty(APPLICATION_UI_USE_TRAY, value)
        }

    var exitDialogAction: String?
        get() = config.getString(APPLICATION_UI_EXIT_DIALOG_ACTION, null)
        set(value) {
            config.setProperty(APPLICATION_UI_EXIT_DIALOG_ACTION, value)
        }

    var zappLivestreamsTabVisible: Boolean
        get() = config.getBoolean(APPLICATION_UI_SHOW_ZAPP_LIVESTREAMS, true)
        set(value) {
            config.setProperty(APPLICATION_UI_SHOW_ZAPP_LIVESTREAMS, value)
        }

    var buttonsPanelVisible: Boolean
        get() = config.getBoolean(APPLICATION_BUTTONS_PANEL_VISIBLE, false)
        set(value) {
            config.setProperty(APPLICATION_BUTTONS_PANEL_VISIBLE, value)
        }

    private companion object {
        private const val TOOLBAR_BLACKLIST_ICON_WITH_TEXT = "toolbar.blacklist_icon.text"
        private const val UI_TAB_FILME_TIME_USE_LONG_FORMAT = "ui.tab.filme.time_use_long_format"
        private const val APPLICATION_INSTALL_TAB_SWITCH_LISTENER = "application.ui.install_tab_listeners"
        private const val APPLICATION_RESTORE_SELECTED_TAB = "application.ui.restore_selected_tab"
        private const val APPLICATION_UI_SELECTED_TAB = "app.ui.tab_position"
        private const val APPLICATION_UI_TAB_POSITION_TOP = "application.ui.tab_position.top"
        private const val APPLICATION_UI_MAINWINDOW_TAB_ICONS = "application.ui.mainwindow.tab_icons"
        private const val APPLICATION_UI_EXIT_DIALOG_ACTION = "application.ui.exit_dialog.action"
        private const val APPLICATION_UI_USE_TRAY = "application.ui.tray.use"
        private const val APPLICATION_UI_SHOW_ZAPP_LIVESTREAMS = "application.ui.zapp.show"
        private const val APPLICATION_UI_DEFAULT_FONT_FAMILY = "ui.default_font.family"
        private const val APPLICATION_UI_DEFAULT_FONT_SIZE = "ui.default_font.size"
        private const val APPLICATION_UI_LOCAL_SENDER_ICONS = "application.sender_icons.use_local"
        private const val APPLICATION_UI_LIST_ICON_POSITION_RIGHT = "ui.list.iconposition_right"
        private const val APPLICATION_UI_PROGRAM_SET_SHOW_ALL_SETTINGS =
            "application.ui.program_set.show_all_settings"
        private const val PROGRAM_SET_STANDARD_VERSION = "program_set.standard.version"
        private const val APPLICATION_BUTTONS_PANEL_VISIBLE = "application.buttons_panel.visible"
        private const val SENDER_LIST_VERTICAL_WRAP = "senderlist.vertical_wrap"
    }
}
