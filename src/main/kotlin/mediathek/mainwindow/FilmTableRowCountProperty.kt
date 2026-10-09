/*
 * Copyright (c) 2026 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */

package mediathek.mainwindow

import java.beans.PropertyChangeListener
import java.beans.PropertyChangeSupport

class FilmTableRowCountProperty(initialRowCount: Int = 0) {
    private val changeSupport = PropertyChangeSupport(this)

    var rowCount: Int = initialRowCount
        private set

    fun addListener(listener: PropertyChangeListener) {
        changeSupport.addPropertyChangeListener(listener)
    }

    fun removeListener(listener: PropertyChangeListener) {
        changeSupport.removePropertyChangeListener(listener)
    }

    fun publish(rowCount: Int) {
        this.rowCount = rowCount
        changeSupport.firePropertyChange(ROW_COUNT_PROPERTY, null, rowCount)
    }

    private companion object {
        private const val ROW_COUNT_PROPERTY = "filmTableRowCount"
    }
}
