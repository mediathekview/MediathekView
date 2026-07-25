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
package ca.odell.glazedlists

import ca.odell.glazedlists.event.ListEventAssembler
import ca.odell.glazedlists.impl.filter.SearchTerm
import ca.odell.glazedlists.matchers.SearchEngineTextMatcherEditor
import org.junit.jupiter.api.Assertions.assertFalse
import org.junit.jupiter.api.Test
import java.io.Serializable

internal class JavaSerializationRemovalTest {
    @Test
    fun glazedListsTypesDoNotExposeJavaObjectSerialization() {
        assertFalse(Serializable::class.java.isAssignableFrom(BasicEventList::class.java))
        assertFalse(ListEventAssembler.createListEventPublisher() is Serializable)
        assertFalse(Serializable::class.java.isAssignableFrom(SearchTerm::class.java))
        assertFalse(Serializable::class.java.isAssignableFrom(SearchEngineTextMatcherEditor.Field::class.java))
    }
}
