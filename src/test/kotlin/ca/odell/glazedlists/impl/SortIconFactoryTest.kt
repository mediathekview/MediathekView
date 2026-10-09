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
package ca.odell.glazedlists.impl

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import javax.swing.ImageIcon
import javax.swing.UIManager
import javax.swing.plaf.metal.DefaultMetalTheme
import javax.swing.plaf.metal.MetalLookAndFeel

internal class SortIconFactoryTest {
    @Test
    fun defaultIconsAreCachedWhileExplicitPathLoadsRemainIndependent() {
        val defaultIconsField = defaultIconsField()
        val previousIcons = defaultIconsField[null]

        try {
            defaultIconsField[null] = null

            val firstDefaultLoad = SortIconFactory.loadIcons()
            val secondDefaultLoad = SortIconFactory.loadIcons()
            val firstExplicitLoad = SortIconFactory.loadIcons("resources/aqua")
            val secondExplicitLoad = SortIconFactory.loadIcons("resources/aqua")

            assertSame(firstDefaultLoad, secondDefaultLoad)
            assertNotSame(firstExplicitLoad, secondExplicitLoad)
        } finally {
            defaultIconsField[null] = previousIcons
        }
    }

    @Test
    fun loadIconsUsesCurrentMetalTheme() {
        val defaultIconsField = defaultIconsField()
        val previousIcons = defaultIconsField[null]
        val previousLookAndFeel = UIManager.getLookAndFeel()
        val previousTheme = MetalLookAndFeel.getCurrentTheme()

        try {
            UIManager.setLookAndFeel(MetalLookAndFeel())
            MetalLookAndFeel.setCurrentTheme(DefaultMetalTheme())
            defaultIconsField[null] = null

            val defaultPrimaryIcon = assertInstanceOf(ImageIcon::class.java, SortIconFactory.loadIcons()[1])
            val metalPrimaryIcon = assertInstanceOf(
                ImageIcon::class.java,
                SortIconFactory.loadIcons("resources/metal")[1],
            )

            assertEquals(metalPrimaryIcon.description, defaultPrimaryIcon.description)
        } finally {
            MetalLookAndFeel.setCurrentTheme(previousTheme)
            UIManager.setLookAndFeel(previousLookAndFeel)
            defaultIconsField[null] = previousIcons
        }
    }

    private fun defaultIconsField() =
        SortIconFactory::class.java.declaredClasses
            .single { it.simpleName == "Loader" }
            .getDeclaredField("defaultIcons")
            .apply { isAccessible = true }
}
