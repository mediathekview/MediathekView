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

import org.apache.commons.configuration2.convert.DefaultConversionHandler
import org.apache.commons.configuration2.interpol.ConfigurationInterpolator
import java.util.*

class CustomConversionHandler : DefaultConversionHandler() {
    protected override fun <T> convertValue(src: Any?, targetCls: Class<T>, ci: ConfigurationInterpolator): T? {
        if (src == null) {
            return null
        }

        if (UUID::class.java == targetCls) {
            val uuidAsString = super.convertValue(src, String::class.java, ci)
            @Suppress("UNCHECKED_CAST")
            return UUID.fromString(uuidAsString) as T
        }

        return super.convertValue(src, targetCls, ci)
    }
}
