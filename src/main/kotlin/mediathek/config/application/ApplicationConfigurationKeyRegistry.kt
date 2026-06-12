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

import java.lang.reflect.Field
import java.lang.reflect.Modifier

interface ApplicationConfigurationKeyRegistry {
    fun isValidKey(key: String): Boolean
}

object AnnotatedApplicationConfigurationKeyRegistry : ApplicationConfigurationKeyRegistry {
    private val registryClasses = listOf(
        ApplicationConfiguration::class.java,
        ApplicationGeneralConfiguration::class.java,
        ApplicationFilmListConfiguration::class.java,
        ApplicationAudiothekConfiguration::class.java,
        ApplicationDownloadConfiguration::class.java,
        ApplicationAboAndFilenameConfiguration::class.java,
        FilterConfiguration::class.java,
        ApplicationTableConfiguration::class.java,
        ApplicationNetworkConfiguration::class.java,
        ApplicationSearchConfiguration::class.java,
        ApplicationBlacklistConfiguration::class.java,
        ApplicationExternalProgramsConfiguration::class.java,
        ApplicationMainWindowConfiguration::class.java,
        ApplicationWindowStateConfiguration::class.java,
    )

    val exactKeys: Set<String> by lazy { annotatedStringValues<ApplicationConfigKey>() }

    val keyPatterns: List<Regex> by lazy {
        annotatedFields<ApplicationConfigKeyPattern>()
            .map { field -> checkNotNull(field.getAnnotation(ApplicationConfigKeyPattern::class.java)).pattern.toRegex() }
            .toList()
    }

    override fun isValidKey(key: String): Boolean =
        key in exactKeys || keyPatterns.any { pattern -> pattern.matches(key) }

    private inline fun <reified T : Annotation> annotatedStringValues(): Set<String> =
        annotatedFields<T>()
            .filter { field -> field.type == String::class.java }
            .mapNotNull { field -> field.staticStringValue() }
            .filter { key -> key.isNotBlank() }
            .toSet()

    private inline fun <reified T : Annotation> annotatedFields(): Sequence<Field> =
        registryClasses.asSequence()
            .flatMap { clazz -> clazz.withDeclaredNestedClasses() }
            .flatMap { clazz -> clazz.declaredFields.asSequence() }
            .filter { field -> field.isAnnotationPresent(T::class.java) }

    private fun Class<*>.withDeclaredNestedClasses(): Sequence<Class<*>> =
        sequence {
            yield(this@withDeclaredNestedClasses)
            declaredClasses.forEach { nestedClass ->
                yieldAll(nestedClass.withDeclaredNestedClasses())
            }
        }

    private fun Field.staticStringValue(): String? {
        if (!Modifier.isStatic(modifiers)) {
            return null
        }
        return try {
            isAccessible = true
            get(null) as? String
        } catch (_: IllegalAccessException) {
            null
        }
    }
}
