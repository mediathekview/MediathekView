/* Glazed Lists                                                 (c) 2003-2014 */
/* http://publicobject.com/glazedlists/                      publicobject.com,*/
/*                                                     O'Dell Engineering Ltd.*/
package ca.odell.glazedlists.impl.matchers

import ca.odell.glazedlists.matchers.Matcher
import java.beans.PropertyChangeEvent

/** Matches or excludes property-change events by property name. */
internal class PropertyEventNameMatcher(
    val isMatchPropertyNames: Boolean,
    vararg properties: String,
) : Matcher<PropertyChangeEvent> {
    private val propertyNames = properties.toHashSet()

    constructor(isMatchPropertyNames: Boolean, properties: Collection<String>) : this(
        isMatchPropertyNames,
        *properties.toTypedArray(),
    )

    override fun matches(item: PropertyChangeEvent): Boolean {
        val containsProperty = item.propertyName in propertyNames
        return if (isMatchPropertyNames) containsProperty else !containsProperty
    }
}
