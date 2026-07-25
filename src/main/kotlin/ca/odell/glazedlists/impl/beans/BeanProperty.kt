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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package ca.odell.glazedlists.impl.beans

import org.apache.commons.lang3.reflect.TypeUtils
import java.lang.reflect.*
import java.util.regex.Pattern

/** Models reflective getter and setter chains for a JavaBean property. */
internal open class BeanProperty<T>(
    private val targetBeanClass: Class<T>,
    private val targetPropertyName: String,
    readable: Boolean,
    writable: Boolean,
) {
    private val identityProperty: Boolean
    private var resolvedValueClass: Class<*>? = null
    private var getterChain: List<Method>? = null
    private var setterChain: List<Method>? = null

    open val beanClass: Class<T>
        get() = targetBeanClass

    open val propertyName: String
        get() = targetPropertyName

    open val valueClass: Class<*>?
        get() = resolvedValueClass

    open val isReadable: Boolean
        get() = getterChain != null || identityProperty

    open val isWritable: Boolean
        get() = setterChain != null

    init {
        require(targetPropertyName.isNotEmpty()) { "propertyName may not be empty" }
        identityProperty = targetPropertyName == "this"
        require(!(identityProperty && writable)) { "The identity property name (this) cannot be writable" }

        val propertyParts = PROPERTY_SEPARATOR.split(targetPropertyName)
        val commonChain = ArrayList<Method>(propertyParts.size)
        var currentClass: Class<*> = targetBeanClass
        for (index in 0 until propertyParts.lastIndex) {
            val partGetter = findGetterMethod(currentClass, propertyParts[index])
            commonChain += partGetter
            currentClass = resolveType(currentClass, partGetter.genericReturnType, partGetter.returnType)
        }

        if (readable) {
            if (identityProperty) {
                resolvedValueClass = targetBeanClass
            } else {
                val chain = ArrayList(commonChain)
                val lastGetter = findGetterMethod(currentClass, propertyParts.last())
                chain += lastGetter
                getterChain = chain
                resolvedValueClass = resolveType(currentClass, lastGetter.genericReturnType, lastGetter.returnType)
            }
        }

        if (writable) {
            val chain = ArrayList(commonChain)
            val lastSetter = findSetterMethod(currentClass, propertyParts.last())
            chain += lastSetter
            setterChain = chain
            if (resolvedValueClass == null) {
                resolvedValueClass = resolveType(
                    currentClass,
                    lastSetter.genericParameterTypes[0],
                    lastSetter.parameterTypes[0],
                )
            }
        }
    }

    private fun findGetterMethod(targetClass: Class<*>, property: String): Method {
        var currentClass: Class<*>? = targetClass
        while (currentClass != null) {
            getMethod(currentClass, "get${capitalize(property)}")?.let {
                validateGetter(it)
                return it
            }
            getMethod(currentClass, "is${capitalize(property)}")?.let {
                validateGetter(it)
                return it
            }
            currentClass = currentClass.superclass
        }
        throw IllegalArgumentException("Failed to find getter for property \"$property\" of $targetClass")
    }

    private fun findSetterMethod(targetClass: Class<*>, property: String): Method {
        val setterName = "set${capitalize(property)}"
        var currentClass: Class<*>? = targetClass
        while (currentClass != null) {
            for (method in currentClass.methods) {
                if (method.name != setterName || method.parameterCount != 1) continue
                validateSetter(method)
                return method
            }
            currentClass = currentClass.superclass
        }
        throw IllegalArgumentException("Failed to find setter for property \"$property\" of $targetClass")
    }

    private fun validateGetter(method: Method) {
        require(Modifier.isPublic(method.modifiers)) { "Getter \"$method\" is not public" }
        require(method.returnType != Void.TYPE) { "Getter \"$method\" returns void" }
        require(method.parameterCount == 0) {
            "Getter \"$method\" has too many parameters; expected 0 but found ${method.parameterCount}"
        }
    }

    private fun validateSetter(method: Method) {
        require(Modifier.isPublic(method.modifiers)) { "Setter \"$method\" is not public" }
        require(method.parameterCount == 1) {
            "Setter \"$method\" takes the wrong number of parameters; expected 1 but found ${method.parameterCount}"
        }
    }

    private fun capitalize(property: String): String =
        Character.toUpperCase(property[0]) + property.substring(1)

    private fun getMethod(targetClass: Class<*>, methodName: String): Method? =
        try {
            targetClass.getMethod(methodName)
        } catch (_: NoSuchMethodException) {
            null
        }

    open operator fun get(member: T): Any? {
        check(isReadable) { "Property $targetPropertyName of $targetBeanClass not readable" }
        if (identityProperty) return member

        return try {
            var currentMember: Any? = member
            for (method in checkNotNull(getterChain)) {
                currentMember = method.invoke(currentMember, *EMPTY_ARGUMENTS)
                if (currentMember == null) return null
            }
            currentMember
        } catch (exception: IllegalAccessException) {
            throw SecurityException(null, exception)
        } catch (exception: InvocationTargetException) {
            throw UndeclaredThrowableException(exception.cause)
        }
    }

    open fun set(member: T, newValue: Any?): Any? {
        check(isWritable) { "Property $targetPropertyName of $targetBeanClass not writable" }

        var setterMethod: Method? = null
        return try {
            val chain = checkNotNull(setterChain)
            var currentMember: Any? = member
            for (index in 0 until chain.lastIndex) {
                currentMember = chain[index].invoke(currentMember, *EMPTY_ARGUMENTS)
                if (currentMember == null) return null
            }

            setterMethod = chain.last()
            setterMethod.invoke(currentMember, newValue)
        } catch (exception: IllegalArgumentException) {
            throw IllegalArgumentException(getSetterErrorMessage(exception, setterMethod, newValue))
        } catch (exception: IllegalAccessException) {
            throw SecurityException(null, exception)
        } catch (exception: InvocationTargetException) {
            throw UndeclaredThrowableException(exception.cause)
        } catch (exception: RuntimeException) {
            val valueDescription = newValue?.let { "instance of ${it.javaClass}" } ?: "null"
            throw RuntimeException(
                "Failed to set property \"$targetPropertyName\" of $targetBeanClass to $valueDescription",
                exception,
            )
        }
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other == null || javaClass != other.javaClass) return false
        other as BeanProperty<*>
        return targetBeanClass == other.targetBeanClass && targetPropertyName == other.targetPropertyName
    }

    override fun hashCode(): Int = 29 * targetBeanClass.hashCode() + targetPropertyName.hashCode()

    private companion object {
        private val EMPTY_ARGUMENTS = emptyArray<Any>()
        private val PROPERTY_SEPARATOR = Pattern.compile("\\.")

        fun resolveType(context: Class<*>, genericType: Type, erasedType: Class<*>): Class<*> =
            TypeUtils.getRawType(genericType, context) ?: erasedType

        fun getSetterErrorMessage(
            exception: IllegalArgumentException,
            setterMethod: Method?,
            newValue: Any?,
        ): String? {
            val message = exception.message
            if (message != "argument type mismatch" || setterMethod == null) return message
            return "${setterMethod.declaringClass.simpleName}.${setterMethod.name}" +
                "(${setterMethod.parameterTypes[0].simpleName}) cannot be called with an instance of " +
                newValue!!.javaClass.simpleName
        }
    }
}
