package mediathek.daten

import mediathek.gui.messages.ProgramSetChangedEvent
import mediathek.tool.MessageBus

class ProgramSetRepository {
    val list: ListePset = ListePset()

    fun activateAsPlayer(programSet: DatenPset) {
        list.activateAsPlayer(programSet)
        notifyChanged()
    }

    fun move(index: Int, up: Boolean): Int {
        val programSet = list.removeAt(index)
        var newIndex = index
        if (up) {
            if (newIndex > 0) {
                --newIndex
            }
        } else if (newIndex < list.size) {
            ++newIndex
        }
        list.add(newIndex, programSet)

        notifyChanged()
        return newIndex
    }

    fun addProgramSet(programSet: DatenPset) {
        list.add(programSet)
        notifyChanged()
    }

    fun addProgramSets(programSets: ListePset): Boolean {
        var addedAll = true
        for (programSet in programSets) {
            if (!list.add(programSet)) {
                addedAll = false
            }
        }

        if (addedAll) {
            notifyChanged()
        }

        return addedAll
    }

    fun removeAtIndexes(indexes: IntArray) {
        indexes.sortedDescending().forEach { index -> list.removeAt(index) }
        notifyChanged()
    }

    fun clear() {
        list.clear()
    }

    fun notifyChanged() {
        MessageBus.messageBus.publish(ProgramSetChangedEvent())
    }
}
