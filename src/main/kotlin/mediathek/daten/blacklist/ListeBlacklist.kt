package mediathek.daten.blacklist

class ListeBlacklist(
    private val onChanged: (() -> Unit)? = null,
) : ArrayList<BlacklistRule>() {
    /**
     * Add item without notifying registered listeners.
     */
    @Synchronized
    fun addWithoutNotification(rule: BlacklistRule): Boolean =
        addUniqueWithoutNotification(rule)

    /**
     * Add items without notifying registered listeners.
     */
    @Synchronized
    fun addAllWithoutNotification(rules: Collection<BlacklistRule>): Boolean =
        super.addAll(uniqueRulesNotAlreadyPresent(rules))

    @Synchronized
    fun removeAtWithoutNotification(index: Int): BlacklistRule =
        super.removeAt(index)

    @Synchronized
    fun removeAllWithoutNotification(rules: Collection<BlacklistRule>): Boolean {
        var changed = false
        rules.forEach { rule ->
            changed = super.remove(rule) || changed
        }
        return changed
    }

    @Synchronized
    fun clearWithoutNotification() {
        super.clear()
    }

    @Synchronized
    override fun add(element: BlacklistRule): Boolean {
        if (containsCriteria(element)) {
            return false
        }
        val result = super.add(element)
        notifyChanged()
        return result
    }

    @Synchronized
    override fun add(index: Int, element: BlacklistRule) {
        checkAddIndex(index)
        if (containsCriteria(element)) {
            return
        }
        super.add(index, element)
        notifyChanged()
    }

    @Synchronized
    override fun addAll(elements: Collection<BlacklistRule>): Boolean {
        val uniqueRules = uniqueRulesNotAlreadyPresent(elements)
        if (uniqueRules.isEmpty()) {
            return false
        }
        val result = super.addAll(uniqueRules)
        notifyChanged()
        return result
    }

    @Synchronized
    override fun addAll(index: Int, elements: Collection<BlacklistRule>): Boolean {
        checkAddIndex(index)
        val uniqueRules = uniqueRulesNotAlreadyPresent(elements)
        if (uniqueRules.isEmpty()) {
            return false
        }
        val result = super.addAll(index, uniqueRules)
        notifyChanged()
        return result
    }

    @Synchronized
    override fun set(index: Int, element: BlacklistRule): BlacklistRule {
        checkElementIndex(index)
        require(!hasDuplicateAt(index, element)) { "Duplicate blacklist rule" }
        val previousRule = super.set(index, element)
        notifyChanged()
        return previousRule
    }

    @Synchronized
    fun replaceAtIfUnique(index: Int, updatedRule: BlacklistRule): Boolean {
        checkElementIndex(index)
        if (hasDuplicateAt(index, updatedRule)) {
            return false
        }

        val rule = super.get(index)
        rule.sender = updatedRule.sender
        rule.thema = updatedRule.thema
        rule.titel = updatedRule.titel
        rule.thema_titel = updatedRule.thema_titel
        rule.active = updatedRule.active
        notifyChanged()
        return true
    }

    @Synchronized
    fun replaceAtIfUniqueWithoutNotification(index: Int, updatedRule: BlacklistRule): Boolean {
        checkElementIndex(index)
        if (hasDuplicateAt(index, updatedRule)) {
            return false
        }

        val rule = super.get(index)
        rule.sender = updatedRule.sender
        rule.thema = updatedRule.thema
        rule.titel = updatedRule.titel
        rule.thema_titel = updatedRule.thema_titel
        rule.active = updatedRule.active
        return true
    }

    private fun uniqueRulesNotAlreadyPresent(elements: Collection<BlacklistRule>): List<BlacklistRule> {
        val seen = mapTo(HashSet(), BlacklistRule::criteria)
        return elements.filter { rule -> seen.add(rule.criteria()) }
    }

    private fun addUniqueWithoutNotification(rule: BlacklistRule): Boolean {
        if (containsCriteria(rule)) {
            return false
        }
        return super.add(rule)
    }

    private fun hasDuplicateAt(index: Int, rule: BlacklistRule): Boolean =
        withIndex().any { (ruleIndex, existingRule) ->
            ruleIndex != index && existingRule.hasSameCriteria(rule)
        }

    private fun containsCriteria(rule: BlacklistRule): Boolean =
        any { existingRule -> existingRule.hasSameCriteria(rule) }

    private fun checkAddIndex(index: Int) {
        if (index !in 0..size) {
            throw IndexOutOfBoundsException("Index: $index, Size: $size")
        }
    }

    private fun checkElementIndex(index: Int) {
        if (index !in indices) {
            throw IndexOutOfBoundsException("Index: $index, Size: $size")
        }
    }

    @Synchronized
    override fun remove(element: BlacklistRule): Boolean {
        val result = super.remove(element)
        notifyChanged()
        return result
    }

    /**
     * Remove a list of rules and filter after all objects have been removed.
     */
    @Synchronized
    fun remove(ruleList: List<BlacklistRule>) {
        ruleList.forEach { super.remove(it) }
        notifyChanged()
    }

    @Synchronized
    override fun removeAt(index: Int): BlacklistRule {
        val result = super.removeAt(index)
        notifyChanged()
        return result
    }

    @Synchronized
    override fun get(index: Int): BlacklistRule =
        super.get(index).copy()

    @Synchronized
    override fun clear() {
        super.clear()
        notifyChanged()
    }

    private fun notifyChanged() {
        onChanged?.invoke()
    }
}
