package mediathek.tool.models

import javax.swing.table.DefaultTableModel

/**
 * Simple extension of DefaultTableModel which prevents cell from being editable.
 */
class NonEditableTableModel : DefaultTableModel {
    constructor() : super()

    constructor(data: Array<Array<Any?>>, columnNames: Array<Any?>) : super(data, columnNames)

    override fun isCellEditable(row: Int, column: Int): Boolean = false
}
