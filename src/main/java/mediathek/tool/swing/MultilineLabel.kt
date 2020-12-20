package mediathek.tool.swing

import java.awt.Dimension
import java.awt.Rectangle
import javax.swing.*
import javax.swing.plaf.UIResource
import javax.swing.text.DefaultCaret

class MultilineLabel : JTextArea() {
    private fun initComponents() {
        adjustUI()
    }

    override fun updateUI() {
        super.updateUI()
        adjustUI()
    }

    /**
     * Adjusts UI to make sure it looks like a label instead of a text area.
     */
    private fun adjustUI() {
        lineWrap = true
        wrapStyleWord = true
        isEditable = false
        isRequestFocusEnabled = false
        isFocusable = false
        setComponentTransparent(this)
        caret = object : DefaultCaret() {
            override fun adjustVisibility(nloc: Rectangle) {}
        }
        LookAndFeel.installBorder(this, "Label.border")
        val fg = foreground
        if (fg == null || fg is UIResource) {
            foreground = UIManager.getColor("Label.foreground")
        }
        val f = font
        if (f == null || f is UIResource) {
            font = UIManager.getFont("Label.font")
        }
        background = null
    }

    override fun getMinimumSize(): Dimension {
        return preferredSize
    }

    private fun setComponentTransparent(component: JComponent) {
        component.isOpaque = false
        component.putClientProperty("Nimbus.Overrides.InheritDefaults", false)
        component.putClientProperty("Nimbus.Overrides", UIDefaults())
    }

    init {
        initComponents()
    }
}