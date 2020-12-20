package mediathek.tool.swing

import java.io.ByteArrayOutputStream
import java.io.PrintStream
import javax.swing.JComponent
import javax.swing.RepaintManager
import javax.swing.SwingUtilities

/**
 * Check Swing EDT violations.
 */
class ThreadCheckingRepaintManager : RepaintManager() {
    private val isCompleteCheck = true
    private val checkIsShowing = false

    @Synchronized
    override fun addInvalidComponent(jComponent: JComponent) {
        checkThreadViolations(jComponent)
        super.addInvalidComponent(jComponent)
    }

    @Synchronized
    override fun addDirtyRegion(jComponent: JComponent, i: Int, i1: Int, i2: Int, i3: Int) {
        checkThreadViolations(jComponent)
        super.addDirtyRegion(jComponent, i, i1, i2, i3)
    }

    private fun checkThreadViolations(c: JComponent) {
        if (!SwingUtilities.isEventDispatchThread() && (isCompleteCheck || checkIfComponentIsShowing(c))) {
            val exception = Exception()
            var repaint = false
            var fromSwing = false
            val stackTrace = exception.stackTrace
            for (st in stackTrace) {
                if (repaint && st.className.startsWith("javax.swing.")) {
                    fromSwing = true
                }
                if ("repaint" == st.methodName) {
                    repaint = true
                }
            }
            if (repaint && !fromSwing) {
                //no problems here, since repaint() is thread safe
                return
            }
            println("----------Wrong Thread START")
            println(getStrackTraceAsString(exception))
            println("----------Wrong Thread END")
        }
    }

    private fun checkIfComponentIsShowing(c: JComponent): Boolean {
        return if (checkIsShowing) {
            c.isShowing
        } else {
            true
        }
    }

    private fun getStrackTraceAsString(e: Exception): String {
        val byteArrayOutputStream = ByteArrayOutputStream()
        val printStream = PrintStream(byteArrayOutputStream)
        e.printStackTrace(printStream)
        printStream.flush()
        return byteArrayOutputStream.toString()
    }
}