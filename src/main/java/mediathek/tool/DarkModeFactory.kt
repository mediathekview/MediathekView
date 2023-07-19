package mediathek.tool

import com.formdev.flatlaf.FlatDarkLaf
import com.formdev.flatlaf.themes.FlatMacDarkLaf
import org.apache.commons.lang3.SystemUtils
import javax.swing.LookAndFeel

object DarkModeFactory {
    @JvmStatic
    val lookAndFeel: LookAndFeel
        get() {
            return if (SystemUtils.IS_OS_MAC_OSX) FlatMacDarkLaf() else FlatDarkLaf()
        }
}