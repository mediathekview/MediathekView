package mediathek.tool

import com.formdev.flatlaf.FlatLightLaf
import com.formdev.flatlaf.themes.FlatMacLightLaf
import org.apache.commons.lang3.SystemUtils
import javax.swing.LookAndFeel

object LightModeFactory {
    @JvmStatic
    val lookAndFeel: LookAndFeel
        get() {
            return if (SystemUtils.IS_OS_MAC_OSX) FlatMacLightLaf() else FlatLightLaf()
        }
}