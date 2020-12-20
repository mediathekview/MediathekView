package mediathek.tool.swing;

import mediathek.config.Config;
import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.Arrays;

/**
 * Utility class which changes the font size for each swing font key.
 * Useful for enhancing the font size of an application.
 */
public class SwingUIFontChanger {
    private static final Logger logger = LogManager.getLogger();
    public static final String NIMBUS_DEFAULT_FONT = "defaultFont";
    public static final String WINDOWS_DEFAULT_FONT = "Table.font";

    public void changeFontSize(float size) {
        //do not change size on macOS
        if (SystemUtils.IS_OS_MAC_OSX)
            return;

        switch (LookAndFeelType.get(UIManager.getLookAndFeel().getClass().getName())) {
            case Windows -> changeWindowsFontSize(size);
            case Nimbus -> changeNimbusFontSize(size);
            default -> logger.error("Unknown Look&Feel, cannot change font size");
        }
    }

    private void changeNimbusFontSize(float size) {
        var defaults = UIManager.getLookAndFeel().getDefaults();
        Font font = (Font) defaults.get(NIMBUS_DEFAULT_FONT);
        font = font.deriveFont(size);
        logger.info("Changing Nimbus font size");
        defaults.put("defaultFont", font);
    }

    private void changeWindowsFontSize(float size) {
        var keyList = new ArrayList<String>();
        var defaults = UIManager.getDefaults();
        var keys = defaults.keys();
        while (keys.hasMoreElements()) {
            if (keys.nextElement() instanceof String str_key) {
                if (str_key.endsWith(".font"))
                    keyList.add(str_key);
            }
        }

        var ui_keys = keyList.stream().distinct().sorted().toArray(String[]::new);

        if (Config.isDebugModeEnabled())
            Arrays.stream(ui_keys).forEach(logger::debug);

        logger.info("Changing Windows font sizes to {}", size);
        Arrays.stream(ui_keys).forEach(key -> {
            var font = defaults.getFont(key);
            font = font.deriveFont(size);
            UIManager.put(key, font);
        });
    }
}
