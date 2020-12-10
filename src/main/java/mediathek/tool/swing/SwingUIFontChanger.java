package mediathek.tool.swing;

import mediathek.config.Config;
import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import java.util.ArrayList;
import java.util.Arrays;

/**
 * Utility class which changes the font size for each swing font key.
 * Useful for enhancing the font size of an application.
 */
public class SwingUIFontChanger {
    private final String[] uiKeys;
    private static final Logger logger = LogManager.getLogger();

    public SwingUIFontChanger() {
        var keyList = new ArrayList<String>();
        var keys = UIManager.getDefaults().keys();
        while (keys.hasMoreElements()) {
            if (keys.nextElement() instanceof String skey) {
                if (skey.endsWith(".font"))
                    keyList.add(skey);
            }
        }

        uiKeys = keyList.stream().distinct().sorted().toArray(String[]::new);
        if (Config.isDebugModeEnabled())
            Arrays.stream(uiKeys).forEach(logger::debug);
    }

    public void changeFontSize(float size) {
        //do not change size on macOS
        if (SystemUtils.IS_OS_MAC_OSX)
            return;

        logger.info("Changing application font size to {}", size);
        Arrays.stream(uiKeys).forEach(key -> changeFontSize(key, size));
    }

    private void changeFontSize(String key, float size) {
        var font = UIManager.getDefaults().getFont(key);
        font = font.deriveFont(size);
        UIManager.put(key, font);
    }
}
