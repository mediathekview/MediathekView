package mediathek.tool.swing;

import org.apache.commons.lang3.SystemUtils;

import javax.swing.*;
import java.util.ArrayList;
import java.util.Arrays;

/**
 * Utility class which changes the font size for each swing font key.
 * Useful for enhancing the font size of an application.
 */
public class SwingUIFontChanger {
    private final String[] uiKeys;

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
        Arrays.stream(uiKeys).forEach(System.out::println);
    }

    public void changeFontSize(float size) {
        //do not change size on macOS
        if (SystemUtils.IS_OS_MAC_OSX)
            return;

        Arrays.stream(uiKeys).forEach(key -> changeFontSize(key, size));
    }

    private void changeFontSize(String key, float size) {
        var font = UIManager.getDefaults().getFont(key);
        font = font.deriveFont(size);
        UIManager.put(key, font);
    }
}
