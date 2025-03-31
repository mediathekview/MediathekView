package mediathek.tool;

import mediathek.config.MVConfig;

import java.awt.*;

public class MVC {

    private final String text;
    private final MVConfig.Configs config;
    public Color color = new Color(0);
    private Color colorReset = new Color(0);
    public MVC(MVConfig.Configs configs, Color color, String ttext) {
        this.config = configs;
        text = ttext;
        this.color = color;
        colorReset = color;
    }

    public MVConfig.Configs getConfig() {
        return config;
    }

    public String getText() {
        return text;
    }

    public void set(Color c) {
        color = c;
    }

    public void reset() {
        color = colorReset;
    }
}
