package mediathek.tool;

import mediathek.config.MVConfig;

import java.awt.*;

public class MVC {

    public MVConfig.Configs configs;
    public String text;
    public Color color = new Color(0);
    public Color colorReset = new Color(0);

    public MVC(MVConfig.Configs configs, Color ccolor, String ttext) {
        this.configs = configs;
        text = ttext;
        color = ccolor;
        colorReset = ccolor;
    }

    public void set(Color c) {
        color = c;
    }

    public void reset() {
        color = colorReset;
    }
}
