package mediathek.gui.filmInformation;

import javax.swing.*;

public class HtmlMultilineLabel extends JLabel {
    @Override
    public void setText(String text) {
        super.setText(prepareString(text));
    }

    private String prepareString(String text) {
        var newText = "<html>";
        if (!text.startsWith("<html>")) {
            newText += text;
            newText += "<html>";
        }
        else
            newText = text;

        return newText;
    }

    public HtmlMultilineLabel() {
        super();
    }
}
