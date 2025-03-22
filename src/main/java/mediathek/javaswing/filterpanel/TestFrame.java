package mediathek.javaswing.filterpanel;

import javax.swing.JFrame;

public class TestFrame {

    public static void main(String[] args) {
        JFrame frame = new JFrame();
        frame.setSize(200, 200);
        new CommonViewSettingsPaneSwing(frame, true).setVisible(true);
    }

}
