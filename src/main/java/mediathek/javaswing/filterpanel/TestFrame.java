package mediathek.javaswing.filterpanel;

import ca.odell.glazedlists.FilterList;
import com.formdev.flatlaf.FlatLaf;
import mediathek.controller.SenderFilmlistLoadApproverSwing;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.DarkModeFactory;
import mediathek.tool.LightModeFactory;
import javax.swing.JFrame;
import javax.swing.LookAndFeel;

public class TestFrame {

    public static void main(String[] args) {
        setupFlatLaf();
        JFrame frame = new JFrame();
        frame.setSize(200, 200);

    }

    private static void setupFlatLaf() {
        var darkMode = ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.APPLICATION_DARK_MODE, false);
        LookAndFeel laf;

        if (darkMode) {
            laf = DarkModeFactory.getLookAndFeel();
        }
        else {
            laf = LightModeFactory.getLookAndFeel();
        }
        FlatLaf.setup(laf);
    }

}
