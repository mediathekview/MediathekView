package mediathek.mainwindow;

import org.jdesktop.swingx.JXStatusBar;

import javax.swing.*;
import java.awt.*;

public class StatusBar extends JComponent {
    private final JXStatusBar statusBar = new JXStatusBar();

    public StatusBar(MediathekGui mediathekGui) {
        setLayout(new BorderLayout());

        add(statusBar, BorderLayout.CENTER);

        statusBar.add(new SelectedListItemsLabel(mediathekGui));
        statusBar.add(new FilmSizeInfoLabel(mediathekGui));
        statusBar.add(new DownloadInformationLabel());

        statusBar.add(new JPanel(), new JXStatusBar.Constraint(JXStatusBar.Constraint.ResizeBehavior.FILL));
        FilmListCreationDateLabel creationDateLabelSwing = new FilmListCreationDateLabel();
        statusBar.add(creationDateLabelSwing);
        FilmAgeLabel ageLabel = new FilmAgeLabel();
        statusBar.add(ageLabel);
        /*JProgressBar pbar2 = new JProgressBar();
        pbar2.setValue(92);
        jxStatusBar.add(pbar2);*/
    }

    public JXStatusBar getStatusBar() {
        return statusBar;
    }

}
