package mediathek.mainwindow;

import org.jdesktop.swingx.JXStatusBar;

import javax.swing.*;
import java.awt.*;

public class StatusBar extends JComponent {
    /**
     * This class tries to fix some redraw issues with JXStatusBar on removal.
     */
    static class FixedRedrawStatusBar extends JXStatusBar {
        @Override
        public void remove(int index) {
            super.remove(index);
            this.revalidate();
        }

        @Override
        public void remove(Component comp) {
            super.remove(comp);
            this.revalidate();
        }

        @Override
        public void removeAll() {
            super.removeAll();
            this.revalidate();
        }
    }
    private final FixedRedrawStatusBar statusBar = new FixedRedrawStatusBar();

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
