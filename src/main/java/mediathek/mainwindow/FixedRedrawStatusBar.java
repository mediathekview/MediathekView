package mediathek.mainwindow;

import org.jdesktop.swingx.JXStatusBar;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;

/**
 * This class tries to fix some redraw issues with JXStatusBar on removal.
 */
public class FixedRedrawStatusBar extends JXStatusBar {
    public FixedRedrawStatusBar(@NotNull MediathekGui mediathekGui) {
        add(new SelectedListItemsLabel(mediathekGui));
        add(new FilmSizeInfoLabel(mediathekGui));
        add(new DownloadInformationLabel());

        add(new JPanel(), new Constraint(Constraint.ResizeBehavior.FILL));
        add(new FilmListCreationDateLabel());
        add(new FilmAgeLabel());
    }

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
