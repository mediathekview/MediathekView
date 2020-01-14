package mediathek.mac.tabs;

import com.thizzer.jtouchbar.JTouchBar;
import com.thizzer.jtouchbar.common.ImageName;
import com.thizzer.jtouchbar.item.TouchBarItem;
import com.thizzer.jtouchbar.item.view.TouchBarButton;
import jiconfont.icons.FontAwesome;
import mediathek.config.Daten;
import mediathek.gui.tabs.tab_downloads.GuiDownloads;
import mediathek.mac.touchbar.TouchBarUtils;
import mediathek.mainwindow.MediathekGui;

import javax.swing.*;

public class TabDownloadsMac extends GuiDownloads {
    public TabDownloadsMac(Daten aDaten, MediathekGui mediathekGui) {
        super(aDaten, mediathekGui);
    }

    @Override
    protected void setupTouchBar() {
        touchBar = new JTouchBar();
        touchBar.setCustomizationIdentifier("tabDownloads");

        TouchBarButton btnFilmInformation = new TouchBarButton();
        btnFilmInformation.setAction(view -> SwingUtilities.invokeLater(MediathekGui.ui().getFilmInfoDialog()::showInfo));
        btnFilmInformation.setImage(TouchBarUtils.touchBarImageFromIcon(TouchBarUtils.iconFromFontAwesome(FontAwesome.INFO_CIRCLE)));
        touchBar.addItem(new TouchBarItem("btnFilmInformation", btnFilmInformation,false));

        TouchBarButton btnUpdateDownloads = new TouchBarButton();
        btnUpdateDownloads.setImage(TouchBarUtils.touchBarImageFromFontAwesome(FontAwesome.REFRESH));
        btnUpdateDownloads.setAction(f -> SwingUtilities.invokeLater(this::updateDownloads));
        touchBar.addItem(new TouchBarItem("btnUpdateDownloads", btnUpdateDownloads, false));

        TouchBarButton btnStartAllDownloads = new TouchBarButton();
        btnStartAllDownloads.setImage(TouchBarUtils.touchBarImageFromFontAwesome(FontAwesome.ANGLE_DOUBLE_DOWN));
        btnStartAllDownloads.setAction(f -> SwingUtilities.invokeLater(() -> starten(true)));
        touchBar.addItem(new TouchBarItem("btnStartAllDownloads", btnStartAllDownloads,false));

        TouchBarButton btnPlayFilm = new TouchBarButton();
        var playImage = new com.thizzer.jtouchbar.common.Image(ImageName.NSImageNameTouchBarPlayTemplate, false);
        btnPlayFilm.setImage(playImage);
        btnPlayFilm.setAction(f -> SwingUtilities.invokeLater(this::filmAbspielen));
        touchBar.addItem(new TouchBarItem("btnPlayFilm", btnPlayFilm,false));

        TouchBarButton btnZurueckstellen = new TouchBarButton();
        btnZurueckstellen.setImage(TouchBarUtils.touchBarImageFromFontAwesome(FontAwesome.CLOCK_O));
        btnZurueckstellen.setAction(f -> SwingUtilities.invokeLater(() -> downloadLoeschen(false)));
        touchBar.addItem(new TouchBarItem("btnZurueckstellen", btnZurueckstellen, false));

        TouchBarButton btnRemoveDownload = new TouchBarButton();
        btnRemoveDownload.setImage(TouchBarUtils.touchBarImageFromFontAwesome(FontAwesome.TRASH_O));
        btnRemoveDownload.setAction(f -> SwingUtilities.invokeLater(() -> downloadLoeschen(true)));
        touchBar.addItem(new TouchBarItem("btnRemoveDownload", btnRemoveDownload,false));

        TouchBarButton btnCleanup = new TouchBarButton();
        btnCleanup.setImage(TouchBarUtils.touchBarImageFromFontAwesome(FontAwesome.ERASER));
        btnCleanup.setAction(f -> SwingUtilities.invokeLater(this::cleanupDownloads));
        touchBar.addItem(new TouchBarItem("btnCleanup", btnCleanup,false));
    }
}
