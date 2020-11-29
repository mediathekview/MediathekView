package mediathek.mac.tabs;

import com.thizzer.jtouchbar.JTouchBar;
import com.thizzer.jtouchbar.common.ImageName;
import com.thizzer.jtouchbar.item.TouchBarItem;
import com.thizzer.jtouchbar.item.view.TouchBarButton;
import jiconfont.icons.font_awesome.FontAwesome;
import mediathek.config.Daten;
import mediathek.gui.tabs.tab_film.GuiFilme;
import mediathek.mac.touchbar.TouchBarUtils;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.FilmListUpdateType;
import mediathek.tool.GuiFunktionen;

import javax.swing.*;

public class TabFilmeMac extends GuiFilme
{
    public TabFilmeMac(Daten aDaten, MediathekGui mediathekGui) {
        super(aDaten,mediathekGui);
        if (TouchBarUtils.isTouchBarSupported())
            setupTouchBar();
    }

    @Override
    public void showTouchBar() {
        touchBar.show(mediathekGui);
    }

    @Override
    public void hideTouchBar() {
        touchBar.hide(mediathekGui);
    }

    protected void setupTouchBar() {
        touchBar = new JTouchBar();
        touchBar.setCustomizationIdentifier("tabFilme");

        TouchBarButton btnLoadFilmlist = new TouchBarButton();
        btnLoadFilmlist.setImage(TouchBarUtils.touchBarImageFromFontAwesome(FontAwesome.CLOUD_DOWNLOAD));
        btnLoadFilmlist.setAction(touchBarView -> SwingUtilities.invokeLater(() ->
                mediathekGui.performFilmListLoadOperation(GuiFunktionen.getFilmListUpdateType() == FilmListUpdateType.MANUAL)));

        TouchBarButton btnFilmInformation = new TouchBarButton();
        btnFilmInformation.setAction(view -> SwingUtilities.invokeLater(MediathekGui.ui().getFilmInfoDialog()::showInfo));
        btnFilmInformation.setImage(TouchBarUtils.touchBarImageFromFontAwesome(FontAwesome.INFO_CIRCLE));

        var playImage = new com.thizzer.jtouchbar.common.Image(ImageName.NSImageNameTouchBarPlayTemplate, false);
        TouchBarButton btnPlay = new TouchBarButton();
        btnPlay.setImage(playImage);
        btnPlay.setAction(f -> SwingUtilities.invokeLater(() -> MediathekGui.ui().tabFilme.playAction.actionPerformed(null)));

        TouchBarButton btnDownload = new TouchBarButton();
        btnDownload.setImage(TouchBarUtils.touchBarImageFromFontAwesome(FontAwesome.DOWNLOAD));
        btnDownload.setAction(f -> SwingUtilities.invokeLater(() -> MediathekGui.ui().tabFilme.saveFilmAction.actionPerformed(null)));

        TouchBarButton btnManageAbo = new TouchBarButton();
        btnManageAbo.setImage(TouchBarUtils.touchBarImageFromFontAwesome(FontAwesome.DATABASE));
        btnManageAbo.setAction(f -> SwingUtilities.invokeLater(() -> {
            if (filmActionPanel.manageAboAction.isEnabled())
                filmActionPanel.manageAboAction.actionPerformed(null);
        }));

        touchBar.addItem(new TouchBarItem("btnLoadFilmList", btnLoadFilmlist, false));
        touchBar.addItem(new TouchBarItem(TouchBarItem.NSTouchBarItemIdentifierFixedSpaceSmall));
        touchBar.addItem(new TouchBarItem("btnFilmInformation", btnFilmInformation, true));
        touchBar.addItem(new TouchBarItem(TouchBarItem.NSTouchBarItemIdentifierFixedSpaceSmall));
        touchBar.addItem(new TouchBarItem("btnPlay", btnPlay, false));
        touchBar.addItem(new TouchBarItem("btnDownload", btnDownload, false));
        touchBar.addItem(new TouchBarItem(TouchBarItem.NSTouchBarItemIdentifierFixedSpaceSmall));
        touchBar.addItem(new TouchBarItem("btnManageAbo", btnManageAbo, false));
    }
}
