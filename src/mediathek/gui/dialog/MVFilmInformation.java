package mediathek.gui.dialog;

import com.explodingpixels.macwidgets.HudWidgetFactory;
import com.explodingpixels.macwidgets.HudWindow;
import com.explodingpixels.macwidgets.plaf.HudPaintingUtils;
import com.jgoodies.forms.layout.ColumnSpec;
import com.jgoodies.forms.layout.FormLayout;
import com.jgoodies.forms.layout.FormSpecs;
import com.jgoodies.forms.layout.RowSpec;
import mediathek.daten.DatenFilm;
import org.jdesktop.swingx.JXHyperlink;

import javax.swing.*;
import java.awt.*;
import java.net.URISyntaxException;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import mediathek.daten.DDaten;
import mediathek.tool.EscBeenden;
import mediathek.tool.UrlHyperlinkAction;

/**
 * Display the current film information in a Apple-style HUD window.
 */
public class MVFilmInformation implements ChangeListener {

    private HudWindow hud;
    private JLabel lblNrField;
    private JLabel lblSenderField;
    private JLabel lblThemaField;
    private JLabel lblTitleField;
    private JLabel lblDatumField;
    private JLabel lblTimeField;
    private JLabel lblUrlField;
    private JLabel lblUrlRtmpField;
    private JLabel lblUrlAuthField;
    private JXHyperlink lblUrlThemaField;
    private JLabel lblAboField;
    private DDaten ddaten;

    public MVFilmInformation(Frame owner, JTabbedPane tabbedPane, DDaten ddaten) {
        hud = new HudWindow("Filminformation", owner);
        hud.makeResizeable();
        this.ddaten = ddaten;
        JComponent content = buildContent();
        //prevents flickering in JDK7, JDK6 is still buggy :(
        content.setOpaque(false);
        hud.setContentPane(content);
        JDialog dialog = hud.getJDialog();
        dialog.pack();
        Dimension size = dialog.getSize();
        size.width = 500;
        dialog.setSize(size);
        calculateHudPosition();

        tabbedPane.addChangeListener(this);

        new EscBeenden(dialog) {
            @Override
            public void beenden_(JDialog d) {
                d.dispose();
            }
        };
    }

    private void calculateHudPosition() {
        //FIXME calculate the HUD position
    }

    public void show() {
        hud.getJDialog().setVisible(true);
    }

    public boolean isVisible() {
        return hud.getJDialog().isVisible();
    }

    public void updateCurrentFilm(DatenFilm film) {
        //update data
        //TODO überprüfen ob Nummeranzeige richtig geschrieben ist! Manchmal falsche Nummern
        lblNrField.setText(film.arr[DatenFilm.FILM_NR_NR]);
        lblSenderField.setText(film.arr[DatenFilm.FILM_SENDER_NR]);
        lblThemaField.setText(film.arr[DatenFilm.FILM_THEMA_NR]);
        lblTitleField.setText(film.arr[DatenFilm.FILM_TITEL_NR]);
        lblDatumField.setText(film.arr[DatenFilm.FILM_DATUM_NR]);
        lblTimeField.setText(film.arr[DatenFilm.FILM_ZEIT_NR]);
        lblUrlField.setText(film.arr[DatenFilm.FILM_URL_NR]);
        lblUrlRtmpField.setText(film.arr[DatenFilm.FILM_URL_RTMP_NR]);
        lblUrlAuthField.setText(film.arr[DatenFilm.FILM_URL_AUTH_NR]);
        // setup Hyperlink
        // FIXME ist das nicht besser zu lösen?
        try {
            lblUrlThemaField.setAction(new UrlHyperlinkAction(ddaten, film.arr[DatenFilm.FILM_URL_THEMA_NR]));
            lblUrlThemaField.setForeground(Color.WHITE);
        } catch (URISyntaxException ignored) {
        }
        lblAboField.setText(film.arr[DatenFilm.FILM_ABO_NAME_NR]);

        hud.getJDialog().repaint();
    }

    private JComponent buildContent() {
        JPanel panel = new JPanel();

        GridBagLayout gridbag = new GridBagLayout();
        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(5, 10, 10, 5);
        panel.setLayout(gridbag);
        int zeile = 0;
        c.gridy = zeile;


        JLabel label = HudWidgetFactory.createHudLabel("Nr:");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        lblNrField = HudWidgetFactory.createHudLabel("");
        addLable(zeile++, gridbag, c, panel, label, lblNrField);

        label = HudWidgetFactory.createHudLabel("Sender:");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        lblSenderField = HudWidgetFactory.createHudLabel("");
        addLable(zeile++, gridbag, c, panel, label, lblSenderField);

        label = HudWidgetFactory.createHudLabel("Thema:");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        lblThemaField = HudWidgetFactory.createHudLabel("");
        addLable(zeile++, gridbag, c, panel, label, lblThemaField);

        label = HudWidgetFactory.createHudLabel("Titel:");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        lblTitleField = HudWidgetFactory.createHudLabel("");
        addLable(zeile++, gridbag, c, panel, label, lblTitleField);

        label = HudWidgetFactory.createHudLabel("Datum:");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        lblDatumField = HudWidgetFactory.createHudLabel("");
        addLable(zeile++, gridbag, c, panel, label, lblDatumField);

        label = HudWidgetFactory.createHudLabel("Zeit:");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        lblTimeField = HudWidgetFactory.createHudLabel("");
        addLable(zeile++, gridbag, c, panel, label, lblTimeField);

        label = HudWidgetFactory.createHudLabel("URL:");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        lblUrlField = HudWidgetFactory.createHudLabel("");
        addLable(zeile++, gridbag, c, panel, label, lblUrlField);

        label = HudWidgetFactory.createHudLabel("UrlRTMP:");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        lblUrlRtmpField = HudWidgetFactory.createHudLabel("");
        addLable(zeile++, gridbag, c, panel, label, lblUrlRtmpField);

        label = HudWidgetFactory.createHudLabel("UrlAuth:");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        lblUrlAuthField = HudWidgetFactory.createHudLabel("");
        addLable(zeile++, gridbag, c, panel, label, lblUrlAuthField);

        label = HudWidgetFactory.createHudLabel("UrlThema:");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        lblUrlThemaField = new JXHyperlink();
        lblUrlThemaField.setForeground(Color.WHITE);
        try {
            lblUrlThemaField.setAction(new UrlHyperlinkAction(ddaten, ""));
        } catch (URISyntaxException ignored) {
        }
        lblUrlThemaField.setFont(HudPaintingUtils.getHudFont());
        addLable(zeile++, gridbag, c, panel, label, lblUrlThemaField);

        label = HudWidgetFactory.createHudLabel("Abo-Name:");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        lblAboField = HudWidgetFactory.createHudLabel("");
        addLable(zeile++, gridbag, c, panel, label, lblAboField);

        panel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));

        return panel;
    }

    private void addLable(int zeile, GridBagLayout gridbag, GridBagConstraints c, JPanel panel, JLabel l1, Component l2) {
        //Label
        c.gridy = zeile;
        c.gridx = 0;
        c.weightx = 0;
        gridbag.setConstraints(l1, c);
        panel.add(l1);
        //Textfeld
        c.gridx = 1;
        c.weightx = 10;
        gridbag.setConstraints(l2, c);
        panel.add(l2);
    }

    @Override
    public void stateChanged(ChangeEvent changeEvent) {
        //Whenever there is a change event, reset HUD info to nothing
        DatenFilm emptyFilm = new DatenFilm();
        updateCurrentFilm(emptyFilm);
    }
}
