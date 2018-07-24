package mediathek.tool.table;

import mSearch.daten.DatenFilm;
import mediathek.config.MVConfig;
import mediathek.tool.MVFont;
import mediathek.tool.TModel;
import mediathek.tool.TModelFilm;

public class MVFilmTable extends MVTable {
    private static final long serialVersionUID = -5362792359176783146L;

    @Override
    protected void setupTableType() {
        maxSpalten = DatenFilm.MAX_ELEM;
        spaltenAnzeigen = getSpaltenEinAus(DatenFilm.spaltenAnzeigen, DatenFilm.MAX_ELEM);
        indexSpalte = DatenFilm.FILM_NR;
        nrDatenSystem = MVConfig.Configs.SYSTEM_EIGENSCHAFTEN_TABELLE_FILME;
        iconAnzeigenStr = MVConfig.Configs.SYSTEM_TAB_FILME_ICON_ANZEIGEN;
        iconKleinStr = MVConfig.Configs.SYSTEM_TAB_FILME_ICON_KLEIN;

        setModel(new TModelFilm(new Object[][]{}, DatenFilm.COLUMN_NAMES));
    }

    private void resetFilmeTab(int i) {
        reihe[i] = i;
        breite[i] = 200;
        switch (i) {
            case DatenFilm.FILM_NR:
                breite[i] = 75;
                break;
            case DatenFilm.FILM_TITEL:
                breite[i] = 300;
                break;
            case DatenFilm.FILM_DATUM:
            case DatenFilm.FILM_ZEIT:
            case DatenFilm.FILM_SENDER:
            case DatenFilm.FILM_GROESSE:
            case DatenFilm.FILM_DAUER:
            case DatenFilm.FILM_GEO:
                breite[i] = 100;
                break;
            case DatenFilm.FILM_URL:
                breite[i] = 500;
                break;
            case DatenFilm.FILM_ABSPIELEN:
            case DatenFilm.FILM_AUFZEICHNEN:
            case DatenFilm.FILM_HD:
            case DatenFilm.FILM_UT:
                breite[i] = 50;
                break;
        }
    }

    @Override
    public void resetTabelle() {
        for (int i = 0; i < maxSpalten; ++i) {
            resetFilmeTab(i);
        }

        super.resetTabelle();
    }

    @Override
    protected void spaltenAusschalten() {
        for (int i = 0; i < maxSpalten; ++i) {
            switch (i) {
                case DatenFilm.FILM_NEU:
                case DatenFilm.FILM_URL_HD:
                case DatenFilm.FILM_URL_KLEIN:
                case DatenFilm.FILM_DATUM_LONG:
                case DatenFilm.FILM_URL_HISTORY:
                case DatenFilm.FILM_URL_SUBTITLE:
                case DatenFilm.FILM_REF:
                    breite[i] = 0;
                    break;
            }
        }
    }

    @Override
    protected int getSizeArea() {
        int sizeArea = 0;

        if (lineBreak) {
            sizeArea = MVFont.fontSize * 4;
        }

        return sizeArea;
    }

    @Override
    public void getSelected() {
        super.getSelected();

        int selIndex = -1;
        if (selRow >= 0) {
            selIndex = (int) getModel().getValueAt(convertRowIndexToModel(selRow), indexSpalte);
        }

        if (selIndex >= 0) {
            selIndexes = new int[selRows.length];
            int k = 0;
            for (int i : selRows) {
                selIndexes[k++] = (int) getModel().getValueAt(convertRowIndexToModel(i), indexSpalte);
            }
        } else {
            selIndexes = null;
        }
    }

    @Override
    protected void setSelected() {
        boolean found = false;

        if (selIndexes != null) {
            int r;
            selectionModel.setValueIsAdjusting(true);
            TModel tModel = (TModel) getModel();
            for (int i : selIndexes) {
                r = tModel.getIdxRow(i);
                if (r >= 0) {
                    // ansonsten gibts die Zeile nicht mehr
                    r = convertRowIndexToView(r);
                    addRowSelectionInterval(r, r);
                    found = true;
                }
            }
            if (!found && selRow >= 0 && this.getRowCount() > selRow) {
                // große Frage was da besser ist???
                for (int i = selRow; i >= 0; --i) {
                    setRowSelectionInterval(i, i);
                    break;
                }
            } else if (!found && selRow >= 0 && this.getRowCount() > 0) {
                setRowSelectionInterval(tModel.getRowCount() - 1, tModel.getRowCount() - 1);
            }
            selectionModel.setValueIsAdjusting(false);
        }
        selIndexes = null;
    }
}
