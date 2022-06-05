package mediathek.tool.table;

import mediathek.config.MVConfig;
import mediathek.daten.abo.DatenAbo;
import mediathek.tool.models.TModelAbo;

public class MVAbosTable extends MVTable {
    public MVAbosTable() {
        super(DatenAbo.MAX_ELEM, DatenAbo.spaltenAnzeigen);
    }

    @Override
    protected void setupTableType() {
        nrDatenSystem = MVConfig.Configs.SYSTEM_EIGENSCHAFTEN_TABELLE_ABOS;
        iconAnzeigenStr = MVConfig.Configs.SYSTEM_TAB_ABO_ICON_ANZEIGEN;
        iconKleinStr = MVConfig.Configs.SYSTEM_TAB_ABO_ICON_KLEIN;

        setModel(new TModelAbo(new Object[][]{}));
    }

    @Override
    public void resetTabelle() {
        for (int i = 0; i < maxSpalten; ++i) {
            resetAbosTab(i);
        }

        super.resetTabelle();
    }

    private void resetAbosTab(int i) {
        reihe[i] = i;
        breite[i] = 200;
        if (i == DatenAbo.ABO_NR
                || i == DatenAbo.ABO_EINGESCHALTET
                || i == DatenAbo.ABO_MIN) {
            breite[i] = 75;
        } else if (i == DatenAbo.ABO_DOWN_DATUM
                || i == DatenAbo.ABO_SENDER) {
            breite[i] = 100;
        }
    }

    private void spaltenAusschaltenAbos(int i) {
        if (i == DatenAbo.ABO_ZIELPFAD
                || i == DatenAbo.ABO_PSET
                || i == DatenAbo.ABO_MINDESTDAUER
                || i == DatenAbo.ABO_MIN
                || i == DatenAbo.ABO_DOWN_DATUM
                || i == DatenAbo.ABO_REF){
            breite[i] = 0;
        }
    }

    @Override
    protected void spaltenAusschalten() {
        for (int i = 0; i < maxSpalten; ++i) {
            spaltenAusschaltenAbos(i);
        }
    }
}
