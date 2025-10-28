/*
 * Copyright (c) 2025 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.gui.filmInformation;

import mediathek.daten.DatenFilm;
import mediathek.gui.expiration.SenderExpirationService;
import mediathek.tool.datum.DateUtil;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.time.LocalDate;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;

class FilmAvailableUntilWorker extends SwingWorker<LocalDate, Integer> {
    private final DatenFilm currentFilm;
    private final JLabel lblAvailableUntil;

    public FilmAvailableUntilWorker(@NotNull DatenFilm currentFilm, @NotNull JLabel lblAvailableUntil) {
        this.currentFilm = currentFilm;
        this.lblAvailableUntil = lblAvailableUntil;

        lblAvailableUntil.setText("Suche...");
    }

    @Override
    protected LocalDate doInBackground() {
        return SenderExpirationService.fetchExpiryDate(currentFilm.getSender(), currentFilm.getWebsiteUrl());
    }

    @Override
    protected void done() {
        try {
            var result = get();
            if (result != null) {
                lblAvailableUntil.setText(DateUtil.FORMATTER.format(result));
                currentFilm.setAvailableUntil(result);
            }
            else {
                lblAvailableUntil.setText("");
                currentFilm.setAvailableUntil(null);
            }

        }
        catch (InterruptedException | ExecutionException | CancellationException _) {
            //ignore
        }
    }
}
