package mediathek.gui.tabs.tab_film;

import javafx.collections.ObservableList;
import mediathek.controller.history.SeenHistoryController;
import mediathek.daten.DatenFilm;
import mediathek.daten.ListeFilme;
import mediathek.javafx.filterpanel.FilmActionPanel;
import mediathek.javafx.filterpanel.FilmLengthSlider;
import mediathek.tool.Filter;
import mediathek.tool.models.TModelFilm;
import org.jetbrains.annotations.NotNull;

import javax.swing.table.TableModel;
import java.util.Objects;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

public class GuiFilmeModelHelper {
  private final FilmActionPanel fap;
  private TModelFilm filmModel;
  private final ListeFilme listeFilme;
  private final SeenHistoryController historyController;
  private boolean searchThroughDescriptions;
  private boolean showNewOnly;
  private boolean showBookmarkedOnly;
  private boolean showSubtitlesOnly;
  private boolean showHqOnly;
  private boolean dontShowSeen;
  private boolean dontShowAbos;
  private boolean showLivestreamsOnly;
  private boolean dontShowTrailers;
  private boolean dontShowGebaerdensprache;
  private boolean dontShowAudioVersions;
  private long maxLength;
  private String[] arrIrgendwo;
  private long minLengthInSeconds;
  private long maxLengthInSeconds;

  public GuiFilmeModelHelper(
      @NotNull FilmActionPanel fap,
      @NotNull ListeFilme filteredList,
      @NotNull SeenHistoryController historyController) {
    this.fap = fap;
    this.historyController = historyController;

    
    listeFilme = filteredList;
  }

  private String getFilterThema() {
    String filterThema = fap.themaBox.getSelectionModel().getSelectedItem();
    if (filterThema == null) {
      filterThema = "";
    }

    return filterThema;
  }

  private String[] evaluateThemaTitel() {
    String[] arrThemaTitel;

    final String filterThemaTitel = fap.roSearchStringProperty.getValueSafe();
    if (Filter.isPattern(filterThemaTitel)) {
      arrThemaTitel = new String[] {filterThemaTitel};
    } else {
      arrThemaTitel = filterThemaTitel.split(",");
      for (int i = 0; i < arrThemaTitel.length; ++i) {
        arrThemaTitel[i] = arrThemaTitel[i].trim().toLowerCase();
      }
    }

    return arrThemaTitel;
  }

  private boolean noFiltersAreSet() {
    boolean ret = false;

    if (fap.senderList.getCheckModel().isEmpty()
        && getFilterThema().isEmpty()
        && fap.roSearchStringProperty.getValueSafe().isEmpty()
        && ((int) fap.filmLengthSlider.getLowValue() == 0)
        && ((int) fap.filmLengthSlider.getHighValue() == FilmLengthSlider.UNLIMITED_VALUE)
        && !fap.dontShowAbos.getValue()
        && !fap.showUnseenOnly.getValue()
        && !fap.showOnlyHd.getValue()
        && !fap.showSubtitlesOnly.getValue()
        && !fap.showLivestreamsOnly.getValue()
        && !fap.showNewOnly.getValue()
        && !fap.showBookMarkedOnly.getValue()
        && !fap.dontShowTrailers.getValue()
        && !fap.dontShowSignLanguage.getValue()
        && !fap.dontShowAudioVersions.getValue()) ret = true;

    return ret;
  }

  private void updateFilterVars() {
    showNewOnly = fap.showNewOnly.getValue();
    showBookmarkedOnly = fap.showBookMarkedOnly.getValue();
    showSubtitlesOnly = fap.showSubtitlesOnly.getValue();
    showHqOnly = fap.showOnlyHd.getValue();
    dontShowSeen = fap.showUnseenOnly.getValue();
    dontShowAbos = fap.dontShowAbos.getValue();
    showLivestreamsOnly = fap.showLivestreamsOnly.getValue();
    dontShowTrailers = fap.dontShowTrailers.getValue();
    dontShowGebaerdensprache = fap.dontShowSignLanguage.getValue();
    dontShowAudioVersions = fap.dontShowAudioVersions.getValue();
    searchThroughDescriptions = fap.searchThroughDescription.getValue();

    arrIrgendwo = evaluateThemaTitel();
  }

  private void calculateFilmLengthSliderValues() {
    final long minLength = (long) fap.filmLengthSlider.getLowValue();
    maxLength = (long) fap.filmLengthSlider.getHighValue();
    minLengthInSeconds = TimeUnit.SECONDS.convert(minLength, TimeUnit.MINUTES);
    maxLengthInSeconds = TimeUnit.SECONDS.convert(maxLength, TimeUnit.MINUTES);
  }

  private void performTableFiltering() {
    updateFilterVars();
    calculateFilmLengthSliderValues();

    final String filterThema = getFilterThema();
    final ObservableList<String> selectedSenders = fap.senderList.getCheckModel().getCheckedItems();

    if (dontShowSeen) historyController.prepareMemoryCache();

    var stream = listeFilme.parallelStream();
    if (!selectedSenders.isEmpty()) {
      //ObservableList.contains() is insanely slow...this speeds up to factor 250!
            Set<String> senderSet = new HashSet<>(selectedSenders.size());
            senderSet.addAll(selectedSenders);
            stream = stream.filter(f -> senderSet.contains(f.getSender()));
    }

    if (showNewOnly) stream = stream.filter(DatenFilm::isNew);
    if (showBookmarkedOnly) stream = stream.filter(DatenFilm::isBookmarked);
    if (showLivestreamsOnly) stream = stream.filter(DatenFilm::isLivestream);
    if (showHqOnly) stream = stream.filter(DatenFilm::isHighQuality);
    if (dontShowTrailers) stream = stream.filter(film -> !film.isTrailerTeaser());
    if (dontShowGebaerdensprache) stream = stream.filter(film -> !film.isSignLanguage());
    if (dontShowAudioVersions) stream = stream.filter(film -> !film.isAudioVersion());
    if (dontShowAbos) stream = stream.filter(film -> film.getAbo() == null);
    if (showSubtitlesOnly) {
      stream = stream.filter(this::subtitleCheck);
    }
    if (!filterThema.isEmpty()) {
      stream = stream.filter(film -> film.getThema().equalsIgnoreCase(filterThema));
    }
    if (maxLength < FilmLengthSlider.UNLIMITED_VALUE) {
      stream = stream.filter(this::maxLengthCheck);
    }
    if (dontShowSeen) {
      stream = stream.filter(this::seenCheck);
    }
    // perform min length filtering after all others may have reduced the available entries...
    stream = stream.filter(this::minLengthCheck);

        //final stage filtering...
        final boolean searchFieldEmpty = arrIrgendwo.length == 0;
        if (!searchFieldEmpty) {
            stream = stream.filter(this::finalStageFiltering);
        }

        var list = stream.filter(Objects::nonNull).collect(Collectors.toList());
        stream.close();

    //adjust initial capacity
        filmModel = new TModelFilm(list.size());
        filmModel.addAll(list);

        list.clear();if (dontShowSeen) historyController.emptyMemoryCache();
  }

  private boolean subtitleCheck(DatenFilm film) {
    return film.hasSubtitle() || film.hasBurnedInSubtitles();
  }

  private boolean maxLengthCheck(DatenFilm film) {
    return film.getFilmLength() < maxLengthInSeconds;
  }

  private boolean seenCheck(DatenFilm film) {
    return !historyController.hasBeenSeenFromCache(film);
  }

  private boolean minLengthCheck(DatenFilm film) {
    final long filmLength = film.getFilmLength();
    if (filmLength == 0)
      return true; // always show entries with length 0, which are internally "no length"
    else return filmLength >= minLengthInSeconds;
  }

  /** Perform the last stage of filtering. Rework!!! */
  private boolean finalStageFiltering(final DatenFilm film) {
    boolean result;

    if (searchThroughDescriptions && !film.getDescription().isEmpty())
      result = searchEntriesWithDescription(film);
    else result = searchEntries(film);

    return result;
  }

  private boolean searchEntries(DatenFilm film) {
    boolean result = false;
    if (Filter.pruefen(arrIrgendwo, film.getThema())
        || Filter.pruefen(arrIrgendwo, film.getTitle())) {
      result = true;
    }
    return result;
  }

  private boolean searchEntriesWithDescription(DatenFilm film) {
    boolean result = false;

    if (Filter.pruefen(arrIrgendwo, film.getDescription()) || searchEntries(film)) {
      result = true;
    }

    return result;
  }

  

  /**
   * Filter the filmlist.
   *
   * @return the filtered table model.
   */
  public TableModel getFilteredTableModel() {
    if (!listeFilme.isEmpty()) {
            if (noFiltersAreSet()) {
                //adjust initial capacity
  filmModel = new TModelFilm(listeFilme.size());
    filmModel.addAll(listeFilme);
      } else {
        performTableFiltering();
      }
    }else
  return new TModelFilm();

  return

    filmModel;
  }
}
