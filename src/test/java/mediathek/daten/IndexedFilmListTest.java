package mediathek.daten;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.List;

class IndexedFilmListTest {

    @Test
    void getFilmByFilmNr_returnsMatchingFilmAfterIndexBuild() {
        var list = new IndexedFilmList();
        var firstFilm = createFilm("Alpha");
        var secondFilm = createFilm("Beta");

        list.addAll(List.of(firstFilm, secondFilm));

        Assertions.assertSame(firstFilm, list.getFilmByFilmNr(firstFilm.getFilmNr()));
        Assertions.assertSame(secondFilm, list.getFilmByFilmNr(secondFilm.getFilmNr()));
    }

    @Test
    void getFilmByFilmNr_rebuildsIndexAfterAdd() {
        var list = new IndexedFilmList();
        var firstFilm = createFilm("Alpha");
        list.add(firstFilm);

        Assertions.assertSame(firstFilm, list.getFilmByFilmNr(firstFilm.getFilmNr()));

        var secondFilm = createFilm("Beta");
        list.add(secondFilm);

        Assertions.assertSame(secondFilm, list.getFilmByFilmNr(secondFilm.getFilmNr()));
    }

    @Test
    void getFilmByFilmNr_rebuildsIndexAfterRemoveAndClear() {
        var list = new IndexedFilmList();
        var firstFilm = createFilm("Alpha");
        var secondFilm = createFilm("Beta");
        list.addAll(List.of(firstFilm, secondFilm));

        Assertions.assertSame(firstFilm, list.getFilmByFilmNr(firstFilm.getFilmNr()));

        list.remove(firstFilm);

        Assertions.assertNull(list.getFilmByFilmNr(firstFilm.getFilmNr()));
        Assertions.assertSame(secondFilm, list.getFilmByFilmNr(secondFilm.getFilmNr()));

        list.clear();

        Assertions.assertNull(list.getFilmByFilmNr(secondFilm.getFilmNr()));
    }

    @Test
    void getFilmByFilmNr_rebuildsIndexAfterSet() {
        var list = new IndexedFilmList();
        var originalFilm = createFilm("Original");
        list.add(originalFilm);

        Assertions.assertSame(originalFilm, list.getFilmByFilmNr(originalFilm.getFilmNr()));

        var replacementFilm = createFilm("Replacement");
        list.set(0, replacementFilm);

        Assertions.assertNull(list.getFilmByFilmNr(originalFilm.getFilmNr()));
        Assertions.assertSame(replacementFilm, list.getFilmByFilmNr(replacementFilm.getFilmNr()));
    }

    private DatenFilm createFilm(String title) {
        var film = new DatenFilm();
        film.setSender("ARD");
        film.setThema("Test");
        film.setTitle(title);
        film.setNormalQualityUrl("https://example.invalid/" + title);
        return film;
    }
}
