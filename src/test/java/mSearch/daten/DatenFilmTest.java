package mSearch.daten;

import mediathek.daten.DatenFilm;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.stream.Stream;

import static org.junit.jupiter.params.provider.Arguments.arguments;

@ExtendWith(MockitoExtension.class)
class DatenFilmTest {

  static Stream<Arguments> filmLengthEdgeCases() {
    return Stream.of(
        arguments("01:21:30", 4890L),
        arguments(null, 0L), // Die Methode fängt alle Exceptions
        arguments("01:91:65", 9125L), // Minuten und Sekungen > 59 werden auch verarbeitet!
        arguments("01:31", 0L), // Es müssen immer Stunden:Minuten:Sekunden eingegeben werden
        arguments("1:0:0", 3600L), // Es müssen keine führenden Nullen verwendet werden
        arguments("100:100:100", 366100L) // dreistellige Zahlen sind kein Problem
    );
  }

  @ParameterizedTest
  @MethodSource("filmLengthEdgeCases")
  void testFilmLengthCalculation(String input, long expected) {
    DatenFilm df = new DatenFilm();

    df.setFilmLength(input);
    df.init();

    Assertions.assertEquals(expected, df.getFilmLength());
  }

}