package mSearch.daten;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.params.provider.Arguments.arguments;
import static org.mockito.Mockito.spy;

import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class DatenFilmTest {

  @Spy
  static DatenFilm testEntity;

  @BeforeAll
  static void initializeTestEntity() {
    DatenFilm df = new DatenFilm();
    testEntity = spy(df);

  }

  static Stream<Arguments> filmLengthEdgeCases() {
    return Stream.of(
        arguments("01:21:30", 4890L),
        arguments((String)null, 0L), // Die Methode fängt alle Exceptions
        arguments("01:91:65", 9125L), // Minuten und Sekungen > 59 werden auch verarbeitet!
        arguments("01:31", 0L), // Es müssen immer Stunden:Minuten:Sekunden eingegeben werden
        arguments("1:0:0", 3600L), // Es müssen keine führenden Nullen verwendet werden
        arguments("100:100:100", 366100L) // dreistellige Zahlen sind kein Problem
    );
  }

  @ParameterizedTest
  @MethodSource("filmLengthEdgeCases")
  void testFilmLengthCalculation(String input, long expected) {

    testEntity.arr[DatenFilm.FILM_DAUER] = input;
    testEntity.init();

    assertThat(expected).isEqualTo(testEntity.getFilmLength());

  }

}