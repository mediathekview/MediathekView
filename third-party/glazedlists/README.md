# Glazed Lists

The Glazed Lists sources in `src/main/kotlin/ca/odell/glazedlists` are a Kotlin
support library vendored from the upstream Glazed Lists core module.

- Repository: https://github.com/glazedlists/glazedlists
- Imported revision: `9ace85251cc0f8e70eebc9dfdb2cb2a920cb0275`
- Revision date: 2025-05-11
- Upstream source path: `core/src/main`

The optional JavaFX, SWT, Hibernate, and other extension modules are not
included. Java object serialization and the obsolete Java 1.4 lock backport
are also intentionally omitted.

## Supported API

The vendored code is maintained as a Kotlin support library. Kotlin source
contracts and observable behavior are supported; historical Java source and
binary compatibility are not. There are no production Java sources or Java
callers in MediathekView.

The reusable collection family remains available even when a type has no
current application call site. This includes `CompositeList`, `SequenceList`,
`UniqueList`, `GroupingList`, `ThresholdList`, `FreezableList`,
`PluggableList`, `DebugList`, `FunctionListMap`, and `GroupingListMultiMap`.
Future cleanup must not remove them or reduce their existing source
accessibility solely because they are currently unused.

Public Kotlin APIs should use Kotlin properties, function types, nullability,
and functional interfaces. Do not add Java-only bridge methods, static facade
shells, or tests that pin compiler-generated JVM members unless a real
interoperability boundary requires them.

Top-level declarations in `ca.odell.glazedlists.impl` remain Kotlin-internal
unless a supported public or protected contract exposes the type. Kotlin
`internal` is a source-level boundary and does not guarantee JVM bytecode
encapsulation.

## Local Differences

- `java.util.concurrent.locks` replaces the removed lock backport. The default
  lock remains a non-fair `ReentrantReadWriteLock` with read-to-write upgrade
  detection.
- Java serialization support is omitted.
- Deprecated compatibility aliases superseded by `DefaultEventListModel`,
  `DefaultEventComboBoxModel`, `DefaultEventSelectionModel`, and
  `DefaultEventTableModel` are omitted.
- `ObservableConnector` uses property-change listeners instead of the removed
  JDK `Observable`/`Observer` API.
- `ThreadedMatcherEditor` uses coroutines and virtual threads and is closeable.
- Kotlin extensions provide list transformations, synchronization helpers,
  read-only views, transactions, and Swing model factories.

Behavioral fixes and intentional deviations should be documented in focused
tests and commit history rather than in a Java-to-Kotlin migration ledger.

## Maintenance

Upstream updates must be ported deliberately and reconciled with the local
Kotlin API and tests. Validate changes with the narrowest relevant tests, then
run the complete Glazed Lists suite:

```shell
./mvnw -Dtest='ca.odell.glazedlists.**' test
```

See `LICENSE` in this directory for the upstream licensing terms.
