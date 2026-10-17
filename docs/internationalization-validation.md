# Interface-language validation

Reviewed on Linux with Java 25 on 17 October 2026.

## Build and existing tests

`./mvnw -B package` passed after the final code changes.

725 tests ran: 0 failures, 0 errors, 3 skipped. No new automated tests were added.

The four newly localized duplicate/subtitle forms parsed with JFormDesigner's form loader.

The About form retains its regenerated layout and protected fields, with one generated bundle declaration.

## Saved-language persistence

The real General settings language control was exercised in disposable profiles under `/tmp`.

The existing profile and IntelliJ run configuration were not modified.

| Step | Loaded language | Selected preference | Current session |
| ---- | --------------- | ------------------- | --------------- |
| 1    | German          | English             | German          |
| 2    | English         | German              | English         |
| 3    | German          | Unchanged           | German          |

Each step ran in a fresh JVM. Selection persisted through `ApplicationConfiguration.writeConfiguration()`.

A second review launched the actual `Main.main` entry point three times with a separate disposable profile.

It loaded German, then English, then German; the main File menu showed `Datei`, `File`, then `Datei`.

The profile used manual film-list loading, disabled automatic update checks, and an empty catalogue.

The review saved configuration explicitly and exited each process; normal quit-confirmation handling was not exercised.

Main-window captures: [German](images/i18n/restarted-de.png) · [English](images/i18n/restarted-en.png).

## Dialogs and help

Actual Swing dialogs were rendered in German and English with disposable profiles.

Reviewed subtitle merging, duplicate statistics/details/overview, and the English Lucene help document.

These previews covered labels, headings, counts, and layout; they did not perform a real ffmpeg merge.

English selected `_en` help resources; German selected the originals. Missing translations retained the original path.

All eleven German help resources have English counterparts, including the Lucene and Audiothek guides.

Search field keys, query examples, language codes, and program-set macros remain configuration or input syntax.

## Remaining validation

Windows and macOS runtime/layout review is pending: neither platform was available in this session.

Native file choosers, macOS menus/shutdown helpers, and Windows integration were not exercised.

A real subtitle merge and a normal quit/reopen flow remain unverified by this review.

Server announcements, programme metadata, and externally hosted release notes retain their source language.

This review covers the translated areas; it is not an exhaustive audit of every specialist tab or diagnostic screen.
