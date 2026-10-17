# Interface translations

Select **Deutsch** or **English** in **File → Settings → General**, then restart MediathekView.

In German, the path is **Datei → Einstellungen → Allgemein → Sprache / Language**.

Existing installations continue to use German. Unsupported or missing language preferences also select German.

The interface language is independent of the geographic location and the language of programme metadata.

## Current coverage

This first contribution introduces the translation infrastructure and two working screens.

| Area                        | Coverage                                          |
|-----------------------------|---------------------------------------------------|
| General settings            | Controls, tooltips, modern-search explanation     |
| Settings window             | Window title, close button, navigation tree       |
| Single-film download        | Controls, subtitles, validation and status text   |
| Navigation to these screens | File/Films menu titles, settings/download actions |

Other screens, first-run setup, most menus, and bundled help still use their existing text.

## Resource bundles

Resources live in `src/main/resources/mediathek/i18n/`:

- `Messages.properties`: German source text and fallback.
- `Messages_en.properties`: English translations.

Both files use UTF-8. Keys are stable and grouped by screen, such as `download.dialog.btnQueueDownload.text`.

`UiMessages` supplies the same bundle to Kotlin code and generated Java forms.

It uses the selected UI locale explicitly and disables fallback to the operating system's default locale.

Missing English entries inherit German. A key missing from both bundles is a programming error.

The JVM default locale stays unchanged, preserving existing filtering and data-processing behaviour.

Swing's component and UI defaults receive the selected UI locale before the main interface is created.

The saved key is `application.ui.language`, with the stable values `de` and `en`.

It is registered with `@ApplicationConfigKey`, so configuration cleanup retains it.

## One complete translation example

The download size lookup reports an HTTP status using one message with a parameter.

German bundle:

```properties
download.sizeHttpError=HTTP {0} beim Ermitteln der Dateigröße.
```

English bundle:

```properties
download.sizeHttpError=HTTP {0} while retrieving the file size.
```

Kotlin caller:

```kotlin
UiMessages.format("download.sizeHttpError", sizeLoadResult.httpStatusCode.toString())
```

The status is a string so `MessageFormat` does not add numeric grouping to an HTTP code.

Use `UiMessages.text(key)` for labels and `UiMessages.format(key, arguments...)` for parameterized messages.

Format whole sentences. Preserve parameter indexes and escape literal apostrophes as `''` in formatted messages.

`MessageFormat` choice patterns can express German/English singular and plural messages when needed.

## Editing forms with JFormDesigner

Open a localized `.jfd` file and use **Form → Localize** to edit its bundle and preview another language.

The bundle is `mediathek.i18n.Messages`. Each form has its own key prefix and automatic externalization enabled.

The form's **I18n 'getBundle' Template** is:

```java
mediathek.i18n.UiMessages.getBundle(${bundleName})
```

Keep that template: the default template would choose the JVM locale instead of the saved UI language.

Save the `.jfd`, generated `.java`, and changed `.properties` files together.

Keep runtime messages in the Kotlin subclass localized too; they can replace text from the generated form.

A regular Maven build compiles the checked-in Java sources and needs no JFormDesigner installation or licence.

## Adding another language

Add a `UiLanguage` entry with a stable code, locale, and native display name, then add `Messages_<code>.properties`.

The settings selector uses the enum entries. Translation keys and screen code remain the same.

Review formatted parameters, keyboard mnemonics, tooltips, and layout in the added language.

## Review workflow

Build with `./mvnw package`. Use the application's positional settings-directory argument for a separate profile.

Select a language, close the application, and reopen the same profile to review its saved preference.

The current pilot is ready for review before translating the remaining screens.

## Pilot screenshots

These screens were rendered on Linux with separate settings directories and sample programme data.

- General settings: [German](images/i18n/settings-de.png) · [English](images/i18n/settings-en.png).
- Single-film download: [German](images/i18n/download-de.png) · [English](images/i18n/download-en.png).
