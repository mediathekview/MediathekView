# Interface translations

Select **Deutsch** or **English** in **File → Settings → General**, then restart MediathekView.

In German, the path is **Datei → Einstellungen → Allgemein → Sprache / Language**.

Existing installations continue to use German. Unsupported or missing language preferences also select German.

The interface language is independent of the geographic location and the language of programme metadata.

## Current coverage

The draft PR covers the catalogue, downloads, subscriptions, bookmarks, and all settings pages.

| Area                    | Coverage                                               |
| ----------------------- | ------------------------------------------------------ |
| General settings        | Controls, tooltips, modern-search explanation          |
| Advanced settings       | Program paths, download integrations, shutdown         |
| Other settings          | Notifications, location, colours, Lucene tuning        |
| Film-list settings      | Loading, channel selection, blacklist rules            |
| Recording settings      | File names, replacements, download options             |
| Program sets            | Simple/full editor, helpers, import/export, validation |
| Settings window         | Window title, close button, navigation tree            |
| Single-film download    | Controls, subtitles, validation and status text        |
| Main window             | Menu names and entries, tab labels, toolbar actions    |
| Film catalogue          | Column headings, search hints, description panel       |
| Catalogue filters       | Controls, tooltips, filter-management dialogs          |
| Film context menu       | Actions, clipboard labels, duplicate-removal prompts   |
| Status bar              | Film/download counts, creation date and list age       |
| Download list           | Columns, filters, toolbars, counts, progress, menus    |
| Download dialogs        | Editing, batch setup, destinations, resume, errors     |
| Download scheduling     | Start time, completion options, waiting messages       |
| Subscription management | Columns, actions, filters, counts, editing, deletion   |
| Subscription history    | Columns, filtering, counts, deletion prompts           |
| Bookmarks               | Columns, actions, notes, expiry picker, deletion       |
| Text-field menus        | Undo, cut, copy, paste, and select all                 |

First-run setup and secondary dialogs remain partly German.

Help documents and background loading messages are still untranslated.

## Resource bundles

Resources live in `src/main/resources/mediathek/i18n/`:

- `Messages.properties`: German source text and fallback.
- `Messages_en.properties`: English translations.

Both files use UTF-8. Keys are stable and grouped by screen, such as `download.dialog.btnQueueDownload.text`.

`UiMessages` supplies the same bundle to Kotlin code and generated Java forms.

It uses the selected UI locale explicitly and disables fallback to the operating system's default locale.

Missing English entries inherit German. A key missing from both bundles is a programming error.

The JVM default locale stays unchanged, preserving existing filtering and data-processing behaviour.

Film and download columns retain numeric indexes for saved order, width, visibility, and sort settings.

Subtitle indicator lookup uses translated headings for both film and download tables.

Subscription columns keep their historical identifiers for saved layouts; headings and column menus use translations.

Bookmark columns also retain their historical identifiers, including icon selection and saved layouts.

The bookmark expiry picker uses the selected UI locale without changing the stored date.

Default subscription widths fit translated headings. Previously saved widths still take precedence.

Download filter values remain unchanged in configuration; combo-box renderers translate their displayed labels.

Download type and source labels translate separately from their stable identifiers and legacy data labels.

The scheduler creates its date and time picker with the selected UI locale.

The macOS shutdown selector translates its labels while retaining the helper's original command values.

Program paths, download-manager URLs, credentials, and shutdown commands remain configuration data.

Blacklist and program-set tables retain historical column identifiers for saved layouts.

Colour keys, Lucene mode values, program-set XML fields, and command macros remain unchanged.

Saved filter names, program-set names, channel names, and programme metadata are user data and are not translated.

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

Review download and subscription management in both languages, including saved filters and column layouts.

Bookmarks and all settings pages are also ready for review in both languages.

The Linux screenshots below cover actual Swing components rendered with separate profiles.

They do not replace a full application restart test or Windows and macOS layout review.

## Screenshots

These screens were rendered on Linux with separate settings directories and sample programme data.

- General settings: [German](images/i18n/settings-de.png) · [English](images/i18n/settings-en.png).
- Single-film download: [German](images/i18n/download-de.png) · [English](images/i18n/download-en.png).
- Main catalogue: [German](images/i18n/catalogue-de.png) · [English](images/i18n/catalogue-en.png).
- Catalogue filters: [German](images/i18n/filter-de.png) · [English](images/i18n/filter-en.png).
- Download list: [German](images/i18n/downloads-de.png) · [English](images/i18n/downloads-en.png).
- Subscriptions: [German](images/i18n/subscriptions-de.png) · [English](images/i18n/subscriptions-en.png).
- Subscription editor: [German](images/i18n/subscription-edit-de.png) · [English](images/i18n/subscription-edit-en.png).
- Download editor: [German](images/i18n/download-edit-de.png) · [English](images/i18n/download-edit-en.png).
- Scheduler: [German](images/i18n/download-schedule-de.png) · [English](images/i18n/download-schedule-en.png).
- Bookmarks: [German](images/i18n/bookmarks-de.png) · [English](images/i18n/bookmarks-en.png).
- Bookmark notes: [German](images/i18n/bookmark-note-de.png) · [English](images/i18n/bookmark-note-en.png).
- Advanced settings: [German](images/i18n/advanced-settings-de.png) · [English](images/i18n/advanced-settings-en.png).
- Colours: [German](images/i18n/colours-de.png) · [English](images/i18n/colours-en.png).
- Blacklist: [German](images/i18n/blacklist-de.png) · [English](images/i18n/blacklist-en.png).
- Program sets: [German](images/i18n/program-sets-de.png) · [English](images/i18n/program-sets-en.png).
- Program-set import: [German](images/i18n/import-set-de.png) · [English](images/i18n/import-set-en.png).
