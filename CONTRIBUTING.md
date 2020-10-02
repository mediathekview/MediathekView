<!-- omit in toc -->
# Contributing to MediathekView

First off, thanks for taking the time to contribute! ❤️

All types of contributions are encouraged and valued. See the [Table of Contents](#table-of-contents) for different ways to help and details about how this project handles them. Please make sure to read the relevant section before making your contribution. It will make it a lot easier for us maintainers and smooth out the experience for all involved. The community looks forward to your contributions. 🎉

> And if you like the project, but just don't have time to contribute, that's fine. There are other easy ways to support the project and show your appreciation, which we would also be very happy about:
> - Star the project
> - Tweet about it
> - Refer this project in your project's readme
> - Mention the project at local meetups and tell your friends/colleagues

**Hint:** As MediathekView is a german software for the DACH tv stations ["Öffentlich-Rechtliche"](https://de.wikipedia.org/wiki/%C3%96ffentlich-rechtlicher_Rundfunk), many things like issue descriptions, UI text and so on are in geman. If you don't understand something feel free to ask one of us. Also, feel free to create issues in english.
<!-- omit in toc -->
## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [I Have a Question](#i-have-a-question)
- [I Want To Contribute](#i-want-to-contribute)
  - [Reporting Bugs](#reporting-bugs)
  - [Suggesting Enhancements](#suggesting-enhancements)
  - [Your First Code Contribution](#your-first-code-contribution)
  - [Improving The Documentation](#improving-the-documentation)
- [Styleguides](#styleguides)
  - [Commit Messages](#commit-messages)
- [Join The Project Team](#join-the-project-team)


## Code of Conduct

This project and everyone participating in it is governed by the
[MediathekView Code of Conduct](https://github.com/mediathekview/MediathekView/blob/master/CODE_OF_CONDUCT.md).
By participating, you are expected to uphold this code. Please report unacceptable behavior
to <info@mediathekview.de>.


## I Have a Question

> If you want to ask a question, we assume that you have read the available [Documentation](https://mediathekview.de/anleitung/).

Please use the [Forum](https://forum.mediathekview.de) to ask questions, give feedback or report missing films. 

## I Want To Contribute

> ### Legal Notice <!-- omit in toc -->
> When contributing to this project, you must agree that you have authored 100% of the content, that you have the necessary rights to the content and that the content you contribute may be provided under the project license.

### Reporting Bugs

<!-- omit in toc -->
#### Before Submitting a Bug Report

A good bug report shouldn't leave others needing to chase you up for more information. Therefore, we ask you to investigate carefully, collect information and describe the issue in detail in your report. Please complete the following steps in advance to help us fix any potential bug as fast as possible.

- Make sure that you are using the latest version.
- Determine if your bug is really a bug and not an error on your side e.g. using incompatible environment components/versions (Make sure that you have read the [documentation](https://mediathekview.de/anleitung/). If you are looking for support, you might want to check [this section](#i-have-a-question)).
- To see if other users have experienced (and potentially already solved) the same issue you are having, check if there is not already a bug report existing for your bug or error in the [bug tracker](https://github.com/mediathekview/MediathekView/issues?q=label%3Abug) or a thread in the [Forum](https://forum.mediathekview.de).
- Also make sure to search the internet (including Stack Overflow) to see if users outside of the GitHub community have discussed the issue.
- Collect information about the bug:
  - Stack trace (Traceback)
  - OS, Platform and Version (Windows, Linux, macOS, x86, ARM)
  - If you don't use the bundled JDK, the Java Version.
  - Possibly your input and the output
  - Can you reliably reproduce the issue? And can you also reproduce it with older versions?
  
 Please make sure to report your issue in the correct project. This project is for the desktop client MediathekView. 
 If your issue relates to the crawlers [MServer](https://github.com/mediathekview/MServer) is the right place.
 If your issue relates to the web client MediathekViewWeb then [MediathekViewWeb](https://github.com/mediathekview/MediathekViewWeb) is the right place.

<!-- omit in toc -->
#### How Do I Submit a Good Bug Report?

> You must never report security related issues, vulnerabilities or bugs to the issue tracker, or elsewhere in public. Instead sensitive bugs must be sent by email to <info@mediathekview.de>.

We use GitHub issues to track bugs and errors. If you run into an issue with the project:

- Open an [Issue](https://github.com/mediathekview/MediathekView/issues/new). (Since we can't be sure at this point whether it is a bug or not, we ask you not to talk about a bug yet and not to label the issue.)
- Explain the behavior you would expect and the actual behavior.
- Please provide as much context as possible and describe the *reproduction steps* that someone else can follow to recreate the issue on their own. This usually includes your code. For good bug reports you should isolate the problem and create a reduced test case.
- Provide the information you collected in the previous section.

Once it's filed:

- The project team will label the issue accordingly.
- A team member will try to reproduce the issue with your provided steps. If there are no reproduction steps or no obvious way to reproduce the issue, the team will ask you for those steps and mark the issue as `needs-repro`. Bugs with the `needs-repro` tag will not be addressed until they are reproduced.
- If the team is able to reproduce the issue, it will be marked `bug`, as well as possibly other tags (such as `critical`), and the issue will be left to be [implemented by someone](#your-first-code-contribution).

<!-- You might want to create an issue template for bugs and errors that can be used as a guide and that defines the structure of the information to be included. If you do so, reference it here in the description. -->


### Suggesting Enhancements

This section guides you through submitting an enhancement suggestion for MediathekView, **including completely new features and minor improvements to existing functionality**. Following these guidelines will help maintainers and the community to understand your suggestion and find related suggestions.

<!-- omit in toc -->
#### Before Submitting an Enhancement

- Make sure that you are using the latest version.
- Read the [documentation](https://mediathekview.de/anleitung/) carefully and find out if the functionality is already covered, maybe by an individual configuration.
- Perform a search on [GitHub](https://github.com/mediathekview/MediathekView/issues) and in the [Forum](https://forum.mediathekview.de) to see if the enhancement has already been suggested. If it has, add a comment to the existing issue instead of opening a new one.
- Find out whether your idea fits with the scope and aims of the project. It's up to you to make a strong case to convince the project's developers of the merits of this feature. Keep in mind that we want features that will be useful to the majority of our users and not just a small subset. If you're just targeting a minority of users, consider writing an add-on/plugin library.

<!-- omit in toc -->
#### How Do I Submit a Good Enhancement Suggestion?

Enhancement suggestions are tracked as [GitHub issues](https://github.com/mediathekview/MediathekView/issues).

- Use a **clear and descriptive title** for the issue to identify the suggestion.
- Provide a **step-by-step description of the suggested enhancement** in as many details as possible.
- **Describe the current behavior** and **explain which behavior you expected to see instead** and why. At this point you can also tell which alternatives do not work for you.
- You may want to **include screenshots and animated GIFs** which help you demonstrate the steps or point out the part which the suggestion is related to. You can use [this tool](https://www.cockos.com/licecap/) to record GIFs on macOS and Windows, and [this tool](https://github.com/colinkeenan/silentcast) or [this tool](https://github.com/GNOME/byzanz) on Linux. <!-- this should only be included if the project has a GUI -->
- **Explain why this enhancement would be useful** to most MediathekView users. You may also want to point out the other projects that solved it better and which could serve as inspiration.

<!-- You might want to create an issue template for enhancement suggestions that can be used as a guide and that defines the structure of the information to be included. If you do so, reference it here in the description. -->

### Your First Code Contribution
#### Dev Environment
**Compiler:**

As MediathekView is written in java you need to have a JDK installed for the correct java version. You can find the currently used java version in the [pom.xml](https://github.com/mediathekview/MediathekView/blob/master/pom.xml) tag `jdk.language.version`.

> We can recommend to use [SDKMan](https://sdkman.io/) to install the right [AdoptOpenJDK](https://adoptopenjdk.net/) version.

**Building:**

We use [maven](https://maven.apache.org/) to build the project. You don't need to install it locally because the project files include a maven wrapper with the currently used maven version.

**IDE:**

We recommend to use [JetBrains IntelliJ IDEA](https://www.jetbrains.com/idea/).
> We are part of the [JetBrains Open-Source program](https://www.jetbrains.com/community/opensource/#support). So if you are an active contributor it's possible for you to get a one-year license for all JetBrains products. To get your license, please contact [Nicklas](https://github.com/Nicklas2751) as he is responsible for this.

We also reccomend these plugins:
- [SonarLint](https://plugins.jetbrains.com/plugin/7973-sonarlint)
- [Save Actions](https://plugins.jetbrains.com/plugin/7642-save-actions)
- [google-java-format](https://plugins.jetbrains.com/plugin/8527-google-java-format)

#### First steps

1. Install the right JDK
2. Clone the git repository
    ```sh
    git clone https://github.com/mediathekview/MediathekView.git
    ```
3. Build the project
    ```sh
    cd MediathekView
    ./mvnw clean install
    ```

That's it. Happy coding! 🥳


### Improving The Documentation
The user documentation is [part](https://github.com/mediathekview/MediathekViewDE/blob/master/content/anleitung.md) of the [website](https://github.com/mediathekview/MediathekViewDE) so it can be edited through this GitHub project: [MediathekViewDE](https://github.com/mediathekview/MediathekViewDE). All new features should get documented there. Make sure to update the documentation if you change some documented behavior.
## Styleguides
We use the [SonarSource](https://rules.sonarsource.com/java) default rules. To check if your code against the rules use [SonarLint](https://www.sonarlint.org/). We also run a [SonarCloud](https://sonarcloud.io/dashboard?id=mediathekview_MediathekView) analysis with our builds. The [SonarCloud Bot](https://github.com/apps/sonarcloud) checks Pull-Request to not break the quality gate.

For **code formatting** we use the [Google Jave Style](https://google.github.io/styleguide/javaguide.html) and for this the [google-java-format](https://github.com/google/google-java-format).
### Branch name schema
We use the [git-flow](https://nvie.com/posts/a-successful-git-branching-model/) branch names so `hotfix/ticketNumber_branch_name` for a hotfix and `feature/ticketNumber_branch_name` for a feature branch.

Som of us use the [git-flow extension](https://github.com/nvie/gitflow).
### Commit Messages
Please read this: [Chris Beams - How to Write a Git Commit Message](https://chris.beams.io/posts/git-commit/)

## Join The Project Team
If you're an active contributor and want to join the core MediathekView Team just [contact one of us](https://mediathekview.de/ueberuns/).

<!-- omit in toc -->
## Attribution
This guide is based on the **contributing-gen**. [Make your own](https://github.com/bttger/contributing-gen)!