Project-Bifrost:<br/>A Scala Client for the Topl Blockchain Protocol 
====================================================================================================================================================================================

Project Bifrost is a Scala client designed over a cryptocurrency/blockchain protocol developed to achieve three specific goals:
- Facilitate movement and investment of capital from investors in developed economies to businesses in developing economies
- Create a commodity market that collateralizes a stable currency via storage and redemption
- Develop a reputation system and historical reference of business behaviour

Begin interacting with the Topl network with the Bifrost client [alpha testnet release](https://github.com/Topl/Project-Bifrost/releases/tag/v0.1.0-alpha)

<!---
Whitepaper
----------
The latest version of the Topl whitepaper can be found [here](https://topl.co/whitepaper).


Documentation
-------------
[Topl protocol technical specification ("Yellow Paper")](https://github.com/Topl/documentation/blob/master/yellowpaper/Topl%20Yellow%20Paper.pdf) (in development)

[Alpha testnet documentation](https://github.com/Topl/Project-Bifrost/wiki/Alpha-Testnet-Guide)
-->

Installation
-------------------
Check out our [Installation Instructions](https://github.com/Topl/Bifrost/wiki/Install-and-Build)


Testing
-------
1. Go to the project directory: `cd Bifrost`
1. Type: `sbt test`
   - NOTE: Using sbt to run tests using the Windows command line may error. Either use Intellij IDEA's test runner or run sbt in a Unix environment. Alternatively, if you have the Linux Subsystem for Windows enabled, you can just use bash.
1. To publish a Docker image for local testing, type: `sbt bifrost/docker:publishLocal`
   - To run the published container, type: `docker run bifrost:x.x.x` (where `x.x.x` is the version that was published).
   - To pass command line arguments, type `docker run bifrost:x.x.x -s mySpecialSeed`
1. To run integration tests:
   1. Install Docker
   1. Build, Run, and Cleanup the Integration Test via Docker:
      - On Unix systems, run: `IMAGE_ID=$(docker build -q -f it/Dockerfile .) && docker run --rm -v /var/run/docker.sock:/var/run/docker.sock $IMAGE_ID && docker image rm -f $IMAGE_ID`
      - On Windows systems, run: ``for /f "tokens=* USEBACKQ" %i in (`docker build -q -f it/Dockerfile .`) do (set IMAGE_ID=%i) && docker run --rm -v //var/run/docker.sock:/var/run/docker.sock %IMAGE_ID% && docker image rm -f %IMAGE_ID%``
      - NOTE: You may not see any output for quite a while.  The Docker image takes a long time to build, and its output is mostly silent.  You will only see output for the run of the integration test.
      - NOTE: To speed up repeated runs, remove the cleanup step (` && docker image rm -f $IMAGE_ID`) from the end of the command.
      - NOTE: When local testing through an IDE instead of through the Dockerfile, it is recommended to create a `version.sbt` at the project root with contents `ThisBuild / version := "it"`.   This file should not be checked into git, but it will help keep the published Docker tag stable.


Contributions
-------------

We appreciate your interest in this project and welcome contributions!

If you'd like to contribute to Project Bifrost, please fork, fix, commit and send a pull request so that we can review your code and merge it when it is complete. For more complex changes, please contact us via Gitter or Slack to ensure those changes are reasonable and/or get some early feedback to expedite the review and merge process.

**Please read our [Contributing Guide](https://github.com/Topl/Project-Bifrost/blob/master/CONTRIBUTING.md) for more information before submitting any pull requests, as the submission of any content to Topl's repositories constitutes your understanding of and agreement to our Contributor License Agreement and Code of Conduct.**

To keep up with our development

- View open [issues](https://github.com/Topl/Project-Bifrost/issues)!

- Join chat with us on [Zulip](https://impact.zulipchat.com/join/register/)!


License
-------
Project Bifrost is licensed under the
[Mozilla Public License 2.0 (MPL 2.0)](https://opensource.org/licenses/MPL-2.0), also included
in our repository in the `LICENSE` file.
