# scda.test

Internal repository to store tests that utilize datasets from the `scda` package.

Repository tests sample templates created using `tern` analysis functions taken from the TLG Catalog to verify functionality and correct output.

Template tests were migrated here from the `tern` package in February 2023.

## Installation

### Installing Package Dependencies

The `scda.test` package has several dependencies required to run the tests within this package. Before installing the `scda.test` repository, please ensure you have the latest versions of the `tern`, `scda`, and `scda.2022` packages installed. Installation instructions are provided for each at these links:

-   [`tern`](https://github.com/insightsengineering/tern#installation)
-   [`scda`](https://github.com/insightsengineering/scda#installation)
-   [`scda.2022`](https://github.com/insightsengineering/scda.2022#installation)

### Installing scda.test

This repository is internal and must be cloned and installed manually from GitHub.

1. **Clone the repository**

   The repository can be downloaded directly from the `github.com` site as an archive (see the [GitHub tutorial on cloning to learn more](https://docs.github.com/en/github/creating-cloning-and-archiving-repositories/cloning-a-repository-from-github/cloning-a-repository)). 
   Alternatively, Git command line tools offer the same functionality, without the need for manual downloading and unpacking the archive, but require you to authenticate GitHub. You can authenticate using a key pair or a Personal Access Token (PAT). 
   Please refer to GitHub tutorials on [connecting to GitHub using SSH](https://docs.github.com/en/github/authenticating-to-github) or [creating and using a PAT](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token).
   
   -   **Using a PAT**: Input in the Git Bash console, PowerShell, or any Linux shell:
   
       ```bash
       $ git clone https://github.com/insightsengineering/scda.test.git
       Username: your_username_goes_here
       Password: your_token_goes_here
       ```
   
   -   **Using SSH**: If set up properly, the repository is ready to be cloned by executing:
   
       ```bash
       git clone https://github.com/insightsengineering/scda.test.git
       ```

       This creates a subdirectory `scda.test` containing the cloned repository.
   
2. **Build and install**

      Native R tools provide a quick way to install a package. Alternatively, to build the package and create an archive run in PowerShell or any Linux shell:

      ```bash
      R CMD build scda.test
      ```
