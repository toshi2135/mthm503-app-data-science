# mthm503

This is a template for course MTHM503 "Applications of Data Science and Statistics".  You should fork this repository and use it for your work on this module

Note: in a professional setting, a DevOps engineer (or team of engineers) would be responsible for setting up infrastructure like this. You only have to operate within the infrastructure. 

## Setting up for the first time

I have tested the set up on a University Virtual Machine, a University Laptop, as well as my own Debian machines. It does work, but occasionally Windows can make things difficult.   If you find that sourcing `init.R` doesn't leave you with a working set up, here are some things you can try:

1. Make sure you are using R version 4.4.0. This is the version on the University machines and hence is the version I have tested against.  If you install R-4.4.0, when you open RStudio you can select Tools -> Global Options and select the [Change] button and navigate to wherever you installed version 4.4.0
2. You can set `options(pkgType = "binary")` by typing in the R console. This should be set for you in recent versions of the `init.R` script
3. You could try installing packages from different repos, by typing something like `options(repos = "https://cloud.r-project.org/")`
4. You also could go back to the steps where you set up the project and make sure you install to a local file that isn't under the control of OneDrive.
5. When sourcing, use the drop down button to the right of [Source] and select "Source as Background Job" (if this fixes anything it is really only helping because you are working in some OneDrive regulated space)

Hopefully, you can get yourself into a workable state, and entering `renv::restore()` happily tells you everything has been installed.

## Developing reproducible pipelines

Therefore, you may find yourself working as follows:

1. Develop a script to perform some analysis task.
2. When you are happy with it, wrap it in a function and save it in the `functions.R` file.
3. You call this function from the `_targets.R` file in the form 
  `tar_target(NAME OF THE OUTPUT, FUNCTION CALL(DATA AND OTHER ARGUMENTS))`
4. Run `targets::tar_make()` in the parent folder. 
5. You might consider adding a unit test for the function you wrote. It only needs to test for example that a data summary function returns a data frame to be helpful in understanding where you might have errors.

## The run file

The run file can be executed from the R Terminal using `./run` although sometimes windows might make you type `bash run` instead.  It is very similar to the script on GitHub actions. Locally, it will lint your files. Linting means performing a static code analysis. Occasionally this finds important errors in your code, but usually it just worries minor code formatting issues. Sometimes in the R ecosystem it's hard to keep it happy and you can type `# nolint` at the end of a line and the linter will ignore that line.   It also runs any unit tests; note that instead of loading data from a database server, there is an sqlite database with a few rows of mock data which is intended to allow some functions to be tested.  Again, a little unit testing can be a useful way of making sure your code does what you think it does.

## Formative feedback

To submit your work for formative feedback, you'll use a Pull Request (PR). This allows you to share your code and analysis while giving instructors visibility for feedback and review.

### Step-by-Step Guide

- You should have forked this repository. Click the "Fork" button at the top right of the page. This creates your own copy of the teaching repo.
- Work in your fork Make your changes (code, report, etc.) in your own forked repo. You can create branches if you’d like, but it’s not required.
- Create a Pull Request against this repository Once you’re ready:

  1. Go to your forked repo.
  2. Click the “Pull Request” tab → “New Pull Request”.
  3. Select your branch on the left, and the main branch of this repo
  4. Pick the appropriate template from the dropdown if available (e.g. classification, regression, etc.).
  5. Use the checklist and prompts to reflect on your work. This helps us give feedback and ensures key components are included.
  6. Submit the PR Once submitted, your instructors will be notified.

### Notes

- Don’t create a PR within your own repo—PRs must be opened against this repository for instructors to see them.

- You can update your PR later by pushing additional commits to your branch.

- If you’re unsure which template to use, ask or consult the assignment brief.

```
        +--------------------+
        | 1. Fork the Repo   |  ←-- Student clicks "Fork"
        +--------------------+
                  |
                  v
        +--------------------+
        | 2. Do the Work     |  ←-- Edit files, commit code, write report
        +--------------------+
                  |
                  v
        +----------------------------+
        | 3. Open Pull Request       |
        |    - Against original repo |
        |    - Choose correct PR     |
        |      template              |
        | (classification, etc.)     |
        +----------------------------+
                  |
                  v
        +--------------------+
        | 4. Instructors See |
        |    and Review PR   |  ←-- Feedback, comments!
        +--------------------+

```
