## How to install Requal

To use Requal, you need to have the[ R language](https://www.r-project.org/) installed and running on your machine. You will also need a web browser with JavaScript support (i.e. any of the standard browsers like Firefox, Chrome, or Safari).
Requal can be installed in two ways - via the R console or RStudio. For inexperienced users, we recommend using RStudio.

## Windows (via RStudio)

**Step one.** Instal R. Go to _https://cran.r-project.org/bin/windows/base/_ and click on `Download R for Windows`. Download the file and run it.

![image7](https://user-images.githubusercontent.com/101122661/198823997-4768df7b-883a-438d-bb43-1208b3a16859.png)

During installation you can agree with all default options or customise the installation according to your needs. 

**Step two.** Install RStudio. Go to [www.rstudio.com](http://www.rstudio.com/) and download a free desktop version.

![image8](https://user-images.githubusercontent.com/101122661/198823998-95077cd7-4b47-40d4-8b22-b7b17dcf88fa.png)

Once downloaded, install Rstudio and launch it.

**Step three.** Install Requal. Go to _https://github.com/RE-QDA/requal/releases_
Click on the newest release (0.5.4) and then scroll down at the bottom of the table, and download the compressed package, e.g. requal_0.5.4.tar.gz

![image1](https://user-images.githubusercontent.com/101122661/198823966-ffe35417-2b91-44e8-8781-0142367a620c.png)

In the RStudio console, install the remotes package by running `install.packages("remotes") `command.
Next, install the Requal package by running `remotes::install_local(file.choose())` and selecting the downloaded release file on your hard-drive.

![image6](https://user-images.githubusercontent.com/101122661/198823995-e1e14baa-3192-4b4e-9857-7cea21cf5f53.png)

If you are asked to update packages, update All (enter 1). 

![image3](https://user-images.githubusercontent.com/101122661/198823986-24d6ac69-645e-4bd6-90f7-01d5981fd2bd.png)

The installation may take a few minutes as it is necessary to download and install several packages. After the installation, the last line reads: _* DONE (requal)_

**Step four.** Launch Requal from the RStudio console with the following command: `requal::run_app(options = list("launch.browser"))`

![image4](https://user-images.githubusercontent.com/101122661/198823989-45049fcd-1475-4581-85f8-ff1f707db457.png)

## Windows (via R console)
**Step one**. Instal R. Go to https://cran.r-project.org/bin/windows/base/ and click on _Download R for Windows_. Download the file and run it.

![image7](https://user-images.githubusercontent.com/101122661/198823997-4768df7b-883a-438d-bb43-1208b3a16859.png)

During installation you can agree with all default options or customise the installation according to your needs. When completed, launch R from the Windows menu. You will see the R console.

![image9](https://user-images.githubusercontent.com/101122661/198823999-a357ee0b-81a8-4a59-9126-1933e9acfd84.png)

**Step two.** Install Requal. Go to _https://github.com/RE-QDA/requal/releases_

Click on the newest release (0.5.4) and then scroll down at the bottom of the table, download the compressed package, e.g. requal_0.5.4.tar.gz.

![image1](https://user-images.githubusercontent.com/101122661/198823966-ffe35417-2b91-44e8-8781-0142367a620c.png)

In the R console, install the remotes package by typing the command `install.packages("remotes")` and hit Enter on your keyboard.

![image10](https://user-images.githubusercontent.com/101122661/198824000-f182cb9b-dcbb-4bce-8317-cd29ad6ade56.png)

If a dialog box appears, click on yes or customise the installation.
Install the Requal package by running `remotes::install_local(file.choose())` and selecting the downloaded release file on your hard-drive.

![image2](https://user-images.githubusercontent.com/101122661/198823973-7f42c59f-afe8-49f3-8135-325d52061c54.png)

The installation may take a few minutes as it is necessary to download and install several packages. After the installation, the last line reads: _* DONE (requal)_

**Step three.** Launch Requal from the R console with the following command: `requal::run_app(options = list("launch.browser"))`

![image5](https://user-images.githubusercontent.com/101122661/198823993-79978f5b-7269-4a50-a3d3-846c9df2f3d8.png)