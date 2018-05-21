# Installation / replication steps

Depending on your own system setup you may not need to do all of this. This assumes you are starting from nothing (not even a sytem running R, but a lowest-common-denominator cloud hosted linux instance).

If you already have some sort of system, possibly running R already, you should probably start from step 2, 5, or 7. The library installs etc will vary depending on your package manager. We may eventually set up a docker image for the project to avoid this.

1. If necessary create your VM, for example I used the default AWS EC2 image.
- allocate public IP address
- open incoming TCP port 8787 in AWS console security group

2. Install docker
- Instructions here: https://docs.aws.amazon.com/AmazonECS/latest/developerguide/docker-basics.html

3. Launch docker with R Studio (based on [this guide](https://ropenscilabs.github.io/r-docker-tutorial/02-Launching-Docker.html))

```
docker run --rm -p 8787:8787 rocker/verse
```

4. Connect to your VM instance (e.g. if on AWS, ssh in from another local terminal). Then at the remote shell, connect do Docker:

```
# Get the container-id
docker ps
# Use the reported container-id below
docker exec -it <container-id> bash
```

5. install some system packages. From with the docker shell

```
apt-get update
# for mogrify
apt-get install imagemagick php-imagick -y
# for proj4
apt-get install libproj-dev proj-data proj-bin -y
# for rgdal
apt-get install libgdal-dev python-gdal gdal-bin -y
# for pdftools
apt-get install libpoppler-cpp-dev -y
```

6. Connect to RStudio at your VM's address port 8787. Login as 'rstudio' pass 'rstudio'.

7. Install some standard packages the base docker image lacks. In RStudio console

```
install.packages(c("countrycode","ggalluvial","ggmosaic","pdftools","png","proj4"  ,"readstata13","rgdal", "OECD"))
```

8. Clone the repo. In RStudio terminal

```
git clone https://github.com/worldbank/sdgatlas2018.git
```

8. Install some development packages we created. In RStudio console:

```
devtools::install_github("worldbank/wbgviz", subdir = "wbgdata")
devtools::install_github("worldbank/wbgviz", subdir = "wbgcharts")
devtools::install_github("worldbank/wbgviz", subdir = "wbgmaps")
devtools::install_github("worldbank/wbgviz", subdir = "wbggeo")

9. As of now `ggtreemap` doesn't work with the latest treemapify, so in RStudio console:

```
devtools::install_version("treemapify", version = "2.4.0")
devtools::install_github("econandrew/ggtreemap")
```

9. If you want to use the nearest open fonts
- In RStudio console

```
extrafont::font_import(system.file("fonts", package = "wbgcharts"))
```

- You will also need to install these fonts at system level (OS dependent)

10. Update the cached API data so everything runs smoothly. In RStudio console

```
wbgdata::refresh_wbcache()
```

11. Open the sdgatlas2018 project in RStudio

12. In RStudio console, the following we remake all figures from scratch.

```
source("make.R")
make_all("docs", styler = style_atlas_open)
```