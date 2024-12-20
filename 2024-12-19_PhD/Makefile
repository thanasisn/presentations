## find all sources
# SRC    = $(wildcard *.Rmd)
# SRC    = $(shell find -iname "*.Rmd" | sort -V -f)
# SRC      = $(shell find -maxdepth 1 -type f \( -iname "*.Rmd" -o -iname "*.yml" \) | sort -V -f) 
# SOURCES  = Makefile $(wildcard *.Rmd) $(wildcard *.yml)

## target files
TARGET := Solar_radiation_aerosols_clouds
SOURCE := $(TARGET).qmd
PDF    := $(TARGET).pdf
HTML   := $(TARGET).html
PPT    := $(TARGET).pptx
## fullpath of makefile
mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))
## just the current folder
current_dir := $(notdir $(patsubst %/,%,$(dir $(mkfile_path))))
## export location
COPYTARGET  := ~/LIBRARY/REPORTS/presentation

.DEFAULT_GOAL := html

clean: rm all
all:   html pdf ppt

## revealjs
html: $(HTML)
$(HTML): Makefile styles.css $(SOURCE)
	@- cd ./images; ./convert_pdf_to_svg.sh
	- quarto render $(SOURCE) --to revealjs
	@ echo "Build:   $@"
	@ echo "Changed: $?"
	#@ cp -r -v -u -p "./." ${COPYTARGET}
	#@ cp -r -v -u -p "./." /home/athan/ZHOST/natasha_share/presentation

## beamer
pdf: $(PDF)
$(PDF): Makefile $(SOURCE)
	- quarto render $(SOURCE) --to beamer
	@ echo "Build:   $@"
	@ echo "Changed: $?"

## draft ppt
ppt: $(PPT)
$(PPT): Makefile $(SOURCE)
	- quarto render $(SOURCE) --to pptx
	@ echo "Build:   $@"
	@ echo "Changed: $?"

force_img:
	@- cd ./images; ./convert_pdf_to_svg.sh force

