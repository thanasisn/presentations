
## target files
TARGET := presentation
SOURCE := $(TARGET).qmd

ASSETS := styles.css Makefile

PDF    := $(TARGET).pdf
BEAMER := $(TARGET)_beamer.pdf
HTML   := $(TARGET).html
PPT    := $(TARGET).pptx
SLIDY  := $(TARGET)_slidy.html

## full path of makefile
mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))

## just the current folder
current_dir := $(notdir $(patsubst %/,%,$(dir $(mkfile_path))))

.DEFAULT_GOAL := html
.PHONY: all clean html pdf beamer ppt images slidy

clean: rm all
all:   html pdf

## revealjs
html: $(HTML)
$(HTML):  $(SOURCE) $(ASSETS)
	@- cd ./images; ./convert_pdf_to_svg.sh
	- quarto render $(SOURCE) --to revealjs
	@ echo "Build:   $@"
	@ echo "Changed: $?"
	#@ cp -r -v -u -p "./." ${COPYTARGET}

slidy: $(SLIDY)
$(SLIDY):  $(SOURCE) $(ASSETS)
	@- cd ./images; ./convert_pdf_to_svg.sh
	- quarto render $(SOURCE) --to slidy
	@ echo "Build:   $@"
	@ echo "Changed: $?"
	#@ cp -r -v -u -p "./." ${COPYTARGET}


## beamer
beamer: $(BEAMER)
$(BEAMER): $(SOURCE) $(ASSETS)
	- quarto render $(SOURCE) --to beamer --pdf-engine=xelatex
	@ echo "Build:   $@"
	@ echo "Changed: $?"

## pdf
pdf: $(PDF)
$(PDF): Makefile $(SOURCE)
	- quarto render $(SOURCE) --to pdf --pdf-engine=xelatex
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

watch:
	quarto preview $(SOURCE) --to revealjs --no-browser

