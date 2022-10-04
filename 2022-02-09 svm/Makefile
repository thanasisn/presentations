## Build a single Rmd file

## targets
TARGET = presentation

RMD    = $(TARGET).Rmd
PDF    = $(TARGET).pdf
SLIDY  = $(TARGET).html

.DEFAULT_GOAL := pdf

all: pdf slidy

## will build default output from yaml
## using  rmarkdown::beamer_presentation output doesnt work well may need yaml file
pdf: $(PDF)
$(PDF): $(RMD)
	Rscript -e "rmarkdown::render( '$?' )"
	@echo "Building: $@"
	@echo "Changed:  $?"
	setsid evince    $@ &

## not working perfect
slidy: $(SLIDY)
$(SLIDY): $(RMD)
	Rscript -e "bookdown::render_book( '$?' , 'rmarkdown::slidy_presentation')"
	@echo "Building: $@"
	@echo "Changed:  $?"
	# setsid mimeopen  $@ &



