# Makefile for R Markdown Presentations
# Usage: make [target] RMD_FILE=your_file.Rmd

# Configuration with default values
RMD_FILE ?= ./presentation.Rmd
BASE_NAME = $(basename $(RMD_FILE))
OUTPUT_DIR = presentation
FIG_DIR   = figures
DATA_DIR = data
IMAGES_DIR = images

# # Validate RMD file exists
# ifeq (,$(wildcard $(RMD_FILE)))
# $(error RMD file '$(RMD_FILE)' not found. Available Rmd files: $(wildcard *.Rmd))
# endif

# Phony targets
.PHONY: all clean help slides pdf powerpoint slidy beamer watch serve install list

# Default target - create HTML slides
all: dirs slides

# List available Rmd files
list:
	@echo "Available R Markdown files:"
	@for file in *.Rmd; do \
		echo "  - $$file"; \
	done

# Create necessary directories
dirs:
	mkdir -p $(OUTPUT_DIR)
	mkdir -p $(FIG_DIR)
	mkdir -p $(DATA_DIR)
	mkdir -p $(IMAGES_DIR)

# Create HTML ioslides presentation
slides: dirs
	@echo "Building HTML slides from $(RMD_FILE)"
	-Rscript -e "rmarkdown::render('$(RMD_FILE)', output_format = 'ioslides_presentation', output_dir = '$(OUTPUT_DIR)', output_file = '$(BASE_NAME)')"
	@echo "HTML presentation created: $(OUTPUT_DIR)/$(BASE_NAME).html"

# Create PDF presentation (Beamer)
pdf: presentation_beamer.pdf
presentation_beamer.pdf : presentation_beamer.Rmd
	@echo "Building PDF from presentation_beamer.Rmd"
	-Rscript -e "rmarkdown::render('presentation_beamer.Rmd', output_format = 'beamer_presentation', output_file = 'presentation_beamer.pdf')"
	@echo "PDF presentation created: presentation_beamer.pdf"

# Create PowerPoint presentation
ppt: presentation_ppt.ppt
presentation_ppt.ppt : presentation_ppt.Rmd
	@echo "Building PowerPoint from presentation_ppt.Rmd"
	-Rscript -e "rmarkdown::render('presentation_ppt.Rmd', output_format = 'powerpoint_presentation', output_file = 'presentation_ppt.pptx')"
	@echo "PowerPoint presentation created: presentation_ppt.pptx"

# Create Slidy HTML presentation
slidy: dirs
	@echo "Building Slidy HTML from $(RMD_FILE)"
	-Rscript -e "rmarkdown::render('$(RMD_FILE)', output_format = 'slidy_presentation', output_dir = '$(OUTPUT_DIR)', output_file = '$(BASE_NAME)')"
	@echo "Slidy HTML presentation created: $(OUTPUT_DIR)/$(BASE_NAME).html"

# Alias for PDF
beamer: pdf

# Create all formats for current RMD file
all-formats: slides pdf powerpoint
	@echo "All formats created from $(RMD_FILE) in $(OUTPUT_DIR)/"

# Watch for changes and auto-rebuild (requires fswatch)
watch:
	@echo "Watching for changes in $(RMD_FILE) and supporting files..."
	@echo "Press Ctrl+C to stop watching"
	fswatch -o $(RMD_FILE) *.css $(DATA_DIR)/*.csv $(IMAGES_DIR)/*.png | while read; do \
		echo "Change detected in $(RMD_FILE), rebuilding HTML..."; \
		make slides RMD_FILE=$(RMD_FILE); \
	done

# Serve HTML presentation locally
serve: slides
	@echo "Serving $(BASE_NAME).html at http://localhost:8000"
	@echo "Press Ctrl+C to stop the server"
	cd $(OUTPUT_DIR) && python3 -m http.server 8000

# Install required R packages
install:
	@echo "Installing required R packages..."
	Rscript -e "install.packages(c('rmarkdown', 'knitr', 'ggplot2', 'dplyr', 'tinytex'))"
	Rscript -e "if(!requireNamespace('tinytex', quietly = TRUE)) tinytex::install_tinytex()"
	@echo "Required packages installed"

# Clean up generated files for current RMD
clean:
	@echo "Cleaning up files for $(RMD_FILE)"
	rm -f $(OUTPUT_DIR)/$(BASE_NAME).html
	rm -f $(OUTPUT_DIR)/$(BASE_NAME).pdf
	rm -f $(OUTPUT_DIR)/$(BASE_NAME).pptx
	rm -rf $(OUTPUT_DIR)/$(BASE_NAME)_files/
	rm -rf $(FIG_DIR)/

# Clean all generated files (all presentations)
clean-all:
	@echo " Cleaning all generated files"
	rm -rf $(OUTPUT_DIR)/
	rm -rf $(FIG_DIR)/

# Show help message
help:
	@echo "R Markdown Presentation Build System"
	@echo "===================================="
	@echo ""
	@echo "Usage: make [target] RMD_FILE=filename.Rmd"
	@echo ""
	@echo "Targets:"
	@echo "  make all           - Build HTML slides (default: presentation.Rmd)"
	@echo "  make slides        - Build HTML presentation"
	@echo "  make pdf           - Build PDF presentation"
	@echo "  make powerpoint    - Build PowerPoint presentation"
	@echo "  make slidy         - Build Slidy HTML presentation"
	@echo "  make all-formats   - Build all formats for specified RMD file"
	@echo "  make watch         - Watch for changes and auto-rebuild"
	@echo "  make serve         - Serve HTML presentation locally"
	@echo "  make list          - List available Rmd files"
	@echo "  make install       - Install required packages"
	@echo "  make clean         - Clean files for current RMD file"
	@echo "  make clean-all     - Clean all generated files"
	@echo "  make help          - Show this help message"
	@echo ""
	@echo "Examples:"
	@echo "  make RMD_FILE=my_talk.Rmd slides"
	@echo "  make RMD_FILE=analysis.Rmd pdf"
	@echo "  make RMD_FILE=report.Rmd all-formats"
	@echo "  make list"
