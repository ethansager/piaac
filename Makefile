QUARTO ?= quarto
PAPER_QMD ?= Quarto/paper.qmd
RESPONSE_QMD ?= Quarto/response-memo.qmd

.PHONY: paper clean-paper check-quarto check-hikmah ensure-hikmah

## Render the main paper (installs hikmah extension if needed)
paper: ensure-hikmah
	$(QUARTO) render $(PAPER_QMD)

## Remove rendered paper artifacts
clean-paper:
	rm -rf docs/papers/

## Check that quarto is installed
check-quarto:
	@$(QUARTO) --version > /dev/null 2>&1 || \
	  (echo "Error: quarto not found. Install from https://quarto.org/docs/get-started/" && exit 1)

## Check that hikmah extension is installed
check-hikmah:
	@test -d Quarto/_extensions/hikmah-academic-quarto || \
	  (echo "hikmah extension not installed — run: make ensure-hikmah" && exit 1)

## Install hikmah extension if not present
ensure-hikmah: check-quarto
	@if [ ! -d Quarto/_extensions/hikmah-academic-quarto ]; then \
	  echo "Installing hikmah-academic-quarto extension..."; \
	  cd Quarto && $(QUARTO) add andrewheiss/hikmah-academic-quarto --no-prompt; \
	fi
