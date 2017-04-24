CASK  ?= cask
WGET  ?= wget
EMACS  = emacs

EMACSFLAGS =
EMACSBATCH = $(EMACS) -L . --batch -Q $(EMACSFLAGS)

export EMACS

PKGDIR    := $(shell EMACS=$(EMACS) $(CASK) package-directory)
TARGETDIR  = dist

SRCS := $(shell EMACS=$(EMACS) $(CASK) files)
OBJS  = $(SRCS:.el=.elc)

.PHONY: all compile clean package

all: compile

compile: $(OBJS)

clean:
	$(CASK) clean-elc
	$(RM) -r $(TARGETDIR)

package:
	$(CASK) package $(TARGETDIR)

%.elc: %.el $(PKGDIR)
	$(CASK) exec $(EMACSBATCH) -f batch-byte-compile $<

$(PKGDIR): Cask
	$(CASK) install
	touch $(PKGDIR)
