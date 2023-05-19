CONFIG_ORG = doom-emacs-config.org
DOOM_PATH = ~/.config/emacs/bin/doom
TANGLED_FILES = init.el config.el packages.el custom.el lisp
BUILD_STAMP = .build_stamp.txt

.phony: tangle clean sync veryclean

rebuild: $(BUILD_STAMP)

$(BUILD_STAMP): $(CONFIG_ORG)
	$(MAKE) tangle
	@echo "Update tangle stamp"
	@echo "Doom build $(shell date)" > $(BUILD_STAMP)

tangle: $(CONFIG_ORG)
	emacs -Q -batch --visit=$^ -f org-babel-tangle

sync: rebuild
	$(DOOM_PATH) sync

clean:
	rm -f \#*\# *.el~

veryclean:
	rm -rf $(TANGLED_FILES) $(BUILD_STAMP)
