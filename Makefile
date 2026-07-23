DOOM_PATH = ~/.config/emacs/bin/doom

.phony: clean sync veryclean

sync:
	$(DOOM_PATH) sync

update:
	$(DOOM_PATH) sync -u

clean:
	rm -f \#*\# *.el~
