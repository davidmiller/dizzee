VERSION=$(shell grep -P -o '(?<=Version: )[0-9.]+' dizzee.el)
RELEASEDIR=dizzee-$(VERSION)
BUILDDIR=build/$(RELEASEDIR)

release: clean package

all:
	@echo "What do you want from me?"

tag:
	$(shell git tag $(VERSION))

clean:
	-@ rm -rf build

package:
	-@ mkdir -p $(BUILDDIR)
	-@ cp dizzee.el $(BUILDDIR)
	-@ cp dizzee-pkg.el $(BUILDDIR)
	-@ cp README.rst $(BUILDDIR)/README
	-@ cd build && tar -cf dizzee-$(VERSION).tar $(RELEASEDIR)