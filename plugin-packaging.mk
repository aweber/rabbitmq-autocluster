# --------------------------------------------------------------------
# Distribution - copy-paste from rabbitmq-server-release
# --------------------------------------------------------------------

# Release artifacts are put in $(PACKAGES_DIR).
PACKAGES_DIR ?= $(abspath PACKAGES)

.PHONY: source-dist clean-source-dist

SOURCE_DIST_BASE ?= autocluster
SOURCE_DIST_SUFFIXES ?= tar.xz zip
SOURCE_DIST ?= $(PACKAGES_DIR)/$(SOURCE_DIST_BASE)-$(VERSION)

# The first source distribution file is used by packages: if the archive
# type changes, you must update all packages' Makefile.
SOURCE_DIST_FILES = $(addprefix $(SOURCE_DIST).,$(SOURCE_DIST_SUFFIXES))

.PHONY: $(SOURCE_DIST_FILES)

source-dist: $(SOURCE_DIST_FILES)
	@:

RSYNC ?= rsync
RSYNC_V_0 =
RSYNC_V_1 = -v
RSYNC_V_2 = -v
RSYNC_V = $(RSYNC_V_$(V))
RSYNC_FLAGS += -a $(RSYNC_V)		\
	       --exclude '.sw?' --exclude '.*.sw?'	\
	       --exclude '*.beam'			\
	       --exclude '*.d'				\
	       --exclude '*.pyc'			\
	       --exclude '.git*'			\
	       --exclude '.hg*'				\
	       --exclude '.travis.yml'			\
	       --exclude '.*.plt'			\
	       --exclude '$(notdir $(ERLANG_MK_TMP))'	\
	       --exclude 'ebin'				\
	       --exclude 'packaging'			\
	       --exclude 'erl_crash.dump'		\
	       --exclude 'MnesiaCore.*'			\
	       --exclude 'cover/'			\
	       --exclude 'deps/'			\
	       --exclude 'ebin/'			\
	       --exclude '$(notdir $(DEPS_DIR))/'	\
	       --exclude 'logs/'			\
	       --exclude 'plugins/'			\
	       --exclude '$(notdir $(DIST_DIR))/'	\
	       --exclude 'test'				\
	       --exclude 'xrefr'			\
	       --exclude '/$(notdir $(PACKAGES_DIR))/'	\
	       --exclude '/PACKAGES/'			\
	       --exclude '/cowboy/doc/'			\
	       --exclude '/cowboy/examples/'		\
	       --exclude '/rabbitmq_amqp1_0/test/swiftmq/build/'\
	       --exclude '/rabbitmq_amqp1_0/test/swiftmq/swiftmq*'\
	       --exclude '/rabbitmq_mqtt/test/build/'	\
	       --exclude '/rabbitmq_mqtt/test/test_client/'\
	       --exclude '/erlang-*-nonroot.tar.*' \
	       --delete					\
	       --delete-excluded

TAR ?= tar
TAR_V_0 =
TAR_V_1 = -v
TAR_V_2 = -v
TAR_V = $(TAR_V_$(V))

GZIP ?= gzip
BZIP2 ?= bzip2
XZ ?= xz

ZIP ?= zip
ZIP_V_0 = -q
ZIP_V_1 =
ZIP_V_2 =
ZIP_V = $(ZIP_V_$(V))

.PHONY: $(SOURCE_DIST)
.PHONY: clean-source-dist distclean-packages clean-unpacked-source-dist

$(SOURCE_DIST): $(ERLANG_MK_RECURSIVE_DEPS_LIST)
	$(verbose) mkdir -p $(dir $@)
	$(gen_verbose) $(RSYNC) $(RSYNC_FLAGS) ./ $@/
	$(verbose) echo "$(PROJECT) $$(git rev-parse HEAD) $$(git describe --tags --exact-match 2>/dev/null || git symbolic-ref -q --short HEAD)" > $@/git-revisions.txt
	$(verbose) mkdir -p $@/deps/licensing
	$(verbose) for dep in $$(cat $(ERLANG_MK_RECURSIVE_DEPS_LIST) | LC_COLLATE=C sort); do \
		$(RSYNC) $(RSYNC_FLAGS) \
		 $$dep \
		 $@/deps; \
		if test -f $@/deps/$$(basename $$dep)/erlang.mk && \
		   test "$$(wc -l $@/deps/$$(basename $$dep)/erlang.mk | awk '{print $$1;}')" = "1" && \
		   grep -qs -E "^[[:blank:]]*include[[:blank:]]+(erlang\.mk|.*/erlang\.mk)$$" $@/deps/$$(basename $$dep)/erlang.mk; then \
			echo "include ../../erlang.mk" > $@/deps/$$(basename $$dep)/erlang.mk; \
		fi; \
		sed -E -i.bak "s|^[[:blank:]]*include[[:blank:]]+\.\./.*erlang.mk$$|include ../../erlang.mk|" \
		 $@/deps/$$(basename $$dep)/Makefile && \
		rm $@/deps/$$(basename $$dep)/Makefile.bak; \
		if test -f "$$dep/license_info"; then \
			cp "$$dep/license_info" "$@/deps/licensing/license_info_$$(basename "$$dep")"; \
			cat "$$dep/license_info" >> $@/LICENSE; \
		fi; \
		find "$$dep" -maxdepth 1 -name 'LICENSE-*' -exec cp '{}' $@/deps/licensing \; ; \
		(cd $$dep; echo "$$(basename "$$dep") $$(git rev-parse HEAD) $$(git describe --tags --exact-match 2>/dev/null || git symbolic-ref -q --short HEAD)") >> $@/git-revisions.txt; \
	done
	$(verbose) for file in $$(find $@ -name '*.app.src'); do \
		sed -E -i.bak -e 's/[{]vsn[[:blank:]]*,[[:blank:]]*(""|"0.0.0")[[:blank:]]*}/{vsn, "$(VERSION)"}/' $$file; \
		rm $$file.bak; \
	done

# TODO: Fix file timestamps to have reproducible source archives.
# $(verbose) find $@ -not -name 'git-revisions.txt' -print0 | xargs -0 touch -r $@/git-revisions.txt

$(SOURCE_DIST).tar.gz: $(SOURCE_DIST)
	$(gen_verbose) cd $(dir $(SOURCE_DIST)) && \
		find $(notdir $(SOURCE_DIST)) -print0 | LC_COLLATE=C sort -z | \
		xargs -0 $(TAR) $(TAR_V) --no-recursion -cf - | \
		$(GZIP) --best > $@

$(SOURCE_DIST).tar.bz2: $(SOURCE_DIST)
	$(gen_verbose) cd $(dir $(SOURCE_DIST)) && \
		find $(notdir $(SOURCE_DIST)) -print0 | LC_COLLATE=C sort -z | \
		xargs -0 $(TAR) $(TAR_V) --no-recursion -cf - | \
		$(BZIP2) > $@

$(SOURCE_DIST).tar.xz: $(SOURCE_DIST)
	$(gen_verbose) cd $(dir $(SOURCE_DIST)) && \
		find $(notdir $(SOURCE_DIST)) -print0 | LC_COLLATE=C sort -z | \
		xargs -0 $(TAR) $(TAR_V) --no-recursion -cf - | \
		$(XZ) > $@

$(SOURCE_DIST).zip: $(SOURCE_DIST)
	$(verbose) rm -f $@
	$(gen_verbose) cd $(dir $(SOURCE_DIST)) && \
		find $(notdir $(SOURCE_DIST)) -print0 | LC_COLLATE=C sort -z | \
		xargs -0 $(ZIP) $(ZIP_V) $@

clean:: clean-source-dist

clean-source-dist:
	$(gen_verbose) rm -rf -- $(SOURCE_DIST_BASE)-*

distclean:: distclean-packages

distclean-packages:
	$(gen_verbose) rm -rf -- $(PACKAGES_DIR)

clean-unpacked-source-dist:
	for d in deps/*; do \
		if test -f $$d/Makefile; then \
			make -C $$d clean || exit $$?; \
		fi; \
	done

# --------------------------------------------------------------------
# Packaging - copy-paste from rabbitmq-server-release
# --------------------------------------------------------------------
.PHONY: packages package-deb package-unix-generic

packages: package-deb package-unix-generic

PACKAGES_SOURCE_DIST_FILE ?= $(firstword $(SOURCE_DIST_FILES))
DEBIAN_ORIG_TARBALL = rabbitmq-autocluster_$(VERSION).orig.tar.xz
UNPACKED_DIR = rabbitmq-autocluster-$(VERSION)
PACKAGENAME = rabbitmq-autocluster

DEB_SIGN_STYLE=$(if $(shell grep 'Precise' /etc/os-release), \
		    -sgpg)

DEB_SIGNING=$(if $(SIGN_KEY), \
		 -p$(shell pwd)/packaging/sign.sh -k$(SIGN_KEY) $(DEB_SIGN_STYLE), \
		 -us -uc)

package-deb: $(PACKAGES_SOURCE_DIST_FILE)
	mkdir -p PACKAGES/deb/
	rm -rf PACKAGES/deb/$(UNPACKED_DIR)
	mkdir -p PACKAGES/deb/$(UNPACKED_DIR)
	ls -la $(PACKAGES_SOURCE_DIST_FILE)
	cp -a $(PACKAGES_SOURCE_DIST_FILE) PACKAGES/deb/$(DEBIAN_ORIG_TARBALL)
	tar -C PACKAGES/deb/$(UNPACKED_DIR) --strip-components=1 -xJf PACKAGES/deb/$(DEBIAN_ORIG_TARBALL)
	cp -a packaging/deb/debian PACKAGES/deb/$(UNPACKED_DIR)
	cd PACKAGES/deb/$(UNPACKED_DIR) && dpkg-buildpackage $(DEB_SIGNING)

package-unix-generic: dist
	mkdir -p PACKAGES
	tar cvfz PACKAGES/autocluster-${TRAVIS_TAG}.tgz plugins/autocluster*.ez plugins/rabbitmq_aws-*.ez
