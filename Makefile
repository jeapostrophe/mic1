DIRS := masm mcc mic1

all: tar build

build:
	$(foreach dir,$(DIRS),cd $(dir) && $(MAKE); cd ..;)

clean:
	$(foreach dir,$(DIRS),cd $(dir) && $(MAKE) clean; cd ..;)

tar: clean
	$(foreach dir,$(DIRS),tar czvf $(dir).tar.gz $(dir);)
