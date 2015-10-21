DIRS := masm mcc mic1 examples

all: tar build

build:
	$(foreach dir,$(DIRS),cd $(dir) && $(MAKE); cd ..;)

clean:
	$(foreach dir,$(DIRS),cd $(dir) && $(MAKE) clean; cd ..;)

tar: clean
	$(foreach dir,$(DIRS),tar czvf $(dir).tar.gz $(dir);)

test:
	cd mcc && $(MAKE)
	cd examples && $(MAKE) prom.dat
	diff -u examples/prom.dat examples/prom.dat.expected 
