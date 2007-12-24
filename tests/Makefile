ODIR = .ghc

all : $(ODIR)
	ghc --make -odir=$(ODIR) -hidir=$(ODIR) QC
	./QC

$(ODIR) :
	mkdir $(ODIR)

clean :
	$(RM) -r $(ODIR) QC

.PHONY: all clean
