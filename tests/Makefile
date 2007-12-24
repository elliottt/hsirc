ODIR   = .ghc
TARGET = Tests

all : $(ODIR)
	ghc --make -odir=$(ODIR) -hidir=$(ODIR) $(TARGET)
	./$(TARGET)

$(ODIR) :
	mkdir $(ODIR)

clean :
	$(RM) -r $(ODIR) $(TARGET)

.PHONY: all clean
