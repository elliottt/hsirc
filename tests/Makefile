ODIR   = .ghc
TARGET = Tests

all : $(ODIR)
	ghc --make -odir=$(ODIR) -hidir=$(ODIR) $(TARGET) -i../
	./$(TARGET)

$(ODIR) :
	mkdir $(ODIR)

clean :
	$(RM) -r $(ODIR) $(TARGET)

.PHONY: all clean
