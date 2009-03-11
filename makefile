PROG	= WBRSS
ARCH	= m68k-amigaos
SYS	= _020
CPU	= -m68020-60
CC	= $(ARCH)-gcc $(CPU) -noixemul #-msmall-code
LIB	= -Wl,-Map,$@.map,--cref -lamiga -ldebug -lxml2-minimal
DEFINES	= -DSTACK_SIZE=48000 -DNDEBUG -DUSESTDLIB
WARNS	= -W -Wall #-Winline
CFLAGS	= -O3 -fomit-frame-pointer -funroll-loops $(WARNS) $(DEFINES)
LDFLAGS	= -s #-nostdlib
OBJDIR	= .objs$(SYS)
RM	= rm -frv

OBJS =	\
	$(OBJDIR)/startup.o	\
	$(OBJDIR)/main.o	\
	$(OBJDIR)/ntptime.o	\
	$(OBJDIR)/debug.o	\
	$(OBJDIR)/xmlsupport.o	\
	$(OBJDIR)/html.o

all: $(PROG)

$(PROG): $(OBJDIR) $(OBJS)
	$(CC) -o $@ $(LDFLAGS) $(OBJS) $(LIB)

$(OBJDIR):
	mkdir -p $(OBJDIR)

$(OBJDIR)/%.o: %.c
	@echo Compiling $@
	@$(CC) $(CFLAGS) -c $< -o $@

$(OBJDIR)/startup.o: startup.c
	@echo Compiling $@
	@$(CC) $(CFLAGS) -fwritable-strings -c $< -o $@

clean:
	$(RM) $(PROG)_0*0 $(OBJDIR)_0*0

