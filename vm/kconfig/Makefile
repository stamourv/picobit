CC := gcc
LDFLAGS := -s
CFLAGS := -g -O2 --std=gnu99 -Wall 

all: conf nconf

conf: conf.o zconf.tab.o
	$(CC) $(LDFLAGS) -o $@ $^ $(LIBS)

nconf: nconf.o nconf.gui.o zconf.tab.o
	$(CC) $(LDFLAGS) -o $@ $^ $(LIBS) -lncurses -lmenu -lpanel

zconf.tab.o: zconf.tab.c zconf.hash.c lex.zconf.c

ifeq (${GENERATE_PARSER}, 1)

%.tab.c: %.y
	bison -l -b $* -p $(notdir $*) $<

lex.%.c: %.l
	flex -L -P$(notdir $*) -o$@ $<

%.hash.c: %.gperf
	gperf < $< > $@
else

%.c: %.c_shipped
	cp $< $@

endif

clean:
	rm -f zconf.tab.c zconf.hash.c lex.zconf.c zconf.tab.o conf.o conf nconf nconf.o nconf.gui.o

