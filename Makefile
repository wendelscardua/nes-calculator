PROJECT=calculator
LD65_FLAGS=
CA65_FLAGS=
NSF2DATA=/mnt/c/NESDev/famitone5.0/nsf2data/nsf2data5.exe
TEXT2DATA=/mnt/c/NESDev/famitone5.0/text2data/text2vol5.exe
FAMITRACKER=/mnt/c/NESDev/famitracker/FamiTracker.exe
EMULATOR=/mnt/c/NESDev/Mesen.exe
VERSION := $(shell git describe --exact-match --tags 2> /dev/null || git rev-parse --short HEAD)

TARGET=${PROJECT}.nes

.PHONY: debug run usage release FORCE

default: ${TARGET}

${TARGET}: src/header.o src/main.o src/reset.o src/irq.o src/nmi.o \
	src/temps.o \
	src/readjoy.o src/famitone5.o \
	src/rand.o src/unrle.o src/vram-buffer.o \
	src/graphics.o \
	src/audio-data.o \
	src/calculator.o \
	src/math.o \
	assets/metasprites.o
	ld65 $^ -t nes -m map.txt -o ${TARGET} ${LD65_FLAGS}

debug: LD65_FLAGS += --dbgfile ${PROJECT}.dbg
debug: CA65_FLAGS += -g -DDEBUG=1
debug: ${TARGET}

src/graphics.o: src/graphics.s src/*.inc \
	assets/*.pal \
	assets/chr/*.chr \
	assets/nametables/*.rle
	ca65 $< ${CA65_FLAGS}

src/audio-data.o: src/audio-data.s audio/sfx.s audio/soundtrack.s
	ca65 src/audio-data.s ${CA65_FLAGS}

audio/soundtrack.s: audio/soundtrack.txt
	${TEXT2DATA} $^ -ca65 -allin

audio/soundtrack.txt: audio/soundtrack.ftm
	${FAMITRACKER} $^ -export $@

audio/sfx.nsf: audio/sfx.ftm
	${FAMITRACKER} audio/sfx.ftm -export audio/sfx.nsf

audio/sfx.s: audio/sfx.nsf
	${NSF2DATA} audio/sfx.nsf -ca65 -ntsc

%.o: %.s src/*.inc
	ca65 $< ${CA65_FLAGS}

clean:
	rm src/*.o assets/*.o src/*/*.o *.nes *.dbg map.txt audio/soundtrack.s audio/soundtrack.txt \
	   audio/sfx.s audio/sfx.nsf -f

run: debug
	${EMULATOR} ${TARGET}

usage: tools/ld65-map.json

tools/ld65-map.json: map.txt tools/ld65-map.rb
	ruby tools/ld65-map.rb map.txt 2 1 tools/ld65-map.json

release: ${TARGET}
	cp ${TARGET} ${PROJECT}-${VERSION}.nes

FORCE:
