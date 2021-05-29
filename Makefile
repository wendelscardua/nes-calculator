PROJECT=calculator
LD65_FLAGS=
CA65_FLAGS=
EMULATOR=/mnt/c/NESDev/Mesen.exe
VERSION := $(shell git describe --exact-match --tags 2> /dev/null || git rev-parse --short HEAD)

TARGET=${PROJECT}.nes

.PHONY: debug run usage release FORCE

default: ${TARGET}

${TARGET}: src/header.o src/main.o src/reset.o src/irq.o src/nmi.o \
	src/temps.o \
	src/readjoy.o \
	src/rand.o src/unrle.o src/vram-buffer.o \
	src/graphics.o \
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

src/calculator.o: src/calculator.s src/*.inc \
	assets/nametables/*.rle
	ca65 $< ${CA65_FLAGS}

%.o: %.s src/*.inc
	ca65 $< ${CA65_FLAGS}

clean:
	rm src/*.o assets/*.o src/*/*.o *.nes *.dbg map.txt -f

run: debug
	${EMULATOR} ${TARGET}

usage: tools/ld65-map.json

tools/ld65-map.json: map.txt tools/ld65-map.rb
	ruby tools/ld65-map.rb map.txt 2 1 tools/ld65-map.json

release: ${TARGET}
	cp ${TARGET} ${PROJECT}-${VERSION}.nes

FORCE:
