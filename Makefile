#########################
# Makefile for Orange'S #
#########################

# Entry point of Orange'S
# It must have the same value with 'KernelEntryPointPhyAddr' in load.inc!
ENTRYPOINT	= 0x30400

# Offset of entry point in kernel file
# It depends on ENTRYPOINT
ENTRYOFFSET	=   0x400

# Programs, flags, etc.
ASM		= nasm
DASM		= ndisasm
CC		= gcc
LD		= ld
ASMBFLAGS	= -I boot/include/
ASMKFLAGS	= -I include/ -f elf
CFLAGS		= -I include/ -m32 -c -fno-builtin -fno-stack-protector
LDFLAGS		= -s -m elf_i386 -Ttext $(ENTRYPOINT)
DASMFLAGS	= -u -o $(ENTRYPOINT) -e $(ENTRYOFFSET)

# This Program
ORANGESBOOT	= boot/boot.bin boot/loader.bin
ORANGESKERNEL	= kernel.bin
OBJS		= kernel/kernel.o kernel/gdtidt.o kernel/main.o kernel/global.o \
              lib/kliba.o lib/string.o kernel/syscall.o kernel/clock.o \
              kernel/i8259.o kernel/interrupt.o kernel/process.o lib/klib.o
DASMOUTPUT	= kernel.bin.asm

# All Phony Targets
.PHONY : everything final image clean realclean disasm all buildimg

boot/boot.bin : boot/boot.asm boot/include/load.inc 
	$(ASM) $(ASMBFLAGS) -o $@ $<

boot/loader.bin : boot/loader.asm boot/include/load.inc
	$(ASM) $(ASMBFLAGS) -o $@ $<

# Default starting position
# nop :
# 	@echo "why not \`make image' huh? :)"

everything : $(ORANGESBOOT) $(ORANGESKERNEL)

all : realclean everything

clean :
	rm -f $(OBJS)

realclean :
	rm -f $(OBJS) $(ORANGESBOOT) $(ORANGESKERNEL)

disasm :
	$(DASM) $(DASMFLAGS) $(ORANGESKERNEL) > $(DASMOUTPUT)


$(ORANGESKERNEL) : $(OBJS)
	$(LD) $(LDFLAGS) -o $(ORANGESKERNEL) $(OBJS)

kernel/kernel.o : kernel/kernel.asm include/sconst.inc
	$(ASM) $(ASMKFLAGS) -o $@ $<

kernel/syscall.o : kernel/syscall.asm include/sconst.inc
	$(ASM) $(ASMKFLAGS) -o $@ $<

kernel/gdtidt.o: kernel/gdtidt.c include/typedef.h include/const.h include/string.h include/prototype.h include/protect.h include/global.h
	$(CC) $(CFLAGS) -o $@ $<

kernel/main.o: kernel/main.c include/protect.h include/global.h include/typedef.h include/prototype.h include/const.h include/string.h
	$(CC) $(CFLAGS) -o $@ $<

kernel/global.o: kernel/global.c include/typedef.h include/const.h include/protect.h include/prototype.h include/global.h
	$(CC) $(CFLAGS) -o $@ $<	

lib/kliba.o : lib/kliba.asm 
	$(ASM) $(ASMKFLAGS) -o $@ $<

lib/string.o : lib/string.asm
	$(ASM) $(ASMKFLAGS) -o $@ $<

kernel/clock.o: kernel/clock.c 
	$(CC) $(CFLAGS) -o $@ $<

kernel/i8259.o: kernel/i8259.c include/typedef.h include/const.h include/protect.h include/prototype.h
	$(CC) $(CFLAGS) -o $@ $<

kernel/interrupt.o: kernel/interrupt.c include/typedef.h include/const.h include/protect.h include/process.h include/prototype.h \
			include/global.h
	$(CC) $(CFLAGS) -o $@ $<

kernel/process.o: kernel/process.c
	$(CC) $(CFLAGS) -o $@ $<

lib/klib.o: lib/klib.c include/typedef.h include/const.h include/protect.h include/string.h include/process.h include/prototype.h \
			include/global.h
	$(CC) $(CFLAGS) -o $@ $<