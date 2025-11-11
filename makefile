	APPNAME=startup
	DISKNAME=GAMESH65.D81
	PNG65=node ./build/aseparse65/png65.js
	LDTK65=node ./build/ldtk65/ldtk65.js
	JTAG=/dev/cu.usbserial-2516330596481
	EMEGA65_FTP_ARGS=-e -F -c "put $(DISKNAME)" -c "quit"
	ETHERLOAD_ARGS=-r -m $(DISKNAME) bin/$(APPNAME).prg.addr.mc

ifeq ($(windows), 1)
	KICK=./KickAss65CE02-5.24f.jar
	C1541=d:/PCTOOLS/c1541.exe
	XEMU=h:/xemu/xmega65.exe
	MEGA65_FTP=d:/PCTOOLS/mega65_ftp.exe
	EMEGA65_FTP=d:/PCTOOLS/mega65_ftp.exe
	ETHERLOAD=d:/PCTOOLS/etherload.exe
	MEGATOOL=./build/megatool/megatool.exe
else
	KICK=~/Applications/KickAss/KickAss65CE02-5.24f.jar
	C1541=/opt/homebrew/Cellar/vice/3.8/bin/c1541
	XEMU=/Applications/Xemu/xmega65.app/Contents/MacOS/xmega65
	MEGA65_FTP=~/Applications/Mega65/mega65_ftp.osx
	EMEGA65_FTP=~/Documents/MEGA65/mega65_ftp.osx
	ETHERLOAD=~/Documents/MEGA65/etherload.osx
	MEGATOOL=./build/megatool/macmegatool.exe
endif

all: data datablobs code disk

disk: 
	$(C1541) -format "game shell 65,0" d81 $(DISKNAME)
	$(C1541) -attach $(DISKNAME) 8 -write bin/$(APPNAME).prg.addr.mc "game shell 65"
	$(C1541) -attach $(DISKNAME) 8 -write sdcard/data.bin.addr.mc "fs-iffl0"

datablobs:
	$(MEGATOOL) -p 00000100 \
		sdcard/bg20_chr.bin \
		sdcard/bg21_chr.bin \
		sdcard/bg22_chr.bin \
		sdcard/font_chr.bin \
		sdcard/32x32sprite_chr.bin \
		sdcard/data.bin

	$(MEGATOOL) -a sdcard/data.bin 00000000

	$(MEGATOOL) -c sdcard/data.bin.addr

code:
	java -cp $(KICK) kickass.KickAssembler65CE02 -showmem -odir bin $(APPNAME).asm –symbolfile -bytedumpfile $(APPNAME).klist
	$(MEGATOOL) -a bin/$(APPNAME).prg 00002000
	$(MEGATOOL) -c -e 00002000 bin/$(APPNAME).prg.addr

map:
	$(LDTK65) --ncm --workdir "./assets/" --input "bg2.ldtk" --output "sdcard"

data: map
	$(PNG65) sprites --ncm --size 16,16 --input "assets/font.png" --output "sdcard" --nofill
	$(PNG65) sprites --ncm --size 32,32 --input "assets/32x32sprite.png" --output "sdcard" --nofill

run: all
	$(XEMU) -autoload -8 $(DISKNAME) -uartmon :4510 -videostd 1

push: all
	$(MEGA65_FTP) -F -l $(JTAG) -c "put $(DISKNAME)" -c "quit"

eth: all
	$(EMEGA65_FTP) $(EMEGA65_FTP_ARGS)
	$(ETHERLOAD) $(ETHERLOAD_ARGS)

qq: all
	$(ETHERLOAD) $(ETHERLOAD_ARGS)

ethrun: all
	$(EMEGA65_FTP) $(EMEGA65_FTP_ARGS)
	$(ETHERLOAD) $(ETHERLOAD_ARGS)

clean:
	rm -f bin/*
	rm -f sdcard/*
	rm -f *.D81
	rm -f *.addr
	rm -f *.addr.mc
	
