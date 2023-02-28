!
! SYS_SIZE is the number of clicks (16 bytes) to be loaded.
! 0x3000 is 0x30000 bytes = 196kB, more than enough for current
! versions of linux
!
SYSSIZE = 0x3000
!
!	bootsect.s		(C) 1991 Linus Torvalds
!
! bootsect.s is loaded at 0x7c00 by the bios-startup routines, and moves
! iself out of the way to address 0x90000, and jumps there.
!
! It then loads 'setup' directly after itself (0x90200), and the system
! at 0x10000, using BIOS interrupts. 
!
! NOTE! currently system is at most 8*65536 bytes long. This should be no
! problem, even in the future. I want to keep it simple. This 512 kB
! kernel size should be enough, especially as this doesn't contain the
! buffer cache as in minix
!
! The loader has been made as simple as possible, and continuos
! read errors will result in a unbreakable loop. Reboot by hand. It
! loads pretty fast by getting whole sectors at a time whenever possible.
; 整个编译过程：
; 1. 把 bootsect.s 编译成 bootsect 放在硬盘的 1 扇区。
; 2. 把 setup.s 编译成 setup 放在硬盘的 2~5 扇区。
; 3. 把剩下的全部代码（head.s 作为开头）编译成 system 放在硬盘的随后 240 个扇区。

.globl begtext, begdata, begbss, endtext, enddata, endbss
.text
begtext:
.data
begdata:
.bss
begbss:
.text

SETUPLEN = 4				! nr of setup-sectors
BOOTSEG  = 0x07c0			! original address of boot-sector
INITSEG  = 0x9000			! we move boot here - out of the way
SETUPSEG = 0x9020			! setup starts here
SYSSEG   = 0x1000			! system loaded at 0x10000 (65536).
ENDSEG   = SYSSEG + SYSSIZE		! where to stop loading

! ROOT_DEV:	0x000 - same type of floppy as boot.
!		0x301 - first partition on first drive etc
ROOT_DEV = 0x306

entry _start
_start:
	mov	ax,#BOOTSEG !
	mov	ds,ax ; 设置 ds 为 0x7c0，这样 0x7c00 就是 bootsect 的起始地址
	mov	ax,#INITSEG
	mov	es,ax
	mov	cx,#256
	sub	si,si
	sub	di,di
	rep ; 重复后面的 movsw 指令 256 次。ds:si => es:di，即 0x7c00 => 0x90000，256*2(w) = 512 字节
	movw
	jmpi	go,INITSEG; jmpi 是 jmp indirect 的意思，这里是跳转到 0x9000:go 处。（此指令会更新 CS）
	; 注意：CS 的更新只能使用 jmpi/call/ret 等指令，不能用 mov。

go:	mov	ax,cs ; 将 cs 的值赋给 ds/es/ss，由于此时 cs = 0x9000，所以 ds/es/ss = 0x9000。这里实际上是复用了当前 CS 的数值。
	mov	ds,ax	; 由于代码已经从 0x7c00 移动到 0x90000，所以所有的其他段寄存器也要更新。这才能保证后面的代码能正常运行。
	mov	es,ax
! put stack at 0x9ff00.
	mov	ss,ax
	mov	sp,#0xFF00		; arbitrary value >>512。由于栈向下生长，栈底不能小于 512 字节，否则会覆盖到 bootsect 的数据段。

! load the setup-sectors directly after the bootblock.
! Note that 'es' is already set up.

load_setup:
	mov	dx,#0x0000		! drive 0, head 0
	mov	cx,#0x0002		! sector 2, track 0
	mov	bx,#0x0200		;! address = 512, in INITSEG，把数据读到 0x90200（即512处，紧邻第一个扇区） 处
	mov	ax,#0x0200+SETUPLEN	;! service 2, nr of sectors。读4个扇区，即 1~4 4个扇区是 setup 所在的扇区
	int	0x13			;! read it。BIOS 设置好的中断向量表中，0x13 号中断是磁盘读写服务。
	jnc	ok_load_setup		; ok - continue。读取成功，跳转到 ok_load_setup 处。CF 由磁盘读写服务设置，如果读取成功，CF=0，否则 CF=1
	mov	dx,#0x0000
	mov	ax,#0x0000		! reset the diskette
	int	0x13
	j	load_setup	; try again

ok_load_setup:

! Get disk drive parameters, specifically nr of sectors/track

	; 读取磁盘参数，特别是每磁道扇区数
	mov	dl,#0x00
	mov	ax,#0x0800		! AH=8 is get drive parameters
	int	0x13
	mov	ch,#0x00
; 先讲一下寄存器的默认组合问题，比如指令mov [si], ax表示将ax中的内容存入ds:si指向的内存单元，也就是说在寄存器间接寻址的情况下,以si间接寻址时总是默认以ds为相应的段地址寄存器。同样di是以es为默认的段地址寄存器。
; 第二个要了解的是“段超越”的问题，就是在某些时候你不想使用默认的段地址寄存器，那
; 么你可以强制指定一个段地址寄存器（当然这种强制是在允许的情况下，建议看一下汇编
; 教材上的说明），同上例mov [si],ax表示存入ds:si中，但如果你想存入cs指向的段中可
; 以这样mov cs:[si],ax， 这样就强制指定将ax中的内容存入cs:si的内存单元。
; 第三个要明白的是seg cs这样的语句只影响到它下一条指令，比如在linux启动代码中的一段：
;      seg cs 
;      mov sectors,ax 
;      mov ax,#INITSEG
; 要说明两点：
;     第一，seg cs 只影响到mov sectors,ax而不影响mov ax,#INITSEG
;     第二，如果以Masm语法写，seg cs和mov sectors,ax两句合起来等
;           价于mov cs:[sectors],ax，这里使用了间接寻址方式。
;           重复一下前面的解释，mov [sectors],ax表示将ax中的内容
;           存入ds:sectors内存单元，而mov cs:[sectors],ax强制以
;           cs作为段地址寄存器，因此是将ax的内容存入cs:sectors内存
;           单元，一般来说cs与ds的值是不同的，如果cs和ds的值一样，
;           那两条指令的运行结果会是一样的。（编译后的指令后者比前
;           者一般长一个字节，多了一个前缀。）
;     结论，seg cs只是表明紧跟它的下一条语句将使用段超越，因为在编
;           译后的代码中可以清楚的看出段超越本质上就是加了一个字节
;           的指令前缀，因此as86把它单独作为一条指令来写也是合理的。
	seg cs	; 段超越：强制指定cs为段地址寄存器
	mov	sectors,cx; 以cs为段地址寄存器，将ax中的内容存入cs:sectors内存单元。这里将每磁道扇区数存入了变量 sectors 中
	mov	ax,#INITSEG
	mov	es,ax	; 将 es 恢复到 INITSEG

! Print some inane message
; 一些打印信息
	mov	ah,#0x03		! read cursor pos
	xor	bh,bh
	int	0x10
	
	mov	cx,#24
	mov	bx,#0x0007		! page 0, attribute 7 (normal)
	mov	bp,#msg1
	mov	ax,#0x1301		! write string, move cursor
	int	0x10

! ok, we've written the message, now
! we want to load the system (at 0x10000)

	mov	ax,#SYSSEG ; 0x1000，系统被加载到这里
	mov	es,ax		! segment of 0x010000
	call	read_it ; 读取系统
	call	kill_motor ; 关闭磁盘马达

! After that we check which root-device to use. If the device is
! defined (!= 0), nothing is done and the given device is used.
! Otherwise, either /dev/PS0 (2,28) or /dev/at0 (2,8), depending
! on the number of sectors that the BIOS reports currently.

	seg cs
	mov	ax,root_dev
	cmp	ax,#0
	jne	root_defined ; 如果root_dev不为0，跳转到root_defined处,表示已经初始化
	seg cs
	mov	bx,sectors	; 读取扇区数
	mov	ax,#0x0208		! /dev/ps0 - 1.2Mb
	cmp	bx,#15
	je	root_defined
	mov	ax,#0x021c		! /dev/PS0 - 1.44Mb
	cmp	bx,#18
	je	root_defined
undef_root:
	jmp undef_root
root_defined:
	seg cs
	mov	root_dev,ax ; 把ax的值存入root_dev变量

! after that (everyting loaded), we jump to
! the setup-routine loaded directly after
! the bootblock:

	jmpi	0,SETUPSEG ; 跳到SETUPSEG:0处执行，即 0x90200处，也就是setup.s程序

! This routine loads the system at address 0x10000, making sure
! no 64kB boundaries are crossed. We try to load it as fast as
! possible, loading whole tracks whenever we can.
!
! in:	es - starting address segment (normally 0x1000)
!
sread:	.word 1+SETUPLEN	! sectors read of current track
head:	.word 0			! current head
track:	.word 0			! current track

read_it:
	mov ax,es
	test ax,#0x0fff
die:	jne die			! es must be at 64kB boundary
	xor bx,bx		! bx is starting address within segment
rp_read:
	mov ax,es
	cmp ax,#ENDSEG		! have we loaded all yet?
	jb ok1_read
	ret
ok1_read:
	seg cs ; 
	mov ax,sectors ; 每个磁道的扇区数
	sub ax,sread ; 减去 setup 和 MBR 占用的扇区数（5）
	mov cx,ax ; 指定要读取的扇区数
	shl cx,#9
	add cx,bx
	jnc ok2_read
	je ok2_read
	xor ax,ax ; 清零
	sub ax,bx
	shr ax,#9
ok2_read:
	call read_track
	mov cx,ax
	add ax,sread
	seg cs
	cmp ax,sectors
	jne ok3_read
	mov ax,#1
	sub ax,head
	jne ok4_read
	inc track
ok4_read:
	mov head,ax
	xor ax,ax
ok3_read:
	mov sread,ax
	shl cx,#9
	add bx,cx
	jnc rp_read
	mov ax,es
	add ax,#0x1000
	mov es,ax
	xor bx,bx
	jmp rp_read

read_track:
	push ax
	push bx
	push cx
	push dx
	mov dx,track
	mov cx,sread
	inc cx
	mov ch,dl
	mov dx,head
	mov dh,dl
	mov dl,#0
	and dx,#0x0100
	mov ah,#2
	int 0x13
	jc bad_rt
	pop dx
	pop cx
	pop bx
	pop ax
	ret
bad_rt:	mov ax,#0
	mov dx,#0
	int 0x13
	pop dx
	pop cx
	pop bx
	pop ax
	jmp read_track

!/*
! * This procedure turns off the floppy drive motor, so
! * that we enter the kernel in a known state, and
! * don't have to worry about it later.
! */
kill_motor:
	push dx
	mov dx,#0x3f2
	mov al,#0
	outb
	pop dx
	ret

sectors:
	.word 0

msg1:
	.byte 13,10
	.ascii "Loading system ..."
	.byte 13,10,13,10

.org 508
root_dev:
	.word ROOT_DEV
boot_flag:
	.word 0xAA55

.text
endtext:
.data
enddata:
.bss
endbss:
