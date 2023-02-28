!
!	setup.s		(C) 1991 Linus Torvalds
!
! setup.s is responsible for getting the system data from the BIOS,
! and putting them into the appropriate places in system memory.
! both setup.s and system has been loaded by the bootblock.
!
! This code asks the bios for memory/disk/other parameters, and
! puts them in a "safe" place: 0x90000-0x901FF, ie where the
! boot-block used to be. It is then up to the protected mode
! system to read them from there before the area is overwritten
! for buffer-blocks.
!

! NOTE! These had better be the same as in bootsect.s!

INITSEG  = 0x9000	! we move boot here - out of the way
SYSSEG   = 0x1000	! system loaded at 0x10000 (65536).
SETUPSEG = 0x9020	! this is the current segment

.globl begtext, begdata, begbss, endtext, enddata, endbss
.text
begtext:
.data
begdata:
.bss
begbss:
.text

entry start
start:

! ok, the read went well so we get current cursor position and save it for
! posterity.

	mov	ax,#INITSEG	! this is done in bootsect already, but...
	mov	ds,ax
	mov	ah,#0x03	! read cursor pos
	xor	bh,bh
	int	0x10		; save it in known place, con_init fetches. 读取当前光标位置，结果在 dx 中。dh:dl = y:x
; 0x9000，2 字节，保存光标位置。
	mov	[0],dx		; it from 0x90000. 放在 0x90000 处。这里注意：已经在覆盖 bootsect 的内存了。
! Get memory size (extended mem, kB)

; 0x9000:2, 内存大小。
	mov	ah,#0x88
	int	0x15
	mov	[2],ax

! Get video-card data: 
; 获取显卡显示模式、显示页、窗口宽度等信息。
; 0x9000:4, 显示页，2字节。
; 0x9000:6, 显卡显示模式，1字节。
; 0x9000:7, 窗口宽度，1字节。

	mov	ah,#0x0f
	int	0x10
	mov	[4],bx		! bh = display page
	mov	[6],ax		! al = video mode, ah = window width

! check for EGA/VGA and some config parameters
; 检查是否为 EGA/VGA 显卡，以及一些配置参数。检查显示方式并取参数
; 0x9000:8, 显卡类型。
; 0x9000:9, 显卡显示模式。
; 0x9000:0xA, 显示内存。
; 0x9000:0xB, 显示状态。
; 0x9000:0xC, 2字节，显卡参数。
	mov	ah,#0x12
	mov	bl,#0x10
	int	0x10
	mov	[8],ax
	mov	[10],bx
	mov	[12],cx

! Get hd0 data
; 获取第一块硬盘的信息。
; 0x9000:0x80, 16字节，第一块硬盘信息。
	mov	ax,#0x0000
	mov	ds,ax
	lds	si,[4*0x41]
	mov	ax,#INITSEG
	mov	es,ax
	mov	di,#0x0080 ; 0x9000:0x80
	mov	cx,#0x10 ; 16 字节
	rep
	movsb

! Get hd1 data
; 获取第二块硬盘的信息。
; 0x9000:0x90, 16字节，第一块硬盘信息。
	mov	ax,#0x0000
	mov	ds,ax
	lds	si,[4*0x46]
	mov	ax,#INITSEG
	mov	es,ax
	mov	di,#0x0090	; 0x9000:0x90
	mov	cx,#0x10  ; 16 字节
	rep
	movsb

! Check that there IS a hd1 :-)
; 0x901FC，2字节，根设备号。

	mov	ax,#0x01500
	mov	dl,#0x81
	int	0x13
	jc	no_disk1
	cmp	ah,#3
	je	is_disk1
no_disk1:
	mov	ax,#INITSEG
	mov	es,ax
	mov	di,#0x0090
	mov	cx,#0x10
	mov	ax,#0x00
	rep
	stosb
is_disk1:

! now we want to move to protected mode ...

	; 关闭中断
	cli			! no interrupts allowed !

! first we move the system to it's rightful place

	mov	ax,#0x0000	; 
	cld			! 'direction'=0, movs moves forward
	; 把 kernel 从 0x10000 处移动到 0x0000 处，即把 system 的 0x80000(512K) 复制到 0x0000 处。
do_move:
	mov	es,ax		! destination segment
	add	ax,#0x1000	; for ax = 0x1000; ax < 0x9000; ax += 0x1000 {  }
	cmp	ax,#0x9000
	jz	end_move
	mov	ds,ax		! source segment
	sub	di,di	 ; di = 0, si = 0
	sub	si,si
	mov 	cx,#0x8000
	rep
	movsw	; ds:si -> es:di, 0x1000:0 => 0x0000:0, 移动 0x8000 字节
	jmp	do_move

! then we load the segment descriptors
; 此处内存布局：
; 0x0 - 0x7FFF, kernel(512KB)
; 0x90000: 几十个字节，存放从上面读到的信息（显卡之类的）
; 0x90200: setup 代码（即本文件代码）
; 0x9ff00: 栈底。

end_move:
	mov	ax,#SETUPSEG	! right, forgot this at first. didn't work :-)
	mov	ds,ax	; 把数据段重新设置为 0x90200
	; 加载中断描述符表。
	lidt	idt_48		! load idt with 0,0
	lgdt	gdt_48		; load gdt with whatever appropriate. 把 gdt_48 对应的变量（48位）加载到 gdtr 中
	; 经过这里后，系统已经可以进入保护模式了。
! that was painless, now we enable A20

	call	empty_8042
	mov	al,#0xD1		! command write
	out	#0x64,al
	call	empty_8042
	mov	al,#0xDF		;! A20 on, 开启 A20 地址线，注意，这里并没有打开保护模式
	out	#0x60,al
	call	empty_8042

! well, that went ok, I hope. Now we have to reprogram the interrupts :-(
! we put them right after the intel-reserved hardware interrupts, at
! int 0x20-0x2F. There they won't mess up anything. Sadly IBM really
! messed this up with the original PC, and they haven't been able to
! rectify it afterwards. Thus the bios puts interrupts at 0x08-0x0f,
! which is used for the internal hardware interrupts as well. We just
! have to reprogram the 8259's, and it isn't fun.
	; 对可编程中断控制器 8259 芯片进行编程，设置中断
	;因为中断号是不能冲突的， Intel 把 0 到 0x19 号中断都作为保留中断，比如 0 号中断就规定为除零异常，软件自定义的中断都应该放在这之后，但是 IBM 在原 PC 机中搞砸了，跟保留中断号发生了冲突，以后也没有纠正过来，所以我们得重新对其进行编程，不得不做，却又一点意思也没有。
	; 经过这步，把 PIC 的 IRQ0-IRQ7 重新映射到 0x20-0x27，IRQ8-IRQ15 重新映射到 0x28-0x2F。
	mov	al,#0x11		! initialization sequence
	out	#0x20,al		! send it to 8259A-1
	.word	0x00eb,0x00eb		! jmp $+2, jmp $+2
	out	#0xA0,al		! and to 8259A-2
	.word	0x00eb,0x00eb
	mov	al,#0x20		! start of hardware int's (0x20)
	out	#0x21,al
	.word	0x00eb,0x00eb
	mov	al,#0x28		! start of hardware int's 2 (0x28)
	out	#0xA1,al
	.word	0x00eb,0x00eb
	mov	al,#0x04		! 8259-1 is master
	out	#0x21,al
	.word	0x00eb,0x00eb
	mov	al,#0x02		! 8259-2 is slave
	out	#0xA1,al
	.word	0x00eb,0x00eb
	mov	al,#0x01		! 8086 mode for both
	out	#0x21,al
	.word	0x00eb,0x00eb
	out	#0xA1,al
	.word	0x00eb,0x00eb
	mov	al,#0xFF		! mask off all interrupts for now
	out	#0x21,al
	.word	0x00eb,0x00eb
	out	#0xA1,al

! well, that certainly wasn't fun :-(. Hopefully it works, and we don't
! need no steenking BIOS anyway (except for the initial loading :-).
! The BIOS-routine wants lots of unnecessary data, and it's less
! "interesting" anyway. This is how REAL programmers do it.
!
! Well, now's the time to actually move into protected mode. To make
! things as simple as possible, we do no register set-up or anything,
! we let the gnu-compiled 32-bit programs do that. We just jump to
! absolute address 0x00000, in 32-bit protected mode.
	; 启动保护模式
	mov	ax,#0x0001	;! protected mode (PE) bit
	lmsw	ax		! This is it!
	; jmp 8:0，注意，此时已经进入了 保护模式，所以这里的 8 是代码段的选择子，而不是段基址。而段选择字的低3位是 RPL，RPL=0 表示内核态，RPL=3 表示用户态；高13位是段索引，所以此时的索引是1，即指向 gdt 中的第二个段描述符，即代码段描述符。
	jmpi	0,8		! jmp offset 0 of segment 8 (cs)
	; 从这里开始，就是 32 位保护模式，并且运行在内核代码了。即 head.S

! This routine checks that the keyboard command queue is empty
! No timeout is used - if this hangs there is something wrong with
! the machine, and we probably couldn't proceed anyway.
; 清除键盘缓冲区。键盘事件会排队，如果不清除，可能会影响后面的操作。
empty_8042:
	.word	0x00eb,0x00eb
	in	al,#0x64	! 8042 status port
	test	al,#2		! is input buffer full?
	jnz	empty_8042	! yes - loop
	ret

gdt: ; 全局描述符表
	; 
	.word	0,0,0,0		;! dummy。第一个段描述符为空

	; 第二个段描述符：代码段
	; 基地址设置为0，指向系统代码
	.word	0x07FF		;! 8Mb - limit=2047 (2048*4096=8Mb)
	.word	0x0000		;! base address=0. 基地址 低16位
	.word	0x9A00		;! code read/exec，基地址 16~23（8）位
	.word	0x00C0		;! granularity=4096, 386 ，高8位为基地址的 24~31位

	; 第三个段描述符：数据段，基地址设置为0，指向系统数据
	.word	0x07FF		! 8Mb - limit=2047 (2048*4096=8Mb)
	.word	0x0000		! base address=0
	.word	0x9200		! data read/write
	.word	0x00C0		! granularity=4096, 386

idt_48:
	.word	0			! idt limit=0
	.word	0,0			! idt base=0L

gdt_48:; 全局描述符表: 长度为 2048(0x800) 字节，基地址为 0x9xxxx
	.word	0x800		;! gdt limit=2048, 256 GDT entries。低16位是 gdt 的长度，高32位是 gdt 的基地址
	.word	512+gdt,0x9	;! gdt base = 0X9xxxx. 512（0x200） 是 setup.s 的基地址, gdt 是 gdt 在 setup.s 中的偏移量
	
.text
endtext:
.data
enddata:
.bss
endbss:
