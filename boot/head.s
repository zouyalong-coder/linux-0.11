/*
 *  linux/boot/head.s
 *
 *  (C) 1991  Linus Torvalds
 */

/*
 *  head.s contains the 32-bit startup code.
 *
 * NOTE!!! Startup happens at absolute address 0x00000000, which is also where
 * the page directory will exist. The startup code will be overwritten by
 * the page directory.
 */
.text
.globl idt,gdt,pg_dir,tmp_floppy_area
pg_dir:; 页目录地址。这里会在后面被使用，即这里的内存很快会被复用。
.globl startup_32
startup_32:
	; 所有段寄存器都固定指向内核数据段、代码段。
	movl $0x10,%eax ; 使所有段选择子的值都为 0x10, 即指向低3个 gdt 表项，也即内核数据段(基地址为 0)。
	mov %ax,%ds
	mov %ax,%es
	mov %ax,%fs
	mov %ax,%gs
	lss stack_start,%esp ; stack_start 在 sched.c 中定义。 lss 同时设置 ss  和 esp 寄存器。
	call setup_idt ; 设置中断描述符表。（前面没有设置过）
	; 为什么原来在 setup.s 已经设置过一遍了，这里又要重新设置一遍?
; 不是什么复杂的原因，就是因为原来设置的 gdt 是在 setup 程序中，之后这个地方要被缓冲区覆盖掉，所以这里重新设置在 head 程序中
	call setup_gdt ; 重新设置 gdt 表。
	; 由于改变了 gdt，所以需要重新加载所有的段寄存器。
	movl $0x10,%eax		; 0x10 指向 gdt 的第 2 项，即数据段。# reload all the segment registers
	mov %ax,%ds		# after changing gdt. CS was already
	mov %ax,%es		# reloaded in 'setup_gdt'
	mov %ax,%fs
	mov %ax,%gs
	lss stack_start,%esp ; 重新设置 ss 和 esp 寄存器。
	xorl %eax,%eax
1:	incl %eax		; A20 已经打开了，这里只是确保。# check that A20 really IS enabled
	movl %eax,0x000000	# loop forever if it isn't
	cmpl %eax,0x100000 ; 如果发生了回绕，说明 A20 没有打开。
	je 1b

/*
 * NOTE! 486 should set bit 16, to check for write-protect in supervisor
 * mode. Then it would be unnecessary with the "verify_area()"-calls.
 * 486 users probably want to set the NE (#5) bit also, so as to use
 * int 16 for math errors.
 */
	movl %cr0,%eax		# check math chip
	andl $0x80000011,%eax	# Save PG,PE,ET
/* "orl $0x10020,%eax" here for 486 might be good */
	orl $2,%eax		# set MP
	movl %eax,%cr0
	call check_x87
	jmp after_page_tables

/*
 * We depend on ET to be correct. This checks for 287/387.
 */
check_x87:
	fninit
	fstsw %ax
	cmpb $0,%al
	je 1f			/* no coprocessor: have to set bits */
	movl %cr0,%eax
	xorl $6,%eax		/* reset MP, set EM */
	movl %eax,%cr0
	ret
.align 2
1:	.byte 0xDB,0xE4		/* fsetpm for 287, ignored by 387 */
	ret

/*
 *  setup_idt
 *
 *  sets up a idt with 256 entries pointing to
 *  ignore_int, interrupt gates. It then loads
 *  idt. Everything that wants to install itself
 *  in the idt-table may do so themselves. Interrupts
 *  are enabled elsewhere, when we can be relatively
 *  sure everything is ok. This routine will be over-
 *  written by the page tables.
 */
setup_idt:
	lea ignore_int,%edx ; 高16位是0
	movl $0x00080000,%eax; 0x0008 为段选择子，0x0000 为偏移地址。
	movw %dx,%ax		;/* selector = 0x0008 = cs */ 将eax的低16位设置为 edx 的低16位，即 ignore_int 的偏移地址。
	movw $0x8E00,%dx	;/* interrupt gate - dpl=0, present */ 设置中断门描述符的属性字。

	lea idt,%edi ; edi 指向中断描述符表的起始地址。
	mov $256,%ecx ; 将256个中断表项都设置为 ignore_int
rp_sidt:
	movl %eax,(%edi) ; 中断描述符表项的低32位是 中断处理程序的 段选择子 和 偏移地址。
	movl %edx,4(%edi); 中断描述符表项的高32位是 中断处理程序的 属性字。
	addl $8,%edi ; 每个中断描述符表项占8字节。
	dec %ecx
	jne rp_sidt
	lidt idt_descr ; 设置中断描述符表寄存器。 idtr 的低16位是中断描述符表的限长，高32位是中断描述符表的基地址。
	ret

/*
 *  setup_gdt
 *
 *  This routines sets up a new gdt and loads it.
 *  Only two entries are currently built, the same
 *  ones that were built in init.s. The routine
 *  is VERY complicated at two whole lines, so this
 *  rather long comment is certainly needed :-).
 *  This routine will beoverwritten by the page tables.
 */
setup_gdt:;  设置 gdt

	lgdt gdt_descr
	ret

/*
 * I put the kernel page tables right after the page directory,
 * using 4 of them to span 16 Mb of physical memory. People with
 * more than 16MB will have to expand this.
 */
.org 0x1000 ; pg0 地址为 0x1000，刚好一页
pg0:

.org 0x2000
pg1:

.org 0x3000
pg2:

.org 0x4000
pg3:

.org 0x5000
/*
 * tmp_floppy_area is used by the floppy-driver when DMA cannot
 * reach to a buffer-block. It needs to be aligned, so that it isn't
 * on a 64kB border.
 */
tmp_floppy_area:
	.fill 1024,1,0

after_page_tables:
	pushl $0		# These are the parameters to main :-)
	pushl $0
	pushl $0
	pushl $L6		# return address for main, if it decides to.
	pushl $main	; main 函数的入口地址。
	jmp setup_paging ; 开启分页机制。这代码不可能在回来了。
	; 注意：这里采用 jmp 而不是 call。这样 CPU 就不进行压栈，而在 setup_paging的末尾，使用了 ret。ret 指令会从栈顶取返回地址，这就实现了往 main 的跳转。
L6:
	jmp L6			# main should never return here, but
				# just in case, we know what happens.

/* This is the default interrupt "handler" :-) */
int_msg:
	.asciz "Unknown interrupt\n\r"
.align 2
; 只是打印一下就退出了。
ignore_int:
	pushl %eax
	pushl %ecx
	pushl %edx
	push %ds
	push %es
	push %fs
	movl $0x10,%eax
	mov %ax,%ds
	mov %ax,%es
	mov %ax,%fs
	pushl $int_msg
	call printk
	popl %eax
	pop %fs
	pop %es
	pop %ds
	popl %edx
	popl %ecx
	popl %eax
	iret


/*
 * Setup_paging
 *
 * This routine sets up paging by setting the page bit
 * in cr0. The page tables are set up, identity-mapping
 * the first 16MB. The pager assumes that no illegal
 * addresses are produced (ie >4Mb on a 4Mb machine).
 *
 * NOTE! Although all physical memory should be identity
 * mapped by this routine, only the kernel page functions
 * use the >1Mb addresses directly. All "normal" functions
 * use just the lower 1Mb, or the local data space, which
 * will be mapped to some other place - mm keeps track of
 * that.
 *
 * For those with more memory than 16 Mb - tough luck. I've
 * not got it, why should you :-) The source is here. Change
 * it. (Seriously - it shouldn't be too difficult. Mostly
 * change some constants etc. I left it at 16Mb, as my machine
 * even cannot be extended past that (ok, but it was cheap :-)
 * I've tried to show which constants to change by having
 * some kind of marker at them (search for "16Mb"), but I
 * won't guarantee that's all :-( )
 */
.align 2
setup_paging:
	; ref https://mp.weixin.qq.com/s?__biz=Mzk0MjE3NDE0Ng==&mid=2247499821&idx=1&sn=df90a7c57607bf501b5ef535f8440d98&chksm=c2c5ba80f5b233969bf591f919107e28e7be51f066821cba1ea39bf19cc0332b95b94d29467d&cur_album_id=2123743679373688834&scene=189#wechat_redirect
	; 当时 linux-0.11 认为，总共可以使用的内存不会超过 16M，也即最大地址空间为 0xFFFFFF。所以只设置了 4 个页表 + 1个页目录。
; 	而按照当前的页目录表和页表这种机制，1 个页目录表最多包含 1024 个页目录项（也就是 1024 个页表），1 个页表最多包含 1024 个页表项（也就是 1024 个页），1 页为 4KB（因为有 12 位偏移地址），因此，16M 的地址空间可以用 1 个页目录表 + 4 个页表搞定。
 
; 4（页表数）* 1024（页表项数） * 4KB（一页大小）= 16MB
	movl $1024*5,%ecx		/* 5 pages - pg_dir+4 page tables */
	xorl %eax,%eax
	xorl %edi,%edi			/* pg_dir is at 0x000 */
	cld;rep;stosl
	movl $pg0+7,pg_dir		;/* set present bit/user r/w 7=0x111, 表示user空间、可读写、present */
	movl $pg1+7,pg_dir+4		/*  --------- " " --------- */
	movl $pg2+7,pg_dir+8		/*  --------- " " --------- */
	movl $pg3+7,pg_dir+12		/*  --------- " " --------- */
	movl $pg3+4092,%edi ; 
	movl $0xfff007,%eax		/*  16Mb - 4096 + 7 (r/w user,p) */
	std		; 从后往前覆盖
1:	stosl			/* fill pages backwards - more efficient :-) */
	subl $0x1000,%eax
	jge 1b
	xorl %eax,%eax		/* pg_dir is at 0x0000 */
	movl %eax,%cr3		; 把 page_dir 设置成页表入口地址。/* cr3 - page directory start */
	movl %cr0,%eax	; 获取 cr0 的值。
	orl $0x80000000,%eax ; cr0 的第 31 位是 PG，设置为 1，开启分页机制。
	movl %eax,%cr0		/* set paging (PG) bit */
	ret			; ret 指令读取 /* this also flushes prefetch-queue */

.align 2
.word 0
idt_descr:; 256个中断表项，每个表项8个字节。
	.word 256*8-1		# idt contains 256 entries
	.long idt ; 放在 idt 位置。
.align 2
.word 0
gdt_descr:; 低16位是gdt的字节长度，高32位是gdt的基地址。
	.word 256*8-1		;# so does gdt (not that that's any
	.long gdt		;# magic number, but it works for me :^)

	.align 8
; 256个中断表项，每个表项8个字节。
idt:	.fill 256,8,0		# idt is uninitialized

; gdt 内容
gdt:	.quad 0x0000000000000000	/* NULL descriptor */
	.quad 0x00c09a0000000fff	; /* 16Mb */ 系统代码段
	.quad 0x00c0920000000fff	; /* 16Mb */ 系统数据段
	.quad 0x0000000000000000	/* TEMPORARY - don't use */
	.fill 252,8,0			; /* space for LDT's and TSS's etc */ 剩下的 252 个表项为0，会用来放置任务状态段描述符 TSS 和局部描述符 LDT
