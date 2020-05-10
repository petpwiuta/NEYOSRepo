


%include "load.inc"
%include "mempage.inc"

;-------------------------------------------------------------------------------
SECTION loader align=16 vstart=0x100   ;这里从100h开始
;-------------------------------------------------------------------------------
	jmp	LABEL_START		; Start

LoaderLenght dw LABEL_CODE_END   ;length



; GDT ------------------------------------------------------------------------------------------------------------------------------------------------------------
;                                              段基址            段界限 , 属性
LABEL_GDT:			    Descriptor             0,                    0, 0						                ; 空描述符
LABEL_DESC_FLAT_C:		Descriptor             0,              0fffffh, DA_CR  | DA_32 | DA_LIMIT_4K			; 0 ~ 4G
LABEL_DESC_FLAT_RW:		Descriptor             0,              0fffffh, DA_DRW | DA_32 | DA_LIMIT_4K			; 0 ~ 4G
LABEL_DESC_VIDEO:		Descriptor	     0B8000h,               0ffffh, DA_DRW | DA_DPL3	                    ; 显存首地址
; GDT ------------------------------------------------------------------------------------------------------------------------------------------------------------

GdtLen		equ	$ - LABEL_GDT
GdtPtr		dw	GdtLen - 1				            ; 段界限
		    dd	BaseOfLoaderPhyAddr + LABEL_GDT		; 基地址

; GDT 选择子 ----------------------------------------------------------------------------------
SelectorFlatC		equ	LABEL_DESC_FLAT_C	- LABEL_GDT
SelectorFlatRW		equ	LABEL_DESC_FLAT_RW	- LABEL_GDT
SelectorVideo		equ	LABEL_DESC_VIDEO	- LABEL_GDT + SA_RPL3
; GDT 选择子 ----------------------------------------------------------------------------------


BaseOfStack	equ	0100h


;============================================================================
;字符串
;----------------------------------------------------------------------------
; 为简化代码, 下面每个字符串的长度均为 MessageLength
MessageLength		equ	24
LoadMessage:		db	"Loader Logic Begin......"; 24字节, 不够则用空格补齐. 序号 0
Message1     		db	"Start Loading Kernel...."; 24字节, 不够则用空格补齐. 序号 1
Message2		    db	"Load KNR first Block OK."; 24字节, 不够则用空格补齐. 序号 2
Message3		    db	"KNR Only One Block.     "; 24字节, 不够则用空格补齐. 序号 3
Message4		    db	"KNR Remain Block OK.    "; 24字节, 不够则用空格补齐. 序号 4


;----------------------------------------------------------------------------
; 函数名: ReadHardDisk0 
;----------------------------------------------------------------------------
; 作用:
;	读取磁盘数据
ReadHardDisk0:                           ;通过lba28地址来读取512字节的数据
                                         ;DI:SI=lba地址,di是高12位,si低16位,小端
                                         ;DS:BX=读取到该地址
         push ax
         push bx
         push cx
         push dx

         mov dx,0x1f2                    ;0x1f2 length port
         mov al,1                        ;read one block
         out dx,al

         inc dx                          ;0x1f3
         mov ax,si
         out dx,al                       ;LBA 7~0

         inc dx                          ;0x1f4
         mov al,ah
         out dx,al                       ;LBA ַ15~8

         inc dx                          ;0x1f5
         mov ax,di
         out dx,al                       ;LBA 23~16

         inc dx                          ;0x1f6
         mov al,0xe0                     ;LBA28,master disk
         or al,ah                        ;LBA 27~24
         out dx,al

         inc dx                          ;0x1f7
         mov al,0x20                     ;request read
         out dx,al

  .waits:
         in al,dx
         and al,0x88
         cmp al,0x08
         jnz .waits                      ;check state

         mov cx,256                      ;256*2 = 512 byte, one block
         mov dx,0x1f0
  .readw:
         in ax,dx
         mov [bx],ax
         add bx,2
         loop .readw

         pop dx
         pop cx
         pop bx
         pop ax
      
         ret


;----------------------------------------------------------------------------
; 函数名: DispStrRealMode
;----------------------------------------------------------------------------
; 运行环境:
;	实模式（保护模式下显示字符串由函数 DispStr 完成）
; 作用:
;	显示一个字符串, 函数开始时 dh 中应该是字符串序号(0-based)
DispStrRealMode:
	push ax
	push bx
	push cx
	push dx
	push bp
	push ds
	push es

	mov	ax, MessageLength
	mul	dh
	add	ax, LoadMessage
	mov	bp, ax			; ┓
	mov	ax, ds			; ┣ ES:BP = 串地址
	mov	es, ax			; ┛
	mov	cx, MessageLength	; CX = 串长度
	mov	ax, 01301h		; AH = 13,  AL = 01h
	mov	bx, 0007h		; 页号为0(BH = 0) 黑底白字(BL = 07h)
	mov	dl, 0
	add	dh, 3			; 从第 3 行往下显示
	int	10h			; int 10h

	pop es
	pop ds
	pop bp
	pop dx
	pop cx
	pop bx
	pop ax

	ret



LABEL_START:            ; <--- 从这里开始 *************
	mov	dh, 0			        ; "Start Loading kernel"
	call	DispStrRealMode		; 显示字符串

	mov	ax, cs
	mov	ds, ax
	mov	es, ax
	mov	ss, ax
	mov	sp, BaseOfStack



	; 得到内存数
	mov	ebx, 0			; ebx = 后续值, 开始时需为 0
	mov	di, _MemChkBuf+0x100		; es:di 指向一个地址范围描述符结构（Address Range Descriptor Structure）
.MemChkLoop:
	mov	eax, 0E820h		; eax = 0000E820h
	mov	ecx, 20			; ecx = 地址范围描述符结构的大小
	mov	edx, 0534D4150h		; edx = 'SMAP'
	int	15h			; int 15h
	jc	.MemChkFail
	add	di, 20
	inc	dword [_dwMCRNumber+0x100]	; dwMCRNumber = ARDS 的个数
	cmp	ebx, 0
	jne	.MemChkLoop
	jmp	.MemChkOK
.MemChkFail:
	mov	dword [_dwMCRNumber+0x100], 0
.MemChkOK:


    ;加载kernel代码
    mov	dh, 1			; "Start Loading Kernel......  "
	; call	DispStrRealMode	    ; 显示字符串

	;从硬盘指定lba加载loader，仅仅加载512byte，再读取laoder中的长度字段决定是否需要加载剩余部分
	xor di,di
    mov si,KernelLBA       ;lba
	mov ax,BaseOfKernelFile
    mov ds,ax              ;segment
	mov bx,OffsetOfKernelFile  ;offset
    call ReadHardDisk0

	mov	dh, 2			; "load first block success"
	; call	DispStrRealMode	    


	;看是否有必要加载剩下的部分
	mov ax,13312
	cmp ax,DiskBlockSize         ;前面两个字节是loader长度，用来判断是否还需要下载剩下的
	jbe LABEL_KNR_ONLY_ONE_BLOCK  ;ax <= 512


	;加载剩余部分loader
	xor dx,dx
	mov cx,DiskBlockSize
	div cx
	cmp dx,0
	jne LABEL_REMAINDER
	jmp LABEL_NO_REMAINDER

	LABEL_REMAINDER:
		add ax,1

	LABEL_NO_REMAINDER:
		dec ax    ;第一个block已经加载
		mov cx,ax

		mov ax,KernelLBA
		add ax,1
		mov dx,OffsetOfKernelFile
		add dx,DiskBlockSize
		mov di,BaseOfKernelFile
		mov ds,di              ;segment
		xor di,di
   		
		LABEL_READ_REMAIN_BLOCK:
    		mov si,ax   ;lba
			mov bx,dx  ;offset
    		call ReadHardDisk0
			inc ax
			add dx,DiskBlockSize
		loop LABEL_READ_REMAIN_BLOCK


	mov	dh, 4			; "load remina block success"
	; call	DispStrRealMode	    
	jnz LABEL_PROTECT_MODE


LABEL_KNR_ONLY_ONE_BLOCK:
	;只有一个block
	mov	dh, 3			; "only one block success"
	; call	DispStrRealMode	    




; 下面准备跳入保护模式 -------------------------------------------
LABEL_PROTECT_MODE:

; 加载 GDTR
	lgdt	[cs:GdtPtr]

; 关中断
	cli

; 打开地址线A20。A20地址线是由于部分代码利用了20地址线溢出的特性，到32位以后为了兼容设置了A20标志
	in	al, 92h
	or	al, 00000010b
	out	92h, al

; 准备切换到保护模式
	mov	eax, cr0
	or	eax, 1
	mov	cr0, eax

; 真正进入保护模式
	; jmp	dword SelectorFlatC:(BaseOfLoaderPhyAddr+LABEL_PM_START+0x100)
	jmp	dword 0x0008:(BaseOfLoaderPhyAddr+LABEL_PM_START+0x100)






; 从此以后的代码在保护模式下执行 ----------------------------------------------------
; 32 位代码段. 由实模式跳入 ---------------------------------------------------------
;-------------------------------------------------------------------------------
SECTION .s32 align=32
;-------------------------------------------------------------------------------

[BITS	32]

LABEL_PM_START:
	mov	ax, SelectorVideo
	mov	gs, ax
	mov	ax, SelectorFlatRW
	mov	ds, ax
	mov	es, ax
	mov	fs, ax
	mov	ss, ax
	mov	esp, TopOfStack

	push	szMemChkTitle
	call	DispStr
	add	esp, 4

	call	DispMemInfo
	call	SetupPaging
	call	InitKernel

	;***************************************************************
	jmp	SelectorFlatC:KernelEntryPointPhyAddr	; 正式进入内核 *
	;***************************************************************
	; 内存看上去是这样的：
	;              ┃                  ┃
	;              ┃        .         ┃
	;              ┃        .         ┃
	;              ┃        .         ┃
	;              ┣━━━━━━━━━━━━━━━━━━┫
	;              ┃■■■■■■■■■■■■■■■■■■┃
	;              ┃■■■Page  Tables■■■┃
	;              ┃■(大小由LOADER决定)■┃
	;    00101000h ┃■■■■■■■■■■■■■■■■■■┃ PageTblBase
	;              ┣━━━━━━━━━━━━━━━━━━┫
	;              ┃■■■■■■■■■■■■■■■■■■┃
	;    00100000h ┃■Page Directory Table■┃ PageDirBase  <- 1M
	;              ┣━━━━━━━━━━━━━━━━━━┫
	;              ┃□□□□□□□□□□□□□□□□□□┃
	;       F0000h ┃□□□□□System ROM□□□┃
	;              ┣━━━━━━━━━━━━━━━━━━┫
	;              ┃□□□□□□□□□□□□□□□□□□┃
	;       E0000h ┃□□Expansion of system ROM □┃
	;              ┣━━━━━━━━━━━━━━━━━━┫
	;              ┃□□□□□□□□□□□□□□□□□□┃
	;       C0000h ┃□□□Reserved for ROM expansion□□┃
	;              ┣━━━━━━━━━━━━━━━━━━┫
	;              ┃□□□□□□□□□□□□□□□□□□┃ B8000h ← gs
	;       A0000h ┃□□□Display adapter reserved□□□┃
	;              ┣━━━━━━━━━━━━━━━━━━┫
	;              ┃□□□□□□□□□□□□□□□□□□┃
	;       9FC00h ┃□□extended BIOS data area (EBDA)□┃
	;              ┣━━━━━━━━━━━━━━━━━━┫
	;              ┃■■■■■■■■■■■■■■■■■■┃
	;       90000h ┃■■■■■LOADER.BIN■■■┃ somewhere in LOADER ← esp
	;              ┣━━━━━━━━━━━━━━━━━━┫
	;              ┃■■■■■■■■■■■■■■■■■■┃
	;       80000h ┃■■■■■KERNEL.BIN■■■┃
	;              ┣━━━━━━━━━━━━━━━━━━┫
	;              ┃■■■■■■■■■■■■■■■■■■┃
	;       30000h ┃■■■■■■■■KERNEL■■■■┃ 30400h ← KERNEL 入口 (KernelEntryPointPhyAddr)
	;              ┣━━━━━━━━━━━━━━━━━━┫
	;              ┃                  ┃
	;        7E00h ┃  F  R  E  E      ┃
	;              ┣━━━━━━━━━━━━━━━━━━┫
	;              ┃■■■■■■■■■■■■■■■■■■┃
	;        7C00h ┃■■■BOOT  SECTOR■■■┃
	;              ┣━━━━━━━━━━━━━━━━━━┫
	;              ┃                  ┃
	;         500h ┃   F  R  E  E     ┃
	;              ┣━━━━━━━━━━━━━━━━━━┫
	;              ┃□□□□□□□□□□□□□□□□□□┃
	;         400h ┃□□ROM BIOS parameter area □┃
	;              ┣━━━━━━━━━━━━━━━━━━┫
	;              ┃◇◇◇◇◇◇◇◇◇◇◇◇◇◇◇◇◇◇┃
	;           0h ┃◇◇◇Int  Vectors◇◇◇┃
	;              ┗━━━━━━━━━━━━━━━━━━┛ ← cs, ds, es, fs, ss
	;
	;
	;		┏━━━┓			    ┏━━━┓
	;		┃■■■┃ 已使用 	 	 ┃□□□┃ 不能使用的内存
	;		┗━━━┛		        ┗━━━┛
	;		┏━━━                ┏━━━┓
	;		┃   ┃  未使用        ┃◇◇◇┃ 可以覆盖的内存
	;		┗━━━┛               ┗━━━┛
	;
	; 注：KERNEL 的位置实际上是很灵活的，可以通过同时改变 LOAD.INC 中的 KernelEntryPointPhyAddr 和 MAKEFILE 中参数 -Ttext 的值来改变。
	;     比如，如果把 KernelEntryPointPhyAddr 和 -Ttext 的值都改为 0x400400，则 KERNEL 就会被加载到内存 0x400000(4M) 处，入口在 0x400400。
	;



; ------------------------------------------------------------------------
; 显示 AL 中的数字
; ------------------------------------------------------------------------
DispAL:
	push	ecx
	push	edx
	push	edi

	mov	edi, [dwDispPos]

	mov	ah, 0Fh			; 0000b: 黑底    1111b: 白字
	mov	dl, al
	shr	al, 4
	mov	ecx, 2
.begin:
	and	al, 01111b
	cmp	al, 9
	ja	.1
	add	al, '0'
	jmp	.2
.1:
	sub	al, 0Ah
	add	al, 'A'
.2:
	mov	[gs:edi], ax
	add	edi, 2

	mov	al, dl
	loop	.begin
	;add	edi, 2

	mov	[dwDispPos], edi

	pop	edi
	pop	edx
	pop	ecx

	ret
; DispAL 结束-------------------------------------------------------------


; ------------------------------------------------------------------------
; 显示一个整形数
; ------------------------------------------------------------------------
DispInt:
	mov	eax, [esp + 4]
	shr	eax, 24
	call	DispAL

	mov	eax, [esp + 4]
	shr	eax, 16
	call	DispAL

	mov	eax, [esp + 4]
	shr	eax, 8
	call	DispAL

	mov	eax, [esp + 4]
	call	DispAL

	mov	ah, 07h			; 0000b: 黑底    0111b: 灰字
	mov	al, 'h'
	push	edi
	mov	edi, [dwDispPos]
	mov	[gs:edi], ax
	add	edi, 4
	mov	[dwDispPos], edi
	pop	edi

	ret
; DispInt 结束------------------------------------------------------------

; ------------------------------------------------------------------------
; 显示一个字符串
; ------------------------------------------------------------------------
DispStr:
	push	ebp
	mov	ebp, esp
	push	ebx
	push	esi
	push	edi

	mov	esi, [ebp + 8]	; pszInfo
	mov	edi, [dwDispPos]
	mov	ah, 0Fh
.1:
	lodsb
	test	al, al
	jz	.2
	cmp	al, 0Ah	; 是回车吗?
	jnz	.3
	push	eax
	mov	eax, edi
	mov	bl, 160
	div	bl
	and	eax, 0FFh
	inc	eax
	mov	bl, 160
	mul	bl
	mov	edi, eax
	pop	eax
	jmp	.1
.3:
	mov	[gs:edi], ax
	add	edi, 2
	jmp	.1

.2:
	mov	[dwDispPos], edi

	pop	edi
	pop	esi
	pop	ebx
	pop	ebp
	ret
; DispStr 结束------------------------------------------------------------

; ------------------------------------------------------------------------
; 换行
; ------------------------------------------------------------------------
DispReturn:
	push	szReturn
	call	DispStr			;printf("\n");
	add	esp, 4

	ret
; DispReturn 结束---------------------------------------------------------


; ------------------------------------------------------------------------
; 内存拷贝，仿 memcpy
; ------------------------------------------------------------------------
; void* MemCpy(void* es:pDest, void* ds:pSrc, int iSize);
; ------------------------------------------------------------------------
MemCpy:
	push	ebp
	mov	ebp, esp

	push	esi
	push	edi
	push	ecx

	mov	edi, [ebp + 8]	; Destination
	mov	esi, [ebp + 12]	; Source
	mov	ecx, [ebp + 16]	; Counter
.1:
	cmp	ecx, 0		; 判断计数器
	jz	.2		; 计数器为零时跳出

	mov	al, [ds:esi]		; ┓
	inc	esi			        ; ┃
					        ; ┣ 逐字节移动
	mov	byte [es:edi], al	; ┃
	inc	edi			        ; ┛

	dec	ecx		; 计数器减一
	jmp	.1		; 循环
.2:
	mov	eax, [ebp + 8]	; 返回值

	pop	ecx
	pop	edi
	pop	esi
	mov	esp, ebp
	pop	ebp

	ret			; 函数结束，返回
; MemCpy 结束-------------------------------------------------------------




; 显示内存信息 --------------------------------------------------------------
DispMemInfo:
	push	esi
	push	edi
	push	ecx

	mov	esi, MemChkBuf
	mov	ecx, [dwMCRNumber]	;for(int i=0;i<[MCRNumber];i++) // 每次得到一个ARDS(Address Range Descriptor Structure)结构
.loop:					    ;{
	mov	edx, 5			    ;	for(int j=0;j<5;j++)	// 每次得到一个ARDS中的成员，共5个成员
	mov	edi, ARDStruct		;	{			// 依次显示：BaseAddrLow，BaseAddrHigh，LengthLow，LengthHigh，Type
.1:					        ;
	push	dword [esi]		;
	call	DispInt			;		DispInt(MemChkBuf[j*4]); // 显示一个成员
	pop	eax			        ;
	stosd				    ;		ARDStruct[j*4] = MemChkBuf[j*4];
	add	esi, 4			    ;
	dec	edx			        ;
	cmp	edx, 0			    ;
	jnz	.1			        ;	}
	call	DispReturn		;	printf("\n");
	cmp	dword [dwType], 1	;	if(Type == AddressRangeMemory) // AddressRangeMemory : 1, AddressRangeReserved : 2
	jne	.2			        ;	{
	mov	eax, [dwBaseAddrLow];
	add	eax, [dwLengthLow]	;
	cmp	eax, [dwMemSize]	;		if(BaseAddrLow + LengthLow > MemSize)
	jb	.2			        ;
	mov	[dwMemSize], eax	;			MemSize = BaseAddrLow + LengthLow;
.2:					        ;	}
	loop	.loop			;}
					        ;
	call	DispReturn		;printf("\n");
	push	szRAMSize		;
	call	DispStr			;printf("RAM size:");
	add	esp, 4			    ;
					        ;
	push	dword [dwMemSize];
	call	DispInt			;DispInt(MemSize);
	add	esp, 4			    ;

	pop	ecx
	pop	edi
	pop	esi
	ret
; ---------------------------------------------------------------------------

; 启动分页机制  ！！！为简化处理, 所有线性地址对应相等的物理地址. 并且不考虑内存空洞.--------------------------------------------------------------
SetupPaging:
	; 根据内存大小计算应初始化多少PDE以及多少页表
	xor	edx, edx
	mov	eax, [dwMemSize]
	mov	ebx, 400000h	; 400000h = 4M = 4096 * 1024, 一个页表对应的内存大小
	div	ebx
	mov	ecx, eax	    ; 此时 ecx 为页表的个数，也即 PDE 应该的个数
	test	edx, edx
	jz	.no_remainder
	inc	ecx		        ; 如果余数不为 0 就需增加一个页表
.no_remainder:
	push	ecx		    ; 暂存页表个数

	

	; 首先初始化页目录
	mov	ax, SelectorFlatRW
	mov	es, ax
	mov	edi, PageDirBase	; 此段首地址为 PageDirBase
	xor	eax, eax
	mov	eax, PageTblBase | PG_P  | PG_USU | PG_RWW
.1:
	stosd
	add	eax, 4096		; 为了简化, 所有页表在内存中是连续的.
	loop	.1

	; 再初始化所有页表
	pop	eax			; 页表个数
	mov	ebx, 1024		; 每个页表 1024 个 PTE
	mul	ebx
	mov	ecx, eax		; PTE个数 = 页表个数 * 1024
	mov	edi, PageTblBase	; 此段首地址为 PageTblBase
	xor	eax, eax
	mov	eax, PG_P  | PG_USU | PG_RWW
.2:
	stosd
	add	eax, 4096		; 每一页指向 4K 的空间
	loop	.2

	mov	eax, PageDirBase
	mov	cr3, eax
	mov	eax, cr0
	or	eax, 80000000h
	mov	cr0, eax
	jmp	short .3
.3:
	nop

	ret
; 分页机制启动完毕 ----------------------------------------------------------



; InitKernel ---------------------------------------------------------------------------------
; 将 KERNEL.BIN 的内容经过整理对齐后放到新的位置
; --------------------------------------------------------------------------------------------
InitKernel:	; 遍历每一个 Program Header，根据 Program Header 中的信息来确定把什么放进内存，放到什么位置，以及放多少。
	xor	esi, esi
	mov	cx, word [BaseOfKernelFilePhyAddr + 2Ch]; ┓ ecx <- pELFHdr->e_phnum
	movzx	ecx, cx					            ; ┛
	mov	esi, [BaseOfKernelFilePhyAddr + 1Ch]	; esi <- pELFHdr->e_phoff
	add	esi, BaseOfKernelFilePhyAddr		    ; esi <- OffsetOfKernel + pELFHdr->e_phoff
.Begin:
	mov	eax, [esi + 0]
	cmp	eax, 0				; PT_NULL
	jz	.NoAction
	push	dword [esi + 010h]		; size	┓
	mov	eax, [esi + 04h]		       ;	┃
	add	eax, BaseOfKernelFilePhyAddr	;	┣ ::memcpy(	(void*)(pPHdr->p_vaddr),
	push	eax				       ; src	┃		uchCode + pPHdr->p_offset,
	push	dword [esi + 08h]		; dst	┃		pPHdr->p_filesz;
	call	MemCpy			         	;	┃
	add	esp, 12				           ;	┛
.NoAction:
	add	esi, 020h			; esi += pELFHdr->e_phentsize
	dec	ecx
	jnz	.Begin

	ret
; InitKernel ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

; SECTION .data1 之开始 ---------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
SECTION .data1 align=32
;-------------------------------------------------------------------------------

LABEL_DATA:
; 实模式下使用这些符号
; 字符串
_szMemChkTitle:			db	"BaseAddrL BaseAddrH LengthLow LengthHigh   Type", 0Ah, 0
_szRAMSize:			db	"RAM size:", 0
_szReturn:			db	0Ah, 0
;; 变量
_dwMCRNumber:			dd	0	; Memory Check Result
_dwDispPos:			dd	(80 * 6 + 0) * 2	; 屏幕第 6 行, 第 0 列。
_dwMemSize:			dd	0
_ARDStruct:			; Address Range Descriptor Structure
	_dwBaseAddrLow:		dd	0
	_dwBaseAddrHigh:	dd	0
	_dwLengthLow:		dd	0
	_dwLengthHigh:		dd	0
	_dwType:		dd	0
_MemChkBuf:	times	256	db	0
;
;; 保护模式下使用这些符号
szMemChkTitle	equ	BaseOfLoaderPhyAddr + _szMemChkTitle  + OffsetOfLoader
szRAMSize		equ	BaseOfLoaderPhyAddr + _szRAMSize + OffsetOfLoader
szReturn		equ	BaseOfLoaderPhyAddr + _szReturn + OffsetOfLoader
dwDispPos		equ	BaseOfLoaderPhyAddr + _dwDispPos + OffsetOfLoader
dwMemSize		equ	BaseOfLoaderPhyAddr + _dwMemSize + OffsetOfLoader
dwMCRNumber		equ	BaseOfLoaderPhyAddr + _dwMCRNumber + OffsetOfLoader
ARDStruct		equ	BaseOfLoaderPhyAddr + _ARDStruct + OffsetOfLoader
	dwBaseAddrLow	equ	BaseOfLoaderPhyAddr + _dwBaseAddrLow + OffsetOfLoader
	dwBaseAddrHigh	equ	BaseOfLoaderPhyAddr + _dwBaseAddrHigh + OffsetOfLoader
	dwLengthLow	    equ	BaseOfLoaderPhyAddr + _dwLengthLow + OffsetOfLoader
	dwLengthHigh	equ	BaseOfLoaderPhyAddr + _dwLengthHigh + OffsetOfLoader
	dwType		    equ	BaseOfLoaderPhyAddr + _dwType + OffsetOfLoader
MemChkBuf		equ	BaseOfLoaderPhyAddr + _MemChkBuf + OffsetOfLoader


; 堆栈就在数据段的末尾
StackSpace:	times	1000h	db	0
TopOfStack	equ	BaseOfLoaderPhyAddr + $	+ OffsetOfLoader; 栈顶
; SECTION .data1 之结束 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


;-------------------------------------------------------------------------------
SECTION trail
;-------------------------------------------------------------------------------
LABEL_CODE_END: