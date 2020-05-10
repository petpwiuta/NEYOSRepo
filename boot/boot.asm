
         
%include "load.inc"

;===============================================================================
SECTION header align=16 vstart=0x7c00
	; 清屏
	mov	ax, 0600h		; AH = 6,  AL = 0h
	mov	bx, 0700h		; 黑底白字(BL = 07h)
	mov	cx, 0			; 左上角: (0, 0)
	mov	dx, 0184fh		; 右下角: (80, 50)
	int	10h			    ; int 10h

	mov	dh, 0			; "Start Loading MBR......  "
	call	DispStr	    ; 显示字符串

	;从硬盘指定lba加载loader，仅仅加载512byte，再读取laoder中的长度字段决定是否需要加载剩余部分
	xor di,di
    mov si,LoaderLBA       ;lba
	mov ax,BaseOfLoader
    mov ds,ax              ;segment
	mov bx,OffsetOfLoader  ;offset
    call ReadHardDisk0

	mov	dh, 1			; "load first block success"
	call	DispStr	    


	;看是否有必要加载剩下的部分
	mov ax,5533
	cmp ax,DiskBlockSize         ;前面两个字节是loader长度，用来判断是否还需要下载剩下的
	jbe LABEL_MBR_ONLY_ONE_BLOCK  ;ax <= 512


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
		xor ecx,ecx
		mov cx,ax

		mov ax,LoaderLBA
		inc ax
		mov dx,OffsetOfLoader
		add dx,DiskBlockSize
		mov di,BaseOfLoader
		mov ds,di              ;segment
		xor di,di
   		
		LABEL_READ_REMAIN_BLOCK:
    		mov si,ax   ;lba
			mov bx,dx  ;offset
    		call ReadHardDisk0
			inc ax
			add dx,DiskBlockSize
		loop LABEL_READ_REMAIN_BLOCK


	mov	dh, 3			; "load remina block success"
	call	DispStr	    
	jnz LABEL_JMP_INTO_LOADER


LABEL_MBR_ONLY_ONE_BLOCK:
	;只有一个block
	mov	dh, 2			; "load first block success"
	call	DispStr	    
	jmp LABEL_JMP_INTO_LOADER


LABEL_JMP_INTO_LOADER:
; *****************************************************************************************************
	jmp	BaseOfLoader:OffsetOfLoader	; 这一句正式跳转到已加载到内存中的 LOADER.BIN 的开始处
						; 开始执行 LOADER.BIN 的代码
						; Boot Sector 的使命到此结束
; *****************************************************************************************************


;============================================================================
;字符串
;----------------------------------------------------------------------------
; 为简化代码, 下面每个字符串的长度均为 MessageLength
MessageLength		equ	24
BootMessage:		db	"Start Loading MBR...... "; 24字节, 不够则用空格补齐. 序号 0
Message1     		db	"Load MBR First Block OK."; 24字节, 不够则用空格补齐. 序号 1
Message2		    db	"MBR Only One Block.     "; 24字节, 不够则用空格补齐. 序号 2
Message3		    db	"Load Remain Block OK.   "; 24字节, 不够则用空格补齐. 序号 3
;============================================================================


;----------------------------------------------------------------------------
; 函数名: DispStr
;----------------------------------------------------------------------------
; 作用:
;	显示一个字符串, 函数开始时 dh 中应该是字符串序号(0-based)
DispStr:
	push ax
	push bx
	push cx
	push dx
	push bp
	push ds
	push es

	mov	ax, MessageLength
	mul	dh
	add	ax, BootMessage
	mov	bp, ax			; ┓
	mov	ax, ds			; ┣ ES:BP = 串地址
	mov	es, ax			; ┛
	mov	cx, MessageLength	; CX = 串长度
	mov	ax, 01301h		; AH = 13,  AL = 01h
	mov	bx, 0007h		; 页号为0(BH = 0) 黑底白字(BL = 07h)
	mov	dl, 0
	int	10h			; int 10h

	pop es
	pop ds
	pop bp
	pop dx
	pop cx
	pop bx
	pop ax

	ret


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

;-------------------------------------------------------------------------------                             
         times 510-($-$$) db 0
                          db 0x55,0xaa