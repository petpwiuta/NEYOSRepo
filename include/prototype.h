
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                            prototype.h
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                                                              Ney 2020
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

#include "typedef.h"

/* klib.asm */
PUBLIC void	out_byte(u16 port, u8 value);
PUBLIC u8	in_byte(u16 port);
PUBLIC void	disp_str(char * info);
PUBLIC void	disp_color_str(char * info, int color);
PUBLIC void disable_irq(int irq);
PUBLIC void enable_irq(int irq);

/* interrupt.c */
PUBLIC void	init_prot();
PUBLIC u32	seg2phys(u16 seg);

/* klib.c */
PUBLIC void disp_int(int input);
PUBLIC void	delay(int time);

/* string.asm */
PUBLIC char* strcpy(char* p_dst, char* p_src);
PUBLIC void memset(void* p_dst, char ch, int size);

/* kernel.asm */
void restart();

/* main.c */
void TestA();
void TestB();
void TestC();

/* i8259.c */
PUBLIC void init_8259A();
PUBLIC void put_irq_handler(int irq, irq_handler handler);
PUBLIC void spurious_irq(int irq);

/* clock.c */
PUBLIC void milli_delay(int milli_sec);
PUBLIC void clock_handler(int irq);


/* 以下是系统调用相关 */

/* process.c */
PUBLIC  int     sys_get_ticks();        /* sys_call */
PUBLIC void     schedule();

/* syscall.asm */
PUBLIC  void    sys_call();             /* int_handler */
PUBLIC  int     get_ticks();

