
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                            typedef.h
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                                                              Ney 2020
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

#ifndef	_NEY_TYPEDEF_H_
#define	_NEY_TYPEDEF_H_


/* EXTERN */
#define	EXTERN	extern	/* EXTERN is defined as extern except in global.c */

/* 函数类型 */
#define	PUBLIC		/* PUBLIC is the opposite of PRIVATE */
#define	PRIVATE	static	/* PRIVATE x limits the scope of x */

/* Boolean */
#define	TRUE	1
#define	FALSE	0


typedef	unsigned int		u32;
typedef	unsigned short		u16;
typedef	unsigned char		u8;


typedef	void	(*int_handler)	();
typedef	void	(*task_f)	();
typedef	void	(*irq_handler)	(int irq);

typedef void*	system_call;


#endif /* _NEY_TYPEDEF_H_ */
