# NEYOS

NEYOS是为了学习和实践现代操作系统理论而开发的内核。基于英特尔x86 32位CPU，目前只实现了启动MBR以及简单的进程，未来有时间会逐渐完善。

最终的目标是实现类Minix式的微内核多任务操作系统，并且兼容POSIX接口。

### 特性

- [x] 硬件引导启动 
- [x] 分页式内存管理
- [x] 保护模式
- [x] 进程结构
- [ ] IO模块
- [ ] 进程间通信
- [ ] 文件系统
- [ ] 高级内存管理
- [ ] 基于message的微内核架构
- [ ] POSIX接口

### 说明

#### 硬件引导启动

系统加电后BIOS系统会进行自检，检查通过后会跳转到内存的0x7c00处，读取booter。由于历史原因booter大小只能是512个字节，所以booter读取到内存中并执行后，一般就是继续加载内核image文件。这里一般系统都会运行文件系统，不过为了简化，这里直接使用了固定地址。

这个系统的设计和实现主要参考了于渊的《Orange's 一个操作系统的实现》，它的设计是使用一个基于软驱的文件系统。但是现在软驱很多90后未必都见过，实现有点过时，所以启动部分参考了《x86汇编语言，从实模式到保护模式》，直接使用虚拟硬盘，用作者的`fixvhdwr`工具把系统镜像写入固定LBA逻辑区块。然后loader用bios提供的中断功能从固定LBA地址读取系统镜像。

#### 分页式内存管理

内存管理部分简化了实现，直接把实时模式的地址空间映射到了保护模式下。

#### 保护模式&进程结构

从这一部分开始就能用C语言了，在C语言使用到的部分系统库用汇编语言实现。写过汇编后，才发现C语言的开发效率是真的高😀

进程相关的是CPU的TSS寄存器，以及进程结构体，这部分对应的C语言的`s_tss` 和 `s_stackframe`结构体。现在TSS仅仅用到了栈寄存器（应该是用任务门切换任务的做法已经不常见了），寄存器保存工作都是使用s_stackframe实现的。

#### 接下来的等以后有空再写......

