

address8254  equ 280h

a8255        equ 288H    ;8255 A口    
c8255        equ 28aH    ;8255 C口
k8255        equ 28bH    ;8255控制口
IO_ADDRESS      equ  288h

data segment
HZ_TAB1     DW 0A1A0H,0A1A0H,0A1A0H,0A1A0H,0A1A0H,0A1A0H,0A1A0H,0A1A0H  ;空白行
            DW 0BFAAH,0CBF8H,0A1A0H,0A1A0H,0A1A0H,0A1A0H,0A1A0H,0A1A0H  ;开锁
HZ_TAB2     DW 0A1A0H,0A1A0H,0A1A0H,0A1A0H,0A1A0H,0A1A0H,0A1A0H,0A1A0H  ;空白行
            DW 0B4EDH,0CEF3H,0A1A0H,0A1A0H,0A1A0H,0A1A0H,0A1A0H,0A1A0H  ;错误
HZ_TAB3     DW 0A1A0H,0A1A0H,0A1A0H,0A1A0H,0A1A0H,0A1A0H,0A1A0H,0A1A0H  ;空白行
            DW 0A3B9H,0C3EBH,0BAF3H,0D6D8H,0CAD4H,0A1A0H,0A1A0H,0A1A0H  ;9秒后重试
HAIGUAN     DW 0A1A0H,0A1A0H,0A1A0H,0A1A0H,0A1A0H,0A1A0H,0A1A0H,0A1A0H  ;空白行
            DW 0BAA3H,0B9D8H,0BFAAH,0CBF8H,0A1A0H,0A1A0H,0A1A0H,0A1A0H  ;海关开锁
XIUGAI      DW 0A1A0H,0A1A0H,0A1A0H,0A1A0H,0A1A0H,0A1A0H,0A1A0H,0A1A0H  ;空白行
            DW 0D0DEH,0B8C4H,0C3DCH,0C2EBH,0A1A0H,0A1A0H,0A1A0H,0A1A0H  ;修改密码

NUM         DW 0A3B0H,0A3B4H,0A3B8H,0A1A0H,0A3B1H,0A3B5H,0A3B9H,0A1A0H  ;0，4，8，
            DW 0A3B2H,0A3B6H,0A1A0H,0A1A0H,0A3B3H,0A3B7H,0A1A0H,0A1A0H  ;
KONG        DW 0A1A0H,0A1A0H,0A1A0H,0A1A0H,0A1A0H,0A1A0H,0A1A0H,0A1A0H  ;空白行
            DW 0A1A0H,0A1A0H,0A1A0H,0A1A0H,0A1A0H,0A1A0H,0A1A0H,0A1A0H  ;空白行
DJS         DW 0A3B0H,0A3B1H,0A3B2H,0A3B3H,0A3B4H,0A3B5H,0A3B6H,0A3B7H  ;0-9
            DW 0A3B8H,0A3B9H
SRMM        DW 0A1A0H,0A1A0H,0A1A0H,0A1A0H,0A1A0H,0A1A0H,0A1A0H,0A1A0H  ;空白行
            DW 0CAE4H,0C8EBH,0C3DCH,0C2EBH,0A1A0H,0A1A0H,0A1A0H,0A1A0H  ;输入密码
XGCG        DW 0A1A0H,0A1A0H,0A1A0H,0A1A0H,0A1A0H,0A1A0H,0A1A0H,0A1A0H  ;空白行
            DW 0D0DEH,0B8C4H,0B3C9H,0B9A6H,0A1A0H,0A1A0H,0A1A0H,0A1A0H  ;修改成功
HZ_ADR      DB  ? 

table1      dw 0770h,0B70h,0D70h,0E70h,07B0h,0BB0h,0DB0h,0EB0h
            dw 07D0h,0BD0h,0DD0h,0ED0h,07E0h,0BE0h,0DE0h,0EE0h    ;键盘扫描码表
LED         DB 3FH,06H,5BH,4FH,66H,6DH,7DH,07H,7FH,6FH,77H,7CH
            DB 39h,5EH,79h,71h,0ffh   ;LED段码表，0,1,2,3,4,5,6,7,8,9,a,b,c,d,e,f
char        db '0123456789ABCDEF'                    ;字符表
mes         db 0ah,0dh,'PLAY ANY KEY IN THE SMALL KEYBOARD! ',0ah,0dh
            db 'IT WILL BE ON THE SCREEN! END WITH E ',0ah,0dh,'$'

count0 dw 00h 
count1 dw 00h
hglock db 00h
key_in db 00h 
buffer db '000000'
string0 db '000000'
string1 db '000000'

data ends

stacks segment stack    ;堆栈空间
    dd 100 dup (?)
stacks ends

code segment
    assume cs:code,ds:data,ss:stacks,es:data
    
start:
    cli 
    mov ax,data
    mov ds,ax
    mov es,ax
    mov ax,stacks
    mov ss,ax
    mov dx,offset mes
    mov ah,09
    int 21

start1:                     ;初始化8255
    mov dx,k8255
    mov al,81h
    out dx,al

    mov si,offset buffer
    mov byte ptr HZ_ADR,90H ;lcd的显示在第二行


;输入密码块
begin:                      ;输入密码，主程序段
    ;显示输入密码
    MOV AX,DATA
    MOV DS,AX               
    MOV DX,IO_ADDRESS
    ADD DX,3
    MOV AL,80H
    OUT DX,AL                   ;8255初始化
    CALL CLEAR                  ;LCD 清除
          ;     CALL FUNCUP              ;LCD 功能设置
    LEA BX,  srmm
    MOV CH,2                    ;显示第2行信息 
    CALL  LCD_DISP
    LEA BX, srmm
    MOV CH,3                    ;显示第3行信息
    CALL LCD_DISP


    mov dx,k8255
    mov al,81h
    out dx,al
 
    
    call key
    call disply            ;读到[key_in]中
    mov cl,[key_in]         ;读到cl

    cmp cl,'A'              ;比对按键
    jz renz                 ;A 确认 B 返回 C 海关锁
    cmp cl,'B'
    jz backbegin
    cmp cl,'C'
    jz hgset
    
    ;putbuf block
    ;mov bx,[count0]         ;若都不是，则放入buffer中
    mov di,offset buffer
    mov [di],cl
    call dis_num            ;输入值后显示该数字
    inc di
    cmp di,06H
    jnz begin
    


jbegin:                     ;jump to begin
    jmp begin

renz:                       ;跳转到密码比对
    jmp compare


backbegin:                  ;海关锁置0，count0置0，返回begin
    mov [hglock],00h 
    mov [count0],00h
    mov byte ptr HZ_ADR,90H ;lcd的显示在第二行
    jmp jbegin

hgset:                      ;设海关变量hglock为1
    mov [hglock],01h
    jmp jbegin 
    
    

compare:                    ;密码比对程序
    mov al,[hglock]         ;先根据hglock变量判断比对的是string0还是string1
    cmp al,01h 
    jz hgpare
    mov si,offset buffer
    mov di,offset string0
    jmp gopare
hgpare:
    mov si,offset buffer
    mov di,offset string1
    jmp gopare

gopare:
    mov cx,00H
compareloop:                   ;string0/string1 和buffer比较
    cmp cx,0006h
    jz succ_rep
    mov al,[si]
    mov bl,[di]
    inc cx
    inc si
    inc di
    cmp al,bl
    jz compareloop
    jmp erro                ;jmpto密码错误
    succ_rep:jmp succ       ;jmpto成功开锁

erro:
errostart:
    
    ;mov ax,[count1]         ;这一段是错误3次，倒计时9秒重试
    ;cmp ax,03h              ;还没完成，先不动
    ;jz gopause              ;8254怎么接？


         MOV AX,DATA
                MOV DS,AX               
                MOV DX,IO_ADDRESS
                ADD DX,3
                MOV AL,80H
                OUT DX,AL                       ;8255初始化
               ;mov al,0ffh
               ;mov dx,300H
               ;out dx, al
                CALL CLEAR              ;LCD 清除
          ;     CALL FUNCUP              ;LCD 功能设置
                LEA BX, hz_tab2
                MOV CH,2                        ;显示第2行信息 
                CALL  LCD_DISP
                LEA BX, hz_tab2
                MOV CH,3                  ;    显示第3行信息
                CALL LCD_DISP





    mov dx,k8255
    mov al,81h
    out dx,al
 
    
    call key
    call disply            ;读到[key_in]中
    mov cl,[key_in]  
    cmp cl,'B'              ;如果是B就跳转 
    jz backbegin            ;跳转回到begin，顺便重置了count0和hglock
    jmp backbegin

myjmp:
    jmp backbegin
    
 mydelay proc
      push bx
      push cx
      mov bx,400h
 for1:mov cx,0ffffh
 for2:loop for2
      dec bx
      jnz for1
      pop cx
      pop bx
      ret
 mydelay endp


succ:
succstart:
    
    mov al,[hglock]
    cmp al,00H
    jz unlock
    jmp hgunlock
    
hgunlock:
            MOV AX,DATA
                MOV DS,AX               
                MOV DX,IO_ADDRESS
                ADD DX,3
                MOV AL,80H
                OUT DX,AL                       ;8255初始化
               ;mov al,0ffh
               ;mov dx,300H
               ;out dx, al
                CALL CLEAR              ;LCD 清除
          ;     CALL FUNCUP              ;LCD 功能设置
                LEA BX, haiguan
                MOV CH,2                        ;显示第2行信息 
                CALL  LCD_DISP
                LEA BX, haiguan
                MOV CH,3                  ;    显示第3行信息
                CALL LCD_DISP
        jmp succmain
unlock:
    MOV AX,DATA
                MOV DS,AX               
                MOV DX,IO_ADDRESS
                ADD DX,3
                MOV AL,80H
                OUT DX,AL                       ;8255初始化
               ;mov al,0ffh
               ;mov dx,300H
               ;out dx, al
                CALL CLEAR              ;LCD 清除
          ;     CALL FUNCUP              ;LCD 功能设置
                LEA BX,  hz_tab1
                MOV CH,2                        ;显示第2行信息 
                CALL  LCD_DISP
                LEA BX, hz_tab1
                MOV CH,3                  ;    显示第3行信息
                CALL LCD_DISP
    jmp succmain

succmain:
    ;开锁状态下的主程序，可以选择返回也可以选择
     mov dx,k8255
    mov al,81h
    out dx,al
 
    
    call key
    call disply            ;读到[key_in]中
    mov cl,[key_in]
      
    cmp cl,'B'              ;如果是B就跳转 
    jz myjmp            ;跳转回到begin，顺便重置了count0和hglock
    cmp cl,'D'
    jz changepin

    jmp succ 
    

changepin:
                MOV AX,DATA
                MOV DS,AX               
                MOV DX,IO_ADDRESS
                ADD DX,3
                MOV AL,80H
                OUT DX,AL                       ;8255初始化
               ;mov al,0ffh
               ;mov dx,300H
               ;out dx, al
                CALL CLEAR              ;LCD 清除
          ;     CALL FUNCUP              ;LCD 功能设置
                LEA BX, xiugai
                MOV CH,2                        ;显示第2行信息 
                CALL  LCD_DISP
                LEA BX, xiugai
                MOV CH,3                  ;    显示第3行信息
                CALL LCD_DISP
    jmp modify

modify:
    ;正式修改密码
     mov dx,k8255
    mov al,81h
    out dx,al
 
    
    call key
    call disply            ;读到[key_in]中
    mov cl,[key_in]
    
    cmp cl,'B'
    jz backtosucc
    cmp cl,'A'
    jz mkchange

        ;若都不是，则放入buffer中
    mov di,offset buffer
    mov [di],cl
    call dis_star       ;输入值后显示*号
    inc di
    jnz modify


myjmp1:
    jmp myjmp

backtosucc:
    jmp succ
mkchange:
    mov di,offset buffer
    mov al,[hglock]
    cmp al,01h 
    jz hgchange
    mov si,offset string0
    jmp changestart
hgchange:
    mov si,offset string1
    
changestart:
    mov cx,06h
changeloop:
    mov al,[di]
    mov [si],al
    inc di
    inc si
    loop changeloop
    jmp changesucc

changesucc:
     
    MOV AX,DATA
                MOV DS,AX               
                MOV DX,IO_ADDRESS
                ADD DX,3
                MOV AL,80H
                OUT DX,AL                       ;8255初始化
               ;mov al,0ffh
               ;mov dx,300H
               ;out dx, al
                CALL CLEAR              ;LCD 清除
          ;     CALL FUNCUP              ;LCD 功能设置
                LEA BX, xgcg
                MOV CH,2                        ;显示第2行信息 
                CALL  LCD_DISP
                LEA BX, xgcg
                MOV CH,3                  ;    显示第3行信息
                CALL LCD_DISP


    mov dx,k8255
    mov al,81h
    out dx,al
 
    
    call key
    call disply            ;读到[key_in]中
    mov cl,[key_in]  
    
    
    cmp cl,'B'                ;按任意键，回到“输入密码状态”
    jz myjmp1
    jmp myjmp1


key proc 
key_loop:
        mov ah,1
        int 16h
        jnz exit                    ;pc键盘有键按下则退出
       
        mov dx,c8255
        mov al,0fh
        out dx,al
        in al,dx                    ;读行扫描值
        and al,0fh
        cmp al,0fh
        jz key_loop                 ;未发现有键按下则转
        call delay                  ;delay for amoment
        mov ah,al
        MOV DX,k8255
        mov al,88h
        out dx,al
        mov dx,c8255
        mov al,ah
        or al,0f0h
        out dx,al
        in al,dx                    ;读列扫描值
        and al,0f0h
        cmp al,0f0h
        jz key_loop                 ;未发现有键按下则转
        
        mov si,offset table1        ;键盘扫描码表首址
        mov di,offset char          ;字符表首址
        mov cx,16                   ;待查表的表大小
key_tonext:
        cmp ax,[si]                 ;cmp (col,row) with every word
        jz key_findkey              ;in the table
        dec cx
        jz key_loop                 ;未找到对应扫描码
        add si,2
        inc di
        jmp key_tonext
key_findkey:
        mov dl,[di]
        mov ah,02
        int 21h            ;显示查找到的键盘码
        mov byte ptr key_in,dl
key_waitup:
        MOV DX,k8255
        mov al,81h
        out dx,al
        mov dx,c8255
        mov al,0fh
        out dx,al
        in al,dx           ;读行扫描值
        and al,0fh
        cmp al,0fh
        jnz key_waitup     ;按键未抬起转
        call delay         ;delay for amoment
        ret
exit:        mov byte ptr key_in,'E'
        ret
        
        
DISPLY    PROC 
        PUSH ax
        MOV BX,OFFSET LED
        MOV AL,byte ptr key_in
      SUB al,30h
      CMP al,09h
      JNG  DIS2
       SUB al,07h
DIS2:  XLAT
         MOV DX,a8255
         OUT DX,AL                     ;输出显示数据，段码
       POP AX
         RET
DISPLY    ENDP
key endp
delay proc 
        push ax            ;delay 50ms--100ms
        mov ah,0
        int 1ah
        mov bx,dx
delay1:
        mov ah,0
        int 1ah
        cmp bx,dx
        jz delay1
        mov bx,dx
delay2:
        mov ah,0
        int 1ah
        cmp bx,dx
        jz delay2
        pop ax
       ret
delay endp



CLEAR           PROC
                MOV AL,0ch
                MOV DX, IO_ADDRESS
                OUT DX,AL               ;设置CLEAR命令
                CALL CMD_SETUP          ;启动LCD执行命令
                RET
CLEAR           ENDP

FUNCUP          PROC
         ;      MOV AL, 0fH             ;LCD功能设置命令
         ;      OUT DX, AL
         ;      CALL CMD_SETUP
                MOV AL, 34H             ;LCD显示状态命令
                OUT DX, AL
                CALL CMD_SETUP
                RET
FUNCUP           ENDP

LCD_DISP        PROC
                CMP CH, 2
                JZ  DISP_SEC
                MOV BYTE PTR HZ_ADR, 88H        ;第三行起始端口地址
                ADD BX,16                        ;指向第二行信息
                JMP  next
DISP_SEC:       MOV BYTE PTR HZ_ADR,90H
next:           mov cl,8
continue:       push cx
                MOV AL,HZ_ADR
                MOV DX, IO_ADDRESS
                OUT DX, AL
                CALL CMD_SETUP          ;设定DDRAM地址命令
                MOV AX,[BX]
                PUSH AX
                MOV AL,AH               ;先送汉字编码高位
                MOV DX,IO_ADDRESS
                OUT DX,AL
                CALL DATA_SETUP         ;输出汉字编码高字节
                CALL delayx             ;延迟
                POP AX
                MOV DX,IO_ADDRESS
                OUT DX, AL
                CALL DATA_SETUP         ;输出汉字编码低字节
                CALL DELAYx
                INC BX
                INC BX                  ;修改显示内码缓冲区指针
                INC BYTE PTR HZ_ADR     ;修改LCD显示端口地址
                POP CX
                DEC CL
                JNZ  COntinue
                RET
LCD_DISP   ENDP

CMD_SETUP       PROC
                MOV DX,IO_ADDRESS                ;指向8255端口控制端口
                ADD DX,2
                NOP
                MOV AL,00000000B                ;PC1置0,pc0置0 （LCD I端=0，W端＝0）
                OUT DX, AL
                call delayx
                NOP
                MOV AL,00000100B                ;PC2置1 （LCD E端＝1）
                OUT DX, AL
                NOP
                call delayx
                MOV AL, 00000000B               ;PC2置0,（LCD E端置0）
                OUT DX, AL
                call delayx

                RET
CMD_SETUP       ENDP

DATA_SETUP      PROC
                MOV DX,IO_ADDRESS                ;指向8255控制端口
                ADD DX,2
                MOV AL,00000001B                ;PC1置0，PC0=1 （LCD I端=1）
                OUT DX, AL
                NOP
                call delayx
                MOV AL,00000101B                ;PC2置1 （LCD E端＝1）
                OUT DX, AL
                NOP
                call delayx
                MOV AL, 00000001B               ;PC2置0,（LCD E端＝0）
                OUT DX, AL
                NOP
                call delayx
                RET
DATA_SETUP      ENDP

DELAYx          PROC
                push cx
                push dx
                MOV CX, 0fffh
 x1:           loop   x1
                pop dx
                pop cx
                RET
DELAYx          ENDP


dis_num proc
    mov al,0ffh
    mov dx,IO_ADDRESS
    out dx,al

    mov al,HZ_ADR
    mov dx,IO_ADDRESS
    out dx,al
    call CMD_SETUP

    mov cl,[key_in]
    sub cl,30h 
    mov ch,00h
    mov bx,cx

    mov ax,djs[bx]
    push ax
    mov al,ah
    mov dx,IO_ADDRESS
    out dx,al
    call DATA_SETUP
    call delay2

    pop ax
    mov dx,IO_ADDRESS
    out dx,al
    call DATA_SETUP
    call delay2
    inc byte ptr HZ_ADR
    ret
dis_num endp

dis_star proc
mov al,0ffh
    mov dx,IO_ADDRESS
    out dx,al

    mov al,HZ_ADR
    mov dx,IO_ADDRESS
    out dx,al
    call CMD_SETUP

    mov ax,0a3aah
    push ax

    mov al,ah           ;输出高位
    mov dx,IO_ADDRESS
    out dx,al
    call DATA_SETUP
    call delay2

    pop ax              ;输出低位
    mov dx,IO_ADDRESS
    out dx,al
    call DATA_SETUP
    call delay2
    inc byte ptr HZ_ADR ;修改显示端口地址
    ret
dis_star endp



code ends
end start
