IDEAL
MODEL small
STACK 100h
DATASEG


; [\x12][\x05][512+7]
input DB 20 DUP(?)

num1  DW ?
num2  DW ?
op    DB ?


CODESEG
proc calculate
	push bp
	mov bp, sp
	mov dx, [word ptr bp+4]
	mov ax, [word ptr bp+6]
	mov bx, [word ptr bp+8]

	xor cx, cx

	cmp dx, '!'
	je factorial_tav
		
	cmp dx, '+'
	je add_tav
		
	cmp dx, '-'
	je sub_tav
		
	cmp dx, '/'
	je div_tav
		
	cmp dx, '*'
	je mul_tav
		
	cmp dx, 's'
	je sqrt_tav
		
	cmp dx, 'p'
	je pow_tav

	cmp dx, 'h'
	je help_tav

	jmp no_tav:

	factorial_tav:
			
	jmp end_calc

	add_tav:
		add ax, bx
		mov cx, ax

	jmp end_calc
		
	sub_tav:
		sub ax, bx
		mov cx, ax
		
	jmp end_calc

	div_tav:

	jmp end_calc

	mul_tav:

	jmp end_calc

	sqrt_tav:

	jmp end_calc

	pow_tav:

	jmp end_calc

	abs_tav:
		neg ax
		mov cx, ax

	jmp end_calc

	no_tav:
		; print to try again and back to the start


	end_calc:

	pop bp
	ret
endp

proc get_input
	mov dx, offset input
	mov bx, dx
	mov [byte ptr bx], 18
	mov ah, 0Ah
	int 21h
	inc dx
	mov ax, dx
	ret
endp get_input

proc read_number
	; this function gets the offset of the input array: pass by reference
	mov ax, 0 ; the result
_input_loop:
	xor ch, ch
	mov cl, [byte ptr bx]
	cmp cl, '0'
	jl _input_end
	cmp cl, '9'
	jg _input_end
	mov dx, 10
	mul dx
	sub cl, '0'
	add ax, cx
	inc bx
	jmp _input_loop
_input_end:
	ret
endp read_number

start:
    mov ax, @data
    mov ds, ax
	call get_input
	mov di, ax ; save input+1 in di
	cmp di, 0
	je exit ; empty input
	
	; read num1
	mov bx, di
	inc bx
	call read_number
	push bx
	mov bx, offset num1
	mov [word ptr bx], ax
	pop bx
	
	; read operator
	mov cl, [byte ptr bx]
	inc bx
	push bx
	mov bx, offset op
	mov [byte ptr bx], cl
	pop bx
	
	; read num2
	call read_number
	push bx
	mov bx, offset num2
	mov [word ptr bx], ax
	pop bx
	
	; calculate
	push ax ; num2
	mov bx, offset num1
	mov ax, [word ptr bx]
	push ax ; num1
	mov bx, offset op
	mov cl, [byte ptr bx]
	xor ch, ch
	push cx ; op
	call calculate
	add sp, 6
	
exit:
    mov ax, 4c00h
    int 21h
END start
