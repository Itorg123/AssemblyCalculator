IDEAL
MODEL small
STACK 100h
DATASEG


; [\x12][\x05][512+7]
input DB 20 DUP(?)

num1  DW ?
num2  DW ?
op    DB ?

last_answer DW ?

help_message DB 'help: ', 10, 13
			 DB 'for add write: num1+num2 ', 10, 13
			 DB 'for sub write: num1-num2', 10, 13
			 DB 'for mul write: num1*num2', 10, 13
			 DB 'for div write: num1/num2', 10, 13
			 DB 'for sqrt write: num1s', 10, 13
			 DB 'for pow write: num1 p num2 without space', 10, 13
			 DB 'for factorial write: num1!', 10, 13
			 DB 'if you want to use the answer as num, write _', 10, 13, '$'
			 


welcome_message	DB '-------------------------', 10, 13
				DB '|      calculator++     |', 10, 13
				DB '|                       |', 10, 13
				DB '| 7         8         9 |', 10, 13
				DB '|                       |', 10, 13
				DB '| 4         5         6 |', 10, 13
				DB '|                       |', 10, 13
				DB '| 1         2         3 |', 10, 13
				DB '|                       |', 10, 13
				DB '-------------------------', 10, 13, '$'

			 
			 
error_message DB 'try again, you can use the help by write h', 10, 13, '$'

CODESEG

proc print_new_line
	push ax
	push dx
	; print \r
	mov ah, 02h
	mov dl, 0dh
	int 21h
	; print \n
	mov ah, 02h
	mov dl, 0ah
	int 21h
	pop dx
	pop ax
	ret
endp print_new_line

; this function gets the number to print in ax
proc print_number
	cmp cx, 0
	je end_printing
	mov cx,0
	mov dx,0
read_digit:
	cmp ax,0
	je print_digit

	mov bx,10
	div bx
	push dx
	inc cx
	xor dx,dx
	jmp read_digit
print_digit:
	cmp cx,0
	je end_printing2

	pop dx
	add dx, 30h
	; print digit
	mov ah,02h
	int 21h
	dec cx
	jmp print_digit
end_printing:
	mov dx, '0'
	mov ah,02h
	int 21h
	
end_printing2:
	call print_new_line
	ret
endp print_number

proc calculate
	push bp
	mov bp, sp
	mov dx, [word ptr bp+4]
	mov ax, [word ptr bp+6] ;secod
	mov bx, [word ptr bp+8] ; first

	xor cx, cx

	cmp dx, '!'
	je factorial_tav
		
	cmp dx, '+'
	je add_tav
		
	cmp dx, '-'
	je sub_tav
		
	cmp dx, '/'
	je mid_div_tav
		
	cmp dx, '*'
	je mid_mul_tav
		
	cmp dx, 's'
	je mid_sqrt_tav
		
	cmp dx, 'p'
	je mid_pow

	cmp dx, 'h'
	je mid_help
	
	cmp dx, 'e'
	je mid_exit

	cmp bx, ""

	jne mid_no_tav
	xor cx, cx
	jmp mid_enter_input


	factorial_tav:
		cmp ax, "ee"
		je mid_no_tav
		
		mov ax, bx
		mov cx, ax
		factorial_loop:
			dec cx ; cx = cx-1
			; check if cx 0			
			cmp cx, 0
			je end_factorial
			mul cx
			; back to the loop because cx != 0
			jmp factorial_loop
		end_factorial:
		
		xor cx, cx
		mov cx, ax
		
	jmp end_calc

	add_tav:
		cmp ax, "ee"
		je mid_no_tav
		cmp bx, "ee"
		je mid_no_tav
		
		add ax, bx
		mov cx, ax

	jmp end_calc
		
	sub_tav:
		cmp ax, "ee"
		je mid_no_tav
		cmp bx, "ee"
		je mid_no_tav

		sub ax, bx
		mov cx, ax
		
	jmp end_calc

	mid_div_tav:
		jmp div_tav

	mid_mul_tav:
		jmp mul_tav

	mid_sqrt_tav:
		jmp sqrt_tav

	mid_exit:
		jmp exit_tav
	
	mid_pow:
		jmp pow_tav
	
	mid_help:
		jmp help_tav

	mid_no_tav:
		jmp no_tav

	mid_enter_input:
		jmp end_calc

	div_tav: ; need to check if bx is 0
		cmp ax, "ee"
		je mid_no_tav
		cmp bx, "ee"
		je mid_no_tav

		xchg ax, bx
		cmp ax, 0
		je mid_no_tav_2
		mov cx, ax
		xor dx, dx ;because I'm use dx as cnt		
		cmp bx, ax
		jl end_div
		inc dx
		jg _div_loop
		jmp end_div
	_div_loop:
		inc dx
		add ax, cx
		
		cmp bx, ax
		jl more
		je end_div
		jg _div_loop
		
		more:
			sub dx, 1
		
		end_div:
			xor cx, cx
			mov cx, dx
			mov bx, dx
			mov ax, dx

	jmp end_calc
	

	mul_tav:
		cmp ax, "ee"
		je mid_no_tav
		cmp bx, "ee"
		je mid_no_tav

		mul bx
		mov cx, ax
		
	jmp end_calc

	mid_no_tav_2:
		jmp no_tav

	sqrt_tav:
		xor cx, cx  
		cmp ax, 0
		je sqrt_0
		cmp ax, 'ee'
		je mid_no_tav
		
		mov ax, 1
		sqrt_loop:
			xor ax, ax

			inc cx
			mov ax, cx
			mul cx
			cmp ax, bx
			je end_sqrt_loop 
			jl sqrt_loop
			sub cx, 1
		end_sqrt_loop:
			xor ax, ax
			mov ax, cx
		jmp end_calc
		
		sqrt_0:
			mov ax, 0
	jmp end_calc

	pow_tav:
		cmp ax, "ee"
		je no_tav
		cmp bx, "ee"
		je no_tav
		cmp bx, 0
		je _0_pow
		cmp ax, 0
		je _pow_0

		; there is a loop that mul bx possession times
		xchg ax, bx
		mov cx, ax ; the possession
		xor ax, ax
		mov ax, 1
		pow_loop:
			mul bx
			loop pow_loop
		xor cx, cx
		mov cx, ax
		jmp end_calc
		
		_0_pow:
			xor cx, cx
			mov cx, 1
			mov bx, 1
			mov ax, 1
		jmp end_calc
		
		_pow_0:
			xor cx, cx
			mov cx, 0
			mov bx, 0
			mov ax, 0

		
	jmp end_calc

	jmp end_calc

	help_tav:
		mov cx, 'h'	
		jmp end_calc
	
	exit_tav:
		mov cx, 'e'	
		jmp end_calc
	
	no_tav:
		mov cx, 'E'


	end_calc:
	
	;xor ax, ax
	;xor bx, bx
	;xor dx, dx
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

proc read_space
	mov cl, [byte ptr bx]
	cmp cl, ' '
	jne not_space
	inc bx
	jmp read_space
not_space:
	ret
endp read_space

proc read_number
	; this function gets the offset of the input array: pass by reference
	call read_space
	mov ax, 0 ; the result
	xor ch, ch
	xor di, di
_input_loop:
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
	inc di ; total number length
	jmp _input_loop
_input_end:
	cmp di, 0
	jne input_ret
	cmp cl, '_'
	jne enter_input
	mov ax, [word ptr last_answer]
	inc bx
	jmp input_ret
	
	enter_input:
		cmp ax, "$"
		jne error_input
	jmp input_ret
	
	error_input:
		mov ax, 'ee'
	
input_ret: 
	ret

endp read_number

start:
    mov ax, @data
    mov ds, ax
	
	mov dx, offset welcome_message
	mov ah,9h
	int 21h

	
	back_start: ; Back to the start after all
	
	call get_input
	mov di, ax ; save input+1 in di
	cmp di, 0
	je mid_exit_1 ; empty input
	
	; read num1
	mov bx, di
	inc bx
	call read_number
	push bx
	mov bx, offset num1
	mov [word ptr bx], ax
	pop bx
	
	; read operator
	call read_space
	mov cl, [byte ptr bx]
	inc bx
	push bx
	mov bx, offset op
	mov [byte ptr bx], cl
	pop bx
	
	; Now i can check if the op is a or ! or s and don't read num2
		
	cmp [op], 's'
	je ignore_num2

	cmp [op], '!'
	je ignore_num2

	; read num2
	call read_number
	push bx
	mov bx, offset num2
	mov [word ptr bx], ax
	pop bx
	
	ignore_num2:
	call print_new_line
	
	; calculate
	push ax ; num2
	mov bx, offset num1
	mov ax, [word ptr bx]
	push ax ; num1
	mov bx, offset op
	mov cl, [byte ptr bx]
	xor ch, ch
	push cx ; op
	
	
	
	
	cmp [num1], ''
	jne start_calculate
	cmp [num2], ''
	jne start_calculate
	cmp [op], ''
	jne start_calculate
	call print_new_line
	jmp back_start
	
	start_calculate:
		
	call calculate
	add sp, 6
	
	; Back from calculate, and the answer in cx
	; Now I will print the answer or print to try again or print the help
	
	jmp after_mid_exit_1
	
	mid_exit_1:
		jmp exit
		
	after_mid_exit_1:
		
			
	cmp cx, 'h' ; h = help
	je print_help
	cmp cx, 'e' ; e = exit
	je exit
	cmp cx, 'E' ; E = error
	je error_print
	; If im here the answer is true
	mov [last_answer], cx
	; Now i will print the answer
	mov cx, ax
	call print_number
	
	jmp back_start
	
	print_help:
		mov dx, offset help_message
		mov ah, 9h
		int 21h
		
	jmp back_start
	
	error_print:
		mov dx, offset error_message
		mov ah, 9h
		int 21h

	ignore_help_error:
		
	jmp back_start
exit:
    mov ax, 4c00h
    int 21h
END start
