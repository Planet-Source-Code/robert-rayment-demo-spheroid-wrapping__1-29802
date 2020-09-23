;WrapRotate.asm  by Robert Rayment  14/12/01

; VB
;
; ReDim ixdent(PICH)
; ReDim zrdsq(PICH)
;
; Spheroid radius
; zR = 0.5 * PICW
;
;' Pre-calculate slice indentation & radius squared
; For iy = 1 To PICH
;   ixdent(iy) = zR - Sqr(iy * (2 * zR - iy)) + 2
;   zrdsq(iy) = (0.5 * (PICW - 2 * ixdent(iy))) ^ 2
; Next iy
;
; Source & destination x-centre coords
; ixsc = PICW / 2
; ixdc = PICW / 2
;
; Proportionality factor
; zLsDpi = (PICW / 2) / pi#
;
; MCODE.PICW = PICW
; MCODE.PICH = PICH
; MCODE.ixsc = ixsc
; MCODE.ixdc = ixdc
; MCODE.zLsDpi = zLsDpi
; MCODE.Ptrixdent = VarPtr(ixdent(1))
; MCODE.Ptrzrdsq = VarPtr(zrdsq(1))
; MCODE.PtrPalBGR = VarPtr(PalBGR(1, 1, 1, 1))
;
; ptrStruc = VarPtr(MCODE.PICW)
; ptMC = VarPtr(WrapRotateMC(1))
;
; Done = False
;
; Do
;
; MCODE.ixsc = ixsc
; MCODE.ixdc = ixdc
;
; ' Public Sub ASM_Rotate()
; res = CallWindowProc(ptMC, ptrStruc, 0&, 0&, 0&)
;
; Loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Zero PalBGR(1,1,1,2) iy=PICH to 1
;
;For iy = PICH To 1 Step -1
;
;	ixlo = ixdent(iy)
;	ixhi = PICW - ixdent(iy)
;
;	For ixd = ixlo To ixhi
;         
;      zddx = ixdc - ixd
;         
;      zy = Sqr(zrdsq(iy) - zddx * zddx)
;      zx = zddx
;      
;      ztheta = zATan2(zy, zx)
;      ixs = ixsc + zLsDpi * ztheta
;      
;      If ixs < 1 Then ixs = PICW + ixs
;      If ixs > PICW Then ixs = ixs - PICW
;      
;      PalBGR(1, ixd, iy, 2) = PalBGR(1, ixs, iy, 1)
;      PalBGR(2, ixd, iy, 2) = PalBGR(2, ixs, iy, 1)
;      PalBGR(3, ixd, iy, 2) = PalBGR(3, ixs, iy, 1)
;      PalBGR(4, ixd, iy, 2) = PalBGR(3, ixs, iy, 1)
;   
;   Next ixd
;
;Next iy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


%macro movab 2		; name & num of parameters
  push dword %2		; 2nd param
  pop dword %1		; 1st param
%endmacro			; use  movab %1,%2
; Allows eg	movab bmW,[ebx+4]

%define PICW            [ebp-4]
%define PICH            [ebp-8]
%define ixsc            [ebp-12]
%define ixdc            [ebp-16]
%define zLsDpi          [ebp-20]
%define Ptrixdent       [ebp-24]
%define Ptrzrdsq        [ebp-28]
%define PtrPalBGR       [ebp-32]


%define iy	  	 [ebp-40]   ; PICH To 1 Step -1
%define ixd    	 [ebp-44]   ; ixdent(iy) To PICW - ixdent(iy)
%define ixlo	 [ebp-48]	; ixdent(iy)
%define ixhi     [ebp-52]	; PICW - ixdent(iy)
%define zddx     [ebp-56]   ; zddx = ixdc - ixd	
%define zrdsq    [ebp-60]   ; zrdsq(iy)
%define zy	     [ebp-64]	; zy = Sqr(zrdsq(iy) - zddx * zddx)
%define zx	     [ebp-68]	; zddx
%define ztheta   [ebp-72]   ; ATAN (zy/zx)
%define ixs      [ebp-76]   ; ixs = ixsc + zLsDpi * ztheta

%define PalSize    [ebp-80]
%define LineBytes  [ebp-84]


[bits 32]

	push ebp
	mov ebp,esp
	sub esp,84
	push edi
	push esi
	push ebx

	; Copy structure
	mov ebx,[ebp+8]
	
	movab PICW,          [ebx]
	movab PICH,          [ebx+4]
	movab ixsc,          [ebx+8]
	movab ixdc,          [ebx+12]
	movab zLsDpi,        [ebx+16]
	movab Ptrixdent,     [ebx+20]
	movab Ptrzrdsq,      [ebx+24]
	movab PtrPalBGR,     [ebx+28]
	
	mov eax,PICH
	mov ebx,PICW
	mul ebx
	mov PalSize,eax		; In 4 byte chunks

	mov esi,PtrPalBGR   ; esi = pts to PalBGR(1,1,1,1)
	push esi
	pop edi
	mov eax,PalSize
	shl eax,2			; x4
	add edi,eax			; edi pts to PalBGR(1,1,1,2) Blue

	; Zero PalBGR(1,1,1,2) iy=PICH to 1
	;push edi
	;mov ecx,PalSize
	;xor eax,eax
	;rep STOSD			; PalBGR(1 to 4,ix=1 to PICW,iy=PICH to 1,2) = eax = 0
	;pop edi


	mov eax,PICW
	shl eax,2			; x4
	mov LineBytes,eax	; 4*PICW

	mov ebx,PICH
	dec ebx				; (PICH-1)
	mov eax,LineBytes
	mul ebx				; LineBytes*(PICH-1)
	add esi,eax			; esi = ptr PalBGR(1,1,iy,1)
	add edi,eax			; edi = ptr PalBGR(1,1,iy,2)
						; - LineBytes will point to iy-1
	mov ecx,PICH
FIY:
	
	push edi
	push ecx

	dec ecx
	shl ecx,2			; 4*(iy-1)
	mov edi,Ptrixdent
	add edi,ecx
	mov eax,dword[edi]  ; ixlo = ixdent(iy)
	mov ixlo,eax        ; ixlo = ixdent(iy)
	
	mov eax,PICW
	sub eax,ixlo
	mov ixhi,eax		; ixhi = PICW - ixdebt(iy)
	
	pop ecx				; back to iy
	push ecx
	
	dec ecx
	shl ecx,2			; 4*(iy-1)
	mov edi,Ptrzrdsq
	add edi,ecx
	mov eax,dword[edi]  ; zrdsq(iy)
	mov zrdsq,eax       ; zrdsq(iy)

	pop ecx				; back to iy
	pop edi				; edi = ptr  PalBGR(1,1,iy,2)
	
	push ecx
	
	mov ecx,ixlo		; ecx=ixd
FIXD:
	
	mov eax,ixdc
	sub eax,ecx
	mov zddx,eax		; zddx=ixdc-ixd
	
    ; Calc ztheta = zATan2(zy, zx)
	; where  zy = Sqr(zrdsq(iy) - zddx * zddx)
	;        zx = zddx
	fld dword zrdsq
	fild dword zddx
	fild dword zddx
	fmulp st1
	fsubp st1
	fsqrt
	fild dword zddx	; zx | zy
	fpatan			; ztheta
	; Calc ixs = ixsc + zLsDpi * ztheta
	fld dword zLsDpi
	fmulp st1
	fild dword ixsc
	faddp st1
	fistp dword ixs
	
	; Keep ixs in range
	mov ebx,PICW
	
	mov eax,ixs
	cmp eax,1
	jge tGTPICW
	add eax,ebx		; PICW+ixs
	jmp ixsOK
tGTPICW:
	cmp eax,PICW
	jle ixsOK
	sub eax,ebx		; ixs-PICW
ixsOK:
	
	; PalBGR(,,,1) to PalBGR(,,,2)
	; eax=ixs
	; ecx=ixd
	; edi = ptr PalBGR(1,1,iy,2)
	; esi = ptr PalBGR(1,1,iy,1)
	
	push edi
	push esi
	
	dec eax
	shl eax,2		; 4*(ixs-1)
	add esi,eax
	mov eax,ecx
	dec eax
	shl eax,2		; 4*(ixd-1)
	add edi,eax
	
	movsd			; [esi]->[edi]
	
	pop esi
	pop edi
	
	inc ecx
	cmp ecx,ixhi
	jle near FIXD
	
	; move esi & edi to next iy
	mov eax,LineBytes
	sub esi,eax
	sub edi,eax
	

	pop ecx
	dec ecx
	jnz near FIY

GETOUT:
	pop ebx
	pop esi
	pop edi
	mov esp,ebp
	pop ebp
	ret 16

;############################################################
