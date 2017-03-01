/* Main function */
	.global	__fj_main
	.equ	__fj_main,__main_closure_03214

/* Spill area */
	.data
	.align	4
	.global	__fj_spills
__fj_spills:
	.skip	36
/* Globals */
	.data

	.align	4
	.long	0x00000018
__HigherOrder1_names_02887:
	.long	__HigherOrder1_02905
	.long	0
	.long	__FjObject_02904
	.long	0
	.long	0
	.long	0

	.align	4
	.long	0x00000014
__HigherOrder1_class_02888:
	.long	__HigherOrder1_names_02887
	.long	__println_closure_03215
	.long	__main_closure_03216
	.long	__g_closure_03217
	.long	__psuedo_add_closure_03218

	.align	4
	.long	0x00000006
__str_02889:
	.string	"8 = "

	.align	4
	.long	0x0000000a
__str_02890:
	.string	"; 9 = "

	.align	4
	.long	0x0000000a
__str_02891:
	.string	"51 = "

	.align	4
	.long	0x0000000a
__str_02892:
	.string	"; 52 = "

	.align	4
	.long	0x0000000e
__str_02893:
	.string	"i = -2 = "

	.align	4
	.long	0x0000000a
__str_02894:
	.string	"33 = "

	.align	4
	.long	0x0000000a
__str_02895:
	.string	"117 = "

	.align	4
	.long	0x0000000a
__str_02896:
	.string	"; 116 = "

	.align	4
	.long	0x0000000a
__str_02897:
	.string	"23 = "

	.align	4
	.long	0x0000000a
__str_02898:
	.string	"134 = "

	.align	4
	.long	0x0000000a
__str_02899:
	.string	"; 141 = "

	.align	4
	.long	0x00000006
__str_02900:
	.string	"i = "

	.align	4
	.long	0x00000016
__str_02901:
	.string	" = useless_val = "

	.align	4
	.long	0x0000000e
__str_02902:
	.string	"good so far"

	.align	4
	.long	0x0000001a
__str_02903:
	.string	"SOMETHING WENT WRONG!!!!"

	.align	4
	.long	0x00000012
__FjObject_02904:
	.string	"FjObject_02904"

	.align	4
	.long	0x00000016
__HigherOrder1_02905:
	.string	"HigherOrder1_02905"

	.align	4
	.long	0x00000018
__names_rttd_03208:
	.long	0
	.long	1
	.long	2
	.long	3
	.long	4
	.long	-1

	.align	4
	.long	0x00000008
__class_rttd_03209:
	.long	1
	.long	-1

	.align	4
	.long	0x00000008
__cont_rttd_03210:
	.long	1
	.long	-1

	.align	4
	.long	0x0000001c
__this_rttd_03211:
	.long	1
	.long	2
	.long	3
	.long	7
	.long	8
	.long	9
	.long	-1

	.align	4
	.long	0x00000014
__this_rttd_03212:
	.long	1
	.long	2
	.long	3
	.long	5
	.long	-1

	.align	4
	.long	0x00000010
__cont_rttd_03213:
	.long	1
	.long	2
	.long	5
	.long	-1


/* Code */
	.text
	.align	4
__while_closure_03236:
	movl	%ecx,%esi
	movl	0(%ebp),%edi
	movl	%edi,20(%ebp)
	jmp	__while_02886
__while_02886:
	jmp	__while_body_03621
__while_body_03621:
/* FIR:
   let binop_03202 : bool = i_03201 < y_hack_03200 in
   ...
*/
	xorl	%ecx,%ecx
	cmpl	%edx,20(%ebp)
	setl	%cl
/* FIR:
   if binop_03202 then ... else ...
*/
	cmpl	$0x00000000,%ecx
	jne	__true_case_03623
	jmp	__false_case_03624
__true_case_03623:
/* FIR:
   let binop_03203 : int = z_03199 + 1 in
   ...
*/
	addl	$0x00000001,%esi
/* FIR:
   let uarith_03204 : int = i_03201 + 1 in
   ...
*/
	movl	20(%ebp),%edi
	addl	$0x00000001,%edi
/* FIR:
   tailcall while_02886(cont_03197,
                        x_03198,
                        binop_03203,
                        y_hack_03200,
                        uarith_03204)
*/
	movl	%edi,20(%ebp)
	jmp	__while_02886
__false_case_03624:
/* FIR:
   let binop_03205 : int = z_03199 + x_03198 in
   ...
*/
	addl	%ebx,%esi
/* FIR:
   tailcall cont_03197(binop_03205)
*/
	movl	(%eax),%ecx
	movl	4(%eax),%eax
	movl	%esi,%ebx
	jmp	*%ecx
__println_cont_closure_03235:
	movl	%eax,12(%ebp)
	jmp	__println_cont_02885
__println_cont_02885:
	movl	__mem_limit,%eax
	subl	__mem_next,%eax
	cmpl	$0x00000050,%eax
	jl	__println_cont_gc_03619
	jmp	__println_cont_body_03595
__println_cont_gc_03619:
	pushl	$80
	pushl	$__gc_record_03802
	call	__gc
	addl	$8,%esp
/* Pointer operands: 12(%ebp) */
	.data
	.align	4
__gc_record_03802:
	.long	0x00000000
	.long	12
	.long	-1
	.text
	jmp	__println_cont_body_03595
__println_cont_body_03595:
/* FIR:
   let array_03180 : int[] = frame_g_00628_03178.array_00767 in
   ...
*/
	cmpl	$0x00000000,12(%ebp)
	je	__seg_fault
	movl	12(%ebp),%eax
	movl	20(%eax),%ecx
/* FIR:
   array_03180[0] : int <- 134;
   ...
*/
	cmpl	$0x00000000,%ecx
	je	__seg_fault
	movl	-4(%ecx),%eax
	shrl	$0,%eax
	andl	$0xfffffffc,%eax
	movl	$0x00000000,%ebx
	cmpl	%ebx,%eax
	jna	__seg_fault
	movl	$0x00000086,%eax
	movl	%eax,0(%ecx,%ebx,4)
/* FIR:
   let array_03181 : int[] = frame_g_00628_03178.array_00767 in
   ...
*/
	cmpl	$0x00000000,12(%ebp)
	je	__seg_fault
	movl	12(%ebp),%eax
	movl	20(%eax),%ecx
/* FIR:
   array_03181[1] : int <- 141;
   ...
*/
	cmpl	$0x00000000,%ecx
	je	__seg_fault
	movl	-4(%ecx),%eax
	shrl	$0,%eax
	andl	$0xfffffffc,%eax
	movl	$0x00000001,%ebx
	cmpl	%ebx,%eax
	jna	__seg_fault
	movl	$0x0000008d,%eax
	movl	%eax,0(%ecx,%ebx,4)
/* FIR:
   let array_03182 : int[] = frame_g_00628_03178.array_00767 in
   ...
*/
	cmpl	$0x00000000,12(%ebp)
	je	__seg_fault
	movl	12(%ebp),%eax
	movl	20(%eax),%ecx
/* FIR:
   let sub_03183 : int = array_03182[0] in
   ...
*/
	cmpl	$0x00000000,%ecx
	je	__seg_fault
	movl	-4(%ecx),%eax
	shrl	$0,%eax
	andl	$0xfffffffc,%eax
	movl	$0x00000000,%ebx
	cmpl	%ebx,%eax
	jna	__seg_fault
	movl	0(%ecx,%ebx,4),%eax
/* FIR:
   let string_03184 :
      string =
      external ("itoa" : (int) -> string)(sub_03183) in
   ...
*/
	pushl	%eax
	call	__itoa
	addl	$0x00000004,%esp
/* FIR:
   let strcat_03185 :
      string =
      external ("strcat" : (string, string) -> string)(str_02898,
                                                       string_03184) in
   ...
*/
	pushl	%eax
	pushl	$__str_02898
	call	__strcat
	addl	$0x00000008,%esp
/* FIR:
   let strcat_03186 :
      string =
      external ("strcat" : (string, string) -> string)(strcat_03185,
                                                       str_02899) in
   ...
*/
	pushl	$__str_02899
	pushl	%eax
	call	__strcat
	addl	$0x00000008,%esp
	movl	%eax,8(%ebp)
/* FIR:
   let array_03187 : int[] = frame_g_00628_03178.array_00767 in
   ...
*/
	cmpl	$0x00000000,12(%ebp)
	je	__seg_fault
	movl	12(%ebp),%eax
	movl	20(%eax),%ecx
/* FIR:
   let sub_03188 : int = array_03187[1] in
   ...
*/
	cmpl	$0x00000000,%ecx
	je	__seg_fault
	movl	-4(%ecx),%eax
	shrl	$0,%eax
	andl	$0xfffffffc,%eax
	movl	$0x00000001,%ebx
	cmpl	%ebx,%eax
	jna	__seg_fault
	movl	0(%ecx,%ebx,4),%eax
/* FIR:
   let string_03189 :
      string =
      external ("itoa" : (int) -> string)(sub_03188) in
   ...
*/
	pushl	%eax
	call	__itoa
	addl	$0x00000004,%esp
/* FIR:
   let strcat_03190 :
      string =
      external ("strcat" : (string, string) -> string)(strcat_03186,
                                                       string_03189) in
   ...
*/
	pushl	%eax
	pushl	8(%ebp)
	call	__strcat
	addl	$0x00000008,%esp
	movl	%eax,%edx
/* FIR:
   let this_03191 : HigherOrder1_02908 = frame_g_00628_03178.this_00763 in
   ...
*/
	cmpl	$0x00000000,12(%ebp)
	je	__seg_fault
	movl	12(%ebp),%eax
	movl	4(%eax),%eax
/* FIR:
   let println_03192 :
      HigherOrder1_classtype_02913 =
      this_03191.class_00115 in
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	4(%eax),%eax
/* FIR:
   let println_03193 :
      method[FjObject_02907] (((unit) -> unit, ty_exnh_02906, string) -> unit =
      println_03192.println_00383 in
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	4(%eax),%esi
/* FIR:
   let this_03194 : HigherOrder1_02908 = frame_g_00628_03178.this_00763 in
   ...
*/
	cmpl	$0x00000000,12(%ebp)
	je	__seg_fault
	movl	12(%ebp),%eax
	movl	4(%eax),%eax
/* FIR:
   let println_cont_03195 :
      (unit) -> unit =
      closure(println_cont_02882, frame_g_00628_03178) in
   ...
*/
	movl	__mem_next,%ebx
	leal	4(%ebx),%ebx
	movl	$0x00000008,-4(%ebx)
	addl	$0x0000000c,__mem_next
	movl	$__println_cont_closure_03232,%ecx
	movl	%ecx,(%ebx)
	movl	12(%ebp),%ecx
	movl	%ecx,4(%ebx)
/* FIR:
   let exnh_03196 : ty_exnh_02906 = frame_g_00628_03178.exnh_00765 in
   ...
*/
	cmpl	$0x00000000,12(%ebp)
	je	__seg_fault
	movl	12(%ebp),%ecx
	movl	12(%ecx),%ecx
/* FIR:
   methodcall println_03193[this_03194](println_cont_03195,
                                        exnh_03196,
                                        strcat_03190)
*/
	jmp	*%esi
__println_cont_closure_03234:
	movl	%eax,12(%ebp)
	jmp	__println_cont_02884
__println_cont_02884:
	movl	__mem_limit,%eax
	subl	__mem_next,%eax
	cmpl	$0x00000050,%eax
	jl	__println_cont_gc_03593
	jmp	__println_cont_body_03569
__println_cont_gc_03593:
	pushl	$80
	pushl	$__gc_record_03803
	call	__gc
	addl	$8,%esp
/* Pointer operands: 12(%ebp) */
	.data
	.align	4
__gc_record_03803:
	.long	0x00000000
	.long	12
	.long	-1
	.text
	jmp	__println_cont_body_03569
__println_cont_body_03569:
/* FIR:
   let array_03161 : int[] = frame_g_00628_03159.array_00767 in
   ...
*/
	cmpl	$0x00000000,12(%ebp)
	je	__seg_fault
	movl	12(%ebp),%eax
	movl	20(%eax),%ecx
/* FIR:
   array_03161[0] : int <- 134;
   ...
*/
	cmpl	$0x00000000,%ecx
	je	__seg_fault
	movl	-4(%ecx),%eax
	shrl	$0,%eax
	andl	$0xfffffffc,%eax
	movl	$0x00000000,%ebx
	cmpl	%ebx,%eax
	jna	__seg_fault
	movl	$0x00000086,%eax
	movl	%eax,0(%ecx,%ebx,4)
/* FIR:
   let array_03162 : int[] = frame_g_00628_03159.array_00767 in
   ...
*/
	cmpl	$0x00000000,12(%ebp)
	je	__seg_fault
	movl	12(%ebp),%eax
	movl	20(%eax),%ecx
/* FIR:
   array_03162[1] : int <- 141;
   ...
*/
	cmpl	$0x00000000,%ecx
	je	__seg_fault
	movl	-4(%ecx),%eax
	shrl	$0,%eax
	andl	$0xfffffffc,%eax
	movl	$0x00000001,%ebx
	cmpl	%ebx,%eax
	jna	__seg_fault
	movl	$0x0000008d,%eax
	movl	%eax,0(%ecx,%ebx,4)
/* FIR:
   let array_03163 : int[] = frame_g_00628_03159.array_00767 in
   ...
*/
	cmpl	$0x00000000,12(%ebp)
	je	__seg_fault
	movl	12(%ebp),%eax
	movl	20(%eax),%ecx
/* FIR:
   let sub_03164 : int = array_03163[0] in
   ...
*/
	cmpl	$0x00000000,%ecx
	je	__seg_fault
	movl	-4(%ecx),%eax
	shrl	$0,%eax
	andl	$0xfffffffc,%eax
	movl	$0x00000000,%ebx
	cmpl	%ebx,%eax
	jna	__seg_fault
	movl	0(%ecx,%ebx,4),%eax
/* FIR:
   let string_03165 :
      string =
      external ("itoa" : (int) -> string)(sub_03164) in
   ...
*/
	pushl	%eax
	call	__itoa
	addl	$0x00000004,%esp
/* FIR:
   let strcat_03166 :
      string =
      external ("strcat" : (string, string) -> string)(str_02898,
                                                       string_03165) in
   ...
*/
	pushl	%eax
	pushl	$__str_02898
	call	__strcat
	addl	$0x00000008,%esp
/* FIR:
   let strcat_03167 :
      string =
      external ("strcat" : (string, string) -> string)(strcat_03166,
                                                       str_02899) in
   ...
*/
	pushl	$__str_02899
	pushl	%eax
	call	__strcat
	addl	$0x00000008,%esp
	movl	%eax,8(%ebp)
/* FIR:
   let array_03168 : int[] = frame_g_00628_03159.array_00767 in
   ...
*/
	cmpl	$0x00000000,12(%ebp)
	je	__seg_fault
	movl	12(%ebp),%eax
	movl	20(%eax),%ecx
/* FIR:
   let sub_03169 : int = array_03168[1] in
   ...
*/
	cmpl	$0x00000000,%ecx
	je	__seg_fault
	movl	-4(%ecx),%eax
	shrl	$0,%eax
	andl	$0xfffffffc,%eax
	movl	$0x00000001,%ebx
	cmpl	%ebx,%eax
	jna	__seg_fault
	movl	0(%ecx,%ebx,4),%eax
/* FIR:
   let string_03170 :
      string =
      external ("itoa" : (int) -> string)(sub_03169) in
   ...
*/
	pushl	%eax
	call	__itoa
	addl	$0x00000004,%esp
/* FIR:
   let strcat_03171 :
      string =
      external ("strcat" : (string, string) -> string)(strcat_03167,
                                                       string_03170) in
   ...
*/
	pushl	%eax
	pushl	8(%ebp)
	call	__strcat
	addl	$0x00000008,%esp
	movl	%eax,%edx
/* FIR:
   let this_03172 : HigherOrder1_02908 = frame_g_00628_03159.this_00763 in
   ...
*/
	cmpl	$0x00000000,12(%ebp)
	je	__seg_fault
	movl	12(%ebp),%eax
	movl	4(%eax),%eax
/* FIR:
   let println_03173 :
      HigherOrder1_classtype_02913 =
      this_03172.class_00115 in
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	4(%eax),%eax
/* FIR:
   let println_03174 :
      method[FjObject_02907] (((unit) -> unit, ty_exnh_02906, string) -> unit =
      println_03173.println_00383 in
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	4(%eax),%esi
/* FIR:
   let this_03175 : HigherOrder1_02908 = frame_g_00628_03159.this_00763 in
   ...
*/
	cmpl	$0x00000000,12(%ebp)
	je	__seg_fault
	movl	12(%ebp),%eax
	movl	4(%eax),%eax
/* FIR:
   let println_cont_03176 :
      (unit) -> unit =
      closure(println_cont_02882, frame_g_00628_03159) in
   ...
*/
	movl	__mem_next,%ebx
	leal	4(%ebx),%ebx
	movl	$0x00000008,-4(%ebx)
	addl	$0x0000000c,__mem_next
	movl	$__println_cont_closure_03232,%ecx
	movl	%ecx,(%ebx)
	movl	12(%ebp),%ecx
	movl	%ecx,4(%ebx)
/* FIR:
   let exnh_03177 : ty_exnh_02906 = frame_g_00628_03159.exnh_00765 in
   ...
*/
	cmpl	$0x00000000,12(%ebp)
	je	__seg_fault
	movl	12(%ebp),%ecx
	movl	12(%ecx),%ecx
/* FIR:
   methodcall println_03174[this_03175](println_cont_03176,
                                        exnh_03177,
                                        strcat_03171)
*/
	jmp	*%esi
__println_cont_closure_03233:
	jmp	__println_cont_02883
__println_cont_02883:
	movl	__mem_limit,%ebx
	subl	__mem_next,%ebx
	cmpl	$0x0000000c,%ebx
	jl	__println_cont_gc_03567
	jmp	__println_cont_body_03563
__println_cont_gc_03567:
	pushl	$12
	pushl	$__gc_record_03804
	call	__gc
	addl	$8,%esp
/* Pointer operands: %eax */
	.data
	.align	4
__gc_record_03804:
	.long	0x00000001
	.long	-1
	.text
	jmp	__println_cont_body_03563
__println_cont_body_03563:
/* FIR:
   let return_fun_03157 :
      (((int) -> unit, ty_exnh_02906, int) -> unit =
      closure(return_fun_02877, frame_g_00628_03155) in
   ...
*/
	movl	__mem_next,%ebx
	leal	4(%ebx),%ebx
	movl	$0x00000008,-4(%ebx)
	addl	$0x0000000c,__mem_next
	movl	$__return_fun_closure_03227,%ecx
	movl	%ecx,(%ebx)
	movl	%eax,4(%ebx)
/* FIR:
   let cont_03158 :
      (((((int) -> unit, ty_exnh_02906, int) -> unit) -> unit =
      frame_g_00628_03155.cont_00764 in
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	8(%eax),%eax
/* FIR:
   tailcall cont_03158(return_fun_03157)
*/
	movl	(%eax),%ecx
	movl	4(%eax),%eax
	jmp	*%ecx
__println_cont_closure_03232:
	movl	%eax,12(%ebp)
	jmp	__println_cont_02882
__println_cont_02882:
	movl	__mem_limit,%eax
	subl	__mem_next,%eax
	cmpl	$0x00000050,%eax
	jl	__println_cont_gc_03561
	jmp	__println_cont_body_03553
__println_cont_gc_03561:
	pushl	$80
	pushl	$__gc_record_03805
	call	__gc
	addl	$8,%esp
/* Pointer operands: 12(%ebp) */
	.data
	.align	4
__gc_record_03805:
	.long	0x00000000
	.long	12
	.long	-1
	.text
	jmp	__println_cont_body_03553
__println_cont_body_03553:
/* FIR:
   let i_03142 : int = frame_g_00628_03140.i_00766 in
   ...
*/
	cmpl	$0x00000000,12(%ebp)
	je	__seg_fault
	movl	12(%ebp),%eax
	movl	16(%eax),%eax
/* FIR:
   let string_03143 :
      string =
      external ("itoa" : (int) -> string)(i_03142) in
   ...
*/
	pushl	%eax
	call	__itoa
	addl	$0x00000004,%esp
/* FIR:
   let strcat_03144 :
      string =
      external ("strcat" : (string, string) -> string)(str_02900,
                                                       string_03143) in
   ...
*/
	pushl	%eax
	pushl	$__str_02900
	call	__strcat
	addl	$0x00000008,%esp
/* FIR:
   let strcat_03145 :
      string =
      external ("strcat" : (string, string) -> string)(strcat_03144,
                                                       str_02901) in
   ...
*/
	pushl	$__str_02901
	pushl	%eax
	call	__strcat
	addl	$0x00000008,%esp
	movl	%eax,8(%ebp)
/* FIR:
   let useless_val_03146 : int = frame_g_00628_03140.useless_val_00770 in
   ...
*/
	cmpl	$0x00000000,12(%ebp)
	je	__seg_fault
	movl	12(%ebp),%eax
	movl	24(%eax),%eax
/* FIR:
   let string_03147 :
      string =
      external ("itoa" : (int) -> string)(useless_val_03146) in
   ...
*/
	pushl	%eax
	call	__itoa
	addl	$0x00000004,%esp
/* FIR:
   let strcat_03148 :
      string =
      external ("strcat" : (string, string) -> string)(strcat_03145,
                                                       string_03147) in
   ...
*/
	pushl	%eax
	pushl	8(%ebp)
	call	__strcat
	addl	$0x00000008,%esp
	movl	%eax,%edx
/* FIR:
   let this_03149 : HigherOrder1_02908 = frame_g_00628_03140.this_00763 in
   ...
*/
	cmpl	$0x00000000,12(%ebp)
	je	__seg_fault
	movl	12(%ebp),%eax
	movl	4(%eax),%eax
/* FIR:
   let println_03150 :
      HigherOrder1_classtype_02913 =
      this_03149.class_00115 in
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	4(%eax),%eax
/* FIR:
   let println_03151 :
      method[FjObject_02907] (((unit) -> unit, ty_exnh_02906, string) -> unit =
      println_03150.println_00383 in
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	4(%eax),%esi
/* FIR:
   let this_03152 : HigherOrder1_02908 = frame_g_00628_03140.this_00763 in
   ...
*/
	cmpl	$0x00000000,12(%ebp)
	je	__seg_fault
	movl	12(%ebp),%eax
	movl	4(%eax),%eax
/* FIR:
   let println_cont_03153 :
      (unit) -> unit =
      closure(println_cont_02883, frame_g_00628_03140) in
   ...
*/
	movl	__mem_next,%ebx
	leal	4(%ebx),%ebx
	movl	$0x00000008,-4(%ebx)
	addl	$0x0000000c,__mem_next
	movl	$__println_cont_closure_03233,%ecx
	movl	%ecx,(%ebx)
	movl	12(%ebp),%ecx
	movl	%ecx,4(%ebx)
/* FIR:
   let exnh_03154 : ty_exnh_02906 = frame_g_00628_03140.exnh_00765 in
   ...
*/
	cmpl	$0x00000000,12(%ebp)
	je	__seg_fault
	movl	12(%ebp),%ecx
	movl	12(%ecx),%ecx
/* FIR:
   methodcall println_03151[this_03152](println_cont_03153,
                                        exnh_03154,
                                        strcat_03148)
*/
	jmp	*%esi
__bogofun_cont_closure_03231:
	movl	%eax,%edx
	jmp	__bogofun_cont_02881
__bogofun_cont_02881:
	movl	__mem_limit,%eax
	subl	__mem_next,%eax
	cmpl	$0x0000000c,%eax
	jl	__bogofun_cont_gc_03551
	jmp	__bogofun_cont_body_03517
__bogofun_cont_gc_03551:
	pushl	$12
	pushl	$__gc_record_03806
	call	__gc
	addl	$8,%esp
/* Pointer operands: %edx */
	.data
	.align	4
__gc_record_03806:
	.long	0x00000008
	.long	-1
	.text
	jmp	__bogofun_cont_body_03517
__bogofun_cont_body_03517:
/* FIR:
   let array_03113 : int[] = frame_g_00628_03111.array_00767 in
   ...
*/
	cmpl	$0x00000000,%edx
	je	__seg_fault
	movl	20(%edx),%esi
/* FIR:
   let sub_03114 : int = array_03113[0] in
   ...
*/
	cmpl	$0x00000000,%esi
	je	__seg_fault
	movl	-4(%esi),%eax
	shrl	$0,%eax
	andl	$0xfffffffc,%eax
	movl	$0x00000000,%ebx
	cmpl	%ebx,%eax
	jna	__seg_fault
	movl	0(%esi,%ebx,4),%ebx
/* FIR:
   let i_03115 : int = frame_g_00628_03111.i_00766 in
   ...
*/
	cmpl	$0x00000000,%edx
	je	__seg_fault
	movl	16(%edx),%eax
/* FIR:
   let binop_03116 : bool = sub_03114 == i_03115 in
   ...
*/
	xorl	%ecx,%ecx
	cmpl	%eax,%ebx
	sete	%cl
/* FIR:
   if binop_03116 then ... else ...
*/
	cmpl	$0x00000000,%ecx
	jne	__true_case_03524
	jmp	__false_case_03525
__true_case_03524:
/* FIR:
   let array_03117 : int[] = frame_g_00628_03111.array_00767 in
   ...
*/
	cmpl	$0x00000000,%edx
	je	__seg_fault
	movl	20(%edx),%esi
/* FIR:
   let sub_03118 : int = array_03117[1] in
   ...
*/
	cmpl	$0x00000000,%esi
	je	__seg_fault
	movl	-4(%esi),%eax
	shrl	$0,%eax
	andl	$0xfffffffc,%eax
	movl	$0x00000001,%ebx
	cmpl	%ebx,%eax
	jna	__seg_fault
	movl	0(%esi,%ebx,4),%ebx
/* FIR:
   let i_03119 : int = frame_g_00628_03111.i_00766 in
   ...
*/
	cmpl	$0x00000000,%edx
	je	__seg_fault
	movl	16(%edx),%eax
/* FIR:
   let binop_03120 : int = i_03119 + 1 in
   ...
*/
	addl	$0x00000001,%eax
/* FIR:
   let binop_03121 : bool = sub_03118 == binop_03120 in
   ...
*/
	xorl	%ecx,%ecx
	cmpl	%eax,%ebx
	sete	%cl
/* FIR:
   if binop_03121 then ... else ...
*/
	cmpl	$0x00000000,%ecx
	jne	__true_case_03532
	jmp	__false_case_03533
__false_case_03525:
/* FIR:
   let this_03134 : HigherOrder1_02908 = frame_g_00628_03111.this_00763 in
   ...
*/
	cmpl	$0x00000000,%edx
	je	__seg_fault
	movl	4(%edx),%eax
/* FIR:
   let println_03135 :
      HigherOrder1_classtype_02913 =
      this_03134.class_00115 in
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	4(%eax),%eax
/* FIR:
   let println_03136 :
      method[FjObject_02907] (((unit) -> unit, ty_exnh_02906, string) -> unit =
      println_03135.println_00383 in
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	4(%eax),%esi
/* FIR:
   let this_03137 : HigherOrder1_02908 = frame_g_00628_03111.this_00763 in
   ...
*/
	cmpl	$0x00000000,%edx
	je	__seg_fault
	movl	4(%edx),%eax
/* FIR:
   let println_cont_03138 :
      (unit) -> unit =
      closure(println_cont_02885, frame_g_00628_03111) in
   ...
*/
	movl	__mem_next,%ebx
	leal	4(%ebx),%ebx
	movl	$0x00000008,-4(%ebx)
	addl	$0x0000000c,__mem_next
	movl	$__println_cont_closure_03235,%ecx
	movl	%ecx,(%ebx)
	movl	%edx,4(%ebx)
/* FIR:
   let exnh_03139 : ty_exnh_02906 = frame_g_00628_03111.exnh_00765 in
   ...
*/
	cmpl	$0x00000000,%edx
	je	__seg_fault
	movl	12(%edx),%ecx
/* FIR:
   methodcall println_03136[this_03137](println_cont_03138,
                                        exnh_03139,
                                        str_02903)
*/
	movl	$__str_02903,%edx
	jmp	*%esi
__true_case_03532:
/* FIR:
   let this_03122 : HigherOrder1_02908 = frame_g_00628_03111.this_00763 in
   ...
*/
	cmpl	$0x00000000,%edx
	je	__seg_fault
	movl	4(%edx),%eax
/* FIR:
   let println_03123 :
      HigherOrder1_classtype_02913 =
      this_03122.class_00115 in
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	4(%eax),%eax
/* FIR:
   let println_03124 :
      method[FjObject_02907] (((unit) -> unit, ty_exnh_02906, string) -> unit =
      println_03123.println_00383 in
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	4(%eax),%esi
/* FIR:
   let this_03125 : HigherOrder1_02908 = frame_g_00628_03111.this_00763 in
   ...
*/
	cmpl	$0x00000000,%edx
	je	__seg_fault
	movl	4(%edx),%eax
/* FIR:
   let println_cont_03126 :
      (unit) -> unit =
      closure(println_cont_02884, frame_g_00628_03111) in
   ...
*/
	movl	__mem_next,%ebx
	leal	4(%ebx),%ebx
	movl	$0x00000008,-4(%ebx)
	addl	$0x0000000c,__mem_next
	movl	$__println_cont_closure_03234,%ecx
	movl	%ecx,(%ebx)
	movl	%edx,4(%ebx)
/* FIR:
   let exnh_03127 : ty_exnh_02906 = frame_g_00628_03111.exnh_00765 in
   ...
*/
	cmpl	$0x00000000,%edx
	je	__seg_fault
	movl	12(%edx),%ecx
/* FIR:
   methodcall println_03124[this_03125](println_cont_03126,
                                        exnh_03127,
                                        str_02902)
*/
	movl	$__str_02902,%edx
	jmp	*%esi
__false_case_03533:
/* FIR:
   let this_03128 : HigherOrder1_02908 = frame_g_00628_03111.this_00763 in
   ...
*/
	cmpl	$0x00000000,%edx
	je	__seg_fault
	movl	4(%edx),%eax
/* FIR:
   let println_03129 :
      HigherOrder1_classtype_02913 =
      this_03128.class_00115 in
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	4(%eax),%eax
/* FIR:
   let println_03130 :
      method[FjObject_02907] (((unit) -> unit, ty_exnh_02906, string) -> unit =
      println_03129.println_00383 in
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	4(%eax),%esi
/* FIR:
   let this_03131 : HigherOrder1_02908 = frame_g_00628_03111.this_00763 in
   ...
*/
	cmpl	$0x00000000,%edx
	je	__seg_fault
	movl	4(%edx),%eax
/* FIR:
   let println_cont_03132 :
      (unit) -> unit =
      closure(println_cont_02885, frame_g_00628_03111) in
   ...
*/
	movl	__mem_next,%ebx
	leal	4(%ebx),%ebx
	movl	$0x00000008,-4(%ebx)
	addl	$0x0000000c,__mem_next
	movl	$__println_cont_closure_03235,%ecx
	movl	%ecx,(%ebx)
	movl	%edx,4(%ebx)
/* FIR:
   let exnh_03133 : ty_exnh_02906 = frame_g_00628_03111.exnh_00765 in
   ...
*/
	cmpl	$0x00000000,%edx
	je	__seg_fault
	movl	12(%edx),%ecx
/* FIR:
   methodcall println_03130[this_03131](println_cont_03132,
                                        exnh_03133,
                                        str_02903)
*/
	movl	$__str_02903,%edx
	jmp	*%esi
__while_closure_03230:
	movl	%ecx,20(%ebp)
	jmp	__while_02880
__while_02880:
	jmp	__while_body_03485
__while_body_03485:
/* FIR:
   let i_03098 : int = frame_g_00628_03095.i_00766 in
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	16(%eax),%edx
/* FIR:
   let binop_03099 : bool = b_03097 < i_03098 in
   ...
*/
	xorl	%ecx,%ecx
	movl	20(%ebp),%ecx
	cmpl	%edx,%ecx
	setl	%cl
/* FIR:
   if binop_03099 then ... else ...
*/
	cmpl	$0x00000000,%ecx
	jne	__true_case_03488
	jmp	__false_case_03489
__true_case_03488:
/* FIR:
   let array_03100 : int[] = frame_g_00628_03095.array_00767 in
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	20(%eax),%edi
/* FIR:
   let subscript_03101 : int = array_03100[0] in
   ...
*/
	cmpl	$0x00000000,%edi
	je	__seg_fault
	movl	-4(%edi),%edx
	shrl	$0,%edx
	andl	$0xfffffffc,%edx
	movl	$0x00000000,%esi
	cmpl	%esi,%edx
	jna	__seg_fault
	movl	0(%edi,%esi,4),%esi
/* FIR:
   let uarith_03102 : int = subscript_03101 + 1 in
   ...
*/
	addl	$0x00000001,%esi
/* FIR:
   let array_03103 : int[] = frame_g_00628_03095.array_00767 in
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	20(%eax),%edx
	movl	%edx,12(%ebp)
/* FIR:
   array_03103[0] : int <- uarith_03102;
   ...
*/
	cmpl	$0x00000000,12(%ebp)
	je	__seg_fault
	movl	12(%ebp),%edx
	movl	-4(%edx),%edx
	shrl	$0,%edx
	andl	$0xfffffffc,%edx
	movl	$0x00000000,%edi
	cmpl	%edi,%edx
	jna	__seg_fault
	shll	$2,%edi
	addl	12(%ebp),%edi
	movl	%esi,0(%edi)
/* FIR:
   let array_03104 : int[] = frame_g_00628_03095.array_00767 in
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	20(%eax),%edi
/* FIR:
   let subscript_03105 : int = array_03104[1] in
   ...
*/
	cmpl	$0x00000000,%edi
	je	__seg_fault
	movl	-4(%edi),%edx
	shrl	$0,%edx
	andl	$0xfffffffc,%edx
	movl	$0x00000001,%esi
	cmpl	%esi,%edx
	jna	__seg_fault
	movl	0(%edi,%esi,4),%esi
/* FIR:
   let uarith_03106 : int = subscript_03105 + 1 in
   ...
*/
	addl	$0x00000001,%esi
/* FIR:
   let array_03107 : int[] = frame_g_00628_03095.array_00767 in
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	20(%eax),%edx
	movl	%edx,12(%ebp)
/* FIR:
   array_03107[1] : int <- uarith_03106;
   ...
*/
	cmpl	$0x00000000,12(%ebp)
	je	__seg_fault
	movl	12(%ebp),%edx
	movl	-4(%edx),%edx
	shrl	$0,%edx
	andl	$0xfffffffc,%edx
	movl	$0x00000001,%edi
	cmpl	%edi,%edx
	jna	__seg_fault
	shll	$2,%edi
	addl	12(%ebp),%edi
	movl	%esi,0(%edi)
/* FIR:
   let useless_val_03108 : int = frame_g_00628_03095.useless_val_00770 in
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	24(%eax),%edx
/* FIR:
   let uarith_03109 : int = useless_val_03108 + 1 in
   ...
*/
	addl	$0x00000001,%edx
/* FIR:
   frame_g_00628_03095.useless_val_00770 : int <- uarith_03109;
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	%edx,24(%eax)
/* FIR:
   let uarith_03110 : int = b_03097 + 1 in
   ...
*/
	movl	20(%ebp),%edx
	addl	$0x00000001,%edx
/* FIR:
   tailcall while_02880(frame_g_00628_03095, cont_03096, uarith_03110)
*/
	movl	%edx,20(%ebp)
	jmp	__while_02880
__false_case_03489:
/* FIR:
   tailcall cont_03096(())
*/
	movl	(%ebx),%ecx
	movl	4(%ebx),%eax
	movl	$0x00000000,%ebx
	jmp	*%ecx
__println_cont_closure_03229:
	jmp	__println_cont_02879
__println_cont_02879:
	jmp	__println_cont_body_03480
__println_cont_body_03480:
/* FIR:
   let c_03093 : int = frame_return_fun_00771_03091.c_00774 in
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	12(%eax),%ebx
/* FIR:
   let cont_03094 :
      (int) -> unit =
      frame_return_fun_00771_03091.cont_00772 in
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	4(%eax),%eax
/* FIR:
   tailcall cont_03094(c_03093)
*/
	movl	(%eax),%ecx
	movl	4(%eax),%eax
	jmp	*%ecx
__psuedo_add_cont_closure_03228:
	movl	%eax,16(%ebp)
	jmp	__psuedo_add_cont_02878
__psuedo_add_cont_02878:
	movl	__mem_limit,%eax
	subl	__mem_next,%eax
	cmpl	$0x00000028,%eax
	jl	__psuedo_add_cont_gc_03478
	jmp	__psuedo_add_cont_body_03468
__psuedo_add_cont_gc_03478:
	pushl	$40
	pushl	$__gc_record_03807
	call	__gc
	addl	$8,%esp
/* Pointer operands: 16(%ebp) */
	.data
	.align	4
__gc_record_03807:
	.long	0x00000000
	.long	16
	.long	-1
	.text
	jmp	__psuedo_add_cont_body_03468
__psuedo_add_cont_body_03468:
/* FIR:
   let frame_g_00628_03081 :
      frame_g_00628_02917 =
      frame_return_fun_00771_03079.frame_g_00628_00908 in
   ...
*/
	cmpl	$0x00000000,16(%ebp)
	je	__seg_fault
	movl	16(%ebp),%eax
	movl	20(%eax),%eax
	movl	%eax,8(%ebp)
/* FIR:
   frame_return_fun_00771_03079.c_00774 : int <- psuedo_add_03080;
   ...
*/
	cmpl	$0x00000000,16(%ebp)
	je	__seg_fault
	movl	16(%ebp),%eax
	movl	%ebx,12(%eax)
/* FIR:
   let j_03082 : int = frame_return_fun_00771_03079.j_00777 in
   ...
*/
	cmpl	$0x00000000,16(%ebp)
	je	__seg_fault
	movl	16(%ebp),%eax
	movl	16(%eax),%eax
/* FIR:
   let string_03083 :
      string =
      external ("itoa" : (int) -> string)(j_03082) in
   ...
*/
	pushl	%eax
	call	__itoa
	addl	$0x00000004,%esp
/* FIR:
   let strcat_03084 :
      string =
      external ("strcat" : (string, string) -> string)(str_02897,
                                                       string_03083) in
   ...
*/
	pushl	%eax
	pushl	$__str_02897
	call	__strcat
	addl	$0x00000008,%esp
	movl	%eax,%edx
/* FIR:
   let this_03085 : HigherOrder1_02908 = frame_g_00628_03081.this_00763 in
   ...
*/
	cmpl	$0x00000000,8(%ebp)
	je	__seg_fault
	movl	8(%ebp),%eax
	movl	4(%eax),%eax
/* FIR:
   let println_03086 :
      HigherOrder1_classtype_02913 =
      this_03085.class_00115 in
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	4(%eax),%eax
/* FIR:
   let println_03087 :
      method[FjObject_02907] (((unit) -> unit, ty_exnh_02906, string) -> unit =
      println_03086.println_00383 in
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	4(%eax),%esi
/* FIR:
   let this_03088 : HigherOrder1_02908 = frame_g_00628_03081.this_00763 in
   ...
*/
	cmpl	$0x00000000,8(%ebp)
	je	__seg_fault
	movl	8(%ebp),%eax
	movl	4(%eax),%eax
/* FIR:
   let println_cont_03089 :
      (unit) -> unit =
      closure(println_cont_02879, frame_return_fun_00771_03079) in
   ...
*/
	movl	__mem_next,%ebx
	leal	4(%ebx),%ebx
	movl	$0x00000008,-4(%ebx)
	addl	$0x0000000c,__mem_next
	movl	$__println_cont_closure_03229,%ecx
	movl	%ecx,(%ebx)
	movl	16(%ebp),%ecx
	movl	%ecx,4(%ebx)
/* FIR:
   let exnh_03090 :
      ty_exnh_02906 =
      frame_return_fun_00771_03079.exnh_00773 in
   ...
*/
	cmpl	$0x00000000,16(%ebp)
	je	__seg_fault
	movl	16(%ebp),%ecx
	movl	8(%ecx),%ecx
/* FIR:
   methodcall println_03087[this_03088](println_cont_03089,
                                        exnh_03090,
                                        strcat_03084)
*/
	jmp	*%esi
__return_fun_closure_03227:
	jmp	__return_fun_02877
__return_fun_02877:
	movl	__mem_limit,%esi
	subl	__mem_next,%esi
	cmpl	$0x00000028,%esi
	jl	__return_fun_gc_03466
	jmp	__return_fun_body_03446
__return_fun_gc_03466:
	pushl	$40
	pushl	$__gc_record_03808
	call	__gc
	addl	$8,%esp
/* Pointer operands: %ecx %ebx %eax */
	.data
	.align	4
__gc_record_03808:
	.long	0x00000007
	.long	-1
	.text
	jmp	__return_fun_body_03446
__return_fun_body_03446:
/* FIR:
   let frame_return_fun_00771_03070 :
      frame_return_fun_00771_02918 =
      frame {
         cont_00772 = cont_03067;
         exnh_00773 = exnh_03068;
         c_00774 = c_03069;
         j_00777 = 0;
         frame_g_00628_00908 = frame_g_00628_03066;
      } in
   ...
*/
	movl	__mem_next,%esi
	leal	4(%esi),%esi
	movl	%esi,20(%ebp)
	movl	20(%ebp),%esi
	movl	$0x00000019,-4(%esi)
	addl	$0x0000001c,__mem_next
	movl	$__cont_rttd_03213,%edi
	movl	20(%ebp),%esi
	movl	%edi,0(%esi)
	movl	20(%ebp),%esi
	movl	%ebx,4(%esi)
	movl	20(%ebp),%ebx
	movl	%ecx,8(%ebx)
	movl	20(%ebp),%ebx
	movl	%edx,12(%ebx)
	movl	20(%ebp),%ebx
	movl	$0x00000000,16(%ebx)
	movl	20(%ebp),%ebx
	movl	%eax,20(%ebx)
/* FIR:
   frame_return_fun_00771_03070.j_00777 : int <- 23;
   ...
*/
	movl	20(%ebp),%esi
	cmpl	$0x00000000,%esi
	je	__seg_fault
	movl	$0x00000017,%ebx
	movl	%ebx,16(%esi)
/* FIR:
   frame_g_00628_03066.useless_val_00770 : int <- -5;
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	$0xfffffffb,%ebx
	movl	%ebx,24(%eax)
/* FIR:
   let array_03071 : int[] = frame_g_00628_03066.array_00767 in
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	20(%eax),%edi
/* FIR:
   array_03071[0] : int <- 117;
   ...
*/
	cmpl	$0x00000000,%edi
	je	__seg_fault
	movl	-4(%edi),%ebx
	shrl	$0,%ebx
	andl	$0xfffffffc,%ebx
	movl	$0x00000000,%esi
	cmpl	%esi,%ebx
	jna	__seg_fault
	movl	$0x00000075,%ebx
	movl	%ebx,0(%edi,%esi,4)
/* FIR:
   let array_03072 : int[] = frame_g_00628_03066.array_00767 in
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	20(%eax),%edi
/* FIR:
   array_03072[1] : int <- 116;
   ...
*/
	cmpl	$0x00000000,%edi
	je	__seg_fault
	movl	-4(%edi),%ebx
	shrl	$0,%ebx
	andl	$0xfffffffc,%ebx
	movl	$0x00000001,%esi
	cmpl	%esi,%ebx
	jna	__seg_fault
	movl	$0x00000074,%ebx
	movl	%ebx,0(%edi,%esi,4)
/* FIR:
   let this_03073 : HigherOrder1_02908 = frame_g_00628_03066.this_00763 in
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	4(%eax),%ebx
/* FIR:
   let psuedo_add_03074 :
      HigherOrder1_classtype_02913 =
      this_03073.class_00115 in
   ...
*/
	cmpl	$0x00000000,%ebx
	je	__seg_fault
	movl	4(%ebx),%ebx
/* FIR:
   let psuedo_add_03075 :
      method[HigherOrder1_02908] (((int) -> unit,
                                  ty_exnh_02906,
                                  int,
                                  int,
                                  int[]) ->
         unit =
      psuedo_add_03074.psuedo_add_00386 in
   ...
*/
	cmpl	$0x00000000,%ebx
	je	__seg_fault
	movl	16(%ebx),%ebx
	movl	%ebx,12(%ebp)
/* FIR:
   let this_03076 : HigherOrder1_02908 = frame_g_00628_03066.this_00763 in
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	4(%eax),%esi
/* FIR:
   let psuedo_add_cont_03077 :
      (int) -> unit =
      closure(psuedo_add_cont_02878, frame_return_fun_00771_03070) in
   ...
*/
	movl	__mem_next,%ebx
	leal	4(%ebx),%ebx
	movl	$0x00000008,-4(%ebx)
	addl	$0x0000000c,__mem_next
	movl	$__psuedo_add_cont_closure_03228,%edi
	movl	%edi,(%ebx)
	movl	20(%ebp),%edi
	movl	%edi,4(%ebx)
/* FIR:
   let array_03078 : int[] = frame_g_00628_03066.array_00767 in
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	20(%eax),%edi
/* FIR:
   methodcall psuedo_add_03075[this_03076](psuedo_add_cont_03077,
                                           exnh_03068,
                                           c_03069,
                                           -10,
                                           array_03078)
*/
	movl	%esi,%eax
	movl	$0xfffffff6,0(%ebp)
	movl	%edi,4(%ebp)
	jmp	*12(%ebp)
__println_cont_closure_03226:
	jmp	__println_cont_02876
__println_cont_02876:
	jmp	__println_cont_body_03442
__println_cont_body_03442:
/* FIR:
   let cont_03065 : (unit) -> unit = frame_main_00627_03063.cont_00692 in
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	8(%eax),%eax
/* FIR:
   tailcall cont_03065(())
*/
	movl	(%eax),%ecx
	movl	4(%eax),%eax
	movl	$0x00000000,%ebx
	jmp	*%ecx
__println_cont_closure_03225:
	movl	%eax,12(%ebp)
	jmp	__println_cont_02875
__println_cont_02875:
	movl	__mem_limit,%eax
	subl	__mem_next,%eax
	cmpl	$0x00000050,%eax
	jl	__println_cont_gc_03440
	jmp	__println_cont_body_03426
__println_cont_gc_03440:
	pushl	$80
	pushl	$__gc_record_03809
	call	__gc
	addl	$8,%esp
/* Pointer operands: 12(%ebp) */
	.data
	.align	4
__gc_record_03809:
	.long	0x00000000
	.long	12
	.long	-1
	.text
	jmp	__println_cont_body_03426
__println_cont_body_03426:
/* FIR:
   let array_03048 : int[] = frame_main_00627_03046.array_00701 in
   ...
*/
	cmpl	$0x00000000,12(%ebp)
	je	__seg_fault
	movl	12(%ebp),%eax
	movl	28(%eax),%ecx
/* FIR:
   let sub_03049 : int = array_03048[0] in
   ...
*/
	cmpl	$0x00000000,%ecx
	je	__seg_fault
	movl	-4(%ecx),%eax
	shrl	$0,%eax
	andl	$0xfffffffc,%eax
	movl	$0x00000000,%ebx
	cmpl	%ebx,%eax
	jna	__seg_fault
	movl	0(%ecx,%ebx,4),%eax
/* FIR:
   let string_03050 :
      string =
      external ("itoa" : (int) -> string)(sub_03049) in
   ...
*/
	pushl	%eax
	call	__itoa
	addl	$0x00000004,%esp
/* FIR:
   let strcat_03051 :
      string =
      external ("strcat" : (string, string) -> string)(str_02895,
                                                       string_03050) in
   ...
*/
	pushl	%eax
	pushl	$__str_02895
	call	__strcat
	addl	$0x00000008,%esp
/* FIR:
   let strcat_03052 :
      string =
      external ("strcat" : (string, string) -> string)(strcat_03051,
                                                       str_02896) in
   ...
*/
	pushl	$__str_02896
	pushl	%eax
	call	__strcat
	addl	$0x00000008,%esp
	movl	%eax,8(%ebp)
/* FIR:
   let array_03053 : int[] = frame_main_00627_03046.array_00701 in
   ...
*/
	cmpl	$0x00000000,12(%ebp)
	je	__seg_fault
	movl	12(%ebp),%eax
	movl	28(%eax),%ecx
/* FIR:
   let sub_03054 : int = array_03053[1] in
   ...
*/
	cmpl	$0x00000000,%ecx
	je	__seg_fault
	movl	-4(%ecx),%eax
	shrl	$0,%eax
	andl	$0xfffffffc,%eax
	movl	$0x00000001,%ebx
	cmpl	%ebx,%eax
	jna	__seg_fault
	movl	0(%ecx,%ebx,4),%eax
/* FIR:
   let string_03055 :
      string =
      external ("itoa" : (int) -> string)(sub_03054) in
   ...
*/
	pushl	%eax
	call	__itoa
	addl	$0x00000004,%esp
/* FIR:
   let strcat_03056 :
      string =
      external ("strcat" : (string, string) -> string)(strcat_03052,
                                                       string_03055) in
   ...
*/
	pushl	%eax
	pushl	8(%ebp)
	call	__strcat
	addl	$0x00000008,%esp
	movl	%eax,%edx
/* FIR:
   let this_03057 : HigherOrder1_02908 = frame_main_00627_03046.this_00691 in
   ...
*/
	cmpl	$0x00000000,12(%ebp)
	je	__seg_fault
	movl	12(%ebp),%eax
	movl	4(%eax),%eax
/* FIR:
   let println_03058 :
      HigherOrder1_classtype_02913 =
      this_03057.class_00115 in
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	4(%eax),%eax
/* FIR:
   let println_03059 :
      method[FjObject_02907] (((unit) -> unit, ty_exnh_02906, string) -> unit =
      println_03058.println_00383 in
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	4(%eax),%esi
/* FIR:
   let this_03060 : HigherOrder1_02908 = frame_main_00627_03046.this_00691 in
   ...
*/
	cmpl	$0x00000000,12(%ebp)
	je	__seg_fault
	movl	12(%ebp),%eax
	movl	4(%eax),%eax
/* FIR:
   let println_cont_03061 :
      (unit) -> unit =
      closure(println_cont_02876, frame_main_00627_03046) in
   ...
*/
	movl	__mem_next,%ebx
	leal	4(%ebx),%ebx
	movl	$0x00000008,-4(%ebx)
	addl	$0x0000000c,__mem_next
	movl	$__println_cont_closure_03226,%ecx
	movl	%ecx,(%ebx)
	movl	12(%ebp),%ecx
	movl	%ecx,4(%ebx)
/* FIR:
   let exnh_03062 : ty_exnh_02906 = frame_main_00627_03046.exnh_00693 in
   ...
*/
	cmpl	$0x00000000,12(%ebp)
	je	__seg_fault
	movl	12(%ebp),%ecx
	movl	12(%ecx),%ecx
/* FIR:
   methodcall println_03059[this_03060](println_cont_03061,
                                        exnh_03062,
                                        strcat_03056)
*/
	jmp	*%esi
__f_cont_closure_03224:
	movl	%eax,8(%ebp)
	jmp	__f_cont_02874
__f_cont_02874:
	movl	__mem_limit,%eax
	subl	__mem_next,%eax
	cmpl	$0x00000028,%eax
	jl	__f_cont_gc_03424
	jmp	__f_cont_body_03417
__f_cont_gc_03424:
	pushl	$40
	pushl	$__gc_record_03810
	call	__gc
	addl	$8,%esp
/* Pointer operands: 8(%ebp) */
	.data
	.align	4
__gc_record_03810:
	.long	0x00000000
	.long	8
	.long	-1
	.text
	jmp	__f_cont_body_03417
__f_cont_body_03417:
/* FIR:
   let string_03037 :
      string =
      external ("itoa" : (int) -> string)(f_03036) in
   ...
*/
	pushl	%ebx
	call	__itoa
	addl	$0x00000004,%esp
/* FIR:
   let str_03038 : string = frame_main_00627_03035.str_00741 in
   ...
*/
	cmpl	$0x00000000,8(%ebp)
	je	__seg_fault
	movl	8(%ebp),%ebx
	movl	36(%ebx),%ebx
/* FIR:
   let strcat_03039 :
      string =
      external ("strcat" : (string, string) -> string)(str_03038,
                                                       string_03037) in
   ...
*/
	pushl	%eax
	pushl	%ebx
	call	__strcat
	addl	$0x00000008,%esp
	movl	%eax,%edx
/* FIR:
   let this_03040 : HigherOrder1_02908 = frame_main_00627_03035.this_00691 in
   ...
*/
	cmpl	$0x00000000,8(%ebp)
	je	__seg_fault
	movl	8(%ebp),%eax
	movl	4(%eax),%eax
/* FIR:
   let println_03041 :
      HigherOrder1_classtype_02913 =
      this_03040.class_00115 in
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	4(%eax),%eax
/* FIR:
   let println_03042 :
      method[FjObject_02907] (((unit) -> unit, ty_exnh_02906, string) -> unit =
      println_03041.println_00383 in
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	4(%eax),%esi
/* FIR:
   let this_03043 : HigherOrder1_02908 = frame_main_00627_03035.this_00691 in
   ...
*/
	cmpl	$0x00000000,8(%ebp)
	je	__seg_fault
	movl	8(%ebp),%eax
	movl	4(%eax),%eax
/* FIR:
   let println_cont_03044 :
      (unit) -> unit =
      closure(println_cont_02875, frame_main_00627_03035) in
   ...
*/
	movl	__mem_next,%ebx
	leal	4(%ebx),%ebx
	movl	$0x00000008,-4(%ebx)
	addl	$0x0000000c,__mem_next
	movl	$__println_cont_closure_03225,%ecx
	movl	%ecx,(%ebx)
	movl	8(%ebp),%ecx
	movl	%ecx,4(%ebx)
/* FIR:
   let exnh_03045 : ty_exnh_02906 = frame_main_00627_03035.exnh_00693 in
   ...
*/
	cmpl	$0x00000000,8(%ebp)
	je	__seg_fault
	movl	8(%ebp),%ecx
	movl	12(%ecx),%ecx
/* FIR:
   methodcall println_03042[this_03043](println_cont_03044,
                                        exnh_03045,
                                        strcat_03039)
*/
	jmp	*%esi
__println_cont_closure_03223:
	jmp	__println_cont_02873
__println_cont_02873:
	movl	__mem_limit,%ebx
	subl	__mem_next,%ebx
	cmpl	$0x0000000c,%ebx
	jl	__println_cont_gc_03415
	jmp	__println_cont_body_03408
__println_cont_gc_03415:
	pushl	$12
	pushl	$__gc_record_03811
	call	__gc
	addl	$8,%esp
/* Pointer operands: %eax */
	.data
	.align	4
__gc_record_03811:
	.long	0x00000001
	.long	-1
	.text
	jmp	__println_cont_body_03408
__println_cont_body_03408:
/* FIR:
   frame_main_00627_03030.str_00741 : string <- str_02894;
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	$__str_02894,%ebx
	movl	%ebx,36(%eax)
/* FIR:
   let f_cont_03032 :
      (int) -> unit =
      closure(f_cont_02874, frame_main_00627_03030) in
   ...
*/
	movl	__mem_next,%ebx
	leal	4(%ebx),%ebx
	movl	$0x00000008,-4(%ebx)
	addl	$0x0000000c,__mem_next
	movl	$__f_cont_closure_03224,%ecx
	movl	%ecx,(%ebx)
	movl	%eax,4(%ebx)
/* FIR:
   let exnh_03033 : ty_exnh_02906 = frame_main_00627_03030.exnh_00693 in
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	12(%eax),%ecx
/* FIR:
   let f_03034 :
      (((int) -> unit, ty_exnh_02906, int) -> unit =
      frame_main_00627_03030.f_00720 in
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	32(%eax),%eax
/* FIR:
   tailcall f_03034(f_cont_03032, exnh_03033, 23)
*/
	movl	(%eax),%esi
	movl	4(%eax),%eax
	movl	$0x00000017,%edx
	jmp	*%esi
__println_cont_closure_03222:
	movl	%eax,8(%ebp)
	jmp	__println_cont_02872
__println_cont_02872:
	movl	__mem_limit,%eax
	subl	__mem_next,%eax
	cmpl	$0x00000028,%eax
	jl	__println_cont_gc_03406
	jmp	__println_cont_body_03399
__println_cont_gc_03406:
	pushl	$40
	pushl	$__gc_record_03812
	call	__gc
	addl	$8,%esp
/* Pointer operands: 8(%ebp) */
	.data
	.align	4
__gc_record_03812:
	.long	0x00000000
	.long	8
	.long	-1
	.text
	jmp	__println_cont_body_03399
__println_cont_body_03399:
/* FIR:
   let i_03021 : int = frame_main_00627_03019.i_00697 in
   ...
*/
	cmpl	$0x00000000,8(%ebp)
	je	__seg_fault
	movl	8(%ebp),%eax
	movl	16(%eax),%eax
/* FIR:
   let string_03022 :
      string =
      external ("itoa" : (int) -> string)(i_03021) in
   ...
*/
	pushl	%eax
	call	__itoa
	addl	$0x00000004,%esp
/* FIR:
   let strcat_03023 :
      string =
      external ("strcat" : (string, string) -> string)(str_02893,
                                                       string_03022) in
   ...
*/
	pushl	%eax
	pushl	$__str_02893
	call	__strcat
	addl	$0x00000008,%esp
	movl	%eax,%edx
/* FIR:
   let this_03024 : HigherOrder1_02908 = frame_main_00627_03019.this_00691 in
   ...
*/
	cmpl	$0x00000000,8(%ebp)
	je	__seg_fault
	movl	8(%ebp),%eax
	movl	4(%eax),%eax
/* FIR:
   let println_03025 :
      HigherOrder1_classtype_02913 =
      this_03024.class_00115 in
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	4(%eax),%eax
/* FIR:
   let println_03026 :
      method[FjObject_02907] (((unit) -> unit, ty_exnh_02906, string) -> unit =
      println_03025.println_00383 in
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	4(%eax),%esi
/* FIR:
   let this_03027 : HigherOrder1_02908 = frame_main_00627_03019.this_00691 in
   ...
*/
	cmpl	$0x00000000,8(%ebp)
	je	__seg_fault
	movl	8(%ebp),%eax
	movl	4(%eax),%eax
/* FIR:
   let println_cont_03028 :
      (unit) -> unit =
      closure(println_cont_02873, frame_main_00627_03019) in
   ...
*/
	movl	__mem_next,%ebx
	leal	4(%ebx),%ebx
	movl	$0x00000008,-4(%ebx)
	addl	$0x0000000c,__mem_next
	movl	$__println_cont_closure_03223,%ecx
	movl	%ecx,(%ebx)
	movl	8(%ebp),%ecx
	movl	%ecx,4(%ebx)
/* FIR:
   let exnh_03029 : ty_exnh_02906 = frame_main_00627_03019.exnh_00693 in
   ...
*/
	cmpl	$0x00000000,8(%ebp)
	je	__seg_fault
	movl	8(%ebp),%ecx
	movl	12(%ecx),%ecx
/* FIR:
   methodcall println_03026[this_03027](println_cont_03028,
                                        exnh_03029,
                                        strcat_03023)
*/
	jmp	*%esi
__g_cont_closure_03221:
	movl	%eax,12(%ebp)
	jmp	__g_cont_02871
__g_cont_02871:
	movl	__mem_limit,%eax
	subl	__mem_next,%eax
	cmpl	$0x00000050,%eax
	jl	__g_cont_gc_03397
	jmp	__g_cont_body_03371
__g_cont_gc_03397:
	pushl	$80
	pushl	$__gc_record_03813
	call	__gc
	addl	$8,%esp
/* Pointer operands: %ebx 12(%ebp) */
	.data
	.align	4
__gc_record_03813:
	.long	0x00000002
	.long	12
	.long	-1
	.text
	jmp	__g_cont_body_03371
__g_cont_body_03371:
/* FIR:
   frame_main_00627_03000.f_00720 : (((int) -> unit, ty_exnh_02906, int) ->
                                       unit <- g_03001;
   ...
*/
	cmpl	$0x00000000,12(%ebp)
	je	__seg_fault
	movl	12(%ebp),%eax
	movl	%ebx,32(%eax)
/* FIR:
   let array_03002 : int[] = frame_main_00627_03000.array_00701 in
   ...
*/
	cmpl	$0x00000000,12(%ebp)
	je	__seg_fault
	movl	12(%ebp),%eax
	movl	28(%eax),%ecx
/* FIR:
   array_03002[0] : int <- 51;
   ...
*/
	cmpl	$0x00000000,%ecx
	je	__seg_fault
	movl	-4(%ecx),%eax
	shrl	$0,%eax
	andl	$0xfffffffc,%eax
	movl	$0x00000000,%ebx
	cmpl	%ebx,%eax
	jna	__seg_fault
	movl	$0x00000033,%eax
	movl	%eax,0(%ecx,%ebx,4)
/* FIR:
   let array_03003 : int[] = frame_main_00627_03000.array_00701 in
   ...
*/
	cmpl	$0x00000000,12(%ebp)
	je	__seg_fault
	movl	12(%ebp),%eax
	movl	28(%eax),%ecx
/* FIR:
   array_03003[1] : int <- 52;
   ...
*/
	cmpl	$0x00000000,%ecx
	je	__seg_fault
	movl	-4(%ecx),%eax
	shrl	$0,%eax
	andl	$0xfffffffc,%eax
	movl	$0x00000001,%ebx
	cmpl	%ebx,%eax
	jna	__seg_fault
	movl	$0x00000034,%eax
	movl	%eax,0(%ecx,%ebx,4)
/* FIR:
   let array_03004 : int[] = frame_main_00627_03000.array_00701 in
   ...
*/
	cmpl	$0x00000000,12(%ebp)
	je	__seg_fault
	movl	12(%ebp),%eax
	movl	28(%eax),%ecx
/* FIR:
   let sub_03005 : int = array_03004[0] in
   ...
*/
	cmpl	$0x00000000,%ecx
	je	__seg_fault
	movl	-4(%ecx),%eax
	shrl	$0,%eax
	andl	$0xfffffffc,%eax
	movl	$0x00000000,%ebx
	cmpl	%ebx,%eax
	jna	__seg_fault
	movl	0(%ecx,%ebx,4),%eax
/* FIR:
   let string_03006 :
      string =
      external ("itoa" : (int) -> string)(sub_03005) in
   ...
*/
	pushl	%eax
	call	__itoa
	addl	$0x00000004,%esp
/* FIR:
   let strcat_03007 :
      string =
      external ("strcat" : (string, string) -> string)(str_02891,
                                                       string_03006) in
   ...
*/
	pushl	%eax
	pushl	$__str_02891
	call	__strcat
	addl	$0x00000008,%esp
/* FIR:
   let strcat_03008 :
      string =
      external ("strcat" : (string, string) -> string)(strcat_03007,
                                                       str_02892) in
   ...
*/
	pushl	$__str_02892
	pushl	%eax
	call	__strcat
	addl	$0x00000008,%esp
	movl	%eax,8(%ebp)
/* FIR:
   let array_03009 : int[] = frame_main_00627_03000.array_00701 in
   ...
*/
	cmpl	$0x00000000,12(%ebp)
	je	__seg_fault
	movl	12(%ebp),%eax
	movl	28(%eax),%ecx
/* FIR:
   let sub_03010 : int = array_03009[1] in
   ...
*/
	cmpl	$0x00000000,%ecx
	je	__seg_fault
	movl	-4(%ecx),%eax
	shrl	$0,%eax
	andl	$0xfffffffc,%eax
	movl	$0x00000001,%ebx
	cmpl	%ebx,%eax
	jna	__seg_fault
	movl	0(%ecx,%ebx,4),%eax
/* FIR:
   let string_03011 :
      string =
      external ("itoa" : (int) -> string)(sub_03010) in
   ...
*/
	pushl	%eax
	call	__itoa
	addl	$0x00000004,%esp
/* FIR:
   let strcat_03012 :
      string =
      external ("strcat" : (string, string) -> string)(strcat_03008,
                                                       string_03011) in
   ...
*/
	pushl	%eax
	pushl	8(%ebp)
	call	__strcat
	addl	$0x00000008,%esp
	movl	%eax,%edx
/* FIR:
   let this_03013 : HigherOrder1_02908 = frame_main_00627_03000.this_00691 in
   ...
*/
	cmpl	$0x00000000,12(%ebp)
	je	__seg_fault
	movl	12(%ebp),%eax
	movl	4(%eax),%eax
/* FIR:
   let println_03014 :
      HigherOrder1_classtype_02913 =
      this_03013.class_00115 in
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	4(%eax),%eax
/* FIR:
   let println_03015 :
      method[FjObject_02907] (((unit) -> unit, ty_exnh_02906, string) -> unit =
      println_03014.println_00383 in
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	4(%eax),%esi
/* FIR:
   let this_03016 : HigherOrder1_02908 = frame_main_00627_03000.this_00691 in
   ...
*/
	cmpl	$0x00000000,12(%ebp)
	je	__seg_fault
	movl	12(%ebp),%eax
	movl	4(%eax),%eax
/* FIR:
   let println_cont_03017 :
      (unit) -> unit =
      closure(println_cont_02872, frame_main_00627_03000) in
   ...
*/
	movl	__mem_next,%ebx
	leal	4(%ebx),%ebx
	movl	$0x00000008,-4(%ebx)
	addl	$0x0000000c,__mem_next
	movl	$__println_cont_closure_03222,%ecx
	movl	%ecx,(%ebx)
	movl	12(%ebp),%ecx
	movl	%ecx,4(%ebx)
/* FIR:
   let exnh_03018 : ty_exnh_02906 = frame_main_00627_03000.exnh_00693 in
   ...
*/
	cmpl	$0x00000000,12(%ebp)
	je	__seg_fault
	movl	12(%ebp),%ecx
	movl	12(%ecx),%ecx
/* FIR:
   methodcall println_03015[this_03016](println_cont_03017,
                                        exnh_03018,
                                        strcat_03012)
*/
	jmp	*%esi
__println_cont_closure_03220:
	movl	%eax,%esi
	jmp	__println_cont_02870
__println_cont_02870:
	movl	__mem_limit,%eax
	subl	__mem_next,%eax
	cmpl	$0x0000000c,%eax
	jl	__println_cont_gc_03369
	jmp	__println_cont_body_03360
__println_cont_gc_03369:
	pushl	$12
	pushl	$__gc_record_03814
	call	__gc
	addl	$8,%esp
/* Pointer operands: %esi */
	.data
	.align	4
__gc_record_03814:
	.long	0x00000010
	.long	-1
	.text
	jmp	__println_cont_body_03360
__println_cont_body_03360:
/* FIR:
   let x_02990 : int = frame_main_00627_02988.x_00698 in
   ...
*/
	cmpl	$0x00000000,%esi
	je	__seg_fault
	movl	20(%esi),%edx
/* FIR:
   let y_02991 : int = frame_main_00627_02988.y_00699 in
   ...
*/
	cmpl	$0x00000000,%esi
	je	__seg_fault
	movl	24(%esi),%eax
/* FIR:
   let binop_02992 : int = x_02990 + y_02991 in
   ...
*/
	addl	%eax,%edx
/* FIR:
   let this_02993 : HigherOrder1_02908 = frame_main_00627_02988.this_00691 in
   ...
*/
	cmpl	$0x00000000,%esi
	je	__seg_fault
	movl	4(%esi),%eax
/* FIR:
   let g_02994 : HigherOrder1_classtype_02913 = this_02993.class_00115 in
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	4(%eax),%eax
/* FIR:
   let g_02995 :
      method[HigherOrder1_02908] (((((((int) -> unit, ty_exnh_02906, int) ->
                                        unit) ->
                                      unit,
                                  ty_exnh_02906,
                                  int,
                                  int[]) ->
         unit =
      g_02994.g_00385 in
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	12(%eax),%edi
/* FIR:
   let this_02996 : HigherOrder1_02908 = frame_main_00627_02988.this_00691 in
   ...
*/
	cmpl	$0x00000000,%esi
	je	__seg_fault
	movl	4(%esi),%eax
/* FIR:
   let g_cont_02997 :
      (((((int) -> unit, ty_exnh_02906, int) -> unit) -> unit =
      closure(g_cont_02871, frame_main_00627_02988) in
   ...
*/
	movl	__mem_next,%ebx
	leal	4(%ebx),%ebx
	movl	$0x00000008,-4(%ebx)
	addl	$0x0000000c,__mem_next
	movl	$__g_cont_closure_03221,%ecx
	movl	%ecx,(%ebx)
	movl	%esi,4(%ebx)
/* FIR:
   let exnh_02998 : ty_exnh_02906 = frame_main_00627_02988.exnh_00693 in
   ...
*/
	cmpl	$0x00000000,%esi
	je	__seg_fault
	movl	12(%esi),%ecx
/* FIR:
   let array_02999 : int[] = frame_main_00627_02988.array_00701 in
   ...
*/
	cmpl	$0x00000000,%esi
	je	__seg_fault
	movl	28(%esi),%esi
/* FIR:
   methodcall g_02995[this_02996](g_cont_02997,
                                  exnh_02998,
                                  binop_02992,
                                  array_02999)
*/
	movl	%esi,0(%ebp)
	jmp	*%edi
__main_cont_closure_03219:
	jmp	__main_cont_02869
__main_cont_02869:
	jmp	__main_cont_body_03356
__main_cont_body_03356:
/* FIR:
   let cont_02987 : (unit) -> unit = frame_main_00622_02985.cont_00663 in
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	4(%eax),%eax
/* FIR:
   tailcall cont_02987(())
*/
	movl	(%eax),%ecx
	movl	4(%eax),%eax
	movl	$0x00000000,%ebx
	jmp	*%ecx
__psuedo_add_closure_03218:
	movl	%ebx,%eax
	movl	0(%ebp),%edi
	movl	4(%ebp),%ebx
	jmp	__psuedo_add_02868
__psuedo_add_02868:
	jmp	__psuedo_add_body_03328
__psuedo_add_body_03328:
/* FIR:
   let binop_02979 : bool = y_02977 < 0 in
   ...
*/
	xorl	%ecx,%ecx
	cmpl	$0x00000000,%edi
	setl	%cl
/* FIR:
   if binop_02979 then ... else ...
*/
	cmpl	$0x00000000,%ecx
	jne	__true_case_03330
	jmp	__false_case_03331
__true_case_03330:
/* FIR:
   let unop_02980 : int = -[int] y_02977 in
   ...
*/
	negl	%edi
/* FIR:
   let binop_02981 : bool = 0 < unop_02980 in
   ...
*/
	xorl	%ecx,%ecx
	movl	$0x00000000,%ebx
	cmpl	%edi,%ebx
	setl	%cl
/* FIR:
   if binop_02981 then ... else ...
*/
	cmpl	$0x00000000,%ecx
	jne	__true_case_03333
	jmp	__false_case_03334
__false_case_03331:
/* FIR:
   let binop_02983 : bool = 0 < y_02977 in
   ...
*/
	xorl	%ecx,%ecx
	movl	$0x00000000,%ebx
	cmpl	%edi,%ebx
	setl	%cl
/* FIR:
   if binop_02983 then ... else ...
*/
	cmpl	$0x00000000,%ecx
	jne	__true_case_03344
	jmp	__false_case_03345
__true_case_03344:
/* FIR:
   tailcall while_02886(cont_02974, x_02976, 1, y_02977, 1)
*/
	movl	%edx,%ebx
	movl	$0x00000001,%esi
	movl	$0x00000001,20(%ebp)
	movl	%edi,%edx
	jmp	__while_02886
__false_case_03345:
/* FIR:
   let binop_02984 : int = 0 + x_02976 in
   ...
*/
	movl	$0x00000000,%ebx
	addl	%edx,%ebx
/* FIR:
   tailcall cont_02974(binop_02984)
*/
	movl	(%eax),%ecx
	movl	4(%eax),%eax
	jmp	*%ecx
__true_case_03333:
/* FIR:
   tailcall while_02886(cont_02974, x_02976, 1, unop_02980, 1)
*/
	movl	%edx,%ebx
	movl	$0x00000001,%esi
	movl	%edi,%edx
	movl	$0x00000001,20(%ebp)
	jmp	__while_02886
__false_case_03334:
/* FIR:
   let binop_02982 : int = 0 + x_02976 in
   ...
*/
	movl	$0x00000000,%ebx
	addl	%edx,%ebx
/* FIR:
   tailcall cont_02974(binop_02982)
*/
	movl	(%eax),%ecx
	movl	4(%eax),%eax
	jmp	*%ecx
__g_closure_03217:
	movl	%ecx,20(%ebp)
	movl	0(%ebp),%esi
	movl	%esi,28(%ebp)
	jmp	__g_02867
__g_02867:
	movl	__mem_limit,%esi
	subl	__mem_next,%esi
	cmpl	$0x00000038,%esi
	jl	__g_gc_03325
	jmp	__g_body_03273
__g_gc_03325:
	pushl	$56
	pushl	$__gc_record_03815
	call	__gc
	addl	$8,%esp
/* Pointer operands: 28(%ebp) 20(%ebp) %ebx %eax */
	.data
	.align	4
__gc_record_03815:
	.long	0x00000003
	.long	20
	.long	28
	.long	-1
	.text
	jmp	__g_body_03273
__g_body_03273:
/* FIR:
   let frame_g_00628_02952 :
      frame_g_00628_02917 =
      frame {
         this_00763 = this_02947;
         cont_00764 = cont_02948;
         exnh_00765 = exnh_02949;
         i_00766 = i_02950;
         array_00767 = array_02951;
         useless_val_00770 = 0;
      } in
   ...
*/
	movl	__mem_next,%esi
	leal	4(%esi),%esi
	movl	%esi,24(%ebp)
	movl	24(%ebp),%esi
	movl	$0x0000001d,-4(%esi)
	addl	$0x00000020,__mem_next
	movl	$__this_rttd_03212,%edi
	movl	24(%ebp),%esi
	movl	%edi,0(%esi)
	movl	24(%ebp),%esi
	movl	%eax,4(%esi)
	movl	24(%ebp),%esi
	movl	%ebx,8(%esi)
	movl	24(%ebp),%esi
	movl	20(%ebp),%ebx
	movl	%ebx,12(%esi)
	movl	24(%ebp),%ebx
	movl	%edx,16(%ebx)
	movl	24(%ebp),%esi
	movl	28(%ebp),%ebx
	movl	%ebx,20(%esi)
	movl	24(%ebp),%ebx
	movl	$0x00000000,24(%ebx)
/* FIR:
   array_02951[0] : int <- 0;
   ...
*/
	movl	28(%ebp),%edi
	cmpl	$0x00000000,%edi
	je	__seg_fault
	movl	-4(%edi),%ebx
	shrl	$0,%ebx
	andl	$0xfffffffc,%ebx
	movl	$0x00000000,%esi
	cmpl	%esi,%ebx
	jna	__seg_fault
	movl	$0x00000000,%ebx
	movl	%ebx,0(%edi,%esi,4)
/* FIR:
   array_02951[1] : int <- 1;
   ...
*/
	movl	28(%ebp),%edi
	cmpl	$0x00000000,%edi
	je	__seg_fault
	movl	-4(%edi),%ebx
	shrl	$0,%ebx
	andl	$0xfffffffc,%ebx
	movl	$0x00000001,%esi
	cmpl	%esi,%ebx
	jna	__seg_fault
	movl	$0x00000001,%ebx
	movl	%ebx,0(%edi,%esi,4)
/* FIR:
   let bogofun_cont_02953 :
      (unit) -> unit =
      closure(bogofun_cont_02881, frame_g_00628_02952) in
   ...
*/
	movl	__mem_next,%ebx
	leal	4(%ebx),%ebx
	movl	$0x00000008,-4(%ebx)
	addl	$0x0000000c,__mem_next
	movl	$__bogofun_cont_closure_03231,%esi
	movl	%esi,(%ebx)
	movl	24(%ebp),%esi
	movl	%esi,4(%ebx)
/* FIR:
   let binop_02954 : bool = 0 < i_02950 in
   ...
*/
	xorl	%ecx,%ecx
	movl	$0x00000000,%ecx
	cmpl	%edx,%ecx
	setl	%cl
/* FIR:
   if binop_02954 then ... else ...
*/
	cmpl	$0x00000000,%ecx
	jne	__true_case_03283
	jmp	__false_case_03284
__true_case_03283:
/* FIR:
   let subscript_02955 : int = array_02951[0] in
   ...
*/
	movl	28(%ebp),%esi
	cmpl	$0x00000000,%esi
	je	__seg_fault
	movl	-4(%esi),%eax
	shrl	$0,%eax
	andl	$0xfffffffc,%eax
	movl	$0x00000000,%edx
	cmpl	%edx,%eax
	jna	__seg_fault
	movl	0(%esi,%edx,4),%edi
/* FIR:
   let uarith_02956 : int = subscript_02955 + 1 in
   ...
*/
	addl	$0x00000001,%edi
/* FIR:
   array_02951[0] : int <- uarith_02956;
   ...
*/
	movl	28(%ebp),%esi
	cmpl	$0x00000000,%esi
	je	__seg_fault
	movl	-4(%esi),%eax
	shrl	$0,%eax
	andl	$0xfffffffc,%eax
	movl	$0x00000000,%edx
	cmpl	%edx,%eax
	jna	__seg_fault
	movl	%edi,0(%esi,%edx,4)
/* FIR:
   let subscript_02957 : int = array_02951[1] in
   ...
*/
	movl	28(%ebp),%esi
	cmpl	$0x00000000,%esi
	je	__seg_fault
	movl	-4(%esi),%eax
	shrl	$0,%eax
	andl	$0xfffffffc,%eax
	movl	$0x00000001,%edx
	cmpl	%edx,%eax
	jna	__seg_fault
	movl	0(%esi,%edx,4),%edi
/* FIR:
   let uarith_02958 : int = subscript_02957 + 1 in
   ...
*/
	addl	$0x00000001,%edi
/* FIR:
   array_02951[1] : int <- uarith_02958;
   ...
*/
	movl	28(%ebp),%esi
	cmpl	$0x00000000,%esi
	je	__seg_fault
	movl	-4(%esi),%eax
	shrl	$0,%eax
	andl	$0xfffffffc,%eax
	movl	$0x00000001,%edx
	cmpl	%edx,%eax
	jna	__seg_fault
	movl	%edi,0(%esi,%edx,4)
/* FIR:
   frame_g_00628_02952.useless_val_00770 : int <- 1;
   ...
*/
	movl	24(%ebp),%edx
	cmpl	$0x00000000,%edx
	je	__seg_fault
	movl	$0x00000001,%eax
	movl	%eax,24(%edx)
/* FIR:
   tailcall while_02880(frame_g_00628_02952, bogofun_cont_02953, 1)
*/
	movl	24(%ebp),%eax
	movl	$0x00000001,%edx
	movl	%edx,20(%ebp)
	jmp	__while_02880
__false_case_03284:
/* FIR:
   let sub_02959 : int = array_02951[0] in
   ...
*/
	movl	28(%ebp),%edi
	cmpl	$0x00000000,%edi
	je	__seg_fault
	movl	-4(%edi),%ebx
	shrl	$0,%ebx
	andl	$0xfffffffc,%ebx
	movl	$0x00000000,%esi
	cmpl	%esi,%ebx
	jna	__seg_fault
	movl	0(%edi,%esi,4),%ebx
/* FIR:
   let binop_02960 : bool = sub_02959 == i_02950 in
   ...
*/
	xorl	%ecx,%ecx
	cmpl	%edx,%ebx
	sete	%cl
/* FIR:
   if binop_02960 then ... else ...
*/
	cmpl	$0x00000000,%ecx
	jne	__true_case_03308
	jmp	__false_case_03309
__true_case_03308:
/* FIR:
   let sub_02961 : int = array_02951[1] in
   ...
*/
	movl	28(%ebp),%edi
	cmpl	$0x00000000,%edi
	je	__seg_fault
	movl	-4(%edi),%ebx
	shrl	$0,%ebx
	andl	$0xfffffffc,%ebx
	movl	$0x00000001,%esi
	cmpl	%esi,%ebx
	jna	__seg_fault
	movl	0(%edi,%esi,4),%ebx
/* FIR:
   let binop_02962 : int = i_02950 + 1 in
   ...
*/
	addl	$0x00000001,%edx
/* FIR:
   let binop_02963 : bool = sub_02961 == binop_02962 in
   ...
*/
	xorl	%ecx,%ecx
	cmpl	%edx,%ebx
	sete	%cl
/* FIR:
   if binop_02963 then ... else ...
*/
	cmpl	$0x00000000,%ecx
	jne	__true_case_03314
	jmp	__false_case_03315
__false_case_03309:
/* FIR:
   let println_02970 :
      HigherOrder1_classtype_02913 =
      this_02947.class_00115 in
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	4(%eax),%ebx
/* FIR:
   let println_02971 :
      method[FjObject_02907] (((unit) -> unit, ty_exnh_02906, string) -> unit =
      println_02970.println_00383 in
   ...
*/
	cmpl	$0x00000000,%ebx
	je	__seg_fault
	movl	4(%ebx),%esi
/* FIR:
   let println_cont_02972 :
      (unit) -> unit =
      closure(println_cont_02885, frame_g_00628_02952) in
   ...
*/
	movl	__mem_next,%ebx
	leal	4(%ebx),%ebx
	movl	$0x00000008,-4(%ebx)
	addl	$0x0000000c,__mem_next
	movl	$__println_cont_closure_03235,%ecx
	movl	%ecx,(%ebx)
	movl	24(%ebp),%ecx
	movl	%ecx,4(%ebx)
/* FIR:
   methodcall println_02971[this_02947](println_cont_02972,
                                        exnh_02949,
                                        str_02903)
*/
	movl	20(%ebp),%ecx
	movl	$__str_02903,%edx
	jmp	*%esi
__true_case_03314:
/* FIR:
   let println_02964 :
      HigherOrder1_classtype_02913 =
      this_02947.class_00115 in
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	4(%eax),%ebx
/* FIR:
   let println_02965 :
      method[FjObject_02907] (((unit) -> unit, ty_exnh_02906, string) -> unit =
      println_02964.println_00383 in
   ...
*/
	cmpl	$0x00000000,%ebx
	je	__seg_fault
	movl	4(%ebx),%esi
/* FIR:
   let println_cont_02966 :
      (unit) -> unit =
      closure(println_cont_02884, frame_g_00628_02952) in
   ...
*/
	movl	__mem_next,%ebx
	leal	4(%ebx),%ebx
	movl	$0x00000008,-4(%ebx)
	addl	$0x0000000c,__mem_next
	movl	$__println_cont_closure_03234,%ecx
	movl	%ecx,(%ebx)
	movl	24(%ebp),%ecx
	movl	%ecx,4(%ebx)
/* FIR:
   methodcall println_02965[this_02947](println_cont_02966,
                                        exnh_02949,
                                        str_02902)
*/
	movl	20(%ebp),%ecx
	movl	$__str_02902,%edx
	jmp	*%esi
__false_case_03315:
/* FIR:
   let println_02967 :
      HigherOrder1_classtype_02913 =
      this_02947.class_00115 in
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	4(%eax),%ebx
/* FIR:
   let println_02968 :
      method[FjObject_02907] (((unit) -> unit, ty_exnh_02906, string) -> unit =
      println_02967.println_00383 in
   ...
*/
	cmpl	$0x00000000,%ebx
	je	__seg_fault
	movl	4(%ebx),%esi
/* FIR:
   let println_cont_02969 :
      (unit) -> unit =
      closure(println_cont_02885, frame_g_00628_02952) in
   ...
*/
	movl	__mem_next,%ebx
	leal	4(%ebx),%ebx
	movl	$0x00000008,-4(%ebx)
	addl	$0x0000000c,__mem_next
	movl	$__println_cont_closure_03235,%ecx
	movl	%ecx,(%ebx)
	movl	24(%ebp),%ecx
	movl	%ecx,4(%ebx)
/* FIR:
   methodcall println_02968[this_02947](println_cont_02969,
                                        exnh_02949,
                                        str_02903)
*/
	movl	20(%ebp),%ecx
	movl	$__str_02903,%edx
	jmp	*%esi
__main_closure_03216:
	movl	%eax,20(%ebp)
	movl	%ecx,24(%ebp)
	jmp	__main_02866
__main_02866:
	movl	__mem_limit,%eax
	subl	__mem_next,%eax
	cmpl	$0x00000088,%eax
	jl	__main_gc_03271
	jmp	__main_body_03244
__main_gc_03271:
	pushl	$136
	pushl	$__gc_record_03816
	call	__gc
	addl	$8,%esp
/* Pointer operands: %edx 24(%ebp) %ebx 20(%ebp) */
	.data
	.align	4
__gc_record_03816:
	.long	0x0000000a
	.long	20
	.long	24
	.long	-1
	.text
	jmp	__main_body_03244
__main_body_03244:
/* FIR:
   let frame_main_00627_02935 :
      frame_main_00627_02916 =
      frame {
         this_00691 = this_02931;
         cont_00692 = cont_02932;
         exnh_00693 = exnh_02933;
         i_00697 = 0;
         x_00698 = 0;
         y_00699 = 0;
         array_00701 = nil;
         f_00720 = nil;
         str_00741 = nil;
      } in
   ...
*/
	movl	__mem_next,%eax
	leal	4(%eax),%eax
	movl	%eax,16(%ebp)
	movl	16(%ebp),%eax
	movl	$0x00000029,-4(%eax)
	addl	$0x0000002c,__mem_next
	movl	$__this_rttd_03211,%ecx
	movl	16(%ebp),%eax
	movl	%ecx,0(%eax)
	movl	16(%ebp),%ecx
	movl	20(%ebp),%eax
	movl	%eax,4(%ecx)
	movl	16(%ebp),%eax
	movl	%ebx,8(%eax)
	movl	16(%ebp),%ebx
	movl	24(%ebp),%eax
	movl	%eax,12(%ebx)
	movl	16(%ebp),%eax
	movl	$0x00000000,16(%eax)
	movl	16(%ebp),%eax
	movl	$0x00000000,20(%eax)
	movl	16(%ebp),%eax
	movl	$0x00000000,24(%eax)
	movl	16(%ebp),%eax
	movl	$0x00000000,28(%eax)
	movl	16(%ebp),%eax
	movl	$0x00000000,32(%eax)
	movl	16(%ebp),%eax
	movl	$0x00000000,36(%eax)
/* FIR:
   frame_main_00627_02935.i_00697 : int <- -2;
   ...
*/
	movl	16(%ebp),%ebx
	cmpl	$0x00000000,%ebx
	je	__seg_fault
	movl	$0xfffffffe,%eax
	movl	%eax,16(%ebx)
/* FIR:
   frame_main_00627_02935.x_00698 : int <- 1;
   ...
*/
	movl	16(%ebp),%ebx
	cmpl	$0x00000000,%ebx
	je	__seg_fault
	movl	$0x00000001,%eax
	movl	%eax,20(%ebx)
/* FIR:
   frame_main_00627_02935.y_00699 : int <- 2;
   ...
*/
	movl	16(%ebp),%ebx
	cmpl	$0x00000000,%ebx
	je	__seg_fault
	movl	$0x00000002,%eax
	movl	%eax,24(%ebx)
/* FIR:
   let alloc_array_02936 : int[] = new array[2] = 0 in
   ...
*/
	movl	__mem_next,%eax
	leal	4(%eax),%eax
	movl	%eax,12(%ebp)
	movl	12(%ebp),%eax
	movl	$0x00000008,-4(%eax)
	addl	$0x0000000c,__mem_next
	movl	12(%ebp),%eax
	movl	$0x00000000,0(%eax)
	movl	12(%ebp),%eax
	movl	$0x00000000,4(%eax)
/* FIR:
   frame_main_00627_02935.array_00701 : int[] <- alloc_array_02936;
   ...
*/
	movl	16(%ebp),%ebx
	cmpl	$0x00000000,%ebx
	je	__seg_fault
	movl	12(%ebp),%eax
	movl	%eax,28(%ebx)
/* FIR:
   alloc_array_02936[0] : int <- 8;
   ...
*/
	movl	12(%ebp),%ecx
	cmpl	$0x00000000,%ecx
	je	__seg_fault
	movl	-4(%ecx),%eax
	shrl	$0,%eax
	andl	$0xfffffffc,%eax
	movl	$0x00000000,%ebx
	cmpl	%ebx,%eax
	jna	__seg_fault
	movl	$0x00000008,%eax
	movl	%eax,0(%ecx,%ebx,4)
/* FIR:
   alloc_array_02936[1] : int <- 9;
   ...
*/
	movl	12(%ebp),%ecx
	cmpl	$0x00000000,%ecx
	je	__seg_fault
	movl	-4(%ecx),%eax
	shrl	$0,%eax
	andl	$0xfffffffc,%eax
	movl	$0x00000001,%ebx
	cmpl	%ebx,%eax
	jna	__seg_fault
	movl	$0x00000009,%eax
	movl	%eax,0(%ecx,%ebx,4)
/* FIR:
   let sub_02937 : int = alloc_array_02936[0] in
   ...
*/
	movl	12(%ebp),%ecx
	cmpl	$0x00000000,%ecx
	je	__seg_fault
	movl	-4(%ecx),%eax
	shrl	$0,%eax
	andl	$0xfffffffc,%eax
	movl	$0x00000000,%ebx
	cmpl	%ebx,%eax
	jna	__seg_fault
	movl	0(%ecx,%ebx,4),%eax
/* FIR:
   let string_02938 :
      string =
      external ("itoa" : (int) -> string)(sub_02937) in
   ...
*/
	pushl	%eax
	call	__itoa
	addl	$0x00000004,%esp
/* FIR:
   let strcat_02939 :
      string =
      external ("strcat" : (string, string) -> string)(str_02889,
                                                       string_02938) in
   ...
*/
	pushl	%eax
	pushl	$__str_02889
	call	__strcat
	addl	$0x00000008,%esp
/* FIR:
   let strcat_02940 :
      string =
      external ("strcat" : (string, string) -> string)(strcat_02939,
                                                       str_02890) in
   ...
*/
	pushl	$__str_02890
	pushl	%eax
	call	__strcat
	addl	$0x00000008,%esp
	movl	%eax,8(%ebp)
/* FIR:
   let sub_02941 : int = alloc_array_02936[1] in
   ...
*/
	movl	12(%ebp),%ecx
	cmpl	$0x00000000,%ecx
	je	__seg_fault
	movl	-4(%ecx),%eax
	shrl	$0,%eax
	andl	$0xfffffffc,%eax
	movl	$0x00000001,%ebx
	cmpl	%ebx,%eax
	jna	__seg_fault
	movl	0(%ecx,%ebx,4),%eax
/* FIR:
   let string_02942 :
      string =
      external ("itoa" : (int) -> string)(sub_02941) in
   ...
*/
	pushl	%eax
	call	__itoa
	addl	$0x00000004,%esp
/* FIR:
   let strcat_02943 :
      string =
      external ("strcat" : (string, string) -> string)(strcat_02940,
                                                       string_02942) in
   ...
*/
	pushl	%eax
	pushl	8(%ebp)
	call	__strcat
	addl	$0x00000008,%esp
	movl	%eax,%edx
/* FIR:
   let println_02944 :
      HigherOrder1_classtype_02913 =
      this_02931.class_00115 in
   ...
*/
	movl	20(%ebp),%eax
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	4(%eax),%eax
/* FIR:
   let println_02945 :
      method[FjObject_02907] (((unit) -> unit, ty_exnh_02906, string) -> unit =
      println_02944.println_00383 in
   ...
*/
	cmpl	$0x00000000,%eax
	je	__seg_fault
	movl	4(%eax),%esi
/* FIR:
   let println_cont_02946 :
      (unit) -> unit =
      closure(println_cont_02870, frame_main_00627_02935) in
   ...
*/
	movl	__mem_next,%eax
	leal	4(%eax),%ebx
	movl	$0x00000008,-4(%ebx)
	addl	$0x0000000c,__mem_next
	movl	$__println_cont_closure_03220,%eax
	movl	%eax,(%ebx)
	movl	16(%ebp),%eax
	movl	%eax,4(%ebx)
/* FIR:
   methodcall println_02945[this_02931](println_cont_02946,
                                        exnh_02933,
                                        strcat_02943)
*/
	movl	20(%ebp),%eax
	movl	24(%ebp),%ecx
	jmp	*%esi
__println_closure_03215:
	movl	%ebx,12(%ebp)
	jmp	__println_02865
__println_02865:
	jmp	__println_body_03241
__println_body_03241:
/* FIR:
   let int_02930 :
      unit =
      external ("println" : (string) -> unit)(string_02929) in
   ...
*/
	pushl	%edx
	call	__println
	addl	$0x00000004,%esp
	movl	%eax,%ebx
/* FIR:
   tailcall cont_02927(int_02930)
*/
	movl	12(%ebp),%eax
	movl	(%eax),%ecx
	movl	12(%ebp),%eax
	movl	4(%eax),%eax
	jmp	*%ecx
__main_closure_03214:
	movl	%ebx,16(%ebp)
	movl	%ecx,%edx
	jmp	__main_02864
__main_02864:
	movl	__mem_limit,%ebx
	subl	__mem_next,%ebx
	cmpl	$0x00000024,%ebx
	jl	__main_gc_03239
	jmp	__main_body_03237
__main_gc_03239:
	pushl	$36
	pushl	$__gc_record_03817
	call	__gc
	addl	$8,%esp
/* Pointer operands: %edx 16(%ebp) %eax */
	.data
	.align	4
__gc_record_03817:
	.long	0x00000009
	.long	16
	.long	-1
	.text
	jmp	__main_body_03237
__main_body_03237:
/* FIR:
   let frame_main_00622_02922 :
      frame_main_00622_02915 =
      frame {
         cont_00663 = cont_02919;
      } in
   ...
*/
	movl	__mem_next,%ebx
	leal	4(%ebx),%ecx
	movl	$0x00000009,-4(%ecx)
	addl	$0x0000000c,__mem_next
	movl	$__cont_rttd_03210,%ebx
	movl	%ebx,0(%ecx)
	movl	%eax,4(%ecx)
/* FIR:
   let HigherOrder1_02923 :
      HigherOrder1_02908 =
      object {
         class_00115 = HigherOrder1_class_02888;
      } in
   ...
*/
	movl	__mem_next,%eax
	leal	4(%eax),%eax
	movl	$0x00000009,-4(%eax)
	addl	$0x0000000c,__mem_next
	movl	$__class_rttd_03209,%ebx
	movl	%ebx,0(%eax)
	movl	$__HigherOrder1_class_02888,%ebx
	movl	%ebx,4(%eax)
/* FIR:
   let main_02924 :
      method[HigherOrder1_02908] (((unit) -> unit, ty_exnh_02906, string[]) ->
         unit =
      HigherOrder1_class_02888.main_00384 in
   ...
*/
	movl	$__HigherOrder1_class_02888,%ebx
	cmpl	$0x00000000,%ebx
	je	__seg_fault
	movl	8(%ebx),%esi
/* FIR:
   let main_cont_02925 :
      (unit) -> unit =
      closure(main_cont_02869, frame_main_00622_02922) in
   ...
*/
	movl	__mem_next,%ebx
	leal	4(%ebx),%ebx
	movl	$0x00000008,-4(%ebx)
	addl	$0x0000000c,__mem_next
	movl	$__main_cont_closure_03219,%edi
	movl	%edi,(%ebx)
	movl	%ecx,4(%ebx)
/* FIR:
   methodcall main_02924[HigherOrder1_02923](main_cont_02925,
                                             exnh_02920,
                                             argv_02921)
*/
	movl	16(%ebp),%ecx
	jmp	*%esi
