; RUN: llc < %s -march=x86-64 | FileCheck %s

; rdar://10436044
define <2 x double> @t3() nounwind readonly {
bb:
; CHECK: t3:
; CHECK: punpcklqdq %xmm1, %xmm0
; CHECK: movq (%rax), %xmm1
; CHECK: movsd %xmm1, %xmm0
  %tmp0 = load i128* null, align 1
  %tmp1 = load <2 x i32>* undef, align 8
  %tmp2 = bitcast i128 %tmp0 to <16 x i8>
  %tmp3 = bitcast <2 x i32> %tmp1 to i64
  %tmp4 = insertelement <2 x i64> undef, i64 %tmp3, i32 0
  %tmp5 = bitcast <16 x i8> %tmp2 to <2 x double>
  %tmp6 = bitcast <2 x i64> %tmp4 to <2 x double>
  %tmp7 = shufflevector <2 x double> %tmp5, <2 x double> %tmp6, <2 x i32> <i32 2, i32 1>
  ret <2 x double> %tmp7
}

; rdar://10450317
define <2 x i64> @t4() nounwind readonly {
bb:
; CHECK: t4:
; CHECK: punpcklqdq %xmm0, %xmm1
; CHECK: movq (%rax), %xmm0
; CHECK: movsd %xmm1, %xmm0
  %tmp0 = load i128* null, align 1
  %tmp1 = load <2 x i32>* undef, align 8
  %tmp2 = bitcast i128 %tmp0 to <16 x i8>
  %tmp3 = bitcast <2 x i32> %tmp1 to i64
  %tmp4 = insertelement <2 x i64> undef, i64 %tmp3, i32 0
  %tmp5 = bitcast <16 x i8> %tmp2 to <2 x i64>
  %tmp6 = shufflevector <2 x i64> %tmp4, <2 x i64> %tmp5, <2 x i32> <i32 2, i32 1>
  ret <2 x i64> %tmp6
}
