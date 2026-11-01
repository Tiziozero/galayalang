; ModuleID = 'c_api_abi_stress.c'
source_filename = "c_api_abi_stress.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.AbiAllSmall = type { i8, i8, i16, i16, i32, i32 }
%struct.AbiMixedOrderB = type { double, i64, i16, i8, i8 }
%struct.AbiArrayFields = type { [7 x i8], [3 x i16], [4 x i32], [2 x float] }
%struct.AbiMixedOrderA = type { i8, i64, i16, double, i8 }
%struct.AbiFloatMix = type { float, double, x86_fp80, float }
%struct.AbiNested = type { i16, %struct.AbiInner, i64, %struct.AbiInner }
%struct.AbiInner = type { i8, i32, double }
%struct.AbiLarge = type { i64, [32 x i8], i8, [7 x i8], [5 x double], [3 x %struct.AbiInner], ptr, i64, i32 }
%union.AbiScalarUnion = type { i64 }
%struct.__va_list_tag = type { i32, i32, ptr, ptr }
%struct.AbiTwoInts = type { i32, i32 }
%struct.AbiThreeDoubles = type { double, double, double }
%struct.AbiEightBytes = type { [8 x i8] }
%struct.AbiIntDouble = type { i32, double }
%struct.AbiDoubleInt = type { double, i32 }
%struct.AbiFourFloats = type { float, float, float, float }
%struct.AbiFiveInts = type { i32, i32, i32, i32, i32 }
%struct.AbiTwoU64 = type { i64, i64 }
%struct.AbiMixedReturn = type { i8, double, i32, i64, float }
%struct.AbiBitFields = type { i32 }
%struct.AbiPacked = type <{ i8, i32, i16, double }>
%struct.AbiMatrix2x2 = type { [2 x [2 x double]] }
%struct.AbiOpaque = type { i64, i32, double }

@__const.abi_make_all_small.s = private unnamed_addr constant %struct.AbiAllSmall { i8 -7, i8 -6, i16 -1234, i16 -11215, i32 -123456789, i32 -838178284 }, align 4
@__const.abi_make_mixed_b.s = private unnamed_addr constant %struct.AbiMixedOrderB { double 1.237500e+02, i64 -8613303245920329199, i16 4321, i8 99, i8 88 }, align 8
@__const.abi_make_array_fields.a = private unnamed_addr constant %struct.AbiArrayFields { [7 x i8] c"\01\02\03\7F\80\FE\FF", [3 x i16] [i16 -1, i16 2, i16 -300], [4 x i32] [i32 100000, i32 -200000, i32 300000, i32 -400000], [2 x float] [float 5.000000e-01, float -1.232500e+02] }, align 4
@.str = private unnamed_addr constant [17 x i8] c"abi-large-struct\00", align 1
@.str.1 = private unnamed_addr constant [13 x i8] c"C says hello\00", align 1
@.str.2 = private unnamed_addr constant [15 x i8] c"first callback\00", align 1
@.str.3 = private unnamed_addr constant [16 x i8] c"second callback\00", align 1
@g_saved_binary_fn = internal global ptr null, align 8
@g_saved_binary_context = internal global ptr null, align 8
@.str.4 = private unnamed_addr constant [3 x i8] c"ok\00", align 1
@.str.5 = private unnamed_addr constant [5 x i8] c"null\00", align 1
@.str.6 = private unnamed_addr constant [6 x i8] c"range\00", align 1
@.str.7 = private unnamed_addr constant [10 x i8] c"bad-value\00", align 1
@.str.8 = private unnamed_addr constant [8 x i8] c"unknown\00", align 1
@abi_return_const_int_array.values = internal constant [6 x i32] [i32 -100, i32 -1, i32 0, i32 1, i32 100, i32 123456789], align 16
@.str.9 = private unnamed_addr constant [6 x i8] c"clang\00", align 1
@.str.10 = private unnamed_addr constant [13 x i8] c"linux-x86_64\00", align 1

; Function Attrs: noinline nounwind optnone uwtable
define dso_local signext i8 @abi_echo_i8(i8 noundef signext %0) #0 {
  %2 = alloca i8, align 1
  store i8 %0, ptr %2, align 1
  %3 = load i8, ptr %2, align 1
  ret i8 %3
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local zeroext i8 @abi_echo_u8(i8 noundef zeroext %0) #0 {
  %2 = alloca i8, align 1
  store i8 %0, ptr %2, align 1
  %3 = load i8, ptr %2, align 1
  ret i8 %3
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local signext i16 @abi_echo_short(i16 noundef signext %0) #0 {
  %2 = alloca i16, align 2
  store i16 %0, ptr %2, align 2
  %3 = load i16, ptr %2, align 2
  ret i16 %3
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local zeroext i16 @abi_echo_ushort(i16 noundef zeroext %0) #0 {
  %2 = alloca i16, align 2
  store i16 %0, ptr %2, align 2
  %3 = load i16, ptr %2, align 2
  ret i16 %3
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @abi_echo_int(i32 noundef %0) #0 {
  %2 = alloca i32, align 4
  store i32 %0, ptr %2, align 4
  %3 = load i32, ptr %2, align 4
  ret i32 %3
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @abi_echo_uint(i32 noundef %0) #0 {
  %2 = alloca i32, align 4
  store i32 %0, ptr %2, align 4
  %3 = load i32, ptr %2, align 4
  ret i32 %3
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_echo_long(i64 noundef %0) #0 {
  %2 = alloca i64, align 8
  store i64 %0, ptr %2, align 8
  %3 = load i64, ptr %2, align 8
  ret i64 %3
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_echo_ulong(i64 noundef %0) #0 {
  %2 = alloca i64, align 8
  store i64 %0, ptr %2, align 8
  %3 = load i64, ptr %2, align 8
  ret i64 %3
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_echo_ll(i64 noundef %0) #0 {
  %2 = alloca i64, align 8
  store i64 %0, ptr %2, align 8
  %3 = load i64, ptr %2, align 8
  ret i64 %3
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_echo_ull(i64 noundef %0) #0 {
  %2 = alloca i64, align 8
  store i64 %0, ptr %2, align 8
  %3 = load i64, ptr %2, align 8
  ret i64 %3
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local signext i8 @abi_echo_int8(i8 noundef signext %0) #0 {
  %2 = alloca i8, align 1
  store i8 %0, ptr %2, align 1
  %3 = load i8, ptr %2, align 1
  ret i8 %3
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local zeroext i8 @abi_echo_uint8(i8 noundef zeroext %0) #0 {
  %2 = alloca i8, align 1
  store i8 %0, ptr %2, align 1
  %3 = load i8, ptr %2, align 1
  ret i8 %3
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local signext i16 @abi_echo_int16(i16 noundef signext %0) #0 {
  %2 = alloca i16, align 2
  store i16 %0, ptr %2, align 2
  %3 = load i16, ptr %2, align 2
  ret i16 %3
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local zeroext i16 @abi_echo_uint16(i16 noundef zeroext %0) #0 {
  %2 = alloca i16, align 2
  store i16 %0, ptr %2, align 2
  %3 = load i16, ptr %2, align 2
  ret i16 %3
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @abi_echo_int32(i32 noundef %0) #0 {
  %2 = alloca i32, align 4
  store i32 %0, ptr %2, align 4
  %3 = load i32, ptr %2, align 4
  ret i32 %3
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @abi_echo_uint32(i32 noundef %0) #0 {
  %2 = alloca i32, align 4
  store i32 %0, ptr %2, align 4
  %3 = load i32, ptr %2, align 4
  ret i32 %3
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_echo_int64(i64 noundef %0) #0 {
  %2 = alloca i64, align 8
  store i64 %0, ptr %2, align 8
  %3 = load i64, ptr %2, align 8
  ret i64 %3
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_echo_uint64(i64 noundef %0) #0 {
  %2 = alloca i64, align 8
  store i64 %0, ptr %2, align 8
  %3 = load i64, ptr %2, align 8
  ret i64 %3
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_echo_size(i64 noundef %0) #0 {
  %2 = alloca i64, align 8
  store i64 %0, ptr %2, align 8
  %3 = load i64, ptr %2, align 8
  ret i64 %3
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_echo_ptrdiff(i64 noundef %0) #0 {
  %2 = alloca i64, align 8
  store i64 %0, ptr %2, align 8
  %3 = load i64, ptr %2, align 8
  ret i64 %3
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_echo_intptr(i64 noundef %0) #0 {
  %2 = alloca i64, align 8
  store i64 %0, ptr %2, align 8
  %3 = load i64, ptr %2, align 8
  ret i64 %3
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_echo_uintptr(i64 noundef %0) #0 {
  %2 = alloca i64, align 8
  store i64 %0, ptr %2, align 8
  %3 = load i64, ptr %2, align 8
  ret i64 %3
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local float @abi_echo_f32(float noundef %0) #0 {
  %2 = alloca float, align 4
  store float %0, ptr %2, align 4
  %3 = load float, ptr %2, align 4
  ret float %3
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local double @abi_echo_f64(double noundef %0) #0 {
  %2 = alloca double, align 8
  store double %0, ptr %2, align 8
  %3 = load double, ptr %2, align 8
  ret double %3
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local x86_fp80 @abi_echo_long_double(x86_fp80 noundef %0) #0 {
  %2 = alloca x86_fp80, align 16
  store x86_fp80 %0, ptr %2, align 16
  %3 = load x86_fp80, ptr %2, align 16
  ret x86_fp80 %3
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local zeroext i1 @abi_echo_bool(i1 noundef zeroext %0) #0 {
  %2 = alloca i8, align 1
  %3 = zext i1 %0 to i8
  store i8 %3, ptr %2, align 1
  %4 = load i8, ptr %2, align 1
  %5 = trunc i8 %4 to i1
  ret i1 %5
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local signext i8 @abi_echo_char(i8 noundef signext %0) #0 {
  %2 = alloca i8, align 1
  store i8 %0, ptr %2, align 1
  %3 = load i8, ptr %2, align 1
  ret i8 %3
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local ptr @abi_echo_ptr(ptr noundef %0) #0 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  %3 = load ptr, ptr %2, align 8
  ret ptr %3
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local ptr @abi_echo_const_ptr(ptr noundef %0) #0 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  %3 = load ptr, ptr %2, align 8
  ret ptr %3
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local ptr @abi_echo_cstr(ptr noundef %0) #0 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  %3 = load ptr, ptr %2, align 8
  ret ptr %3
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @abi_mix_1(i32 noundef %0, double noundef %1, i32 noundef %2) #0 {
  %4 = alloca i32, align 4
  %5 = alloca double, align 8
  %6 = alloca i32, align 4
  store i32 %0, ptr %4, align 4
  store double %1, ptr %5, align 8
  store i32 %2, ptr %6, align 4
  %7 = load i32, ptr %4, align 4
  %8 = mul nsw i32 %7, 100
  %9 = load double, ptr %5, align 8
  %10 = fptosi double %9 to i32
  %11 = mul nsw i32 %10, 10
  %12 = add nsw i32 %8, %11
  %13 = load i32, ptr %6, align 4
  %14 = add nsw i32 %12, %13
  ret i32 %14
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local double @abi_mix_2(double noundef %0, i32 noundef %1, float noundef %2, i64 noundef %3, i16 noundef signext %4) #0 {
  %6 = alloca double, align 8
  %7 = alloca i32, align 4
  %8 = alloca float, align 4
  %9 = alloca i64, align 8
  %10 = alloca i16, align 2
  store double %0, ptr %6, align 8
  store i32 %1, ptr %7, align 4
  store float %2, ptr %8, align 4
  store i64 %3, ptr %9, align 8
  store i16 %4, ptr %10, align 2
  %11 = load double, ptr %6, align 8
  %12 = load i32, ptr %7, align 4
  %13 = sitofp i32 %12 to double
  %14 = call double @llvm.fmuladd.f64(double %13, double 2.000000e+00, double %11)
  %15 = load float, ptr %8, align 4
  %16 = fpext float %15 to double
  %17 = call double @llvm.fmuladd.f64(double %16, double 3.000000e+00, double %14)
  %18 = load i64, ptr %9, align 8
  %19 = sitofp i64 %18 to double
  %20 = call double @llvm.fmuladd.f64(double %19, double 4.000000e+00, double %17)
  %21 = load i16, ptr %10, align 2
  %22 = sitofp i16 %21 to double
  %23 = call double @llvm.fmuladd.f64(double %22, double 5.000000e+00, double %20)
  ret double %23
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare double @llvm.fmuladd.f64(double, double, double) #1

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_mix_3(i8 noundef zeroext %0, i64 noundef %1, i16 noundef zeroext %2, double noundef %3, i32 noundef %4) #0 {
  %6 = alloca i8, align 1
  %7 = alloca i64, align 8
  %8 = alloca i16, align 2
  %9 = alloca double, align 8
  %10 = alloca i32, align 4
  store i8 %0, ptr %6, align 1
  store i64 %1, ptr %7, align 8
  store i16 %2, ptr %8, align 2
  store double %3, ptr %9, align 8
  store i32 %4, ptr %10, align 4
  %11 = load i8, ptr %6, align 1
  %12 = zext i8 %11 to i64
  %13 = load i64, ptr %7, align 8
  %14 = mul i64 %13, 3
  %15 = add i64 %12, %14
  %16 = load i16, ptr %8, align 2
  %17 = zext i16 %16 to i64
  %18 = mul i64 %17, 5
  %19 = add i64 %15, %18
  %20 = load double, ptr %9, align 8
  %21 = fptoui double %20 to i64
  %22 = add i64 %19, %21
  %23 = load i32, ptr %10, align 4
  %24 = zext i32 %23 to i64
  %25 = mul i64 %24, 7
  %26 = add i64 %22, %25
  ret i64 %26
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local double @abi_many_args(i32 noundef %0, double noundef %1, i8 noundef zeroext %2, float noundef %3, i64 noundef %4, i16 noundef zeroext %5, ptr noundef %6, i32 noundef %7, double noundef %8, i32 noundef %9, i16 noundef signext %10, float noundef %11, i64 noundef %12, i8 noundef zeroext %13, double noundef %14) #0 {
  %16 = alloca i32, align 4
  %17 = alloca double, align 8
  %18 = alloca i8, align 1
  %19 = alloca float, align 4
  %20 = alloca i64, align 8
  %21 = alloca i16, align 2
  %22 = alloca ptr, align 8
  %23 = alloca i32, align 4
  %24 = alloca double, align 8
  %25 = alloca i32, align 4
  %26 = alloca i16, align 2
  %27 = alloca float, align 4
  %28 = alloca i64, align 8
  %29 = alloca i8, align 1
  %30 = alloca double, align 8
  store i32 %0, ptr %16, align 4
  store double %1, ptr %17, align 8
  store i8 %2, ptr %18, align 1
  store float %3, ptr %19, align 4
  store i64 %4, ptr %20, align 8
  store i16 %5, ptr %21, align 2
  store ptr %6, ptr %22, align 8
  store i32 %7, ptr %23, align 4
  store double %8, ptr %24, align 8
  store i32 %9, ptr %25, align 4
  store i16 %10, ptr %26, align 2
  store float %11, ptr %27, align 4
  store i64 %12, ptr %28, align 8
  store i8 %13, ptr %29, align 1
  store double %14, ptr %30, align 8
  %31 = load i32, ptr %16, align 4
  %32 = sitofp i32 %31 to double
  %33 = load double, ptr %17, align 8
  %34 = fadd double %32, %33
  %35 = load i8, ptr %18, align 1
  %36 = uitofp i8 %35 to double
  %37 = fadd double %34, %36
  %38 = load float, ptr %19, align 4
  %39 = fpext float %38 to double
  %40 = fadd double %37, %39
  %41 = load i64, ptr %20, align 8
  %42 = sitofp i64 %41 to double
  %43 = fadd double %40, %42
  %44 = load i16, ptr %21, align 2
  %45 = uitofp i16 %44 to double
  %46 = fadd double %43, %45
  %47 = load ptr, ptr %22, align 8
  %48 = icmp ne ptr %47, null
  %49 = zext i1 %48 to i64
  %50 = select i1 %48, double 1.700000e+01, double 0.000000e+00
  %51 = fadd double %46, %50
  %52 = load i32, ptr %23, align 4
  %53 = sitofp i32 %52 to double
  %54 = fadd double %51, %53
  %55 = load double, ptr %24, align 8
  %56 = fadd double %54, %55
  %57 = load i32, ptr %25, align 4
  %58 = uitofp i32 %57 to double
  %59 = fadd double %56, %58
  %60 = load i16, ptr %26, align 2
  %61 = sitofp i16 %60 to double
  %62 = fadd double %59, %61
  %63 = load float, ptr %27, align 4
  %64 = fpext float %63 to double
  %65 = fadd double %62, %64
  %66 = load i64, ptr %28, align 8
  %67 = sitofp i64 %66 to double
  %68 = fadd double %65, %67
  %69 = load i8, ptr %29, align 1
  %70 = uitofp i8 %69 to double
  %71 = fadd double %68, %70
  %72 = load double, ptr %30, align 8
  %73 = fadd double %71, %72
  ret double %73
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @abi_write_scalars(ptr noundef %0, ptr noundef %1, ptr noundef %2, ptr noundef %3, ptr noundef %4, ptr noundef %5, ptr noundef %6, ptr noundef %7, ptr noundef %8, ptr noundef %9, ptr noundef %10) #0 {
  %12 = alloca ptr, align 8
  %13 = alloca ptr, align 8
  %14 = alloca ptr, align 8
  %15 = alloca ptr, align 8
  %16 = alloca ptr, align 8
  %17 = alloca ptr, align 8
  %18 = alloca ptr, align 8
  %19 = alloca ptr, align 8
  %20 = alloca ptr, align 8
  %21 = alloca ptr, align 8
  %22 = alloca ptr, align 8
  store ptr %0, ptr %12, align 8
  store ptr %1, ptr %13, align 8
  store ptr %2, ptr %14, align 8
  store ptr %3, ptr %15, align 8
  store ptr %4, ptr %16, align 8
  store ptr %5, ptr %17, align 8
  store ptr %6, ptr %18, align 8
  store ptr %7, ptr %19, align 8
  store ptr %8, ptr %20, align 8
  store ptr %9, ptr %21, align 8
  store ptr %10, ptr %22, align 8
  %23 = load ptr, ptr %12, align 8
  %24 = icmp ne ptr %23, null
  br i1 %24, label %25, label %27

25:                                               ; preds = %11
  %26 = load ptr, ptr %12, align 8
  store i8 -101, ptr %26, align 1
  br label %27

27:                                               ; preds = %25, %11
  %28 = load ptr, ptr %13, align 8
  %29 = icmp ne ptr %28, null
  br i1 %29, label %30, label %32

30:                                               ; preds = %27
  %31 = load ptr, ptr %13, align 8
  store i8 -25, ptr %31, align 1
  br label %32

32:                                               ; preds = %30, %27
  %33 = load ptr, ptr %14, align 8
  %34 = icmp ne ptr %33, null
  br i1 %34, label %35, label %37

35:                                               ; preds = %32
  %36 = load ptr, ptr %14, align 8
  store i16 -30001, ptr %36, align 2
  br label %37

37:                                               ; preds = %35, %32
  %38 = load ptr, ptr %15, align 8
  %39 = icmp ne ptr %38, null
  br i1 %39, label %40, label %42

40:                                               ; preds = %37
  %41 = load ptr, ptr %15, align 8
  store i16 -5535, ptr %41, align 2
  br label %42

42:                                               ; preds = %40, %37
  %43 = load ptr, ptr %16, align 8
  %44 = icmp ne ptr %43, null
  br i1 %44, label %45, label %47

45:                                               ; preds = %42
  %46 = load ptr, ptr %16, align 8
  store i32 -2000000001, ptr %46, align 4
  br label %47

47:                                               ; preds = %45, %42
  %48 = load ptr, ptr %17, align 8
  %49 = icmp ne ptr %48, null
  br i1 %49, label %50, label %52

50:                                               ; preds = %47
  %51 = load ptr, ptr %17, align 8
  store i32 -294967295, ptr %51, align 4
  br label %52

52:                                               ; preds = %50, %47
  %53 = load ptr, ptr %18, align 8
  %54 = icmp ne ptr %53, null
  br i1 %54, label %55, label %57

55:                                               ; preds = %52
  %56 = load ptr, ptr %18, align 8
  store i64 -8000000000000000000, ptr %56, align 8
  br label %57

57:                                               ; preds = %55, %52
  %58 = load ptr, ptr %19, align 8
  %59 = icmp ne ptr %58, null
  br i1 %59, label %60, label %62

60:                                               ; preds = %57
  %61 = load ptr, ptr %19, align 8
  store i64 -2446744073709551616, ptr %61, align 8
  br label %62

62:                                               ; preds = %60, %57
  %63 = load ptr, ptr %20, align 8
  %64 = icmp ne ptr %63, null
  br i1 %64, label %65, label %67

65:                                               ; preds = %62
  %66 = load ptr, ptr %20, align 8
  store float 1.232500e+02, ptr %66, align 4
  br label %67

67:                                               ; preds = %65, %62
  %68 = load ptr, ptr %21, align 8
  %69 = icmp ne ptr %68, null
  br i1 %69, label %70, label %72

70:                                               ; preds = %67
  %71 = load ptr, ptr %21, align 8
  store double 0xC0F81CD200000000, ptr %71, align 8
  br label %72

72:                                               ; preds = %70, %67
  %73 = load ptr, ptr %22, align 8
  %74 = icmp ne ptr %73, null
  br i1 %74, label %75, label %77

75:                                               ; preds = %72
  %76 = load ptr, ptr %22, align 8
  store i8 1, ptr %76, align 1
  br label %77

77:                                               ; preds = %75, %72
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local { i64, i64 } @abi_make_all_small() #0 {
  %1 = alloca %struct.AbiAllSmall, align 4
  call void @llvm.memcpy.p0.p0.i64(ptr align 4 %1, ptr align 4 @__const.abi_make_all_small.s, i64 16, i1 false)
  %2 = load { i64, i64 }, ptr %1, align 4
  ret { i64, i64 } %2
}

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg) #2

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @abi_make_mixed_a(ptr dead_on_unwind noalias writable sret(%struct.AbiMixedOrderA) align 8 %0) #0 {
  call void @llvm.memset.p0.i64(ptr align 8 %0, i8 0, i64 40, i1 false)
  %2 = getelementptr inbounds %struct.AbiMixedOrderA, ptr %0, i32 0, i32 0
  store i8 11, ptr %2, align 8
  %3 = getelementptr inbounds %struct.AbiMixedOrderA, ptr %0, i32 0, i32 1
  store i64 1234605616436508552, ptr %3, align 8
  %4 = getelementptr inbounds %struct.AbiMixedOrderA, ptr %0, i32 0, i32 2
  store i16 2222, ptr %4, align 8
  %5 = getelementptr inbounds %struct.AbiMixedOrderA, ptr %0, i32 0, i32 3
  store double 3.335000e+02, ptr %5, align 8
  %6 = getelementptr inbounds %struct.AbiMixedOrderA, ptr %0, i32 0, i32 4
  store i8 44, ptr %6, align 8
  ret void
}

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: write)
declare void @llvm.memset.p0.i64(ptr nocapture writeonly, i8, i64, i1 immarg) #3

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @abi_make_mixed_b(ptr dead_on_unwind noalias writable sret(%struct.AbiMixedOrderB) align 8 %0) #0 {
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %0, ptr align 8 @__const.abi_make_mixed_b.s, i64 24, i1 false)
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @abi_make_float_mix(ptr dead_on_unwind noalias writable sret(%struct.AbiFloatMix) align 16 %0) #0 {
  call void @llvm.memset.p0.i64(ptr align 16 %0, i8 0, i64 48, i1 false)
  %2 = getelementptr inbounds %struct.AbiFloatMix, ptr %0, i32 0, i32 0
  store float 1.250000e+00, ptr %2, align 16
  %3 = getelementptr inbounds %struct.AbiFloatMix, ptr %0, i32 0, i32 1
  store double -2.500000e+00, ptr %3, align 8
  %4 = getelementptr inbounds %struct.AbiFloatMix, ptr %0, i32 0, i32 2
  store x86_fp80 0xK4000F000000000000000, ptr %4, align 16
  %5 = getelementptr inbounds %struct.AbiFloatMix, ptr %0, i32 0, i32 3
  store float 4.500000e+00, ptr %5, align 16
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @abi_make_nested(ptr dead_on_unwind noalias writable sret(%struct.AbiNested) align 8 %0) #0 {
  %2 = getelementptr inbounds %struct.AbiNested, ptr %0, i32 0, i32 0
  store i16 -123, ptr %2, align 8
  %3 = getelementptr inbounds %struct.AbiNested, ptr %0, i32 0, i32 1
  %4 = getelementptr inbounds %struct.AbiInner, ptr %3, i32 0, i32 0
  store i8 9, ptr %4, align 8
  %5 = getelementptr inbounds %struct.AbiNested, ptr %0, i32 0, i32 1
  %6 = getelementptr inbounds %struct.AbiInner, ptr %5, i32 0, i32 1
  store i32 123456, ptr %6, align 4
  %7 = getelementptr inbounds %struct.AbiNested, ptr %0, i32 0, i32 1
  %8 = getelementptr inbounds %struct.AbiInner, ptr %7, i32 0, i32 2
  store double 1.250000e-01, ptr %8, align 8
  %9 = getelementptr inbounds %struct.AbiNested, ptr %0, i32 0, i32 2
  store i64 -81985529216486896, ptr %9, align 8
  %10 = getelementptr inbounds %struct.AbiNested, ptr %0, i32 0, i32 3
  %11 = getelementptr inbounds %struct.AbiInner, ptr %10, i32 0, i32 0
  store i8 17, ptr %11, align 8
  %12 = getelementptr inbounds %struct.AbiNested, ptr %0, i32 0, i32 3
  %13 = getelementptr inbounds %struct.AbiInner, ptr %12, i32 0, i32 1
  store i32 -654321, ptr %13, align 4
  %14 = getelementptr inbounds %struct.AbiNested, ptr %0, i32 0, i32 3
  %15 = getelementptr inbounds %struct.AbiInner, ptr %14, i32 0, i32 2
  store double -9.998750e+02, ptr %15, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @abi_make_array_fields(ptr dead_on_unwind noalias writable sret(%struct.AbiArrayFields) align 4 %0) #0 {
  call void @llvm.memcpy.p0.p0.i64(ptr align 4 %0, ptr align 4 @__const.abi_make_array_fields.a, i64 40, i1 false)
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @abi_make_large(ptr dead_on_unwind noalias writable sret(%struct.AbiLarge) align 8 %0) #0 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  call void @llvm.memset.p0.i64(ptr align 8 %0, i8 0, i64 160, i1 false)
  %5 = getelementptr inbounds %struct.AbiLarge, ptr %0, i32 0, i32 0
  store i64 81985529216486895, ptr %5, align 8
  %6 = getelementptr inbounds %struct.AbiLarge, ptr %0, i32 0, i32 1
  %7 = getelementptr inbounds [32 x i8], ptr %6, i64 0, i64 0
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %7, ptr align 1 @.str, i64 17, i1 false)
  %8 = getelementptr inbounds %struct.AbiLarge, ptr %0, i32 0, i32 2
  store i8 -91, ptr %8, align 8
  store i64 0, ptr %2, align 8
  br label %9

9:                                                ; preds = %19, %1
  %10 = load i64, ptr %2, align 8
  %11 = icmp ult i64 %10, 7
  br i1 %11, label %12, label %22

12:                                               ; preds = %9
  %13 = load i64, ptr %2, align 8
  %14 = add i64 240, %13
  %15 = trunc i64 %14 to i8
  %16 = getelementptr inbounds %struct.AbiLarge, ptr %0, i32 0, i32 3
  %17 = load i64, ptr %2, align 8
  %18 = getelementptr inbounds [7 x i8], ptr %16, i64 0, i64 %17
  store i8 %15, ptr %18, align 1
  br label %19

19:                                               ; preds = %12
  %20 = load i64, ptr %2, align 8
  %21 = add i64 %20, 1
  store i64 %21, ptr %2, align 8
  br label %9, !llvm.loop !6

22:                                               ; preds = %9
  store i64 0, ptr %3, align 8
  br label %23

23:                                               ; preds = %33, %22
  %24 = load i64, ptr %3, align 8
  %25 = icmp ult i64 %24, 5
  br i1 %25, label %26, label %36

26:                                               ; preds = %23
  %27 = load i64, ptr %3, align 8
  %28 = uitofp i64 %27 to double
  %29 = call double @llvm.fmuladd.f64(double %28, double -1.250000e+00, double 5.000000e-01)
  %30 = getelementptr inbounds %struct.AbiLarge, ptr %0, i32 0, i32 4
  %31 = load i64, ptr %3, align 8
  %32 = getelementptr inbounds [5 x double], ptr %30, i64 0, i64 %31
  store double %29, ptr %32, align 8
  br label %33

33:                                               ; preds = %26
  %34 = load i64, ptr %3, align 8
  %35 = add i64 %34, 1
  store i64 %35, ptr %3, align 8
  br label %23, !llvm.loop !8

36:                                               ; preds = %23
  store i64 0, ptr %4, align 8
  br label %37

37:                                               ; preds = %63, %36
  %38 = load i64, ptr %4, align 8
  %39 = icmp ult i64 %38, 3
  br i1 %39, label %40, label %66

40:                                               ; preds = %37
  %41 = load i64, ptr %4, align 8
  %42 = add i64 %41, 10
  %43 = trunc i64 %42 to i8
  %44 = getelementptr inbounds %struct.AbiLarge, ptr %0, i32 0, i32 5
  %45 = load i64, ptr %4, align 8
  %46 = getelementptr inbounds [3 x %struct.AbiInner], ptr %44, i64 0, i64 %45
  %47 = getelementptr inbounds %struct.AbiInner, ptr %46, i32 0, i32 0
  store i8 %43, ptr %47, align 8
  %48 = load i64, ptr %4, align 8
  %49 = mul i64 %48, 1000
  %50 = trunc i64 %49 to i32
  %51 = sub nsw i32 %50, 100
  %52 = getelementptr inbounds %struct.AbiLarge, ptr %0, i32 0, i32 5
  %53 = load i64, ptr %4, align 8
  %54 = getelementptr inbounds [3 x %struct.AbiInner], ptr %52, i64 0, i64 %53
  %55 = getelementptr inbounds %struct.AbiInner, ptr %54, i32 0, i32 1
  store i32 %51, ptr %55, align 4
  %56 = load i64, ptr %4, align 8
  %57 = uitofp i64 %56 to double
  %58 = fadd double %57, 6.250000e-02
  %59 = getelementptr inbounds %struct.AbiLarge, ptr %0, i32 0, i32 5
  %60 = load i64, ptr %4, align 8
  %61 = getelementptr inbounds [3 x %struct.AbiInner], ptr %59, i64 0, i64 %60
  %62 = getelementptr inbounds %struct.AbiInner, ptr %61, i32 0, i32 2
  store double %58, ptr %62, align 8
  br label %63

63:                                               ; preds = %40
  %64 = load i64, ptr %4, align 8
  %65 = add i64 %64, 1
  store i64 %65, ptr %4, align 8
  br label %37, !llvm.loop !9

66:                                               ; preds = %37
  %67 = getelementptr inbounds %struct.AbiLarge, ptr %0, i32 0, i32 6
  store ptr inttoptr (i64 4660 to ptr), ptr %67, align 8
  %68 = getelementptr inbounds %struct.AbiLarge, ptr %0, i32 0, i32 7
  store i64 74565, ptr %68, align 8
  %69 = getelementptr inbounds %struct.AbiLarge, ptr %0, i32 0, i32 8
  store i32 -77, ptr %69, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @abi_sum_all_small(i64 %0, i64 %1) #0 {
  %3 = alloca %struct.AbiAllSmall, align 4
  %4 = getelementptr inbounds { i64, i64 }, ptr %3, i32 0, i32 0
  store i64 %0, ptr %4, align 4
  %5 = getelementptr inbounds { i64, i64 }, ptr %3, i32 0, i32 1
  store i64 %1, ptr %5, align 4
  %6 = getelementptr inbounds %struct.AbiAllSmall, ptr %3, i32 0, i32 0
  %7 = load i8, ptr %6, align 4
  %8 = sext i8 %7 to i32
  %9 = getelementptr inbounds %struct.AbiAllSmall, ptr %3, i32 0, i32 1
  %10 = load i8, ptr %9, align 1
  %11 = zext i8 %10 to i32
  %12 = add nsw i32 %8, %11
  %13 = getelementptr inbounds %struct.AbiAllSmall, ptr %3, i32 0, i32 2
  %14 = load i16, ptr %13, align 2
  %15 = sext i16 %14 to i32
  %16 = add nsw i32 %12, %15
  %17 = getelementptr inbounds %struct.AbiAllSmall, ptr %3, i32 0, i32 3
  %18 = load i16, ptr %17, align 4
  %19 = zext i16 %18 to i32
  %20 = add nsw i32 %16, %19
  %21 = getelementptr inbounds %struct.AbiAllSmall, ptr %3, i32 0, i32 4
  %22 = load i32, ptr %21, align 4
  %23 = add nsw i32 %20, %22
  %24 = getelementptr inbounds %struct.AbiAllSmall, ptr %3, i32 0, i32 5
  %25 = load i32, ptr %24, align 4
  %26 = add nsw i32 %23, %25
  ret i32 %26
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local double @abi_score_mixed_a(ptr noundef byval(%struct.AbiMixedOrderA) align 8 %0) #0 {
  %2 = getelementptr inbounds %struct.AbiMixedOrderA, ptr %0, i32 0, i32 0
  %3 = load i8, ptr %2, align 8
  %4 = uitofp i8 %3 to double
  %5 = getelementptr inbounds %struct.AbiMixedOrderA, ptr %0, i32 0, i32 1
  %6 = load i64, ptr %5, align 8
  %7 = uitofp i64 %6 to double
  %8 = call double @llvm.fmuladd.f64(double %7, double 1.000000e-03, double %4)
  %9 = getelementptr inbounds %struct.AbiMixedOrderA, ptr %0, i32 0, i32 2
  %10 = load i16, ptr %9, align 8
  %11 = uitofp i16 %10 to double
  %12 = fadd double %8, %11
  %13 = getelementptr inbounds %struct.AbiMixedOrderA, ptr %0, i32 0, i32 3
  %14 = load double, ptr %13, align 8
  %15 = fadd double %12, %14
  %16 = getelementptr inbounds %struct.AbiMixedOrderA, ptr %0, i32 0, i32 4
  %17 = load i8, ptr %16, align 8
  %18 = uitofp i8 %17 to double
  %19 = fadd double %15, %18
  ret double %19
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local double @abi_score_mixed_b(ptr noundef byval(%struct.AbiMixedOrderB) align 8 %0) #0 {
  %2 = getelementptr inbounds %struct.AbiMixedOrderB, ptr %0, i32 0, i32 0
  %3 = load double, ptr %2, align 8
  %4 = getelementptr inbounds %struct.AbiMixedOrderB, ptr %0, i32 0, i32 1
  %5 = load i64, ptr %4, align 8
  %6 = uitofp i64 %5 to double
  %7 = call double @llvm.fmuladd.f64(double %6, double 1.000000e-03, double %3)
  %8 = getelementptr inbounds %struct.AbiMixedOrderB, ptr %0, i32 0, i32 2
  %9 = load i16, ptr %8, align 8
  %10 = uitofp i16 %9 to double
  %11 = fadd double %7, %10
  %12 = getelementptr inbounds %struct.AbiMixedOrderB, ptr %0, i32 0, i32 3
  %13 = load i8, ptr %12, align 2
  %14 = uitofp i8 %13 to double
  %15 = fadd double %11, %14
  %16 = getelementptr inbounds %struct.AbiMixedOrderB, ptr %0, i32 0, i32 4
  %17 = load i8, ptr %16, align 1
  %18 = uitofp i8 %17 to double
  %19 = fadd double %15, %18
  ret double %19
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local double @abi_score_nested(ptr noundef byval(%struct.AbiNested) align 8 %0) #0 {
  %2 = getelementptr inbounds %struct.AbiNested, ptr %0, i32 0, i32 0
  %3 = load i16, ptr %2, align 8
  %4 = sitofp i16 %3 to double
  %5 = getelementptr inbounds %struct.AbiNested, ptr %0, i32 0, i32 1
  %6 = getelementptr inbounds %struct.AbiInner, ptr %5, i32 0, i32 2
  %7 = load double, ptr %6, align 8
  %8 = fadd double %4, %7
  %9 = getelementptr inbounds %struct.AbiNested, ptr %0, i32 0, i32 1
  %10 = getelementptr inbounds %struct.AbiInner, ptr %9, i32 0, i32 1
  %11 = load i32, ptr %10, align 4
  %12 = sitofp i32 %11 to double
  %13 = fadd double %8, %12
  %14 = getelementptr inbounds %struct.AbiNested, ptr %0, i32 0, i32 2
  %15 = load i64, ptr %14, align 8
  %16 = uitofp i64 %15 to double
  %17 = call double @llvm.fmuladd.f64(double %16, double 0x3EB0C6F7A0B5ED8D, double %13)
  %18 = getelementptr inbounds %struct.AbiNested, ptr %0, i32 0, i32 3
  %19 = getelementptr inbounds %struct.AbiInner, ptr %18, i32 0, i32 2
  %20 = load double, ptr %19, align 8
  %21 = fadd double %17, %20
  %22 = getelementptr inbounds %struct.AbiNested, ptr %0, i32 0, i32 3
  %23 = getelementptr inbounds %struct.AbiInner, ptr %22, i32 0, i32 1
  %24 = load i32, ptr %23, align 4
  %25 = sitofp i32 %24 to double
  %26 = fadd double %21, %25
  %27 = getelementptr inbounds %struct.AbiNested, ptr %0, i32 0, i32 1
  %28 = getelementptr inbounds %struct.AbiInner, ptr %27, i32 0, i32 0
  %29 = load i8, ptr %28, align 8
  %30 = uitofp i8 %29 to double
  %31 = fadd double %26, %30
  %32 = getelementptr inbounds %struct.AbiNested, ptr %0, i32 0, i32 3
  %33 = getelementptr inbounds %struct.AbiInner, ptr %32, i32 0, i32 0
  %34 = load i8, ptr %33, align 8
  %35 = uitofp i8 %34 to double
  %36 = fadd double %31, %35
  ret double %36
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_hash_array_fields(ptr noundef byval(%struct.AbiArrayFields) align 8 %0) #0 {
  %2 = alloca i64, align 8
  %3 = alloca ptr, align 8
  %4 = alloca i64, align 8
  store i64 1469598103934665603, ptr %2, align 8
  store ptr %0, ptr %3, align 8
  store i64 0, ptr %4, align 8
  br label %5

5:                                                ; preds = %18, %1
  %6 = load i64, ptr %4, align 8
  %7 = icmp ult i64 %6, 40
  br i1 %7, label %8, label %21

8:                                                ; preds = %5
  %9 = load ptr, ptr %3, align 8
  %10 = load i64, ptr %4, align 8
  %11 = getelementptr inbounds i8, ptr %9, i64 %10
  %12 = load i8, ptr %11, align 1
  %13 = zext i8 %12 to i64
  %14 = load i64, ptr %2, align 8
  %15 = xor i64 %14, %13
  store i64 %15, ptr %2, align 8
  %16 = load i64, ptr %2, align 8
  %17 = mul i64 %16, 1099511628211
  store i64 %17, ptr %2, align 8
  br label %18

18:                                               ; preds = %8
  %19 = load i64, ptr %4, align 8
  %20 = add i64 %19, 1
  store i64 %20, ptr %4, align 8
  br label %5, !llvm.loop !10

21:                                               ; preds = %5
  %22 = load i64, ptr %2, align 8
  ret i64 %22
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @abi_large_check(ptr noundef byval(%struct.AbiLarge) align 8 %0) #0 {
  %2 = getelementptr inbounds %struct.AbiLarge, ptr %0, i32 0, i32 0
  %3 = load i64, ptr %2, align 8
  %4 = icmp eq i64 %3, 81985529216486895
  br i1 %4, label %5, label %25

5:                                                ; preds = %1
  %6 = getelementptr inbounds %struct.AbiLarge, ptr %0, i32 0, i32 1
  %7 = getelementptr inbounds [32 x i8], ptr %6, i64 0, i64 0
  %8 = call i32 @strcmp(ptr noundef %7, ptr noundef @.str) #9
  %9 = icmp eq i32 %8, 0
  br i1 %9, label %10, label %25

10:                                               ; preds = %5
  %11 = getelementptr inbounds %struct.AbiLarge, ptr %0, i32 0, i32 2
  %12 = load i8, ptr %11, align 8
  %13 = zext i8 %12 to i32
  %14 = icmp eq i32 %13, 165
  br i1 %14, label %15, label %25

15:                                               ; preds = %10
  %16 = getelementptr inbounds %struct.AbiLarge, ptr %0, i32 0, i32 5
  %17 = getelementptr inbounds [3 x %struct.AbiInner], ptr %16, i64 0, i64 2
  %18 = getelementptr inbounds %struct.AbiInner, ptr %17, i32 0, i32 1
  %19 = load i32, ptr %18, align 4
  %20 = icmp eq i32 %19, 1900
  br i1 %20, label %21, label %25

21:                                               ; preds = %15
  %22 = getelementptr inbounds %struct.AbiLarge, ptr %0, i32 0, i32 8
  %23 = load i32, ptr %22, align 8
  %24 = icmp eq i32 %23, -77
  br label %25

25:                                               ; preds = %21, %15, %10, %5, %1
  %26 = phi i1 [ false, %15 ], [ false, %10 ], [ false, %5 ], [ false, %1 ], [ %24, %21 ]
  %27 = zext i1 %26 to i64
  %28 = select i1 %26, i32 1, i32 0
  ret i32 %28
}

; Function Attrs: nounwind willreturn memory(read)
declare i32 @strcmp(ptr noundef, ptr noundef) #4

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @abi_mutate_nested(ptr noundef %0, i32 noundef %1, double noundef %2) #0 {
  %4 = alloca ptr, align 8
  %5 = alloca i32, align 4
  %6 = alloca double, align 8
  store ptr %0, ptr %4, align 8
  store i32 %1, ptr %5, align 4
  store double %2, ptr %6, align 8
  %7 = load ptr, ptr %4, align 8
  %8 = icmp ne ptr %7, null
  br i1 %8, label %10, label %9

9:                                                ; preds = %3
  br label %58

10:                                               ; preds = %3
  %11 = load ptr, ptr %4, align 8
  %12 = getelementptr inbounds %struct.AbiNested, ptr %11, i32 0, i32 0
  %13 = load i16, ptr %12, align 8
  %14 = sext i16 %13 to i32
  %15 = load i32, ptr %5, align 4
  %16 = add nsw i32 %14, %15
  %17 = trunc i32 %16 to i16
  %18 = load ptr, ptr %4, align 8
  %19 = getelementptr inbounds %struct.AbiNested, ptr %18, i32 0, i32 0
  store i16 %17, ptr %19, align 8
  %20 = load i32, ptr %5, align 4
  %21 = load ptr, ptr %4, align 8
  %22 = getelementptr inbounds %struct.AbiNested, ptr %21, i32 0, i32 1
  %23 = getelementptr inbounds %struct.AbiInner, ptr %22, i32 0, i32 1
  %24 = load i32, ptr %23, align 4
  %25 = add nsw i32 %24, %20
  store i32 %25, ptr %23, align 4
  %26 = load double, ptr %6, align 8
  %27 = load ptr, ptr %4, align 8
  %28 = getelementptr inbounds %struct.AbiNested, ptr %27, i32 0, i32 1
  %29 = getelementptr inbounds %struct.AbiInner, ptr %28, i32 0, i32 2
  %30 = load double, ptr %29, align 8
  %31 = fmul double %30, %26
  store double %31, ptr %29, align 8
  %32 = load i32, ptr %5, align 4
  %33 = icmp slt i32 %32, 0
  br i1 %33, label %34, label %37

34:                                               ; preds = %10
  %35 = load i32, ptr %5, align 4
  %36 = sub nsw i32 0, %35
  br label %39

37:                                               ; preds = %10
  %38 = load i32, ptr %5, align 4
  br label %39

39:                                               ; preds = %37, %34
  %40 = phi i32 [ %36, %34 ], [ %38, %37 ]
  %41 = sext i32 %40 to i64
  %42 = load ptr, ptr %4, align 8
  %43 = getelementptr inbounds %struct.AbiNested, ptr %42, i32 0, i32 2
  %44 = load i64, ptr %43, align 8
  %45 = add i64 %44, %41
  store i64 %45, ptr %43, align 8
  %46 = load i32, ptr %5, align 4
  %47 = load ptr, ptr %4, align 8
  %48 = getelementptr inbounds %struct.AbiNested, ptr %47, i32 0, i32 3
  %49 = getelementptr inbounds %struct.AbiInner, ptr %48, i32 0, i32 1
  %50 = load i32, ptr %49, align 4
  %51 = sub nsw i32 %50, %46
  store i32 %51, ptr %49, align 4
  %52 = load double, ptr %6, align 8
  %53 = load ptr, ptr %4, align 8
  %54 = getelementptr inbounds %struct.AbiNested, ptr %53, i32 0, i32 3
  %55 = getelementptr inbounds %struct.AbiInner, ptr %54, i32 0, i32 2
  %56 = load double, ptr %55, align 8
  %57 = fmul double %56, %52
  store double %57, ptr %55, align 8
  br label %58

58:                                               ; preds = %39, %9
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_sizeof_char() #0 {
  ret i64 1
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_sizeof_short() #0 {
  ret i64 2
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_sizeof_int() #0 {
  ret i64 4
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_sizeof_long() #0 {
  ret i64 8
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_sizeof_long_long() #0 {
  ret i64 8
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_sizeof_size_t() #0 {
  ret i64 8
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_sizeof_pointer() #0 {
  ret i64 8
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_sizeof_bool() #0 {
  ret i64 1
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_sizeof_float() #0 {
  ret i64 4
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_sizeof_double() #0 {
  ret i64 8
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_sizeof_long_double() #0 {
  ret i64 16
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_sizeof_all_small() #0 {
  ret i64 16
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_alignof_all_small() #0 {
  ret i64 4
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_sizeof_mixed_order_a() #0 {
  ret i64 40
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_alignof_mixed_order_a() #0 {
  ret i64 8
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_sizeof_mixed_order_b() #0 {
  ret i64 24
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_alignof_mixed_order_b() #0 {
  ret i64 8
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_sizeof_float_mix() #0 {
  ret i64 48
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_alignof_float_mix() #0 {
  ret i64 16
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_sizeof_pointers() #0 {
  ret i64 40
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_alignof_pointers() #0 {
  ret i64 8
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_sizeof_inner() #0 {
  ret i64 16
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_alignof_inner() #0 {
  ret i64 8
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_sizeof_nested() #0 {
  ret i64 48
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_alignof_nested() #0 {
  ret i64 8
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_sizeof_array_fields() #0 {
  ret i64 40
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_alignof_array_fields() #0 {
  ret i64 4
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_sizeof_large() #0 {
  ret i64 160
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_alignof_large() #0 {
  ret i64 8
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_sizeof_odd_sizes() #0 {
  ret i64 80
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_alignof_odd_sizes() #0 {
  ret i64 16
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_offset_mixed_a_a() #0 {
  ret i64 0
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_offset_mixed_a_b() #0 {
  ret i64 8
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_offset_mixed_a_c() #0 {
  ret i64 16
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_offset_mixed_a_d() #0 {
  ret i64 24
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_offset_mixed_a_e() #0 {
  ret i64 32
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_offset_nested_inner() #0 {
  ret i64 8
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_offset_nested_after() #0 {
  ret i64 24
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_offset_nested_second() #0 {
  ret i64 32
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_offset_large_name() #0 {
  ret i64 8
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_offset_large_values() #0 {
  ret i64 48
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_offset_large_nested() #0 {
  ret i64 88
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_offset_large_user_data() #0 {
  ret i64 136
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_offset_large_length() #0 {
  ret i64 144
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_offset_large_status() #0 {
  ret i64 152
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @abi_enum_echo(i32 noundef %0) #0 {
  %2 = alloca i32, align 4
  store i32 %0, ptr %2, align 4
  %3 = load i32, ptr %2, align 4
  ret i32 %3
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @abi_flags_or(i32 noundef %0, i32 noundef %1) #0 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  store i32 %0, ptr %3, align 4
  store i32 %1, ptr %4, align 4
  %5 = load i32, ptr %3, align 4
  %6 = load i32, ptr %4, align 4
  %7 = or i32 %5, %6
  ret i32 %7
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @abi_enum_size() #0 {
  ret i32 4
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @abi_enum_value_negative() #0 {
  ret i32 -7
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @abi_enum_value_big() #0 {
  ret i32 100000
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_union_from_u64(i64 noundef %0) #0 {
  %2 = alloca %union.AbiScalarUnion, align 8
  %3 = alloca i64, align 8
  store i64 %0, ptr %3, align 8
  %4 = load i64, ptr %3, align 8
  store i64 %4, ptr %2, align 8
  %5 = getelementptr inbounds %union.AbiScalarUnion, ptr %2, i32 0, i32 0
  %6 = load i64, ptr %5, align 8
  ret i64 %6
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_union_from_double(double noundef %0) #0 {
  %2 = alloca %union.AbiScalarUnion, align 8
  %3 = alloca double, align 8
  store double %0, ptr %3, align 8
  %4 = load double, ptr %3, align 8
  store double %4, ptr %2, align 8
  %5 = getelementptr inbounds %union.AbiScalarUnion, ptr %2, i32 0, i32 0
  %6 = load i64, ptr %5, align 8
  ret i64 %6
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_union_read_u64(i64 %0) #0 {
  %2 = alloca %union.AbiScalarUnion, align 8
  %3 = getelementptr inbounds %union.AbiScalarUnion, ptr %2, i32 0, i32 0
  store i64 %0, ptr %3, align 8
  %4 = load i64, ptr %2, align 8
  ret i64 %4
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local double @abi_union_read_double(i64 %0) #0 {
  %2 = alloca %union.AbiScalarUnion, align 8
  %3 = getelementptr inbounds %union.AbiScalarUnion, ptr %2, i32 0, i32 0
  store i64 %0, ptr %3, align 8
  %4 = load double, ptr %2, align 8
  ret double %4
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_sizeof_scalar_union() #0 {
  ret i64 8
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_alignof_scalar_union() #0 {
  ret i64 8
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_sizeof_tagged_union() #0 {
  ret i64 24
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_offset_tagged_union_value() #0 {
  ret i64 8
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @abi_read_int_ptr(ptr noundef %0) #0 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  %3 = load ptr, ptr %2, align 8
  %4 = icmp ne ptr %3, null
  br i1 %4, label %5, label %8

5:                                                ; preds = %1
  %6 = load ptr, ptr %2, align 8
  %7 = load i32, ptr %6, align 4
  br label %9

8:                                                ; preds = %1
  br label %9

9:                                                ; preds = %8, %5
  %10 = phi i32 [ %7, %5 ], [ -2147483648, %8 ]
  ret i32 %10
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @abi_write_int_ptr(ptr noundef %0, i32 noundef %1) #0 {
  %3 = alloca ptr, align 8
  %4 = alloca i32, align 4
  store ptr %0, ptr %3, align 8
  store i32 %1, ptr %4, align 4
  %5 = load ptr, ptr %3, align 8
  %6 = icmp ne ptr %5, null
  br i1 %6, label %7, label %10

7:                                                ; preds = %2
  %8 = load i32, ptr %4, align 4
  %9 = load ptr, ptr %3, align 8
  store i32 %8, ptr %9, align 4
  br label %10

10:                                               ; preds = %7, %2
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local double @abi_read_double_ptr(ptr noundef %0) #0 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  %3 = load ptr, ptr %2, align 8
  %4 = icmp ne ptr %3, null
  br i1 %4, label %5, label %8

5:                                                ; preds = %1
  %6 = load ptr, ptr %2, align 8
  %7 = load double, ptr %6, align 8
  br label %9

8:                                                ; preds = %1
  br label %9

9:                                                ; preds = %8, %5
  %10 = phi double [ %7, %5 ], [ 0xFFEFFFFFFFFFFFFF, %8 ]
  ret double %10
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @abi_write_double_ptr(ptr noundef %0, double noundef %1) #0 {
  %3 = alloca ptr, align 8
  %4 = alloca double, align 8
  store ptr %0, ptr %3, align 8
  store double %1, ptr %4, align 8
  %5 = load ptr, ptr %3, align 8
  %6 = icmp ne ptr %5, null
  br i1 %6, label %7, label %10

7:                                                ; preds = %2
  %8 = load double, ptr %4, align 8
  %9 = load ptr, ptr %3, align 8
  store double %8, ptr %9, align 8
  br label %10

10:                                               ; preds = %7, %2
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @abi_set_ptr(ptr noundef %0, ptr noundef %1) #0 {
  %3 = alloca ptr, align 8
  %4 = alloca ptr, align 8
  store ptr %0, ptr %3, align 8
  store ptr %1, ptr %4, align 8
  %5 = load ptr, ptr %3, align 8
  %6 = icmp ne ptr %5, null
  br i1 %6, label %7, label %10

7:                                                ; preds = %2
  %8 = load ptr, ptr %4, align 8
  %9 = load ptr, ptr %3, align 8
  store ptr %8, ptr %9, align 8
  br label %10

10:                                               ; preds = %7, %2
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local ptr @abi_get_ptr(ptr noundef %0) #0 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  %3 = load ptr, ptr %2, align 8
  %4 = icmp ne ptr %3, null
  br i1 %4, label %5, label %8

5:                                                ; preds = %1
  %6 = load ptr, ptr %2, align 8
  %7 = load ptr, ptr %6, align 8
  br label %9

8:                                                ; preds = %1
  br label %9

9:                                                ; preds = %8, %5
  %10 = phi ptr [ %7, %5 ], [ null, %8 ]
  ret ptr %10
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_strlen_or_zero(ptr noundef %0) #0 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  %3 = load ptr, ptr %2, align 8
  %4 = icmp ne ptr %3, null
  br i1 %4, label %5, label %8

5:                                                ; preds = %1
  %6 = load ptr, ptr %2, align 8
  %7 = call i64 @strlen(ptr noundef %6) #9
  br label %9

8:                                                ; preds = %1
  br label %9

9:                                                ; preds = %8, %5
  %10 = phi i64 [ %7, %5 ], [ 0, %8 ]
  ret i64 %10
}

; Function Attrs: nounwind willreturn memory(read)
declare i64 @strlen(ptr noundef) #4

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @abi_copy_string(ptr noundef %0, i64 noundef %1, ptr noundef %2) #0 {
  %4 = alloca i32, align 4
  %5 = alloca ptr, align 8
  %6 = alloca i64, align 8
  %7 = alloca ptr, align 8
  %8 = alloca i64, align 8
  store ptr %0, ptr %5, align 8
  store i64 %1, ptr %6, align 8
  store ptr %2, ptr %7, align 8
  %9 = load ptr, ptr %5, align 8
  %10 = icmp ne ptr %9, null
  br i1 %10, label %11, label %17

11:                                               ; preds = %3
  %12 = load i64, ptr %6, align 8
  %13 = icmp eq i64 %12, 0
  br i1 %13, label %17, label %14

14:                                               ; preds = %11
  %15 = load ptr, ptr %7, align 8
  %16 = icmp ne ptr %15, null
  br i1 %16, label %18, label %17

17:                                               ; preds = %14, %11, %3
  store i32 -1, ptr %4, align 4
  br label %38

18:                                               ; preds = %14
  %19 = load ptr, ptr %7, align 8
  %20 = call i64 @strlen(ptr noundef %19) #9
  store i64 %20, ptr %8, align 8
  %21 = load i64, ptr %8, align 8
  %22 = load i64, ptr %6, align 8
  %23 = icmp uge i64 %21, %22
  br i1 %23, label %24, label %33

24:                                               ; preds = %18
  %25 = load ptr, ptr %5, align 8
  %26 = load ptr, ptr %7, align 8
  %27 = load i64, ptr %6, align 8
  %28 = sub i64 %27, 1
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %25, ptr align 1 %26, i64 %28, i1 false)
  %29 = load ptr, ptr %5, align 8
  %30 = load i64, ptr %6, align 8
  %31 = sub i64 %30, 1
  %32 = getelementptr inbounds i8, ptr %29, i64 %31
  store i8 0, ptr %32, align 1
  store i32 1, ptr %4, align 4
  br label %38

33:                                               ; preds = %18
  %34 = load ptr, ptr %5, align 8
  %35 = load ptr, ptr %7, align 8
  %36 = load i64, ptr %8, align 8
  %37 = add i64 %36, 1
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %34, ptr align 1 %35, i64 %37, i1 false)
  store i32 0, ptr %4, align 4
  br label %38

38:                                               ; preds = %33, %24, %17
  %39 = load i32, ptr %4, align 4
  ret i32 %39
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_sum_u8(ptr noundef %0, i64 noundef %1) #0 {
  %3 = alloca i64, align 8
  %4 = alloca ptr, align 8
  %5 = alloca i64, align 8
  %6 = alloca i64, align 8
  %7 = alloca i64, align 8
  store ptr %0, ptr %4, align 8
  store i64 %1, ptr %5, align 8
  %8 = load ptr, ptr %4, align 8
  %9 = icmp ne ptr %8, null
  br i1 %9, label %14, label %10

10:                                               ; preds = %2
  %11 = load i64, ptr %5, align 8
  %12 = icmp ne i64 %11, 0
  br i1 %12, label %13, label %14

13:                                               ; preds = %10
  store i64 -1, ptr %3, align 8
  br label %32

14:                                               ; preds = %10, %2
  store i64 0, ptr %6, align 8
  store i64 0, ptr %7, align 8
  br label %15

15:                                               ; preds = %27, %14
  %16 = load i64, ptr %7, align 8
  %17 = load i64, ptr %5, align 8
  %18 = icmp ult i64 %16, %17
  br i1 %18, label %19, label %30

19:                                               ; preds = %15
  %20 = load ptr, ptr %4, align 8
  %21 = load i64, ptr %7, align 8
  %22 = getelementptr inbounds i8, ptr %20, i64 %21
  %23 = load i8, ptr %22, align 1
  %24 = zext i8 %23 to i64
  %25 = load i64, ptr %6, align 8
  %26 = add i64 %25, %24
  store i64 %26, ptr %6, align 8
  br label %27

27:                                               ; preds = %19
  %28 = load i64, ptr %7, align 8
  %29 = add i64 %28, 1
  store i64 %29, ptr %7, align 8
  br label %15, !llvm.loop !11

30:                                               ; preds = %15
  %31 = load i64, ptr %6, align 8
  store i64 %31, ptr %3, align 8
  br label %32

32:                                               ; preds = %30, %13
  %33 = load i64, ptr %3, align 8
  ret i64 %33
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_sum_i32(ptr noundef %0, i64 noundef %1) #0 {
  %3 = alloca i64, align 8
  %4 = alloca ptr, align 8
  %5 = alloca i64, align 8
  %6 = alloca i64, align 8
  %7 = alloca i64, align 8
  store ptr %0, ptr %4, align 8
  store i64 %1, ptr %5, align 8
  %8 = load ptr, ptr %4, align 8
  %9 = icmp ne ptr %8, null
  br i1 %9, label %14, label %10

10:                                               ; preds = %2
  %11 = load i64, ptr %5, align 8
  %12 = icmp ne i64 %11, 0
  br i1 %12, label %13, label %14

13:                                               ; preds = %10
  store i64 -9223372036854775808, ptr %3, align 8
  br label %32

14:                                               ; preds = %10, %2
  store i64 0, ptr %6, align 8
  store i64 0, ptr %7, align 8
  br label %15

15:                                               ; preds = %27, %14
  %16 = load i64, ptr %7, align 8
  %17 = load i64, ptr %5, align 8
  %18 = icmp ult i64 %16, %17
  br i1 %18, label %19, label %30

19:                                               ; preds = %15
  %20 = load ptr, ptr %4, align 8
  %21 = load i64, ptr %7, align 8
  %22 = getelementptr inbounds i32, ptr %20, i64 %21
  %23 = load i32, ptr %22, align 4
  %24 = sext i32 %23 to i64
  %25 = load i64, ptr %6, align 8
  %26 = add nsw i64 %25, %24
  store i64 %26, ptr %6, align 8
  br label %27

27:                                               ; preds = %19
  %28 = load i64, ptr %7, align 8
  %29 = add i64 %28, 1
  store i64 %29, ptr %7, align 8
  br label %15, !llvm.loop !12

30:                                               ; preds = %15
  %31 = load i64, ptr %6, align 8
  store i64 %31, ptr %3, align 8
  br label %32

32:                                               ; preds = %30, %13
  %33 = load i64, ptr %3, align 8
  ret i64 %33
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local double @abi_sum_f64(ptr noundef %0, i64 noundef %1) #0 {
  %3 = alloca double, align 8
  %4 = alloca ptr, align 8
  %5 = alloca i64, align 8
  %6 = alloca double, align 8
  %7 = alloca i64, align 8
  store ptr %0, ptr %4, align 8
  store i64 %1, ptr %5, align 8
  %8 = load ptr, ptr %4, align 8
  %9 = icmp ne ptr %8, null
  br i1 %9, label %14, label %10

10:                                               ; preds = %2
  %11 = load i64, ptr %5, align 8
  %12 = icmp ne i64 %11, 0
  br i1 %12, label %13, label %14

13:                                               ; preds = %10
  store double 0xFFEFFFFFFFFFFFFF, ptr %3, align 8
  br label %31

14:                                               ; preds = %10, %2
  store double 0.000000e+00, ptr %6, align 8
  store i64 0, ptr %7, align 8
  br label %15

15:                                               ; preds = %26, %14
  %16 = load i64, ptr %7, align 8
  %17 = load i64, ptr %5, align 8
  %18 = icmp ult i64 %16, %17
  br i1 %18, label %19, label %29

19:                                               ; preds = %15
  %20 = load ptr, ptr %4, align 8
  %21 = load i64, ptr %7, align 8
  %22 = getelementptr inbounds double, ptr %20, i64 %21
  %23 = load double, ptr %22, align 8
  %24 = load double, ptr %6, align 8
  %25 = fadd double %24, %23
  store double %25, ptr %6, align 8
  br label %26

26:                                               ; preds = %19
  %27 = load i64, ptr %7, align 8
  %28 = add i64 %27, 1
  store i64 %28, ptr %7, align 8
  br label %15, !llvm.loop !13

29:                                               ; preds = %15
  %30 = load double, ptr %6, align 8
  store double %30, ptr %3, align 8
  br label %31

31:                                               ; preds = %29, %13
  %32 = load double, ptr %3, align 8
  ret double %32
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @abi_fill_u8(ptr noundef %0, i64 noundef %1, i8 noundef zeroext %2, i8 noundef zeroext %3) #0 {
  %5 = alloca ptr, align 8
  %6 = alloca i64, align 8
  %7 = alloca i8, align 1
  %8 = alloca i8, align 1
  %9 = alloca i64, align 8
  store ptr %0, ptr %5, align 8
  store i64 %1, ptr %6, align 8
  store i8 %2, ptr %7, align 1
  store i8 %3, ptr %8, align 1
  %10 = load ptr, ptr %5, align 8
  %11 = icmp ne ptr %10, null
  br i1 %11, label %13, label %12

12:                                               ; preds = %4
  br label %33

13:                                               ; preds = %4
  store i64 0, ptr %9, align 8
  br label %14

14:                                               ; preds = %30, %13
  %15 = load i64, ptr %9, align 8
  %16 = load i64, ptr %6, align 8
  %17 = icmp ult i64 %15, %16
  br i1 %17, label %18, label %33

18:                                               ; preds = %14
  %19 = load i8, ptr %7, align 1
  %20 = zext i8 %19 to i64
  %21 = load i64, ptr %9, align 8
  %22 = load i8, ptr %8, align 1
  %23 = zext i8 %22 to i64
  %24 = mul i64 %21, %23
  %25 = add i64 %20, %24
  %26 = trunc i64 %25 to i8
  %27 = load ptr, ptr %5, align 8
  %28 = load i64, ptr %9, align 8
  %29 = getelementptr inbounds i8, ptr %27, i64 %28
  store i8 %26, ptr %29, align 1
  br label %30

30:                                               ; preds = %18
  %31 = load i64, ptr %9, align 8
  %32 = add i64 %31, 1
  store i64 %32, ptr %9, align 8
  br label %14, !llvm.loop !14

33:                                               ; preds = %12, %14
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @abi_fill_i32(ptr noundef %0, i64 noundef %1, i32 noundef %2, i32 noundef %3) #0 {
  %5 = alloca ptr, align 8
  %6 = alloca i64, align 8
  %7 = alloca i32, align 4
  %8 = alloca i32, align 4
  %9 = alloca i64, align 8
  store ptr %0, ptr %5, align 8
  store i64 %1, ptr %6, align 8
  store i32 %2, ptr %7, align 4
  store i32 %3, ptr %8, align 4
  %10 = load ptr, ptr %5, align 8
  %11 = icmp ne ptr %10, null
  br i1 %11, label %13, label %12

12:                                               ; preds = %4
  br label %31

13:                                               ; preds = %4
  store i64 0, ptr %9, align 8
  br label %14

14:                                               ; preds = %28, %13
  %15 = load i64, ptr %9, align 8
  %16 = load i64, ptr %6, align 8
  %17 = icmp ult i64 %15, %16
  br i1 %17, label %18, label %31

18:                                               ; preds = %14
  %19 = load i32, ptr %7, align 4
  %20 = load i64, ptr %9, align 8
  %21 = trunc i64 %20 to i32
  %22 = load i32, ptr %8, align 4
  %23 = mul nsw i32 %21, %22
  %24 = add nsw i32 %19, %23
  %25 = load ptr, ptr %5, align 8
  %26 = load i64, ptr %9, align 8
  %27 = getelementptr inbounds i32, ptr %25, i64 %26
  store i32 %24, ptr %27, align 4
  br label %28

28:                                               ; preds = %18
  %29 = load i64, ptr %9, align 8
  %30 = add i64 %29, 1
  store i64 %30, ptr %9, align 8
  br label %14, !llvm.loop !15

31:                                               ; preds = %12, %14
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @abi_reverse_bytes(ptr noundef %0, ptr noundef %1, i64 noundef %2) #0 {
  %4 = alloca ptr, align 8
  %5 = alloca ptr, align 8
  %6 = alloca i64, align 8
  %7 = alloca i64, align 8
  store ptr %0, ptr %4, align 8
  store ptr %1, ptr %5, align 8
  store i64 %2, ptr %6, align 8
  %8 = load ptr, ptr %4, align 8
  %9 = icmp ne ptr %8, null
  br i1 %9, label %10, label %13

10:                                               ; preds = %3
  %11 = load ptr, ptr %5, align 8
  %12 = icmp ne ptr %11, null
  br i1 %12, label %14, label %13

13:                                               ; preds = %10, %3
  br label %33

14:                                               ; preds = %10
  store i64 0, ptr %7, align 8
  br label %15

15:                                               ; preds = %30, %14
  %16 = load i64, ptr %7, align 8
  %17 = load i64, ptr %6, align 8
  %18 = icmp ult i64 %16, %17
  br i1 %18, label %19, label %33

19:                                               ; preds = %15
  %20 = load ptr, ptr %5, align 8
  %21 = load i64, ptr %6, align 8
  %22 = sub i64 %21, 1
  %23 = load i64, ptr %7, align 8
  %24 = sub i64 %22, %23
  %25 = getelementptr inbounds i8, ptr %20, i64 %24
  %26 = load i8, ptr %25, align 1
  %27 = load ptr, ptr %4, align 8
  %28 = load i64, ptr %7, align 8
  %29 = getelementptr inbounds i8, ptr %27, i64 %28
  store i8 %26, ptr %29, align 1
  br label %30

30:                                               ; preds = %19
  %31 = load i64, ptr %7, align 8
  %32 = add i64 %31, 1
  store i64 %32, ptr %7, align 8
  br label %15, !llvm.loop !16

33:                                               ; preds = %13, %15
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @abi_pointer_difference(ptr noundef %0, ptr noundef %1) #0 {
  %3 = alloca i32, align 4
  %4 = alloca ptr, align 8
  %5 = alloca ptr, align 8
  store ptr %0, ptr %4, align 8
  store ptr %1, ptr %5, align 8
  %6 = load ptr, ptr %4, align 8
  %7 = icmp ne ptr %6, null
  br i1 %7, label %8, label %11

8:                                                ; preds = %2
  %9 = load ptr, ptr %5, align 8
  %10 = icmp ne ptr %9, null
  br i1 %10, label %12, label %11

11:                                               ; preds = %8, %2
  store i32 -2147483648, ptr %3, align 4
  br label %20

12:                                               ; preds = %8
  %13 = load ptr, ptr %4, align 8
  %14 = load ptr, ptr %5, align 8
  %15 = ptrtoint ptr %13 to i64
  %16 = ptrtoint ptr %14 to i64
  %17 = sub i64 %15, %16
  %18 = sdiv exact i64 %17, 4
  %19 = trunc i64 %18 to i32
  store i32 %19, ptr %3, align 4
  br label %20

20:                                               ; preds = %12, %11
  %21 = load i32, ptr %3, align 4
  ret i32 %21
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @abi_sum_fixed_2x3(ptr noundef %0) #0 {
  %2 = alloca i32, align 4
  %3 = alloca ptr, align 8
  store ptr %0, ptr %3, align 8
  %4 = load ptr, ptr %3, align 8
  %5 = icmp ne ptr %4, null
  br i1 %5, label %7, label %6

6:                                                ; preds = %1
  store i32 -2147483648, ptr %2, align 4
  br label %37

7:                                                ; preds = %1
  %8 = load ptr, ptr %3, align 8
  %9 = getelementptr inbounds [3 x i32], ptr %8, i64 0
  %10 = getelementptr inbounds [3 x i32], ptr %9, i64 0, i64 0
  %11 = load i32, ptr %10, align 4
  %12 = load ptr, ptr %3, align 8
  %13 = getelementptr inbounds [3 x i32], ptr %12, i64 0
  %14 = getelementptr inbounds [3 x i32], ptr %13, i64 0, i64 1
  %15 = load i32, ptr %14, align 4
  %16 = add nsw i32 %11, %15
  %17 = load ptr, ptr %3, align 8
  %18 = getelementptr inbounds [3 x i32], ptr %17, i64 0
  %19 = getelementptr inbounds [3 x i32], ptr %18, i64 0, i64 2
  %20 = load i32, ptr %19, align 4
  %21 = add nsw i32 %16, %20
  %22 = load ptr, ptr %3, align 8
  %23 = getelementptr inbounds [3 x i32], ptr %22, i64 1
  %24 = getelementptr inbounds [3 x i32], ptr %23, i64 0, i64 0
  %25 = load i32, ptr %24, align 4
  %26 = add nsw i32 %21, %25
  %27 = load ptr, ptr %3, align 8
  %28 = getelementptr inbounds [3 x i32], ptr %27, i64 1
  %29 = getelementptr inbounds [3 x i32], ptr %28, i64 0, i64 1
  %30 = load i32, ptr %29, align 4
  %31 = add nsw i32 %26, %30
  %32 = load ptr, ptr %3, align 8
  %33 = getelementptr inbounds [3 x i32], ptr %32, i64 1
  %34 = getelementptr inbounds [3 x i32], ptr %33, i64 0, i64 2
  %35 = load i32, ptr %34, align 4
  %36 = add nsw i32 %31, %35
  store i32 %36, ptr %2, align 4
  br label %37

37:                                               ; preds = %7, %6
  %38 = load i32, ptr %2, align 4
  ret i32 %38
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_sum_cstrings(ptr noundef %0, i64 noundef %1) #0 {
  %3 = alloca i64, align 8
  %4 = alloca ptr, align 8
  %5 = alloca i64, align 8
  %6 = alloca i64, align 8
  %7 = alloca i64, align 8
  store ptr %0, ptr %4, align 8
  store i64 %1, ptr %5, align 8
  %8 = load ptr, ptr %4, align 8
  %9 = icmp ne ptr %8, null
  br i1 %9, label %14, label %10

10:                                               ; preds = %2
  %11 = load i64, ptr %5, align 8
  %12 = icmp ne i64 %11, 0
  br i1 %12, label %13, label %14

13:                                               ; preds = %10
  store i64 -1, ptr %3, align 8
  br label %39

14:                                               ; preds = %10, %2
  store i64 0, ptr %6, align 8
  store i64 0, ptr %7, align 8
  br label %15

15:                                               ; preds = %34, %14
  %16 = load i64, ptr %7, align 8
  %17 = load i64, ptr %5, align 8
  %18 = icmp ult i64 %16, %17
  br i1 %18, label %19, label %37

19:                                               ; preds = %15
  %20 = load ptr, ptr %4, align 8
  %21 = load i64, ptr %7, align 8
  %22 = getelementptr inbounds ptr, ptr %20, i64 %21
  %23 = load ptr, ptr %22, align 8
  %24 = icmp ne ptr %23, null
  br i1 %24, label %25, label %33

25:                                               ; preds = %19
  %26 = load ptr, ptr %4, align 8
  %27 = load i64, ptr %7, align 8
  %28 = getelementptr inbounds ptr, ptr %26, i64 %27
  %29 = load ptr, ptr %28, align 8
  %30 = call i64 @strlen(ptr noundef %29) #9
  %31 = load i64, ptr %6, align 8
  %32 = add i64 %31, %30
  store i64 %32, ptr %6, align 8
  br label %33

33:                                               ; preds = %25, %19
  br label %34

34:                                               ; preds = %33
  %35 = load i64, ptr %7, align 8
  %36 = add i64 %35, 1
  store i64 %36, ptr %7, align 8
  br label %15, !llvm.loop !17

37:                                               ; preds = %15
  %38 = load i64, ptr %6, align 8
  store i64 %38, ptr %3, align 8
  br label %39

39:                                               ; preds = %37, %13
  %40 = load i64, ptr %3, align 8
  ret i64 %40
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @abi_out_two_ints(i32 noundef %0, ptr noundef %1, ptr noundef %2) #0 {
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  %6 = alloca ptr, align 8
  %7 = alloca ptr, align 8
  store i32 %0, ptr %5, align 4
  store ptr %1, ptr %6, align 8
  store ptr %2, ptr %7, align 8
  %8 = load ptr, ptr %6, align 8
  %9 = icmp ne ptr %8, null
  br i1 %9, label %10, label %13

10:                                               ; preds = %3
  %11 = load ptr, ptr %7, align 8
  %12 = icmp ne ptr %11, null
  br i1 %12, label %14, label %13

13:                                               ; preds = %10, %3
  store i32 -1, ptr %4, align 4
  br label %21

14:                                               ; preds = %10
  %15 = load i32, ptr %5, align 4
  %16 = add nsw i32 %15, 111
  %17 = load ptr, ptr %6, align 8
  store i32 %16, ptr %17, align 4
  %18 = load i32, ptr %5, align 4
  %19 = mul nsw i32 %18, 3
  %20 = load ptr, ptr %7, align 8
  store i32 %19, ptr %20, align 4
  store i32 0, ptr %4, align 4
  br label %21

21:                                               ; preds = %14, %13
  %22 = load i32, ptr %4, align 4
  ret i32 %22
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @abi_out_many(ptr noundef %0, ptr noundef %1, ptr noundef %2, ptr noundef %3, ptr noundef %4, ptr noundef %5, ptr noundef %6, ptr noundef %7) #0 {
  %9 = alloca i32, align 4
  %10 = alloca ptr, align 8
  %11 = alloca ptr, align 8
  %12 = alloca ptr, align 8
  %13 = alloca ptr, align 8
  %14 = alloca ptr, align 8
  %15 = alloca ptr, align 8
  %16 = alloca ptr, align 8
  %17 = alloca ptr, align 8
  store ptr %0, ptr %10, align 8
  store ptr %1, ptr %11, align 8
  store ptr %2, ptr %12, align 8
  store ptr %3, ptr %13, align 8
  store ptr %4, ptr %14, align 8
  store ptr %5, ptr %15, align 8
  store ptr %6, ptr %16, align 8
  store ptr %7, ptr %17, align 8
  %18 = load ptr, ptr %10, align 8
  %19 = icmp ne ptr %18, null
  br i1 %19, label %20, label %41

20:                                               ; preds = %8
  %21 = load ptr, ptr %11, align 8
  %22 = icmp ne ptr %21, null
  br i1 %22, label %23, label %41

23:                                               ; preds = %20
  %24 = load ptr, ptr %12, align 8
  %25 = icmp ne ptr %24, null
  br i1 %25, label %26, label %41

26:                                               ; preds = %23
  %27 = load ptr, ptr %13, align 8
  %28 = icmp ne ptr %27, null
  br i1 %28, label %29, label %41

29:                                               ; preds = %26
  %30 = load ptr, ptr %14, align 8
  %31 = icmp ne ptr %30, null
  br i1 %31, label %32, label %41

32:                                               ; preds = %29
  %33 = load ptr, ptr %15, align 8
  %34 = icmp ne ptr %33, null
  br i1 %34, label %35, label %41

35:                                               ; preds = %32
  %36 = load ptr, ptr %16, align 8
  %37 = icmp ne ptr %36, null
  br i1 %37, label %38, label %41

38:                                               ; preds = %35
  %39 = load ptr, ptr %17, align 8
  %40 = icmp ne ptr %39, null
  br i1 %40, label %42, label %41

41:                                               ; preds = %38, %35, %32, %29, %26, %23, %20, %8
  store i32 -1, ptr %9, align 4
  br label %51

42:                                               ; preds = %38
  %43 = load ptr, ptr %10, align 8
  store i8 -25, ptr %43, align 1
  %44 = load ptr, ptr %11, align 8
  store i16 -23456, ptr %44, align 2
  %45 = load ptr, ptr %12, align 8
  store i32 -559038737, ptr %45, align 4
  %46 = load ptr, ptr %13, align 8
  store i64 -81985529216486895, ptr %46, align 8
  %47 = load ptr, ptr %14, align 8
  store float 6.125000e+00, ptr %47, align 4
  %48 = load ptr, ptr %15, align 8
  store double 0xBF50000000000000, ptr %48, align 8
  %49 = load ptr, ptr %16, align 8
  store ptr @.str.1, ptr %49, align 8
  %50 = load ptr, ptr %17, align 8
  store ptr inttoptr (i64 48879 to ptr), ptr %50, align 8
  store i32 0, ptr %9, align 4
  br label %51

51:                                               ; preds = %42, %41
  %52 = load i32, ptr %9, align 4
  ret i32 %52
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @abi_increment_pointer_chain(ptr noundef %0, i32 noundef %1) #0 {
  %3 = alloca ptr, align 8
  %4 = alloca i32, align 4
  store ptr %0, ptr %3, align 8
  store i32 %1, ptr %4, align 4
  %5 = load ptr, ptr %3, align 8
  %6 = icmp ne ptr %5, null
  br i1 %6, label %7, label %23

7:                                                ; preds = %2
  %8 = load ptr, ptr %3, align 8
  %9 = load ptr, ptr %8, align 8
  %10 = icmp ne ptr %9, null
  br i1 %10, label %11, label %23

11:                                               ; preds = %7
  %12 = load ptr, ptr %3, align 8
  %13 = load ptr, ptr %12, align 8
  %14 = load ptr, ptr %13, align 8
  %15 = icmp ne ptr %14, null
  br i1 %15, label %16, label %23

16:                                               ; preds = %11
  %17 = load i32, ptr %4, align 4
  %18 = load ptr, ptr %3, align 8
  %19 = load ptr, ptr %18, align 8
  %20 = load ptr, ptr %19, align 8
  %21 = load i32, ptr %20, align 4
  %22 = add nsw i32 %21, %17
  store i32 %22, ptr %20, align 4
  br label %23

23:                                               ; preds = %16, %11, %7, %2
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @abi_set_and_return_pointer(ptr noundef %0, ptr noundef %1, i32 noundef %2) #0 {
  %4 = alloca i32, align 4
  %5 = alloca ptr, align 8
  %6 = alloca ptr, align 8
  %7 = alloca i32, align 4
  store ptr %0, ptr %5, align 8
  store ptr %1, ptr %6, align 8
  store i32 %2, ptr %7, align 4
  %8 = load ptr, ptr %5, align 8
  %9 = icmp ne ptr %8, null
  br i1 %9, label %10, label %13

10:                                               ; preds = %3
  %11 = load ptr, ptr %6, align 8
  %12 = icmp ne ptr %11, null
  br i1 %12, label %14, label %13

13:                                               ; preds = %10, %3
  store i32 -1, ptr %4, align 4
  br label %21

14:                                               ; preds = %10
  %15 = load i32, ptr %7, align 4
  %16 = load ptr, ptr %5, align 8
  store i32 %15, ptr %16, align 4
  %17 = load ptr, ptr %5, align 8
  %18 = load ptr, ptr %6, align 8
  store ptr %17, ptr %18, align 8
  %19 = load ptr, ptr %5, align 8
  %20 = load i32, ptr %19, align 4
  store i32 %20, ptr %4, align 4
  br label %21

21:                                               ; preds = %14, %13
  %22 = load i32, ptr %4, align 4
  ret i32 %22
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @abi_add(i32 noundef %0, i32 noundef %1) #0 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  store i32 %0, ptr %3, align 4
  store i32 %1, ptr %4, align 4
  %5 = load i32, ptr %3, align 4
  %6 = load i32, ptr %4, align 4
  %7 = add nsw i32 %5, %6
  ret i32 %7
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @abi_subtract(i32 noundef %0, i32 noundef %1) #0 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  store i32 %0, ptr %3, align 4
  store i32 %1, ptr %4, align 4
  %5 = load i32, ptr %3, align 4
  %6 = load i32, ptr %4, align 4
  %7 = sub nsw i32 %5, %6
  ret i32 %7
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @abi_multiply(i32 noundef %0, i32 noundef %1) #0 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  store i32 %0, ptr %3, align 4
  store i32 %1, ptr %4, align 4
  %5 = load i32, ptr %3, align 4
  %6 = load i32, ptr %4, align 4
  %7 = mul nsw i32 %5, %6
  ret i32 %7
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local double @abi_square_double(double noundef %0) #0 {
  %2 = alloca double, align 8
  store double %0, ptr %2, align 8
  %3 = load double, ptr %2, align 8
  %4 = load double, ptr %2, align 8
  %5 = fmul double %3, %4
  ret double %5
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local double @abi_sin_approx_input(double noundef %0) #0 {
  %2 = alloca double, align 8
  store double %0, ptr %2, align 8
  %3 = load double, ptr %2, align 8
  ret double %3
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @abi_call_binary(ptr noundef %0, i32 noundef %1, i32 noundef %2) #0 {
  %4 = alloca ptr, align 8
  %5 = alloca i32, align 4
  %6 = alloca i32, align 4
  store ptr %0, ptr %4, align 8
  store i32 %1, ptr %5, align 4
  store i32 %2, ptr %6, align 4
  %7 = load ptr, ptr %4, align 8
  %8 = icmp ne ptr %7, null
  br i1 %8, label %9, label %14

9:                                                ; preds = %3
  %10 = load ptr, ptr %4, align 8
  %11 = load i32, ptr %5, align 4
  %12 = load i32, ptr %6, align 4
  %13 = call i32 %10(i32 noundef %11, i32 noundef %12)
  br label %15

14:                                               ; preds = %3
  br label %15

15:                                               ; preds = %14, %9
  %16 = phi i32 [ %13, %9 ], [ -2147483648, %14 ]
  ret i32 %16
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local double @abi_call_unary(ptr noundef %0, double noundef %1) #0 {
  %3 = alloca ptr, align 8
  %4 = alloca double, align 8
  store ptr %0, ptr %3, align 8
  store double %1, ptr %4, align 8
  %5 = load ptr, ptr %3, align 8
  %6 = icmp ne ptr %5, null
  br i1 %6, label %7, label %11

7:                                                ; preds = %2
  %8 = load ptr, ptr %3, align 8
  %9 = load double, ptr %4, align 8
  %10 = call double %8(double noundef %9)
  br label %12

11:                                               ; preds = %2
  br label %12

12:                                               ; preds = %11, %7
  %13 = phi double [ %10, %7 ], [ 0xFFEFFFFFFFFFFFFF, %11 ]
  ret double %13
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @abi_apply_transform(ptr noundef %0, i64 noundef %1, ptr noundef %2, ptr noundef %3) #0 {
  %5 = alloca i32, align 4
  %6 = alloca ptr, align 8
  %7 = alloca i64, align 8
  %8 = alloca ptr, align 8
  %9 = alloca ptr, align 8
  %10 = alloca i64, align 8
  store ptr %0, ptr %6, align 8
  store i64 %1, ptr %7, align 8
  store ptr %2, ptr %8, align 8
  store ptr %3, ptr %9, align 8
  %11 = load ptr, ptr %6, align 8
  %12 = icmp ne ptr %11, null
  br i1 %12, label %16, label %13

13:                                               ; preds = %4
  %14 = load i64, ptr %7, align 8
  %15 = icmp ne i64 %14, 0
  br i1 %15, label %19, label %16

16:                                               ; preds = %13, %4
  %17 = load ptr, ptr %8, align 8
  %18 = icmp ne ptr %17, null
  br i1 %18, label %20, label %19

19:                                               ; preds = %16, %13
  store i32 -1, ptr %5, align 4
  br label %42

20:                                               ; preds = %16
  store i64 0, ptr %10, align 8
  br label %21

21:                                               ; preds = %36, %20
  %22 = load i64, ptr %10, align 8
  %23 = load i64, ptr %7, align 8
  %24 = icmp ult i64 %22, %23
  br i1 %24, label %25, label %39

25:                                               ; preds = %21
  %26 = load ptr, ptr %8, align 8
  %27 = load ptr, ptr %6, align 8
  %28 = load i64, ptr %10, align 8
  %29 = getelementptr inbounds i32, ptr %27, i64 %28
  %30 = load i32, ptr %29, align 4
  %31 = load ptr, ptr %9, align 8
  %32 = call i32 %26(i32 noundef %30, ptr noundef %31)
  %33 = load ptr, ptr %6, align 8
  %34 = load i64, ptr %10, align 8
  %35 = getelementptr inbounds i32, ptr %33, i64 %34
  store i32 %32, ptr %35, align 4
  br label %36

36:                                               ; preds = %25
  %37 = load i64, ptr %10, align 8
  %38 = add i64 %37, 1
  store i64 %38, ptr %10, align 8
  br label %21, !llvm.loop !18

39:                                               ; preds = %21
  %40 = load i64, ptr %7, align 8
  %41 = trunc i64 %40 to i32
  store i32 %41, ptr %5, align 4
  br label %42

42:                                               ; preds = %39, %19
  %43 = load i32, ptr %5, align 4
  ret i32 %43
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @abi_visit_values(ptr noundef %0, i64 noundef %1, ptr noundef %2, ptr noundef %3) #0 {
  %5 = alloca i32, align 4
  %6 = alloca ptr, align 8
  %7 = alloca i64, align 8
  %8 = alloca ptr, align 8
  %9 = alloca ptr, align 8
  %10 = alloca i64, align 8
  store ptr %0, ptr %6, align 8
  store i64 %1, ptr %7, align 8
  store ptr %2, ptr %8, align 8
  store ptr %3, ptr %9, align 8
  %11 = load ptr, ptr %6, align 8
  %12 = icmp ne ptr %11, null
  br i1 %12, label %16, label %13

13:                                               ; preds = %4
  %14 = load i64, ptr %7, align 8
  %15 = icmp ne i64 %14, 0
  br i1 %15, label %19, label %16

16:                                               ; preds = %13, %4
  %17 = load ptr, ptr %8, align 8
  %18 = icmp ne ptr %17, null
  br i1 %18, label %20, label %19

19:                                               ; preds = %16, %13
  store i32 -1, ptr %5, align 4
  br label %39

20:                                               ; preds = %16
  store i64 0, ptr %10, align 8
  br label %21

21:                                               ; preds = %33, %20
  %22 = load i64, ptr %10, align 8
  %23 = load i64, ptr %7, align 8
  %24 = icmp ult i64 %22, %23
  br i1 %24, label %25, label %36

25:                                               ; preds = %21
  %26 = load ptr, ptr %8, align 8
  %27 = load i64, ptr %10, align 8
  %28 = load ptr, ptr %6, align 8
  %29 = load i64, ptr %10, align 8
  %30 = getelementptr inbounds i32, ptr %28, i64 %29
  %31 = load i32, ptr %30, align 4
  %32 = load ptr, ptr %9, align 8
  call void %26(i64 noundef %27, i32 noundef %31, ptr noundef %32)
  br label %33

33:                                               ; preds = %25
  %34 = load i64, ptr %10, align 8
  %35 = add i64 %34, 1
  store i64 %35, ptr %10, align 8
  br label %21, !llvm.loop !19

36:                                               ; preds = %21
  %37 = load i64, ptr %7, align 8
  %38 = trunc i64 %37 to i32
  store i32 %38, ptr %5, align 4
  br label %39

39:                                               ; preds = %36, %19
  %40 = load i32, ptr %5, align 4
  ret i32 %40
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @abi_log_twice(ptr noundef %0, ptr noundef %1) #0 {
  %3 = alloca ptr, align 8
  %4 = alloca ptr, align 8
  store ptr %0, ptr %3, align 8
  store ptr %1, ptr %4, align 8
  %5 = load ptr, ptr %3, align 8
  %6 = icmp ne ptr %5, null
  br i1 %6, label %8, label %7

7:                                                ; preds = %2
  br label %13

8:                                                ; preds = %2
  %9 = load ptr, ptr %3, align 8
  %10 = load ptr, ptr %4, align 8
  call void %9(i32 noundef 1, ptr noundef @.str.2, ptr noundef %10)
  %11 = load ptr, ptr %3, align 8
  %12 = load ptr, ptr %4, align 8
  call void %11(i32 noundef 2, ptr noundef @.str.3, ptr noundef %12)
  br label %13

13:                                               ; preds = %8, %7
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @abi_save_binary_callback(ptr noundef %0, ptr noundef %1) #0 {
  %3 = alloca ptr, align 8
  %4 = alloca ptr, align 8
  store ptr %0, ptr %3, align 8
  store ptr %1, ptr %4, align 8
  %5 = load ptr, ptr %3, align 8
  store ptr %5, ptr @g_saved_binary_fn, align 8
  %6 = load ptr, ptr %4, align 8
  store ptr %6, ptr @g_saved_binary_context, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @abi_call_saved_binary(i32 noundef %0, i32 noundef %1) #0 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  store i32 %0, ptr %4, align 4
  store i32 %1, ptr %5, align 4
  %6 = load ptr, ptr @g_saved_binary_fn, align 8
  %7 = icmp ne ptr %6, null
  br i1 %7, label %9, label %8

8:                                                ; preds = %2
  store i32 -2147483648, ptr %3, align 4
  br label %15

9:                                                ; preds = %2
  %10 = load ptr, ptr @g_saved_binary_context, align 8
  %11 = load ptr, ptr @g_saved_binary_fn, align 8
  %12 = load i32, ptr %4, align 4
  %13 = load i32, ptr %5, align 4
  %14 = call i32 %11(i32 noundef %12, i32 noundef %13)
  store i32 %14, ptr %3, align 4
  br label %15

15:                                               ; preds = %9, %8
  %16 = load i32, ptr %3, align 4
  ret i32 %16
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @abi_clear_saved_callback() #0 {
  store ptr null, ptr @g_saved_binary_fn, align 8
  store ptr null, ptr @g_saved_binary_context, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local double @abi_variadic_sum_ints(i32 noundef %0, ...) #0 {
  %2 = alloca double, align 8
  %3 = alloca i32, align 4
  %4 = alloca [1 x %struct.__va_list_tag], align 16
  %5 = alloca double, align 8
  %6 = alloca i32, align 4
  store i32 %0, ptr %3, align 4
  %7 = load i32, ptr %3, align 4
  %8 = icmp slt i32 %7, 0
  br i1 %8, label %9, label %10

9:                                                ; preds = %1
  store double 0xFFEFFFFFFFFFFFFF, ptr %2, align 8
  br label %42

10:                                               ; preds = %1
  %11 = getelementptr inbounds [1 x %struct.__va_list_tag], ptr %4, i64 0, i64 0
  call void @llvm.va_start(ptr %11)
  store double 0.000000e+00, ptr %5, align 8
  store i32 0, ptr %6, align 4
  br label %12

12:                                               ; preds = %36, %10
  %13 = load i32, ptr %6, align 4
  %14 = load i32, ptr %3, align 4
  %15 = icmp slt i32 %13, %14
  br i1 %15, label %16, label %39

16:                                               ; preds = %12
  %17 = getelementptr inbounds [1 x %struct.__va_list_tag], ptr %4, i64 0, i64 0
  %18 = getelementptr inbounds %struct.__va_list_tag, ptr %17, i32 0, i32 0
  %19 = load i32, ptr %18, align 16
  %20 = icmp ule i32 %19, 40
  br i1 %20, label %21, label %26

21:                                               ; preds = %16
  %22 = getelementptr inbounds %struct.__va_list_tag, ptr %17, i32 0, i32 3
  %23 = load ptr, ptr %22, align 16
  %24 = getelementptr i8, ptr %23, i32 %19
  %25 = add i32 %19, 8
  store i32 %25, ptr %18, align 16
  br label %30

26:                                               ; preds = %16
  %27 = getelementptr inbounds %struct.__va_list_tag, ptr %17, i32 0, i32 2
  %28 = load ptr, ptr %27, align 8
  %29 = getelementptr i8, ptr %28, i32 8
  store ptr %29, ptr %27, align 8
  br label %30

30:                                               ; preds = %26, %21
  %31 = phi ptr [ %24, %21 ], [ %28, %26 ]
  %32 = load i32, ptr %31, align 4
  %33 = sitofp i32 %32 to double
  %34 = load double, ptr %5, align 8
  %35 = fadd double %34, %33
  store double %35, ptr %5, align 8
  br label %36

36:                                               ; preds = %30
  %37 = load i32, ptr %6, align 4
  %38 = add nsw i32 %37, 1
  store i32 %38, ptr %6, align 4
  br label %12, !llvm.loop !20

39:                                               ; preds = %12
  %40 = getelementptr inbounds [1 x %struct.__va_list_tag], ptr %4, i64 0, i64 0
  call void @llvm.va_end(ptr %40)
  %41 = load double, ptr %5, align 8
  store double %41, ptr %2, align 8
  br label %42

42:                                               ; preds = %39, %9
  %43 = load double, ptr %2, align 8
  ret double %43
}

; Function Attrs: nocallback nofree nosync nounwind willreturn
declare void @llvm.va_start(ptr) #5

; Function Attrs: nocallback nofree nosync nounwind willreturn
declare void @llvm.va_end(ptr) #5

; Function Attrs: noinline nounwind optnone uwtable
define dso_local double @abi_variadic_sum_doubles(i32 noundef %0, ...) #0 {
  %2 = alloca double, align 8
  %3 = alloca i32, align 4
  %4 = alloca [1 x %struct.__va_list_tag], align 16
  %5 = alloca double, align 8
  %6 = alloca i32, align 4
  store i32 %0, ptr %3, align 4
  %7 = load i32, ptr %3, align 4
  %8 = icmp slt i32 %7, 0
  br i1 %8, label %9, label %10

9:                                                ; preds = %1
  store double 0xFFEFFFFFFFFFFFFF, ptr %2, align 8
  br label %41

10:                                               ; preds = %1
  %11 = getelementptr inbounds [1 x %struct.__va_list_tag], ptr %4, i64 0, i64 0
  call void @llvm.va_start(ptr %11)
  store double 0.000000e+00, ptr %5, align 8
  store i32 0, ptr %6, align 4
  br label %12

12:                                               ; preds = %35, %10
  %13 = load i32, ptr %6, align 4
  %14 = load i32, ptr %3, align 4
  %15 = icmp slt i32 %13, %14
  br i1 %15, label %16, label %38

16:                                               ; preds = %12
  %17 = getelementptr inbounds [1 x %struct.__va_list_tag], ptr %4, i64 0, i64 0
  %18 = getelementptr inbounds %struct.__va_list_tag, ptr %17, i32 0, i32 1
  %19 = load i32, ptr %18, align 4
  %20 = icmp ule i32 %19, 160
  br i1 %20, label %21, label %26

21:                                               ; preds = %16
  %22 = getelementptr inbounds %struct.__va_list_tag, ptr %17, i32 0, i32 3
  %23 = load ptr, ptr %22, align 16
  %24 = getelementptr i8, ptr %23, i32 %19
  %25 = add i32 %19, 16
  store i32 %25, ptr %18, align 4
  br label %30

26:                                               ; preds = %16
  %27 = getelementptr inbounds %struct.__va_list_tag, ptr %17, i32 0, i32 2
  %28 = load ptr, ptr %27, align 8
  %29 = getelementptr i8, ptr %28, i32 8
  store ptr %29, ptr %27, align 8
  br label %30

30:                                               ; preds = %26, %21
  %31 = phi ptr [ %24, %21 ], [ %28, %26 ]
  %32 = load double, ptr %31, align 8
  %33 = load double, ptr %5, align 8
  %34 = fadd double %33, %32
  store double %34, ptr %5, align 8
  br label %35

35:                                               ; preds = %30
  %36 = load i32, ptr %6, align 4
  %37 = add nsw i32 %36, 1
  store i32 %37, ptr %6, align 4
  br label %12, !llvm.loop !21

38:                                               ; preds = %12
  %39 = getelementptr inbounds [1 x %struct.__va_list_tag], ptr %4, i64 0, i64 0
  call void @llvm.va_end(ptr %39)
  %40 = load double, ptr %5, align 8
  store double %40, ptr %2, align 8
  br label %41

41:                                               ; preds = %38, %9
  %42 = load double, ptr %2, align 8
  ret double %42
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local double @abi_variadic_mixed(i32 noundef %0, ...) #0 {
  %2 = alloca double, align 8
  %3 = alloca i32, align 4
  %4 = alloca [1 x %struct.__va_list_tag], align 16
  %5 = alloca double, align 8
  %6 = alloca i32, align 4
  %7 = alloca i32, align 4
  %8 = alloca ptr, align 8
  store i32 %0, ptr %3, align 4
  %9 = load i32, ptr %3, align 4
  %10 = icmp slt i32 %9, 0
  br i1 %10, label %11, label %12

11:                                               ; preds = %1
  store double 0xFFEFFFFFFFFFFFFF, ptr %2, align 8
  br label %119

12:                                               ; preds = %1
  %13 = getelementptr inbounds [1 x %struct.__va_list_tag], ptr %4, i64 0, i64 0
  call void @llvm.va_start(ptr %13)
  store double 0.000000e+00, ptr %5, align 8
  store i32 0, ptr %6, align 4
  br label %14

14:                                               ; preds = %113, %12
  %15 = load i32, ptr %6, align 4
  %16 = load i32, ptr %3, align 4
  %17 = icmp slt i32 %15, %16
  br i1 %17, label %18, label %116

18:                                               ; preds = %14
  %19 = getelementptr inbounds [1 x %struct.__va_list_tag], ptr %4, i64 0, i64 0
  %20 = getelementptr inbounds %struct.__va_list_tag, ptr %19, i32 0, i32 0
  %21 = load i32, ptr %20, align 16
  %22 = icmp ule i32 %21, 40
  br i1 %22, label %23, label %28

23:                                               ; preds = %18
  %24 = getelementptr inbounds %struct.__va_list_tag, ptr %19, i32 0, i32 3
  %25 = load ptr, ptr %24, align 16
  %26 = getelementptr i8, ptr %25, i32 %21
  %27 = add i32 %21, 8
  store i32 %27, ptr %20, align 16
  br label %32

28:                                               ; preds = %18
  %29 = getelementptr inbounds %struct.__va_list_tag, ptr %19, i32 0, i32 2
  %30 = load ptr, ptr %29, align 8
  %31 = getelementptr i8, ptr %30, i32 8
  store ptr %31, ptr %29, align 8
  br label %32

32:                                               ; preds = %28, %23
  %33 = phi ptr [ %26, %23 ], [ %30, %28 ]
  %34 = load i32, ptr %33, align 4
  store i32 %34, ptr %7, align 4
  %35 = load i32, ptr %7, align 4
  %36 = icmp eq i32 %35, 0
  br i1 %36, label %37, label %57

37:                                               ; preds = %32
  %38 = getelementptr inbounds [1 x %struct.__va_list_tag], ptr %4, i64 0, i64 0
  %39 = getelementptr inbounds %struct.__va_list_tag, ptr %38, i32 0, i32 0
  %40 = load i32, ptr %39, align 16
  %41 = icmp ule i32 %40, 40
  br i1 %41, label %42, label %47

42:                                               ; preds = %37
  %43 = getelementptr inbounds %struct.__va_list_tag, ptr %38, i32 0, i32 3
  %44 = load ptr, ptr %43, align 16
  %45 = getelementptr i8, ptr %44, i32 %40
  %46 = add i32 %40, 8
  store i32 %46, ptr %39, align 16
  br label %51

47:                                               ; preds = %37
  %48 = getelementptr inbounds %struct.__va_list_tag, ptr %38, i32 0, i32 2
  %49 = load ptr, ptr %48, align 8
  %50 = getelementptr i8, ptr %49, i32 8
  store ptr %50, ptr %48, align 8
  br label %51

51:                                               ; preds = %47, %42
  %52 = phi ptr [ %45, %42 ], [ %49, %47 ]
  %53 = load i32, ptr %52, align 4
  %54 = sitofp i32 %53 to double
  %55 = load double, ptr %5, align 8
  %56 = fadd double %55, %54
  store double %56, ptr %5, align 8
  br label %112

57:                                               ; preds = %32
  %58 = load i32, ptr %7, align 4
  %59 = icmp eq i32 %58, 1
  br i1 %59, label %60, label %79

60:                                               ; preds = %57
  %61 = getelementptr inbounds [1 x %struct.__va_list_tag], ptr %4, i64 0, i64 0
  %62 = getelementptr inbounds %struct.__va_list_tag, ptr %61, i32 0, i32 1
  %63 = load i32, ptr %62, align 4
  %64 = icmp ule i32 %63, 160
  br i1 %64, label %65, label %70

65:                                               ; preds = %60
  %66 = getelementptr inbounds %struct.__va_list_tag, ptr %61, i32 0, i32 3
  %67 = load ptr, ptr %66, align 16
  %68 = getelementptr i8, ptr %67, i32 %63
  %69 = add i32 %63, 16
  store i32 %69, ptr %62, align 4
  br label %74

70:                                               ; preds = %60
  %71 = getelementptr inbounds %struct.__va_list_tag, ptr %61, i32 0, i32 2
  %72 = load ptr, ptr %71, align 8
  %73 = getelementptr i8, ptr %72, i32 8
  store ptr %73, ptr %71, align 8
  br label %74

74:                                               ; preds = %70, %65
  %75 = phi ptr [ %68, %65 ], [ %72, %70 ]
  %76 = load double, ptr %75, align 8
  %77 = load double, ptr %5, align 8
  %78 = fadd double %77, %76
  store double %78, ptr %5, align 8
  br label %111

79:                                               ; preds = %57
  %80 = load i32, ptr %7, align 4
  %81 = icmp eq i32 %80, 2
  br i1 %81, label %82, label %108

82:                                               ; preds = %79
  %83 = getelementptr inbounds [1 x %struct.__va_list_tag], ptr %4, i64 0, i64 0
  %84 = getelementptr inbounds %struct.__va_list_tag, ptr %83, i32 0, i32 0
  %85 = load i32, ptr %84, align 16
  %86 = icmp ule i32 %85, 40
  br i1 %86, label %87, label %92

87:                                               ; preds = %82
  %88 = getelementptr inbounds %struct.__va_list_tag, ptr %83, i32 0, i32 3
  %89 = load ptr, ptr %88, align 16
  %90 = getelementptr i8, ptr %89, i32 %85
  %91 = add i32 %85, 8
  store i32 %91, ptr %84, align 16
  br label %96

92:                                               ; preds = %82
  %93 = getelementptr inbounds %struct.__va_list_tag, ptr %83, i32 0, i32 2
  %94 = load ptr, ptr %93, align 8
  %95 = getelementptr i8, ptr %94, i32 8
  store ptr %95, ptr %93, align 8
  br label %96

96:                                               ; preds = %92, %87
  %97 = phi ptr [ %90, %87 ], [ %94, %92 ]
  %98 = load ptr, ptr %97, align 8
  store ptr %98, ptr %8, align 8
  %99 = load ptr, ptr %8, align 8
  %100 = icmp ne ptr %99, null
  br i1 %100, label %101, label %107

101:                                              ; preds = %96
  %102 = load ptr, ptr %8, align 8
  %103 = call i64 @strlen(ptr noundef %102) #9
  %104 = uitofp i64 %103 to double
  %105 = load double, ptr %5, align 8
  %106 = fadd double %105, %104
  store double %106, ptr %5, align 8
  br label %107

107:                                              ; preds = %101, %96
  br label %110

108:                                              ; preds = %79
  %109 = getelementptr inbounds [1 x %struct.__va_list_tag], ptr %4, i64 0, i64 0
  call void @llvm.va_end(ptr %109)
  store double 0xFFEFFFFFFFFFFFFF, ptr %2, align 8
  br label %119

110:                                              ; preds = %107
  br label %111

111:                                              ; preds = %110, %74
  br label %112

112:                                              ; preds = %111, %51
  br label %113

113:                                              ; preds = %112
  %114 = load i32, ptr %6, align 4
  %115 = add nsw i32 %114, 1
  store i32 %115, ptr %6, align 4
  br label %14, !llvm.loop !22

116:                                              ; preds = %14
  %117 = getelementptr inbounds [1 x %struct.__va_list_tag], ptr %4, i64 0, i64 0
  call void @llvm.va_end(ptr %117)
  %118 = load double, ptr %5, align 8
  store double %118, ptr %2, align 8
  br label %119

119:                                              ; preds = %116, %108, %11
  %120 = load double, ptr %2, align 8
  ret double %120
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_make_two_ints(i32 noundef %0, i32 noundef %1) #0 {
  %3 = alloca %struct.AbiTwoInts, align 4
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  store i32 %0, ptr %4, align 4
  store i32 %1, ptr %5, align 4
  %6 = getelementptr inbounds %struct.AbiTwoInts, ptr %3, i32 0, i32 0
  %7 = load i32, ptr %4, align 4
  store i32 %7, ptr %6, align 4
  %8 = getelementptr inbounds %struct.AbiTwoInts, ptr %3, i32 0, i32 1
  %9 = load i32, ptr %5, align 4
  store i32 %9, ptr %8, align 4
  %10 = load i64, ptr %3, align 4
  ret i64 %10
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @abi_sum_two_ints(i64 %0) #0 {
  %2 = alloca %struct.AbiTwoInts, align 4
  store i64 %0, ptr %2, align 4
  %3 = getelementptr inbounds %struct.AbiTwoInts, ptr %2, i32 0, i32 0
  %4 = load i32, ptr %3, align 4
  %5 = getelementptr inbounds %struct.AbiTwoInts, ptr %2, i32 0, i32 1
  %6 = load i32, ptr %5, align 4
  %7 = add nsw i32 %4, %6
  ret i32 %7
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @abi_make_three_doubles(ptr dead_on_unwind noalias writable sret(%struct.AbiThreeDoubles) align 8 %0, double noundef %1, double noundef %2, double noundef %3) #0 {
  %5 = alloca double, align 8
  %6 = alloca double, align 8
  %7 = alloca double, align 8
  store double %1, ptr %5, align 8
  store double %2, ptr %6, align 8
  store double %3, ptr %7, align 8
  %8 = getelementptr inbounds %struct.AbiThreeDoubles, ptr %0, i32 0, i32 0
  %9 = load double, ptr %5, align 8
  store double %9, ptr %8, align 8
  %10 = getelementptr inbounds %struct.AbiThreeDoubles, ptr %0, i32 0, i32 1
  %11 = load double, ptr %6, align 8
  store double %11, ptr %10, align 8
  %12 = getelementptr inbounds %struct.AbiThreeDoubles, ptr %0, i32 0, i32 2
  %13 = load double, ptr %7, align 8
  store double %13, ptr %12, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local double @abi_sum_three_doubles(ptr noundef byval(%struct.AbiThreeDoubles) align 8 %0) #0 {
  %2 = getelementptr inbounds %struct.AbiThreeDoubles, ptr %0, i32 0, i32 0
  %3 = load double, ptr %2, align 8
  %4 = getelementptr inbounds %struct.AbiThreeDoubles, ptr %0, i32 0, i32 1
  %5 = load double, ptr %4, align 8
  %6 = fadd double %3, %5
  %7 = getelementptr inbounds %struct.AbiThreeDoubles, ptr %0, i32 0, i32 2
  %8 = load double, ptr %7, align 8
  %9 = fadd double %6, %8
  ret double %9
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_make_eight_bytes(i8 noundef zeroext %0) #0 {
  %2 = alloca %struct.AbiEightBytes, align 1
  %3 = alloca i8, align 1
  %4 = alloca i64, align 8
  store i8 %0, ptr %3, align 1
  store i64 0, ptr %4, align 8
  br label %5

5:                                                ; preds = %17, %1
  %6 = load i64, ptr %4, align 8
  %7 = icmp ult i64 %6, 8
  br i1 %7, label %8, label %20

8:                                                ; preds = %5
  %9 = load i8, ptr %3, align 1
  %10 = zext i8 %9 to i64
  %11 = load i64, ptr %4, align 8
  %12 = add i64 %10, %11
  %13 = trunc i64 %12 to i8
  %14 = getelementptr inbounds %struct.AbiEightBytes, ptr %2, i32 0, i32 0
  %15 = load i64, ptr %4, align 8
  %16 = getelementptr inbounds [8 x i8], ptr %14, i64 0, i64 %15
  store i8 %13, ptr %16, align 1
  br label %17

17:                                               ; preds = %8
  %18 = load i64, ptr %4, align 8
  %19 = add i64 %18, 1
  store i64 %19, ptr %4, align 8
  br label %5, !llvm.loop !23

20:                                               ; preds = %5
  %21 = getelementptr inbounds %struct.AbiEightBytes, ptr %2, i32 0, i32 0
  %22 = load i64, ptr %21, align 1
  ret i64 %22
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_pack_eight_bytes(i64 %0) #0 {
  %2 = alloca %struct.AbiEightBytes, align 1
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  %5 = getelementptr inbounds %struct.AbiEightBytes, ptr %2, i32 0, i32 0
  store i64 %0, ptr %5, align 1
  store i64 0, ptr %3, align 8
  store i64 0, ptr %4, align 8
  br label %6

6:                                                ; preds = %20, %1
  %7 = load i64, ptr %4, align 8
  %8 = icmp ult i64 %7, 8
  br i1 %8, label %9, label %23

9:                                                ; preds = %6
  %10 = getelementptr inbounds %struct.AbiEightBytes, ptr %2, i32 0, i32 0
  %11 = load i64, ptr %4, align 8
  %12 = getelementptr inbounds [8 x i8], ptr %10, i64 0, i64 %11
  %13 = load i8, ptr %12, align 1
  %14 = zext i8 %13 to i64
  %15 = load i64, ptr %4, align 8
  %16 = mul i64 %15, 8
  %17 = shl i64 %14, %16
  %18 = load i64, ptr %3, align 8
  %19 = or i64 %18, %17
  store i64 %19, ptr %3, align 8
  br label %20

20:                                               ; preds = %9
  %21 = load i64, ptr %4, align 8
  %22 = add i64 %21, 1
  store i64 %22, ptr %4, align 8
  br label %6, !llvm.loop !24

23:                                               ; preds = %6
  %24 = load i64, ptr %3, align 8
  ret i64 %24
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local { i32, double } @abi_make_int_double(i32 noundef %0, double noundef %1) #0 {
  %3 = alloca %struct.AbiIntDouble, align 8
  %4 = alloca i32, align 4
  %5 = alloca double, align 8
  store i32 %0, ptr %4, align 4
  store double %1, ptr %5, align 8
  %6 = getelementptr inbounds %struct.AbiIntDouble, ptr %3, i32 0, i32 0
  %7 = load i32, ptr %4, align 4
  store i32 %7, ptr %6, align 8
  %8 = getelementptr inbounds %struct.AbiIntDouble, ptr %3, i32 0, i32 1
  %9 = load double, ptr %5, align 8
  store double %9, ptr %8, align 8
  %10 = load { i32, double }, ptr %3, align 8
  ret { i32, double } %10
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local { double, i32 } @abi_make_double_int(double noundef %0, i32 noundef %1) #0 {
  %3 = alloca %struct.AbiDoubleInt, align 8
  %4 = alloca double, align 8
  %5 = alloca i32, align 4
  store double %0, ptr %4, align 8
  store i32 %1, ptr %5, align 4
  %6 = getelementptr inbounds %struct.AbiDoubleInt, ptr %3, i32 0, i32 0
  %7 = load double, ptr %4, align 8
  store double %7, ptr %6, align 8
  %8 = getelementptr inbounds %struct.AbiDoubleInt, ptr %3, i32 0, i32 1
  %9 = load i32, ptr %5, align 4
  store i32 %9, ptr %8, align 8
  %10 = load { double, i32 }, ptr %3, align 8
  ret { double, i32 } %10
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local double @abi_score_int_double(i32 %0, double %1) #0 {
  %3 = alloca %struct.AbiIntDouble, align 8
  %4 = getelementptr inbounds { i32, double }, ptr %3, i32 0, i32 0
  store i32 %0, ptr %4, align 8
  %5 = getelementptr inbounds { i32, double }, ptr %3, i32 0, i32 1
  store double %1, ptr %5, align 8
  %6 = getelementptr inbounds %struct.AbiIntDouble, ptr %3, i32 0, i32 0
  %7 = load i32, ptr %6, align 8
  %8 = sitofp i32 %7 to double
  %9 = getelementptr inbounds %struct.AbiIntDouble, ptr %3, i32 0, i32 1
  %10 = load double, ptr %9, align 8
  %11 = fadd double %8, %10
  ret double %11
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local double @abi_score_double_int(double %0, i32 %1) #0 {
  %3 = alloca %struct.AbiDoubleInt, align 8
  %4 = getelementptr inbounds { double, i32 }, ptr %3, i32 0, i32 0
  store double %0, ptr %4, align 8
  %5 = getelementptr inbounds { double, i32 }, ptr %3, i32 0, i32 1
  store i32 %1, ptr %5, align 8
  %6 = getelementptr inbounds %struct.AbiDoubleInt, ptr %3, i32 0, i32 0
  %7 = load double, ptr %6, align 8
  %8 = getelementptr inbounds %struct.AbiDoubleInt, ptr %3, i32 0, i32 1
  %9 = load i32, ptr %8, align 8
  %10 = sitofp i32 %9 to double
  %11 = fadd double %7, %10
  ret double %11
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local { <2 x float>, <2 x float> } @abi_make_four_floats(float noundef %0, float noundef %1, float noundef %2, float noundef %3) #0 {
  %5 = alloca %struct.AbiFourFloats, align 4
  %6 = alloca float, align 4
  %7 = alloca float, align 4
  %8 = alloca float, align 4
  %9 = alloca float, align 4
  store float %0, ptr %6, align 4
  store float %1, ptr %7, align 4
  store float %2, ptr %8, align 4
  store float %3, ptr %9, align 4
  %10 = getelementptr inbounds %struct.AbiFourFloats, ptr %5, i32 0, i32 0
  %11 = load float, ptr %6, align 4
  store float %11, ptr %10, align 4
  %12 = getelementptr inbounds %struct.AbiFourFloats, ptr %5, i32 0, i32 1
  %13 = load float, ptr %7, align 4
  store float %13, ptr %12, align 4
  %14 = getelementptr inbounds %struct.AbiFourFloats, ptr %5, i32 0, i32 2
  %15 = load float, ptr %8, align 4
  store float %15, ptr %14, align 4
  %16 = getelementptr inbounds %struct.AbiFourFloats, ptr %5, i32 0, i32 3
  %17 = load float, ptr %9, align 4
  store float %17, ptr %16, align 4
  %18 = load { <2 x float>, <2 x float> }, ptr %5, align 4
  ret { <2 x float>, <2 x float> } %18
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local float @abi_sum_four_floats(<2 x float> %0, <2 x float> %1) #6 {
  %3 = alloca %struct.AbiFourFloats, align 4
  %4 = getelementptr inbounds { <2 x float>, <2 x float> }, ptr %3, i32 0, i32 0
  store <2 x float> %0, ptr %4, align 4
  %5 = getelementptr inbounds { <2 x float>, <2 x float> }, ptr %3, i32 0, i32 1
  store <2 x float> %1, ptr %5, align 4
  %6 = getelementptr inbounds %struct.AbiFourFloats, ptr %3, i32 0, i32 0
  %7 = load float, ptr %6, align 4
  %8 = getelementptr inbounds %struct.AbiFourFloats, ptr %3, i32 0, i32 1
  %9 = load float, ptr %8, align 4
  %10 = fadd float %7, %9
  %11 = getelementptr inbounds %struct.AbiFourFloats, ptr %3, i32 0, i32 2
  %12 = load float, ptr %11, align 4
  %13 = fadd float %10, %12
  %14 = getelementptr inbounds %struct.AbiFourFloats, ptr %3, i32 0, i32 3
  %15 = load float, ptr %14, align 4
  %16 = fadd float %13, %15
  ret float %16
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @abi_make_five_ints(ptr dead_on_unwind noalias writable sret(%struct.AbiFiveInts) align 4 %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4, i32 noundef %5) #0 {
  %7 = alloca i32, align 4
  %8 = alloca i32, align 4
  %9 = alloca i32, align 4
  %10 = alloca i32, align 4
  %11 = alloca i32, align 4
  store i32 %1, ptr %7, align 4
  store i32 %2, ptr %8, align 4
  store i32 %3, ptr %9, align 4
  store i32 %4, ptr %10, align 4
  store i32 %5, ptr %11, align 4
  %12 = getelementptr inbounds %struct.AbiFiveInts, ptr %0, i32 0, i32 0
  %13 = load i32, ptr %7, align 4
  store i32 %13, ptr %12, align 4
  %14 = getelementptr inbounds %struct.AbiFiveInts, ptr %0, i32 0, i32 1
  %15 = load i32, ptr %8, align 4
  store i32 %15, ptr %14, align 4
  %16 = getelementptr inbounds %struct.AbiFiveInts, ptr %0, i32 0, i32 2
  %17 = load i32, ptr %9, align 4
  store i32 %17, ptr %16, align 4
  %18 = getelementptr inbounds %struct.AbiFiveInts, ptr %0, i32 0, i32 3
  %19 = load i32, ptr %10, align 4
  store i32 %19, ptr %18, align 4
  %20 = getelementptr inbounds %struct.AbiFiveInts, ptr %0, i32 0, i32 4
  %21 = load i32, ptr %11, align 4
  store i32 %21, ptr %20, align 4
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @abi_sum_five_ints(ptr noundef byval(%struct.AbiFiveInts) align 8 %0) #0 {
  %2 = getelementptr inbounds %struct.AbiFiveInts, ptr %0, i32 0, i32 0
  %3 = load i32, ptr %2, align 8
  %4 = getelementptr inbounds %struct.AbiFiveInts, ptr %0, i32 0, i32 1
  %5 = load i32, ptr %4, align 4
  %6 = add nsw i32 %3, %5
  %7 = getelementptr inbounds %struct.AbiFiveInts, ptr %0, i32 0, i32 2
  %8 = load i32, ptr %7, align 8
  %9 = add nsw i32 %6, %8
  %10 = getelementptr inbounds %struct.AbiFiveInts, ptr %0, i32 0, i32 3
  %11 = load i32, ptr %10, align 4
  %12 = add nsw i32 %9, %11
  %13 = getelementptr inbounds %struct.AbiFiveInts, ptr %0, i32 0, i32 4
  %14 = load i32, ptr %13, align 8
  %15 = add nsw i32 %12, %14
  ret i32 %15
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local { i64, i64 } @abi_make_two_u64(i64 noundef %0, i64 noundef %1) #0 {
  %3 = alloca %struct.AbiTwoU64, align 8
  %4 = alloca i64, align 8
  %5 = alloca i64, align 8
  store i64 %0, ptr %4, align 8
  store i64 %1, ptr %5, align 8
  %6 = getelementptr inbounds %struct.AbiTwoU64, ptr %3, i32 0, i32 0
  %7 = load i64, ptr %4, align 8
  store i64 %7, ptr %6, align 8
  %8 = getelementptr inbounds %struct.AbiTwoU64, ptr %3, i32 0, i32 1
  %9 = load i64, ptr %5, align 8
  store i64 %9, ptr %8, align 8
  %10 = load { i64, i64 }, ptr %3, align 8
  ret { i64, i64 } %10
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_xor_two_u64(i64 %0, i64 %1) #0 {
  %3 = alloca %struct.AbiTwoU64, align 8
  %4 = getelementptr inbounds { i64, i64 }, ptr %3, i32 0, i32 0
  store i64 %0, ptr %4, align 8
  %5 = getelementptr inbounds { i64, i64 }, ptr %3, i32 0, i32 1
  store i64 %1, ptr %5, align 8
  %6 = getelementptr inbounds %struct.AbiTwoU64, ptr %3, i32 0, i32 0
  %7 = load i64, ptr %6, align 8
  %8 = getelementptr inbounds %struct.AbiTwoU64, ptr %3, i32 0, i32 1
  %9 = load i64, ptr %8, align 8
  %10 = xor i64 %7, %9
  ret i64 %10
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @abi_make_mixed_return(ptr dead_on_unwind noalias writable sret(%struct.AbiMixedReturn) align 8 %0, i8 noundef zeroext %1, double noundef %2, i32 noundef %3, i64 noundef %4, float noundef %5) #0 {
  %7 = alloca i8, align 1
  %8 = alloca double, align 8
  %9 = alloca i32, align 4
  %10 = alloca i64, align 8
  %11 = alloca float, align 4
  store i8 %1, ptr %7, align 1
  store double %2, ptr %8, align 8
  store i32 %3, ptr %9, align 4
  store i64 %4, ptr %10, align 8
  store float %5, ptr %11, align 4
  %12 = getelementptr inbounds %struct.AbiMixedReturn, ptr %0, i32 0, i32 0
  %13 = load i8, ptr %7, align 1
  store i8 %13, ptr %12, align 8
  %14 = getelementptr inbounds %struct.AbiMixedReturn, ptr %0, i32 0, i32 1
  %15 = load double, ptr %8, align 8
  store double %15, ptr %14, align 8
  %16 = getelementptr inbounds %struct.AbiMixedReturn, ptr %0, i32 0, i32 2
  %17 = load i32, ptr %9, align 4
  store i32 %17, ptr %16, align 8
  %18 = getelementptr inbounds %struct.AbiMixedReturn, ptr %0, i32 0, i32 3
  %19 = load i64, ptr %10, align 8
  store i64 %19, ptr %18, align 8
  %20 = getelementptr inbounds %struct.AbiMixedReturn, ptr %0, i32 0, i32 4
  %21 = load float, ptr %11, align 4
  store float %21, ptr %20, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local double @abi_score_mixed_return(ptr noundef byval(%struct.AbiMixedReturn) align 8 %0) #0 {
  %2 = getelementptr inbounds %struct.AbiMixedReturn, ptr %0, i32 0, i32 0
  %3 = load i8, ptr %2, align 8
  %4 = uitofp i8 %3 to double
  %5 = getelementptr inbounds %struct.AbiMixedReturn, ptr %0, i32 0, i32 1
  %6 = load double, ptr %5, align 8
  %7 = fadd double %4, %6
  %8 = getelementptr inbounds %struct.AbiMixedReturn, ptr %0, i32 0, i32 2
  %9 = load i32, ptr %8, align 8
  %10 = sitofp i32 %9 to double
  %11 = fadd double %7, %10
  %12 = getelementptr inbounds %struct.AbiMixedReturn, ptr %0, i32 0, i32 3
  %13 = load i64, ptr %12, align 8
  %14 = uitofp i64 %13 to double
  %15 = call double @llvm.fmuladd.f64(double %14, double 1.000000e-04, double %11)
  %16 = getelementptr inbounds %struct.AbiMixedReturn, ptr %0, i32 0, i32 4
  %17 = load float, ptr %16, align 8
  %18 = fpext float %17 to double
  %19 = fadd double %15, %18
  ret double %19
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @abi_make_bitfields() #0 {
  %1 = alloca %struct.AbiBitFields, align 4
  %2 = load i32, ptr %1, align 4
  %3 = and i32 %2, -8
  %4 = or i32 %3, 5
  store i32 %4, ptr %1, align 4
  %5 = load i32, ptr %1, align 4
  %6 = and i32 %5, -249
  %7 = or i32 %6, 152
  store i32 %7, ptr %1, align 4
  %8 = load i32, ptr %1, align 4
  %9 = and i32 %8, -32513
  %10 = or i32 %9, 26880
  store i32 %10, ptr %1, align 4
  %11 = load i32, ptr %1, align 4
  %12 = and i32 %11, -16744449
  %13 = or i32 %12, 8421376
  store i32 %13, ptr %1, align 4
  %14 = load i32, ptr %1, align 4
  %15 = and i32 %14, 16777215
  %16 = or i32 %15, -1509949440
  store i32 %16, ptr %1, align 4
  %17 = getelementptr inbounds %struct.AbiBitFields, ptr %1, i32 0, i32 0
  %18 = load i32, ptr %17, align 4
  ret i32 %18
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @abi_bitfields_checksum(i32 %0) #0 {
  %2 = alloca %struct.AbiBitFields, align 4
  %3 = getelementptr inbounds %struct.AbiBitFields, ptr %2, i32 0, i32 0
  store i32 %0, ptr %3, align 4
  %4 = load i32, ptr %2, align 4
  %5 = and i32 %4, 7
  %6 = load i32, ptr %2, align 4
  %7 = lshr i32 %6, 3
  %8 = and i32 %7, 31
  %9 = mul i32 %8, 3
  %10 = add i32 %5, %9
  %11 = load i32, ptr %2, align 4
  %12 = shl i32 %11, 17
  %13 = ashr i32 %12, 25
  %14 = add nsw i32 %13, 64
  %15 = mul i32 %14, 5
  %16 = add i32 %10, %15
  %17 = load i32, ptr %2, align 4
  %18 = lshr i32 %17, 15
  %19 = and i32 %18, 511
  %20 = mul i32 %19, 7
  %21 = add i32 %16, %20
  %22 = load i32, ptr %2, align 4
  %23 = lshr i32 %22, 24
  %24 = mul i32 %23, 11
  %25 = add i32 %21, %24
  ret i32 %25
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_sizeof_bitfields() #0 {
  ret i64 4
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_alignof_bitfields() #0 {
  ret i64 4
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @abi_make_packed(ptr dead_on_unwind noalias writable sret(%struct.AbiPacked) align 1 %0) #0 {
  %2 = getelementptr inbounds %struct.AbiPacked, ptr %0, i32 0, i32 0
  store i8 -85, ptr %2, align 1
  %3 = getelementptr inbounds %struct.AbiPacked, ptr %0, i32 0, i32 1
  store i32 305419896, ptr %3, align 1
  %4 = getelementptr inbounds %struct.AbiPacked, ptr %0, i32 0, i32 2
  store i16 -25924, ptr %4, align 1
  %5 = getelementptr inbounds %struct.AbiPacked, ptr %0, i32 0, i32 3
  store double 1.235000e+02, ptr %5, align 1
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local double @abi_score_packed(ptr noundef byval(%struct.AbiPacked) align 8 %0) #0 {
  %2 = getelementptr inbounds %struct.AbiPacked, ptr %0, i32 0, i32 0
  %3 = load i8, ptr %2, align 8
  %4 = uitofp i8 %3 to double
  %5 = getelementptr inbounds %struct.AbiPacked, ptr %0, i32 0, i32 1
  %6 = load i32, ptr %5, align 1
  %7 = uitofp i32 %6 to double
  %8 = call double @llvm.fmuladd.f64(double %7, double 0x3EB0C6F7A0B5ED8D, double %4)
  %9 = getelementptr inbounds %struct.AbiPacked, ptr %0, i32 0, i32 2
  %10 = load i16, ptr %9, align 1
  %11 = uitofp i16 %10 to double
  %12 = fadd double %8, %11
  %13 = getelementptr inbounds %struct.AbiPacked, ptr %0, i32 0, i32 3
  %14 = load double, ptr %13, align 1
  %15 = fadd double %12, %14
  ret double %15
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @abi_has_packed_extension() #0 {
  ret i32 1
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_sizeof_packed() #0 {
  ret i64 15
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_alignof_packed() #0 {
  ret i64 1
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_offset_packed_b() #0 {
  ret i64 1
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_offset_packed_d() #0 {
  ret i64 7
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @abi_make_matrix(ptr dead_on_unwind noalias writable sret(%struct.AbiMatrix2x2) align 8 %0, double noundef %1, double noundef %2, double noundef %3, double noundef %4) #0 {
  %6 = alloca double, align 8
  %7 = alloca double, align 8
  %8 = alloca double, align 8
  %9 = alloca double, align 8
  store double %1, ptr %6, align 8
  store double %2, ptr %7, align 8
  store double %3, ptr %8, align 8
  store double %4, ptr %9, align 8
  %10 = getelementptr inbounds %struct.AbiMatrix2x2, ptr %0, i32 0, i32 0
  %11 = getelementptr inbounds [2 x [2 x double]], ptr %10, i64 0, i64 0
  %12 = getelementptr inbounds [2 x double], ptr %11, i64 0, i64 0
  %13 = load double, ptr %6, align 8
  store double %13, ptr %12, align 8
  %14 = getelementptr inbounds double, ptr %12, i64 1
  %15 = load double, ptr %7, align 8
  store double %15, ptr %14, align 8
  %16 = getelementptr inbounds [2 x double], ptr %11, i64 1
  %17 = getelementptr inbounds [2 x double], ptr %16, i64 0, i64 0
  %18 = load double, ptr %8, align 8
  store double %18, ptr %17, align 8
  %19 = getelementptr inbounds double, ptr %17, i64 1
  %20 = load double, ptr %9, align 8
  store double %20, ptr %19, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local double @abi_matrix_determinant(ptr noundef byval(%struct.AbiMatrix2x2) align 8 %0) #0 {
  %2 = getelementptr inbounds %struct.AbiMatrix2x2, ptr %0, i32 0, i32 0
  %3 = getelementptr inbounds [2 x [2 x double]], ptr %2, i64 0, i64 0
  %4 = getelementptr inbounds [2 x double], ptr %3, i64 0, i64 0
  %5 = load double, ptr %4, align 8
  %6 = getelementptr inbounds %struct.AbiMatrix2x2, ptr %0, i32 0, i32 0
  %7 = getelementptr inbounds [2 x [2 x double]], ptr %6, i64 0, i64 1
  %8 = getelementptr inbounds [2 x double], ptr %7, i64 0, i64 1
  %9 = load double, ptr %8, align 8
  %10 = getelementptr inbounds %struct.AbiMatrix2x2, ptr %0, i32 0, i32 0
  %11 = getelementptr inbounds [2 x [2 x double]], ptr %10, i64 0, i64 0
  %12 = getelementptr inbounds [2 x double], ptr %11, i64 0, i64 1
  %13 = load double, ptr %12, align 8
  %14 = getelementptr inbounds %struct.AbiMatrix2x2, ptr %0, i32 0, i32 0
  %15 = getelementptr inbounds [2 x [2 x double]], ptr %14, i64 0, i64 1
  %16 = getelementptr inbounds [2 x double], ptr %15, i64 0, i64 0
  %17 = load double, ptr %16, align 8
  %18 = fmul double %13, %17
  %19 = fneg double %18
  %20 = call double @llvm.fmuladd.f64(double %5, double %9, double %19)
  ret double %20
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @abi_matrix_transpose(ptr noundef %0) #0 {
  %2 = alloca ptr, align 8
  %3 = alloca double, align 8
  store ptr %0, ptr %2, align 8
  %4 = load ptr, ptr %2, align 8
  %5 = icmp ne ptr %4, null
  br i1 %5, label %7, label %6

6:                                                ; preds = %1
  br label %27

7:                                                ; preds = %1
  %8 = load ptr, ptr %2, align 8
  %9 = getelementptr inbounds %struct.AbiMatrix2x2, ptr %8, i32 0, i32 0
  %10 = getelementptr inbounds [2 x [2 x double]], ptr %9, i64 0, i64 0
  %11 = getelementptr inbounds [2 x double], ptr %10, i64 0, i64 1
  %12 = load double, ptr %11, align 8
  store double %12, ptr %3, align 8
  %13 = load ptr, ptr %2, align 8
  %14 = getelementptr inbounds %struct.AbiMatrix2x2, ptr %13, i32 0, i32 0
  %15 = getelementptr inbounds [2 x [2 x double]], ptr %14, i64 0, i64 1
  %16 = getelementptr inbounds [2 x double], ptr %15, i64 0, i64 0
  %17 = load double, ptr %16, align 8
  %18 = load ptr, ptr %2, align 8
  %19 = getelementptr inbounds %struct.AbiMatrix2x2, ptr %18, i32 0, i32 0
  %20 = getelementptr inbounds [2 x [2 x double]], ptr %19, i64 0, i64 0
  %21 = getelementptr inbounds [2 x double], ptr %20, i64 0, i64 1
  store double %17, ptr %21, align 8
  %22 = load double, ptr %3, align 8
  %23 = load ptr, ptr %2, align 8
  %24 = getelementptr inbounds %struct.AbiMatrix2x2, ptr %23, i32 0, i32 0
  %25 = getelementptr inbounds [2 x [2 x double]], ptr %24, i64 0, i64 1
  %26 = getelementptr inbounds [2 x double], ptr %25, i64 0, i64 0
  store double %22, ptr %26, align 8
  br label %27

27:                                               ; preds = %7, %6
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local ptr @abi_opaque_create(i32 noundef %0, double noundef %1) #0 {
  %3 = alloca ptr, align 8
  %4 = alloca i32, align 4
  %5 = alloca double, align 8
  %6 = alloca ptr, align 8
  store i32 %0, ptr %4, align 4
  store double %1, ptr %5, align 8
  %7 = call noalias ptr @malloc(i64 noundef 24) #10
  store ptr %7, ptr %6, align 8
  %8 = load ptr, ptr %6, align 8
  %9 = icmp ne ptr %8, null
  br i1 %9, label %11, label %10

10:                                               ; preds = %2
  store ptr null, ptr %3, align 8
  br label %21

11:                                               ; preds = %2
  %12 = load ptr, ptr %6, align 8
  %13 = getelementptr inbounds %struct.AbiOpaque, ptr %12, i32 0, i32 0
  store i64 -6117381795384022246, ptr %13, align 8
  %14 = load i32, ptr %4, align 4
  %15 = load ptr, ptr %6, align 8
  %16 = getelementptr inbounds %struct.AbiOpaque, ptr %15, i32 0, i32 1
  store i32 %14, ptr %16, align 8
  %17 = load double, ptr %5, align 8
  %18 = load ptr, ptr %6, align 8
  %19 = getelementptr inbounds %struct.AbiOpaque, ptr %18, i32 0, i32 2
  store double %17, ptr %19, align 8
  %20 = load ptr, ptr %6, align 8
  store ptr %20, ptr %3, align 8
  br label %21

21:                                               ; preds = %11, %10
  %22 = load ptr, ptr %3, align 8
  ret ptr %22
}

; Function Attrs: nounwind allocsize(0)
declare noalias ptr @malloc(i64 noundef) #7

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @abi_opaque_destroy(ptr noundef %0) #0 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  %3 = load ptr, ptr %2, align 8
  %4 = icmp ne ptr %3, null
  br i1 %4, label %6, label %5

5:                                                ; preds = %1
  br label %10

6:                                                ; preds = %1
  %7 = load ptr, ptr %2, align 8
  %8 = getelementptr inbounds %struct.AbiOpaque, ptr %7, i32 0, i32 0
  store i64 0, ptr %8, align 8
  %9 = load ptr, ptr %2, align 8
  call void @free(ptr noundef %9) #11
  br label %10

10:                                               ; preds = %6, %5
  ret void
}

; Function Attrs: nounwind
declare void @free(ptr noundef) #8

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @abi_opaque_get_value(ptr noundef %0) #0 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  %3 = load ptr, ptr %2, align 8
  %4 = icmp ne ptr %3, null
  br i1 %4, label %5, label %14

5:                                                ; preds = %1
  %6 = load ptr, ptr %2, align 8
  %7 = getelementptr inbounds %struct.AbiOpaque, ptr %6, i32 0, i32 0
  %8 = load i64, ptr %7, align 8
  %9 = icmp eq i64 %8, -6117381795384022246
  br i1 %9, label %10, label %14

10:                                               ; preds = %5
  %11 = load ptr, ptr %2, align 8
  %12 = getelementptr inbounds %struct.AbiOpaque, ptr %11, i32 0, i32 1
  %13 = load i32, ptr %12, align 8
  br label %15

14:                                               ; preds = %5, %1
  br label %15

15:                                               ; preds = %14, %10
  %16 = phi i32 [ %13, %10 ], [ -2147483648, %14 ]
  ret i32 %16
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local double @abi_opaque_get_weight(ptr noundef %0) #0 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  %3 = load ptr, ptr %2, align 8
  %4 = icmp ne ptr %3, null
  br i1 %4, label %5, label %14

5:                                                ; preds = %1
  %6 = load ptr, ptr %2, align 8
  %7 = getelementptr inbounds %struct.AbiOpaque, ptr %6, i32 0, i32 0
  %8 = load i64, ptr %7, align 8
  %9 = icmp eq i64 %8, -6117381795384022246
  br i1 %9, label %10, label %14

10:                                               ; preds = %5
  %11 = load ptr, ptr %2, align 8
  %12 = getelementptr inbounds %struct.AbiOpaque, ptr %11, i32 0, i32 2
  %13 = load double, ptr %12, align 8
  br label %15

14:                                               ; preds = %5, %1
  br label %15

15:                                               ; preds = %14, %10
  %16 = phi double [ %13, %10 ], [ 0xFFEFFFFFFFFFFFFF, %14 ]
  ret double %16
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @abi_opaque_set(ptr noundef %0, i32 noundef %1, double noundef %2) #0 {
  %4 = alloca i32, align 4
  %5 = alloca ptr, align 8
  %6 = alloca i32, align 4
  %7 = alloca double, align 8
  store ptr %0, ptr %5, align 8
  store i32 %1, ptr %6, align 4
  store double %2, ptr %7, align 8
  %8 = load ptr, ptr %5, align 8
  %9 = icmp ne ptr %8, null
  br i1 %9, label %10, label %15

10:                                               ; preds = %3
  %11 = load ptr, ptr %5, align 8
  %12 = getelementptr inbounds %struct.AbiOpaque, ptr %11, i32 0, i32 0
  %13 = load i64, ptr %12, align 8
  %14 = icmp ne i64 %13, -6117381795384022246
  br i1 %14, label %15, label %16

15:                                               ; preds = %10, %3
  store i32 -1, ptr %4, align 4
  br label %23

16:                                               ; preds = %10
  %17 = load i32, ptr %6, align 4
  %18 = load ptr, ptr %5, align 8
  %19 = getelementptr inbounds %struct.AbiOpaque, ptr %18, i32 0, i32 1
  store i32 %17, ptr %19, align 8
  %20 = load double, ptr %7, align 8
  %21 = load ptr, ptr %5, align 8
  %22 = getelementptr inbounds %struct.AbiOpaque, ptr %21, i32 0, i32 2
  store double %20, ptr %22, align 8
  store i32 0, ptr %4, align 4
  br label %23

23:                                               ; preds = %16, %15
  %24 = load i32, ptr %4, align 4
  ret i32 %24
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @abi_validate_range(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 {
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  %6 = alloca i32, align 4
  %7 = alloca i32, align 4
  store i32 %0, ptr %5, align 4
  store i32 %1, ptr %6, align 4
  store i32 %2, ptr %7, align 4
  %8 = load i32, ptr %6, align 4
  %9 = load i32, ptr %7, align 4
  %10 = icmp sgt i32 %8, %9
  br i1 %10, label %11, label %12

11:                                               ; preds = %3
  store i32 -3, ptr %4, align 4
  br label %22

12:                                               ; preds = %3
  %13 = load i32, ptr %5, align 4
  %14 = load i32, ptr %6, align 4
  %15 = icmp slt i32 %13, %14
  br i1 %15, label %20, label %16

16:                                               ; preds = %12
  %17 = load i32, ptr %5, align 4
  %18 = load i32, ptr %7, align 4
  %19 = icmp sgt i32 %17, %18
  br i1 %19, label %20, label %21

20:                                               ; preds = %16, %12
  store i32 -2, ptr %4, align 4
  br label %22

21:                                               ; preds = %16
  store i32 0, ptr %4, align 4
  br label %22

22:                                               ; preds = %21, %20, %11
  %23 = load i32, ptr %4, align 4
  ret i32 %23
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local ptr @abi_status_string(i32 noundef %0) #0 {
  %2 = alloca ptr, align 8
  %3 = alloca i32, align 4
  store i32 %0, ptr %3, align 4
  %4 = load i32, ptr %3, align 4
  switch i32 %4, label %9 [
    i32 0, label %5
    i32 -1, label %6
    i32 -2, label %7
    i32 -3, label %8
  ]

5:                                                ; preds = %1
  store ptr @.str.4, ptr %2, align 8
  br label %10

6:                                                ; preds = %1
  store ptr @.str.5, ptr %2, align 8
  br label %10

7:                                                ; preds = %1
  store ptr @.str.6, ptr %2, align 8
  br label %10

8:                                                ; preds = %1
  store ptr @.str.7, ptr %2, align 8
  br label %10

9:                                                ; preds = %1
  store ptr @.str.8, ptr %2, align 8
  br label %10

10:                                               ; preds = %9, %8, %7, %6, %5
  %11 = load ptr, ptr %2, align 8
  ret ptr %11
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @abi_read_volatile_int(ptr noundef %0) #0 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  %3 = load ptr, ptr %2, align 8
  %4 = icmp ne ptr %3, null
  br i1 %4, label %5, label %8

5:                                                ; preds = %1
  %6 = load ptr, ptr %2, align 8
  %7 = load volatile i32, ptr %6, align 4
  br label %9

8:                                                ; preds = %1
  br label %9

9:                                                ; preds = %8, %5
  %10 = phi i32 [ %7, %5 ], [ -2147483648, %8 ]
  ret i32 %10
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @abi_write_volatile_int(ptr noundef %0, i32 noundef %1) #0 {
  %3 = alloca ptr, align 8
  %4 = alloca i32, align 4
  store ptr %0, ptr %3, align 8
  store i32 %1, ptr %4, align 4
  %5 = load ptr, ptr %3, align 8
  %6 = icmp ne ptr %5, null
  br i1 %6, label %7, label %10

7:                                                ; preds = %2
  %8 = load i32, ptr %4, align 4
  %9 = load ptr, ptr %3, align 8
  store volatile i32 %8, ptr %9, align 4
  br label %10

10:                                               ; preds = %7, %2
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local ptr @abi_return_const_int_array() #0 {
  ret ptr @abi_return_const_int_array.values
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_return_const_int_array_count() #0 {
  ret i64 6
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @abi_read_const_int_array(i64 noundef %0) #0 {
  %2 = alloca i64, align 8
  %3 = alloca ptr, align 8
  store i64 %0, ptr %2, align 8
  %4 = call ptr @abi_return_const_int_array()
  store ptr %4, ptr %3, align 8
  %5 = load i64, ptr %2, align 8
  %6 = call i64 @abi_return_const_int_array_count()
  %7 = icmp ult i64 %5, %6
  br i1 %7, label %8, label %13

8:                                                ; preds = %1
  %9 = load ptr, ptr %3, align 8
  %10 = load i64, ptr %2, align 8
  %11 = getelementptr inbounds i32, ptr %9, i64 %10
  %12 = load i32, ptr %11, align 4
  br label %14

13:                                               ; preds = %1
  br label %14

14:                                               ; preds = %13, %8
  %15 = phi i32 [ %12, %8 ], [ -2147483648, %13 ]
  ret i32 %15
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local ptr @abi_compiler_family() #0 {
  ret ptr @.str.9
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local ptr @abi_platform_hint() #0 {
  ret ptr @.str.10
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @abi_integer_width_mask(i32 noundef %0) #0 {
  %2 = alloca i64, align 8
  %3 = alloca i32, align 4
  store i32 %0, ptr %3, align 4
  %4 = load i32, ptr %3, align 4
  %5 = icmp eq i32 %4, 0
  br i1 %5, label %6, label %7

6:                                                ; preds = %1
  store i64 0, ptr %2, align 8
  br label %16

7:                                                ; preds = %1
  %8 = load i32, ptr %3, align 4
  %9 = icmp uge i32 %8, 64
  br i1 %9, label %10, label %11

10:                                               ; preds = %7
  store i64 -1, ptr %2, align 8
  br label %16

11:                                               ; preds = %7
  %12 = load i32, ptr %3, align 4
  %13 = zext i32 %12 to i64
  %14 = shl i64 1, %13
  %15 = sub i64 %14, 1
  store i64 %15, ptr %2, align 8
  br label %16

16:                                               ; preds = %11, %10, %6
  %17 = load i64, ptr %2, align 8
  ret i64 %17
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @abi_compare_bytes(ptr noundef %0, ptr noundef %1, i64 noundef %2) #0 {
  %4 = alloca i32, align 4
  %5 = alloca ptr, align 8
  %6 = alloca ptr, align 8
  %7 = alloca i64, align 8
  store ptr %0, ptr %5, align 8
  store ptr %1, ptr %6, align 8
  store i64 %2, ptr %7, align 8
  %8 = load ptr, ptr %5, align 8
  %9 = icmp ne ptr %8, null
  br i1 %9, label %10, label %13

10:                                               ; preds = %3
  %11 = load ptr, ptr %6, align 8
  %12 = icmp ne ptr %11, null
  br i1 %12, label %17, label %13

13:                                               ; preds = %10, %3
  %14 = load i64, ptr %7, align 8
  %15 = icmp ne i64 %14, 0
  br i1 %15, label %16, label %17

16:                                               ; preds = %13
  store i32 -2147483648, ptr %4, align 4
  br label %22

17:                                               ; preds = %13, %10
  %18 = load ptr, ptr %5, align 8
  %19 = load ptr, ptr %6, align 8
  %20 = load i64, ptr %7, align 8
  %21 = call i32 @memcmp(ptr noundef %18, ptr noundef %19, i64 noundef %20) #9
  store i32 %21, ptr %4, align 4
  br label %22

22:                                               ; preds = %17, %16
  %23 = load i32, ptr %4, align 4
  ret i32 %23
}

; Function Attrs: nounwind willreturn memory(read)
declare i32 @memcmp(ptr noundef, ptr noundef, i64 noundef) #4

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @abi_memset_pattern(ptr noundef %0, i64 noundef %1, i8 noundef zeroext %2) #0 {
  %4 = alloca ptr, align 8
  %5 = alloca i64, align 8
  %6 = alloca i8, align 1
  store ptr %0, ptr %4, align 8
  store i64 %1, ptr %5, align 8
  store i8 %2, ptr %6, align 1
  %7 = load ptr, ptr %4, align 8
  %8 = icmp ne ptr %7, null
  br i1 %8, label %9, label %15

9:                                                ; preds = %3
  %10 = load ptr, ptr %4, align 8
  %11 = load i8, ptr %6, align 1
  %12 = zext i8 %11 to i32
  %13 = trunc i32 %12 to i8
  %14 = load i64, ptr %5, align 8
  call void @llvm.memset.p0.i64(ptr align 1 %10, i8 %13, i64 %14, i1 false)
  br label %15

15:                                               ; preds = %9, %3
  ret void
}

attributes #0 = { noinline nounwind optnone uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }
attributes #2 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }
attributes #3 = { nocallback nofree nounwind willreturn memory(argmem: write) }
attributes #4 = { nounwind willreturn memory(read) "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #5 = { nocallback nofree nosync nounwind willreturn }
attributes #6 = { noinline nounwind optnone uwtable "frame-pointer"="all" "min-legal-vector-width"="64" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #7 = { nounwind allocsize(0) "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #8 = { nounwind "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #9 = { nounwind willreturn memory(read) }
attributes #10 = { nounwind allocsize(0) }
attributes #11 = { nounwind }

!llvm.module.flags = !{!0, !1, !2, !3, !4}
!llvm.ident = !{!5}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 8, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{i32 7, !"uwtable", i32 2}
!4 = !{i32 7, !"frame-pointer", i32 2}
!5 = !{!"Ubuntu clang version 18.1.3 (1ubuntu1)"}
!6 = distinct !{!6, !7}
!7 = !{!"llvm.loop.mustprogress"}
!8 = distinct !{!8, !7}
!9 = distinct !{!9, !7}
!10 = distinct !{!10, !7}
!11 = distinct !{!11, !7}
!12 = distinct !{!12, !7}
!13 = distinct !{!13, !7}
!14 = distinct !{!14, !7}
!15 = distinct !{!15, !7}
!16 = distinct !{!16, !7}
!17 = distinct !{!17, !7}
!18 = distinct !{!18, !7}
!19 = distinct !{!19, !7}
!20 = distinct !{!20, !7}
!21 = distinct !{!21, !7}
!22 = distinct !{!22, !7}
!23 = distinct !{!23, !7}
!24 = distinct !{!24, !7}
