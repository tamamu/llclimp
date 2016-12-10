; ModuleID = 'llclimp'
source_filename = "llclimp"

%cell = type { i64, i64 }

@.int = global i32 24
@.float = global float 0x404BCCCCC0000000

define i64 @cons(i64, i64) {
entry:
  %cell = alloca %cell
  %car = getelementptr inbounds %cell, %cell* %cell, i32 0, i32 0
  store i64 %0, i64* %car
  %cdr = getelementptr inbounds %cell, %cell* %cell, i32 0, i32 1
  store i64 %1, i64* %cdr
  %addr = ptrtoint %cell* %cell to i64
  ret i64 %addr
}

define i64 @car(i64) {
entry:
  %consptr = inttoptr i64 %0 to %cell*
  %cons = getelementptr inbounds %cell, %cell* %consptr, i32 0, i32 0
  %car = load i64, i64* %cons
  ret i64 %car
}

define i32 @retint(i64) {
entry:
  %vptr = inttoptr i64 %0 to i32*
  %v = load i32, i32* %vptr
  ret i32 %v
}

define i32 @main() {
entry:
  %calltmp = call i64 @cons(i64 ptrtoint (i32* @.int to i64), i64 ptrtoint (float* @.float to i64))
  %calltmp1 = call i64 @car(i64 %calltmp)
  %calltmp2 = call i32 @retint(i64 %calltmp1)
	ret i32 %calltmp2
}

