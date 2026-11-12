package main


TcContext :: struct {
    in_function: bool,
    fn_ret_ty: Maybe(TypeId),
    in_loop: Maybe(StmtId),
}