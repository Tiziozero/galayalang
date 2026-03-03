package main

import "core:fmt";

main ::proc() {
    x := proc() {} + 1;
    fmt.printfln("c %zu", x);
    fmt.printfln("c %zu", x);
}
