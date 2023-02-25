module Flux.Bit

[<Measure>] type b
[<Measure>] type bit = b

[<AutoOpen>]
module Operators =

    let inline asBits value = LanguagePrimitives.Int32WithMeasure<bit> (int value)

    let inline lshift value (bits: int<bit>) = value <<< (int bits)

    let inline rshift value (bits: int<bit>) = value >>> (int bits)

let countBitsOn32 value =
    let a = value - ((rshift value 1<bit>) &&& 0x55555555u)
    let b = (a &&& 0x33333333u) + ((rshift a 2<bit>) &&& 0x33333333u)
    asBits (rshift (((b + (rshift b 4<bit>)) &&& 0x0F0F0F0Fu) * 0x01010101u) 24<bit>)

let countBitsOn64 (value: uint64) =
    let a = value - ((rshift value 1<bit>) &&& 0x5555555555555555UL)
    let b = (a &&& 0x3333333333333333UL) + ((rshift a 2<bit>) &&& 0x3333333333333333UL)
    let c = (b + (rshift b 4<bit>)) &&& 0x0F0F0F0F0F0F0F0FUL
    asBits (rshift (c * 0x0101010101010101UL) 56<bit>)