namespace Flux

open Bit

[<Struct>]
type Bitmap<'T> =
    | Bitmap of 'T
    static member inline (<<<) ((Bitmap value), offset) = Bitmap(value <<< offset)
    static member inline (>>>) ((Bitmap value), offset) = Bitmap(value >>> offset)
    static member inline (&&&) (Bitmap left, Bitmap right) = Bitmap(left &&& right)
    static member inline (|||) (Bitmap left, Bitmap right) = Bitmap(left ||| right)
    static member inline (~~~) (Bitmap single) = Bitmap(~~~single)

module Bitmap =

    let inline private (-) ((Bitmap minuend): Bitmap<'T>) ((Bitmap subtrahend): Bitmap<'T>): Bitmap<'T> =
        Bitmap(minuend - subtrahend)

    let inline underlying< ^T, ^A when ^T: (static member Underlying: 'T -> 'A)> (v: 'T) =
        (^T: (static member Underlying: 'T -> 'A) (v))

    let inline private unequal ((Bitmap a): Bitmap<'T>) ((Bitmap b): Bitmap<'T>) = underlying a <> underlying b

    let inline noBit< ^T when ^T: (static member NoBit: unit -> ^T)>() =
        Bitmap(^T: (static member NoBit: unit -> 'T) ())

    let inline firstBit< ^T when ^T: (static member FirstBit: unit -> ^T)>() =
        Bitmap(^T: (static member FirstBit: unit -> 'T) ())

    let inline allBits< ^T when ^T: (static member FirstBit: unit -> ^T)>() =
        Bitmap(^T: (static member FirstBit: unit -> 'T) ())

    let inline bitIndexBits< ^T when ^T: (static member BitIndexBits: unit -> int<bit>)>() =
        (^T: (static member BitIndexBits: unit -> int<bit>) ())

    let inline bitIndexMask< ^T when ^T: (static member BitIndexMask: unit -> uint32)>() =
        (^T: (static member BitIndexMask: unit -> uint32) ())

    let inline countBitsOn< ^T when ^T: (static member CountBitsOn: 'T -> int<bit>)> (Bitmap actualBits) =
        (^T: (static member CountBitsOn: 'T -> int<bit>) actualBits)

    let inline bit index = lshift (firstBit()) index

    let inline isBitOn index bitmap = unequal (bit index &&& bitmap) (noBit())

    let inline bitsLowerThan index bitmap = (bit index - firstBit()) &&& bitmap

    let inline setBit index bitmap = bitmap ||| (bit index)

    let inline clearBit index bitmap = bitmap &&& ~~~(bit index)


[<Struct>]
[<NoEquality>]
[<NoComparison>]
type Bitmap32 =
    | Bitmap32 of uint32

    ///All bits off.
    static member inline NoBit() = Bitmap32(0u)

    ///Only the first (least significant) bit on.
    static member inline FirstBit() = Bitmap32(1u)
    
    ///Gives the count of bits that are on.
    static member inline CountBitsOn(Bitmap32 v) = countBitsOn32 v

    ///A bit mask that represent the necessary bits to get an index to any bit in this type.
    ///0x1Fu is (00000000_00000000_00000000_00011111) which is the five bits required to index any bit in a 32 bit bitmap.
    static member inline BitIndexMask() = 0x1Fu

    ///The number of bits needed to represent the index of a bit in this type.
    static member inline BitIndexBits() = 5<bit>

    ///Returns the underlying type used to store this bitmap
    static member inline Underlying(Bitmap32 v) = v

    static member inline (-) (Bitmap32 minuend, Bitmap32 subtrahend) = Bitmap32(minuend - subtrahend)
    static member inline (<<<) ((Bitmap32 value), offset) = Bitmap32(value <<< offset)
    static member inline (>>>) ((Bitmap32 value), offset) = Bitmap32(value >>> offset)
    static member inline (&&&) (Bitmap32 left, Bitmap32 right) = Bitmap32(left &&& right)
    static member inline (|||) (Bitmap32 left, Bitmap32 right) = Bitmap32(left ||| right)
    static member inline (~~~) (Bitmap32 single) = Bitmap32(~~~single)

[<Struct>]
[<NoEquality>]
[<NoComparison>]
type Bitmap64 =
    | Bitmap64 of uint64
    static member inline NoBit() = Bitmap64(0UL)
    static member inline FirstBit() = Bitmap64(1UL)
    static member inline CountBitsOn(Bitmap64 v) = countBitsOn64 v
    static member inline BitIndexMask() = 0x3Fu
    static member inline BitIndexBits() = 6<bit>
    static member inline Underlying(Bitmap64 v) = v
    static member inline (-) (Bitmap64 minuend, Bitmap64 subtrahend) = Bitmap64(minuend - subtrahend)
    static member inline (<<<) ((Bitmap64 value), offset) = Bitmap64(value <<< offset)
    static member inline (>>>) ((Bitmap64 value), offset) = Bitmap64(value >>> offset)
    static member inline (&&&) (Bitmap64 left, Bitmap64 right) = Bitmap64(left &&& right)
    static member inline (|||) (Bitmap64 left, Bitmap64 right) = Bitmap64(left ||| right)
    static member inline (~~~) (Bitmap64 single) = Bitmap64(~~~single)
