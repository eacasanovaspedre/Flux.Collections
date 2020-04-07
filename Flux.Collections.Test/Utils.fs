module Flux.TestUtils

open FsCheck
open Expecto.ExpectoFsCheck
open Expecto
open Flux.Collections

let inline fail strf = failtestf strf
let pass = ()

let failProp = false
let passProp = true

let throws<'T, 'A when 'T :> exn> (v: 'A Lazy) =
    try
        do v.Force() |> ignore
        false
    with
    | :? 'T -> true
    | _     -> false

[<Literal>]
let TruncInfinite = 100

let infiniteSequenceEqualWithLength maxLength seq1 seq2 =
    List.ofSeq (Seq.truncate maxLength seq1) = List.ofSeq (Seq.truncate maxLength seq2)

let infiniteSequenceEqual seq1 seq2 =
    infiniteSequenceEqualWithLength TruncInfinite seq1 seq2

type 'T FiniteLazyLList = Finite of 'T Stream
type 'T InfiniteLList = Infinite of 'T Stream
type 'T NonEmptyLList = NonEmpty of 'T Stream
type 'T SizedLList = Sized of 'T Stream * int

let finite = function | Finite a -> a
let infinite = function | Infinite a -> a

type StreamGenerators =
    static member FiniteList<'T>() =
        { new Arbitrary<'T FiniteLazyLList>() with
            override __.Generator = 
                gen{
                    let! size = Arb.Default.UInt32().Generator
                    let! o = Arb.Default.Arrow().Generator
                    return Finite (Stream.init (int32 size) o) }}

    static member InfiniteList<'T>() =
        { new Arbitrary<'T InfiniteLList>() with
            override __.Generator = 
                gen{
                    let! o = Arb.Default.Arrow().Generator
                    let f index =
                        if  index = 10 * TruncInfinite 
                            then failwith "This list is supposed to be infinite but it was destroyed here to avoid non termination"
                            else o index
                    return Infinite (Stream.initInfinite f) }}

    static member NonEmptyList<'T>() =
        { new Arbitrary<'T NonEmptyLList>() with
            override __.Generator =
                gen{
                    let! size = Arb.Default.UInt32().Generator
                    let! o = Arb.Default.Arrow().Generator
                    return NonEmpty (Stream.init (max (int32 size) 1) o) }}

    static member Any<'T>() =
        { new Arbitrary<'T Stream>() with
            override __.Generator = 
                gen{
                    let! finite = Arb.Default.Bool().Generator
                    let ret =
                        if  finite then
                            gen {
                                let! list = StreamGenerators.FiniteList().Generator
                                return match list with
                                       | Finite finite -> finite}
                        else
                            gen {
                                let! list = StreamGenerators.InfiniteList().Generator
                                return match list with
                                       | Infinite potentiallyInfite -> potentiallyInfite}
                    return! ret}}

    static member SizedList<'T>() =
        { new Arbitrary<'T SizedLList>() with
            override __.Generator = 
                gen{
                    let! size = Arb.Default.UInt32().Generator
                    let! o = Arb.Default.Arrow().Generator
                    return Sized (Stream.init (int32 size) o, int32 size) }}

let testPropertyWithConfig c n t =
    testPropertyWithConfig { 
        c with 
            arbitrary = 
                [ typeof<StreamGenerators>] } n t

let testProperty n t = testPropertyWithConfig FsCheckConfig.defaultConfig n t