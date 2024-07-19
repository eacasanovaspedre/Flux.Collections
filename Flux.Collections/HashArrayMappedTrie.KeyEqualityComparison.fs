[<CompiledName("InternalUse")>]
module internal Flux.Collections.KeyEqualityComparison

open System
open System.Collections.Generic
open System.Reflection

type ComparerHolder =
    static member NonStructuralEqualityComparerForIEquatable<'T when 'T: equality and 'T :> IEquatable<'T>>() =
        { new IEqualityComparer<'T> with
            member _.GetHashCode x = NonStructuralComparison.hash x

            member _.Equals(x, y) =
                match box x, box y with
                | null, null -> true
                | _, null
                | null, _ -> false
                | _ -> x.Equals y (* IEquatable Equals*) }

let nonStructuralEqualityComparerForIEquatableMethodInfo =
    lazy
        typeof<ComparerHolder>
            .GetMethod ("NonStructuralEqualityComparerForIEquatable", BindingFlags.Static ||| BindingFlags.Public)

let inline nonStructuralEqualityComparer<'T when 'T: equality> =
    { new IEqualityComparer<'T> with
        member _.GetHashCode x = NonStructuralComparison.hash x

        member _.Equals(x, y) =
            match box x, box y with
            | null, null -> true
            | _, null
            | null, _ -> false
            | _ -> x.Equals y (* Object Equals*) }

module EqualityComparers =
    let int32_ = HashIdentity.NonStructural<int32>
    let string_ = HashIdentity.NonStructural<string>
    let Guid_ = HashIdentity.NonStructural<Guid>
    let uint32_ = HashIdentity.NonStructural<uint32>
    let int64_ = HashIdentity.NonStructural<int64>
    let uint64_ = HashIdentity.NonStructural<uint64>
    let bool_ = HashIdentity.NonStructural<bool>
    let byte_ = HashIdentity.NonStructural<byte>
    let char_ = HashIdentity.NonStructural<char>
    let sbyte_ = HashIdentity.NonStructural<sbyte>
    let int16_ = HashIdentity.NonStructural<int16>
    let nativeint_ = HashIdentity.NonStructural<nativeint>
    let unativeint_ = HashIdentity.NonStructural<unativeint>
    let uint16_ = HashIdentity.NonStructural<uint16>
    let float_ = HashIdentity.NonStructural<float>
    let float32_ = HashIdentity.NonStructural<float32>
    let decimal_ = HashIdentity.NonStructural<decimal>

let selectStructuralEqualityComparer<'T when 'T: equality> =
    match typeof<'T> with
    //Primitive types  is not structural and is a known type relatively common as a key in a Dictionary it makes no sense
    //to try to create an structural equality comparer
    | ty when typeof<int32>.Equals ty -> EqualityComparers.int32_ :?> IEqualityComparer<'T>
    | ty when typeof<string>.Equals ty -> EqualityComparers.string_ :?> IEqualityComparer<'T>
    | ty when typeof<Guid>.Equals ty -> EqualityComparers.Guid_ :?> IEqualityComparer<'T>
    | ty when typeof<uint32>.Equals ty -> EqualityComparers.uint32_ :?> IEqualityComparer<'T>
    | ty when typeof<int64>.Equals ty -> EqualityComparers.int64_ :?> IEqualityComparer<'T>
    | ty when typeof<uint64>.Equals ty -> EqualityComparers.uint64_ :?> IEqualityComparer<'T>
    | ty when typeof<bool>.Equals ty -> EqualityComparers.bool_ :?> IEqualityComparer<'T>
    | ty when typeof<byte>.Equals ty -> EqualityComparers.byte_ :?> IEqualityComparer<'T>
    | ty when typeof<char>.Equals ty -> EqualityComparers.char_ :?> IEqualityComparer<'T>
    | ty when typeof<sbyte>.Equals ty -> EqualityComparers.sbyte_ :?> IEqualityComparer<'T>
    | ty when typeof<int16>.Equals ty -> EqualityComparers.int16_ :?> IEqualityComparer<'T>
    | ty when typeof<nativeint>.Equals ty -> EqualityComparers.nativeint_ :?> IEqualityComparer<'T>
    | ty when typeof<unativeint>.Equals ty -> EqualityComparers.unativeint_ :?> IEqualityComparer<'T>
    | ty when typeof<uint16>.Equals ty -> EqualityComparers.uint16_ :?> IEqualityComparer<'T>
    | ty when typeof<float>.Equals ty -> EqualityComparers.float_ :?> IEqualityComparer<'T>
    | ty when typeof<float32>.Equals ty -> EqualityComparers.float32_ :?> IEqualityComparer<'T>
    | ty when typeof<decimal>.Equals ty -> EqualityComparers.decimal_ :?> IEqualityComparer<'T>
    | _ -> HashIdentity.Structural<'T>

let selectNonStructuralEqualityComparer<'T when 'T: equality> : IEqualityComparer<'T> =
    match typeof<'T> with
    | ty when typeof<int32>.Equals ty -> EqualityComparers.int32_ :?> IEqualityComparer<'T>
    | ty when typeof<string>.Equals ty -> EqualityComparers.string_ :?> IEqualityComparer<'T>
    | ty when typeof<Guid>.Equals ty -> EqualityComparers.Guid_ :?> IEqualityComparer<'T>
    | ty when typeof<uint32>.Equals ty -> EqualityComparers.uint32_ :?> IEqualityComparer<'T>
    | ty when typeof<int64>.Equals ty -> EqualityComparers.int64_ :?> IEqualityComparer<'T>
    | ty when typeof<uint64>.Equals ty -> EqualityComparers.uint64_ :?> IEqualityComparer<'T>
    | ty when typeof<bool>.Equals ty -> EqualityComparers.bool_ :?> IEqualityComparer<'T>
    | ty when typeof<byte>.Equals ty -> EqualityComparers.byte_ :?> IEqualityComparer<'T>
    | ty when typeof<char>.Equals ty -> EqualityComparers.char_ :?> IEqualityComparer<'T>
    | ty when typeof<sbyte>.Equals ty -> EqualityComparers.sbyte_ :?> IEqualityComparer<'T>
    | ty when typeof<int16>.Equals ty -> EqualityComparers.int16_ :?> IEqualityComparer<'T>
    | ty when typeof<nativeint>.Equals ty -> EqualityComparers.nativeint_ :?> IEqualityComparer<'T>
    | ty when typeof<unativeint>.Equals ty -> EqualityComparers.unativeint_ :?> IEqualityComparer<'T>
    | ty when typeof<uint16>.Equals ty -> EqualityComparers.uint16_ :?> IEqualityComparer<'T>
    | ty when typeof<float>.Equals ty -> EqualityComparers.float_ :?> IEqualityComparer<'T>
    | ty when typeof<float32>.Equals ty -> EqualityComparers.float32_ :?> IEqualityComparer<'T>
    | ty when typeof<decimal>.Equals ty -> EqualityComparers.decimal_ :?> IEqualityComparer<'T>
    //For IEquatable types we create a IEqualityComparer that uses the interface Equals
    | ty when typeof<IEquatable<'T>>.IsAssignableFrom ty ->
        let method = nonStructuralEqualityComparerForIEquatableMethodInfo.Value.MakeGenericMethod typeof<'T>

        method.Invoke (null, [||]) :?> IEqualityComparer<'T>
    | _ -> nonStructuralEqualityComparer<'T> //HashIdentity.NonStructural<'T> would force static member (=) constraint on 'T
