namespace Flux.Collections.Benchmark.Keys

open System

type CustomClassStr(value: string) =
    member _.Value = value

    interface IComparable<CustomClassStr> with

        member this.CompareTo other = value.CompareTo other.Value

    interface IComparable with

        member this.CompareTo other =
            match other with
            | null -> 1
            | :? CustomClassStr as otherCS -> value.CompareTo otherCS.Value
            | _ -> raise (ArgumentException $"The %s{nameof other} argument has to be a %s{this.GetType().Name}")

    override this.Equals other =
        match other with
        | :? CustomClassStr as other -> this.Value.Equals other.Value
        | _ -> false

    override _.GetHashCode() = value.GetHashCode()

    override _.ToString() = $"CustomClassStr(%s{value})"

[<Struct>]
[<CustomComparison>]
[<CustomEquality>]
type CustomStructStr(value: string) =
    member _.Value = value

    interface IComparable<CustomClassStr> with

        member this.CompareTo other = value.CompareTo other.Value

    interface IComparable with

        member this.CompareTo other =
            match other with
            | null -> 1
            | :? CustomStructStr as otherCS -> value.CompareTo otherCS.Value
            | _ -> raise (ArgumentException $"The %s{nameof other} argument has to be a %s{this.GetType().Name}")

    override this.Equals other =
        match other with
        | :? CustomStructStr as other -> this.Value.Equals other.Value
        | _ -> false

    override _.GetHashCode() = value.GetHashCode()

    override _.ToString() = $"CustomStructStr(%s{value})"
