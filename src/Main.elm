module Main exposing (main)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram)


main : BenchmarkProgram
main =
    Benchmark.Runner.program suite


fibTCO : Int -> Int
fibTCO =
    let
        fib a b n =
            if n <= 0 then
                a

            else
                fib (a + b) a (n - 1)
    in
    fib 1 0


suite : Benchmark
suite =
    describe "tail call optimization"
        [ benchmark "range 1 10000 |> foldr (+) 0" <|
            \_ ->
                List.range 1 10000
                    |> List.foldr (+) 0
        , benchmark "range 1 10000 |> foldl (+) 0" <|
            \_ ->
                List.range 1 10000
                    |> List.foldl (+) 0
        , benchmark "fibTCO 100" <| \_ -> fibTCO 100
        ]
