module Tests exposing (..)

import Test.Runner.Html as Runner
import UnitTests
import IntegrationTests
import Test exposing (concat)


main : Runner.TestProgram
main =
    UnitTests.all
        |> List.append IntegrationTests.all
        |> concat
        |> Runner.run
