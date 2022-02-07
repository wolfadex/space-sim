module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.
To add packages that contain rules, add them to this review project using
    `elm install author/packagename`
when inside the directory containing this file.
-}

import NoDebug.Log
import NoDebug.TodoOrToString
import NoDeprecated
import NoExposingEverything
import NoFunctionOutsideOfModules
import NoImportingEverything
import NoInconsistentAliases
import NoMissingTypeAnnotation
import NoMissingTypeAnnotationInLetIn
import NoMissingTypeExpose
import NoModuleOnExposedNames
import NoPrematureLetComputation
import NoSimpleLetBody
import NoSinglePatternCase
import NoUnoptimizedRecursion
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import Review.Rule exposing (Rule)
import ReviewPipelineStyles
import ReviewPipelineStyles.Fixes
import Simplify


config : List Rule
config =
    [ NoDebug.Log.rule
    , NoDebug.TodoOrToString.rule
    , NoDeprecated.rule NoDeprecated.defaults
    , NoExposingEverything.rule
        |> Review.Rule.ignoreErrorsForDirectories [ "tests" ]
    , NoFunctionOutsideOfModules.rule
        [ ( [ "Element.Input.text" ], [ "Ui.Text" ] )
        , ( [ "Element.Input.button" ], [ "Ui.Button" ] )
        ]
    , NoImportingEverything.rule [ "Element" ]
    , NoInconsistentAliases.config
        [ ( "Element.Input", "Input" )
        , ( "Element.Font", "Font" )
        , ( "Element.Background", "Background" )
        , ( "Element.Border", "Border" )
        , ( "Element.Region", "Region" )
        , ( "Element.Keyed", "Keyed" )
        ]
        |> NoInconsistentAliases.noMissingAliases
        |> NoInconsistentAliases.rule
    , NoMissingTypeAnnotation.rule
    , NoMissingTypeAnnotationInLetIn.rule
    , NoMissingTypeExpose.rule
    , NoModuleOnExposedNames.rule
        |> Review.Rule.ignoreErrorsForFiles [ "src/View.elm" ]
    , NoPrematureLetComputation.rule
    , NoSimpleLetBody.rule
    , NoSinglePatternCase.rule NoSinglePatternCase.fixInArgument
    , NoUnoptimizedRecursion.rule (NoUnoptimizedRecursion.optOutWithComment "IGNORE TCO")
    , NoUnused.CustomTypeConstructorArgs.rule
    , NoUnused.CustomTypeConstructors.rule []
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
    , NoUnused.Modules.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    , ReviewPipelineStyles.rule
        [ ReviewPipelineStyles.forbid ReviewPipelineStyles.rightPizzaPipelines
            |> ReviewPipelineStyles.andTryToFixThemBy ReviewPipelineStyles.Fixes.convertingToParentheticalApplication
            |> ReviewPipelineStyles.andCallThem "Non-performant pipe"
        , ReviewPipelineStyles.forbid ReviewPipelineStyles.leftPizzaPipelines
            |> ReviewPipelineStyles.andTryToFixThemBy ReviewPipelineStyles.Fixes.convertingToParentheticalApplication
            |> ReviewPipelineStyles.andCallThem "Non-performant reverse-pipe"
        ]
    , Simplify.rule Simplify.defaults
    ]