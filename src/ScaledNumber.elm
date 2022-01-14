module ScaledNumber exposing
    ( ScaledNumber
    , millions
    , scaleBy
    , sum
    , toString
    )


type ScaledNumber
    = Millions Float
    | Billions Float
    | Trillions Float


millions : Float -> ScaledNumber
millions =
    Millions


toString : ScaledNumber -> String
toString size =
    case size of
        Millions f ->
            String.fromFloat f ++ " million"

        Billions f ->
            String.fromFloat f ++ " billion"

        Trillions f ->
            String.fromFloat f ++ " trillion"


scaleBy : Float -> ScaledNumber -> ScaledNumber
scaleBy n scaled =
    reScale <|
        case scaled of
            Millions f ->
                Millions (n * f)

            Billions f ->
                Billions (n * f)

            Trillions f ->
                Trillions (n * f)


sum : ScaledNumber -> ScaledNumber -> ScaledNumber
sum numA numB =
    reScale <|
        case ( numA, numB ) of
            ( Millions a, Millions b ) ->
                Millions (a + b)

            ( Millions a, Billions b ) ->
                Billions (a / 1000 + b)

            ( Millions a, Trillions b ) ->
                Billions (a / 1000000 + b)

            ( Billions a, Billions b ) ->
                Billions (a + b)

            ( Billions a, Millions b ) ->
                Billions (a + b / 1000)

            ( Billions a, Trillions b ) ->
                Trillions (a / 1000 + b)

            ( Trillions a, Trillions b ) ->
                Trillions (a + b)

            ( Trillions a, Billions b ) ->
                Trillions (a + b / 1000)

            ( Trillions a, Millions b ) ->
                Trillions (a + b / 1000000)


reScale : ScaledNumber -> ScaledNumber
reScale num =
    let
        sizeOfWHoleNum : Float -> Int
        sizeOfWHoleNum f =
            String.length (String.fromInt (floor f))
    in
    case num of
        Millions f ->
            if sizeOfWHoleNum f > 10 then
                reScale (Billions (f / 1000))

            else
                Millions f

        Billions f ->
            if sizeOfWHoleNum f < 10 then
                Millions (f * 1000)

            else if sizeOfWHoleNum f > 10 then
                Trillions (f / 1000)

            else
                Billions f

        Trillions f ->
            if sizeOfWHoleNum f < 10 then
                reScale (Billions (f * 1000))

            else
                Trillions f
