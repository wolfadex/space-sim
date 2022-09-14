module Data.Name exposing
    ( Name
    , allNames
    , enhancedEventDescription
    , fromString
    , plurualize
    , possessive
    , randomPerson
    , randomPersonSource
    , toString
    )

import Markov
import Markov.String
import Random exposing (Generator)


type Name
    = Name String


toString : Name -> String
toString (Name n) =
    n


fromString : String -> Name
fromString =
    Name


randomPerson : Markov.String.MarkovString -> Generator Name
randomPerson nameSource =
    Random.map (\chars -> Name (String.dropRight 1 (String.fromList chars)))
        (Markov.generateSequence
            Markov.String.comparableConfig
            { maxLength = 15 }
            nameSource
        )


randomPersonSource : Generator Markov.String.MarkovString
randomPersonSource =
    Random.map
        (\nameSource -> Markov.String.trainList nameSource Markov.empty)
        (Random.uniform afghanNames
            [ algonquianNames
            , klingonNames
            , koreanNames
            ]
        )


possessive : Name -> Name
possessive (Name n) =
    Name
        (if String.endsWith "s" n then
            n ++ "'"

         else
            n ++ "'s"
        )


plurualize : Name -> Name
plurualize (Name n) =
    Name
        (if String.endsWith "s" n then
            n ++ "es"

         else
            n ++ "s"
        )


enhancedEventDescription : Name -> (Name -> String)
enhancedEventDescription (Name civName) =
    case civName of
        "Morlock" ->
            \(Name personName) ->
                "High Archbrain " ++ personName ++ " of the Morlocks"

        "Gorn" ->
            \_ ->
                "Gorn of the Gorn"

        "Borg" ->
            \_ ->
                "The Collective"

        "Empire" ->
            \(Name personName) -> personName ++ " of the Empire"

        "Federation" ->
            \(Name personName) -> "Science officer " ++ personName ++ " of the Federation"

        "Klingon" ->
            \(Name personName) -> personName ++ " of the Klingon Empire"

        "Talonite" ->
            \(Name personName) -> "Lead " ++ personName ++ " of the United Talonite"

        "Sha' Tao" ->
            \(Name personName) -> "Most Respected " ++ personName ++ " of the Ancient Sha' Tao"

        _ ->
            toString


allNames : List Name
allNames =
    [ Name "Morlock"
    , Name "Klingon"
    , Name "Federation"
    , Name "Borg"
    , Name "Empire"
    , Name "Gorn"
    , Name "Talonite"
    , Name "Sha' Tao"
    ]



---- INTERNAL ----


{-| From <https://en.wikipedia.org/wiki/List_of_Korean_given_names> column 1 in tables 6-10.
-}
koreanNames : List String
koreanNames =
    [ "Dal-rae"
    , "Ji"
    , "Gi-ppeum"
    , "Bi"
    , "Seong"
    , "Han-saem"
    , "Sa-rang"
    , "Ba-ram"
    , "Ra-on"
    , "Il-sung"
    , "Jae-shin"
    , "Woo-ram"
    , "Na-rae"
    , "Min-seo"
    , "Seul-ki"
    , "Gun"
    , "Ah-rong"
    , "Sang"
    , "Gyeo-wool"
    , "Ae-jung"
    , "Yi-seul"
    , "Go-eun"
    , "Him-chan"
    , "Ru-da"
    , "Kyung-lim"
    , "Woong"
    , "Bo-da"
    , "Jae-in"
    , "Ah-ram"
    , "Sae-byeok"
    , "Young-shin"
    , "Gu-seul"
    , "Hoon"
    , "Bitgaram"
    , "Sandara"
    , "Dong-gun"
    , "Young-ae"
    , "Deok-su"
    , "Byung-hun"
    , "Hyun-seung"
    , "Da-som"
    , "Byeol-bit"
    , "Eun-chae"
    , "Hye-rin"
    , "Ba-da"
    , "Seong-han"
    , "Eun"
    , "Saem-na"
    , "Seol"
    , "Bo-reum"
    , "Kang-min"
    , "Ra-woom"
    , "Ho"
    , "Byeol"
    , "Eun-saem"
    , "Bom"
    , "Chul"
    , "So-ra"
    , "Kwang-jo"
    , "Mi-reu"
    , "Ru-ri"
    , "Beo-deul"
    , "Oh-seong"
    , "Yeo-reum"
    , "Cho-rong"
    , "Jae-seop"
    , "Sae-ron"
    , "Jang-mi"
    , "Jae-gyu"
    , "Na-bi"
    , "Ma-eum"
    , "Noo-ri"
    , "Kkot"
    , "Da-rae"
    , "Dal"
    , "Sook-ja"
    , "Woo-ri"
    , "Byeol-jji"
    , "Kyung-wan"
    , "Ga-young"
    , "Mi-ran"
    , "Mi-rae"
    , "Geu-roo"
    , "Narabit"
    , "Ga-ram"
    , "Mu-yeol"
    , "Ha-neul"
    , "Kyung-taek"
    , "Haet-sal"
    , "Jin"
    , "Jae"
    , "Han-gyeol"
    , "Ja-kyung"
    , "Narauram"
    , "Han-gil"
    , "Kwang"
    , "Lin"
    , "Ha-ru"
    , "Ga-eul"
    , "Hye-rim"
    , "Sol"
    , "Nam-kyu"
    , "So-ri"
    , "Jo-eun"
    , "Mit-eum"
    , "Sae-rom"
    , "Yeo-jin"
    , "Yi-soo"
    , "Ja-ram"
    , "No-eul"
    , "Saem"
    , "Kyung-mo"
    , "Chi-won"
    , "Ah-ri"
    , "Hyun"
    , "Da-woon"
    , "Du-ri"
    , "Ha-na"
    , "Mu-young"
    , "On"
    , "Ah-reum"
    , "Na-gil"
    , "Ji-hae"
    , "Eu-tteum"
    , "Eun-bi"
    , "Gu-reum"
    , "Ha-nui"
    , "Ma-ri"
    , "Jan-di"
    , "Nam-sun"
    , "Chul-soon"
    , "Jae-beom"
    , "Haet-bit"
    , "Ran"
    , "Na-bit"
    , "Na-ri"
    , "Saet-byeol"
    , "Song-yi"
    , "Ah-ra"
    , "Ha-da"
    , "Hyuk"
    , "Mindeulle"
    , "Seung-heon"
    , "Bo-ram"
    , "Yo-han"
    , "Dam-bi"
    , "Na-young"
    , "Na-bom"
    , "Bo-ra"
    , "Bit-na"
    , "Eun-byul"
    , "Na-moo"
    , "Kyung-gu"
    , "Lee"
    , "Na-ra"
    , "Dan-bi"
    , "Yi-re"
    , "Yi-kyung"
    , "Yu-ri"
    , "Han-wool"
    , "Han-byul"
    , "Pu-reun"
    , "Ha-yan"
    ]


{-| <http://klingon.wiki/En/Names> column 2 in table 2
-}
klingonNames : List String
klingonNames =
    [ "barot"
    , "DennaS"
    , "tI'vIS"
    , "qoreQ"
    , "DuraS"
    , "qor"
    , "ghorqon"
    , "matlh"
    , "qeng"
    , "ghIrIlqa'"
    , "qarghan"
    , "tIquvma"
    , "Daghor"
    , "molor"
    , "'atrom"
    , "paq"
    , "valQIS"
    , "'ujIllI'"
    , "martaq"
    , "'or'eq"
    , "qeylIS"
    , "torgh"
    , "torghen"
    , "vIqSIS"
    , "wo'rIv"
    , "ghorqan"
    , "mo'qay"
    , "HuS"
    , "luqara'"
    , "gha'vIq"
    , "qotar"
    , "lIr'el"
    , "Qel"
    , "qo'leq"
    , "Qa'taq"
    , "Qen"
    , "qI'empeq"
    , "be'etor"
    , "wa'jo''a'"
    , "mogh"
    , "Qugh"
    , "cheng"
    , "lurSa'"
    , "SIlreq"
    , "QaS"
    , "mara"
    , "tlha'a"
    , "Qotmagh"
    , "'a'Setbur"
    , "mI'lInnID"
    , "ruq'e'vet"
    , "qol"
    , "ghawran"
    , "qolotlh"
    , "tlha'veq"
    ]


{-| <https://en.m.wikipedia.org/wiki/Afghan_name> column 1 in tables 1-2
-}
afghanNames : List String
afghanNames =
    [ "Wawrina"
    , "Khatol"
    , "Bibi"
    , "Dagar"
    , "Wais"
    , "Nazo"
    , "Beltoon"
    , "Nasrin"
    , "Gulnar"
    , "Nangial"
    , "Taban"
    , "Zarghun"
    , "Zarghuna"
    , "Parisa"
    , "Palwasha"
    , "Ziar"
    , "Anahita"
    , "Zalmay"
    , "Torpekai"
    , "Ariana"
    , "Aryana"
    , "Nurani"
    , "Khyber"
    , "Rustam"
    , "Marjan"
    , "Shinkai"
    , "Forozan"
    , "Lalzari"
    , "Kushan"
    , "Rodaba"
    , "Bakht"
    , "Malala"
    , "Malalai"
    , "Paiman"
    , "Arman"
    , "Zohal"
    , "Zargar"
    , "Zaituna"
    , "Wadana"
    , "Lmar"
    , "Shahzar"
    , "Zarmast"
    , "Freshta"
    , "Farishta"
    , "Naghma"
    , "Hila"
    , "Yama"
    , "Aurang"
    , "Toofan"
    , "Sarbaz"
    , "Zareena"
    , "Gulzar"
    , "Wazmakai"
    , "Ghatool"
    , "Yalda"
    , "Khushal"
    , "Rangeen"
    , "Aimal"
    , "Mina"
    , "Kawtara"
    , "Kontara"
    , "Shino"
    , "Spogmay"
    , "Spozhmay"
    , "Spetselai"
    , "Babak"
    , "Shanzai"
    ]


{-| <https://en.wikipedia.org/wiki/List_of_Algonquian_personal_names> all `li` within `ul`s 1-18
-}
algonquianNames : List String
algonquianNames =
    [ "Powhatan"
    , "Senachwine"
    , "Wabanquot"
    , "Opchanacanough"
    , "Pocahontas"
    , "Lappawinsoe"
    , "Uncas"
    , "Tomocomo"
    , "Metacomet"
    , "Hobomok"
    , "Shingabawossin"
    , "Nescambious"
    , "Beshekee"
    , "Ozhaguscodaywayquay"
    , "Wahunsunacock"
    , "Totopotomoi"
    , "Nemattanew"
    , "Monoco"
    , "Andaigweos"
    , "Canonchet"
    , "Corbitant"
    , "Shingas"
    , "Shingwauk"
    , "Egushawa"
    , "Tagwagane"
    , "Chanco"
    , "Massasoit"
    , "Wyandanch"
    , "Wingina"
    , "Weyapiersenwah"
    , "Wasson"
    , "Assacumet"
    , "Shaw-shaw-way-nay-beece"
    , "Canonicus"
    , "Moluntha"
    , "Oratam"
    , "Necotowance"
    , "Tecumseh"
    , "Waubonsie"
    , "Gomo"
    , "Wampage"
    , "Wonalancet"
    , "Kineubenae"
    , "Tamanend"
    , "Wanchese"
    , "Wahbanosay"
    , "Kechewaishke"
    , "Wosso"
    , "Keokuk"
    , "Passaconaway"
    , "Biauswah"
    , "Gelelemend"
    , "Aysh-ke-bah-ke-ko-zhay"
    , "Kennekuk"
    , "Watseka"
    , "Manteo"
    , "Chief Niwot"
    , "Masconomet"
    , "Mecosta"
    , "Wawatam"
    , "Menominee"
    , "Wamsutta"
    , "Petosegay"
    , "Waubojeeg"
    , "Tacumwah"
    , "Metea"
    , "Wawasee"
    , "Buckongahelas"
    , "Katonah"
    , "Iyannough"
    , "Cheeseekau"
    , "Comas"
    , "Shabbona"
    , "Papakeecha"
    , "Winamac"
    , "Senachewine"
    , "Memeskia"
    , "Taphance"
    , "Lawoughqua"
    , "Plausawa"
    , "Mamongazeda"
    , "Weetamoo"
    , "Squanto"
    , "Miantonomoh"
    , "Awashonks"
    , "Debedeavon"
    , "Mahackemo"
    , "Wequash"
    , "Epenow"
    , "Metallak"
    , "Ma-Ko-Ko-Mo"
    , "Match-E-Be-Nash-She-Wish"
    , "Wainchemahdub"
    , "Chicagou"
    , "Custaloga"
    , "Shick Shack"
    , "Weyonomon"
    , "Medweganoonind"
    , "Neaatooshing"
    , "Nahnebahwequa"
    , "Tuhbenahneequay"
    ]
