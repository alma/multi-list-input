module ListType exposing (ListType(..), fromString, placeholder, separators, tagId, validator)

import Regex


defaultSeparators : List String
defaultSeparators =
    [ "\n", "\t", " ", "," ]


type ListType
    = Tags
    | Emails
    | IPS
    | CardFilters
    | Other


tagId : ListType -> String
tagId list_type =
    case list_type of
        Tags ->
            "tags-input"

        Emails ->
            "emails-input"

        IPS ->
            "ip-addresses-input"

        CardFilters ->
            "card-filters-input"

        Other ->
            "items-input"


fromString : String -> ListType
fromString list_type =
    case list_type of
        "tags" ->
            Tags

        "emails" ->
            Emails

        "ips" ->
            IPS

        "card_filters" ->
            CardFilters

        _ ->
            Other


placeholder : ListType -> String
placeholder listType =
    case listType of
        Tags ->
            "Ajouter des tags"

        Emails ->
            "Ajouter des adresses email"

        IPS ->
            "Ajouter des adresses IP"

        CardFilters ->
            "Ajouter vos filtres"

        Other ->
            "Ajouter votre liste"


separators : ListType -> List String
separators listType =
    case listType of
        CardFilters ->
            [ "\n", "\t" ]

        _ ->
            defaultSeparators


validator : ListType -> (String -> Bool)
validator list_type =
    case list_type of
        Tags ->
            matches "^[a-z0-9:_]+$"

        Emails ->
            matches "^\\w+([\\.\\+]?[\\w\\-])*@[a-z0-9]+(\\.?[-a-z0-9]+)*\\.[a-z]+$"

        IPS ->
            \ip -> matches ipv6_regex ip || matches ipv4_regex ip

        CardFilters ->
            \filter -> matches card_filters_regex filter

        Other ->
            matches "^[a-zA-Z0-9]+$"


matches : String -> String -> Bool
matches regex =
    let
        validRegex =
            Regex.fromString regex
                |> Maybe.withDefault Regex.never
    in
    Regex.findAtMost 1 validRegex >> List.isEmpty >> not


ipv4_regex : String
ipv4_regex =
    "^((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])$"


ipv6_regex : String
ipv6_regex =
    -- https://gist.github.com/syzdek/6086792
    "^([0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(:[0-9a-fA-F]{1,4}){2,2}%[0-9a-zA-Z]{1,}|::(ffff(:0{1,4}){0,1}:){0,1}((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|([0-9a-fA-F]{1,4}:){1,4}:((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])$"


card_filters_regex : String
card_filters_regex =
    "^("
        ++ "issuer:%?[^%]+%?|"
        ++ "card_type:[a-zA-Z / ()-]+|card_type:%[a-zA-Z / ()-]+%|"
        ++ "brand:(visa|mastercard|american express)|"
        ++ "country:[A-Z]{2}|"
        ++ "funding:(DEBIT|CREDIT|PREPAID|UNKNOWN)|"
        ++ "iin:[0-9]{6}"
        ++ ")$"
