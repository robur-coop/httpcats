module Day = struct
  type t = Mon | Tue | Wed | Thu | Fri | Sat | Sun
end

module Month = struct
  type t =
    | Jan
    | Feb
    | Mar
    | Apr
    | May
    | Jun
    | Jul
    | Aug
    | Sep
    | Oct
    | Nov
    | Dec

  let to_int = function
    | Jan -> 1
    | Feb -> 2
    | Mar -> 3
    | Apr -> 4
    | May -> 5
    | Jun -> 6
    | Jul -> 7
    | Aug -> 8
    | Sep -> 9
    | Oct -> 10
    | Nov -> 11
    | Dec -> 12
end

module Zone = struct
  type t =
    | UT
    | GMT
    | EST
    | EDT
    | CST
    | CDT
    | MST
    | MDT
    | PST
    | PDT
    | Military_zone of char
    | TZ of int * int

  let is_military_zone = function
    | '\065' .. '\073' | '\075' .. '\090' | '\097' .. '\105' | '\107' .. '\122'
      ->
        true
    | _ -> false

  let to_int = function
    | UT | GMT -> (00, 00)
    | EST -> (-05, 00)
    | EDT -> (-04, 00)
    | CST -> (-06, 00)
    | CDT -> (-05, 00)
    | MST -> (-07, 00)
    | MDT -> (-06, 00)
    | PST -> (-08, 00)
    | PDT -> (-07, 00)
    | TZ (hh, mm) -> (hh, mm)
    | Military_zone c -> (
        match c with
        | 'A' -> (01, 00)
        | 'B' -> (02, 00)
        | 'C' -> (03, 00)
        | 'D' -> (04, 00)
        | 'E' -> (05, 00)
        | 'F' -> (06, 00)
        | 'G' -> (07, 00)
        | 'H' -> (08, 00)
        | 'I' -> (09, 00)
        | 'K' -> (10, 00)
        | 'L' -> (11, 00)
        | 'M' -> (12, 00)
        | 'N' -> (-01, 00)
        | 'O' -> (-02, 00)
        | 'P' -> (-03, 00)
        | 'Q' -> (-04, 00)
        | 'R' -> (-05, 00)
        | 'S' -> (-06, 00)
        | 'T' -> (-07, 00)
        | 'U' -> (-08, 00)
        | 'V' -> (-09, 00)
        | 'W' -> (-10, 00)
        | 'X' -> (-11, 00)
        | 'Y' -> (-12, 00)
        | 'Z' -> (00, 00)
        | c -> Fmt.invalid_arg "Invalid military zone %c" c)
end

module Date = struct
  type t = {
      day: Day.t option
    ; date: int * Month.t * int
    ; time: int * int * int option
    ; zone: Zone.t
  }

  open Angstrom

  let is_digit = function '0' .. '9' -> true | _ -> false
  let is_wsp = function ' ' | '\t' -> true | _ -> false

  let two_digit =
    lift2
      (fun a b ->
        let res = Bytes.create 2 in
        Bytes.unsafe_set res 0 a;
        Bytes.unsafe_set res 1 b;
        Bytes.unsafe_to_string res)
      (satisfy is_digit) (satisfy is_digit)

  let four_digit =
    lift4
      (fun a b c d ->
        let res = Bytes.create 4 in
        Bytes.unsafe_set res 0 a;
        Bytes.unsafe_set res 1 b;
        Bytes.unsafe_set res 2 c;
        Bytes.unsafe_set res 3 d;
        Bytes.unsafe_to_string res)
      (satisfy is_digit) (satisfy is_digit) (satisfy is_digit)
      (satisfy is_digit)

  let at_least_n_digit n =
    take_while1 is_digit >>= fun res ->
    if String.length res >= n then return res else fail "at_least_n_digit"

  let one_or_two_digit =
    satisfy is_digit >>= fun one ->
    peek_char >>= function
    | Some two when is_digit two ->
        let res = Bytes.create 2 in
        Bytes.unsafe_set res 0 one;
        Bytes.unsafe_set res 1 two;
        advance 1 *> return (Bytes.unsafe_to_string res)
    | _ -> return (String.make 1 one)

  (* From RFC 2822

       obs-hour        =       [CFWS] 2DIGIT [CFWS]

     From RFC 5322

       obs-hour        =   [CFWS] 2DIGIT [CFWS]
  *)
  let obs_hour =
    skip_while is_wsp *> two_digit <* skip_while is_wsp >>| int_of_string

  (* From RFC 2822

       obs-minute      =       [CFWS] 2DIGIT [CFWS]

     From RFC 5322

       obs-minute      =   [CFWS] 2DIGIT [CFWS]
  *)
  let obs_minute =
    skip_while is_wsp *> two_digit <* skip_while is_wsp >>| int_of_string

  (* From RFC 2822

       obs-second      =       [CFWS] 2DIGIT [CFWS]

     From RFC 5322

       obs-second      =   [CFWS] 2DIGIT [CFWS]
  *)
  let obs_second =
    skip_while is_wsp *> two_digit <* skip_while is_wsp >>| int_of_string

  (* From RFC 2822

       hour            =       2DIGIT / obs-hour

     From RFC 5322

       hour            =   2DIGIT / obs-hour
  *)
  let hour = obs_hour <|> (two_digit >>| int_of_string)

  (* From RFC 2822

       minute          =       2DIGIT / obs-minute

     From RFC 5322

       minute          =   2DIGIT / obs-minute
  *)
  let minute = obs_minute <|> (two_digit >>| int_of_string)

  (* From RFC 2822

       second          =       2DIGIT / obs-second

     From RFC 5322

       second          =   2DIGIT / obs-second
  *)
  let second =
    obs_second <|> (two_digit >>| int_of_string) >>= fun res ->
    option "" (char '.' *> take_while1 is_digit) >>= fun _ns -> return res
  (* XXX(dinosaure): On [Received] field, the date can have nano-second. Such
   * value does not follow any standards but we must consume it to be able to
   * parse then zone value. It's an hot-fix to be able to accept several wrong
   * [Received] fields. *)

  (* From RFC 2822

       obs-year        =       [CFWS] 2*DIGIT [CFWS]

     From RFC 5322

       obs-year        =   [CFWS] 2*DIGIT [CFWS]
  *)
  let obs_year =
    skip_while is_wsp *> at_least_n_digit 2
    <* skip_while is_wsp
    >>| int_of_string

  (* From RFC 2822

       year            =       4*DIGIT / obs-year

       Where a two or three digit year occurs in a date, the year is to be
       interpreted as follows: If a two digit year is encountered whose
       value is between 00 and 49, the year is interpreted by adding 2000,
       ending up with a value between 2000 and 2049.  If a two digit year is
       encountered with a value between 50 and 99, or any three digit year
       is encountered, the year is interpreted by adding 1900.

       Differences from Earlier Specifications
       3.   Four or more digits allowed for year.
       15.  Two digit years not allowed.*
       16.  Three digit years interpreted, but not allowed for generation.*

     From RFC 5322

       year            =   (FWS 4*DIGIT FWS) / obs-year

       Where a two or three digit year occurs in a date, the year is to be
       interpreted as follows: If a two digit year is encountered whose
       value is between 00 and 49, the year is interpreted by adding 2000,
       ending up with a value between 2000 and 2049.  If a two digit year is
       encountered with a value between 50 and 99, or any three digit year
       is encountered, the year is interpreted by adding 1900.

       Differences from Earlier Specifications
       3.   Four or more digits allowed for year.
       15.  Two digit years not allowed.*
       16.  Three digit years interpreted, but not allowed for generation.*
  *)
  let year =
    skip_while is_wsp *> at_least_n_digit 4
    <* skip_while is_wsp
    >>| int_of_string
    <|> obs_year

  (* From RFC 2822

       obs-day         =       [CFWS] 1*2DIGIT [CFWS]

     From RFC 5322

       obs-day         =   [CFWS] 1*2DIGIT [CFWS]
  *)
  let obs_day =
    skip_while is_wsp *> one_or_two_digit <* skip_while is_wsp >>| int_of_string

  (* From RFC 2822

       day             =       ([FWS] 1*2DIGIT) / obs-day

     From RFC 5322

       day             =   ([FWS] 1*2DIGIT FWS) / obs-day
  *)
  let day =
    obs_day
    <|> (skip_while is_wsp *> one_or_two_digit
        <* skip_while is_wsp
        >>| int_of_string)

  (* From RFC 822

       month       =  "Jan"  /  "Feb" /  "Mar"  /  "Apr"
                   /  "May"  /  "Jun" /  "Jul"  /  "Aug"
                   /  "Sep"  /  "Oct" /  "Nov"  /  "Dec"

     From RFC 2822

       month           =       (FWS month-name FWS) / obs-month

       month-name      =       "Jan" / "Feb" / "Mar" / "Apr" /
                               "May" / "Jun" / "Jul" / "Aug" /
                               "Sep" / "Oct" / "Nov" / "Dec"

     From RFC 5322

       month           =   "Jan" / "Feb" / "Mar" / "Apr" /
                           "May" / "Jun" / "Jul" / "Aug" /
                           "Sep" / "Oct" / "Nov" / "Dec"
  *)
  let month =
    string "Jan" *> return Month.Jan
    <|> string "Feb" *> return Month.Feb
    <|> string "Mar" *> return Month.Mar
    <|> string "Apr" *> return Month.Apr
    <|> string "May" *> return Month.May
    <|> string "Jun" *> return Month.Jun
    <|> string "Jul" *> return Month.Jul
    <|> string "Aug" *> return Month.Aug
    <|> string "Sep" *> return Month.Sep
    <|> string "Oct" *> return Month.Oct
    <|> string "Nov" *> return Month.Nov
    <|> string "Dec" *> return Month.Dec

  (* From RFC 822

       day         =  "Mon"  / "Tue" /  "Wed"  / "Thu"
                   /  "Fri"  / "Sat" /  "Sun"

     From RFC 2822

       day-name        =       "Mon" / "Tue" / "Wed" / "Thu" /
                               "Fri" / "Sat" / "Sun"

     From RFC 5322

       day-name        =   "Mon" / "Tue" / "Wed" / "Thu" /
                           "Fri" / "Sat" / "Sun"
  *)
  let day_name =
    string "Mon" *> return Day.Mon
    <|> string "Tue" *> return Day.Tue
    <|> string "Wed" *> return Day.Wed
    <|> string "Thu" *> return Day.Thu
    <|> string "Fri" *> return Day.Fri
    <|> string "Sat" *> return Day.Sat
    <|> string "Sun" *> return Day.Sun

  (* From RFC 2822

       obs-day-of-week =       [CFWS] day-name [CFWS]

     From RFC 5322

       obs-day-of-week =   [CFWS] day-name [CFWS]
  *)
  let obs_day_of_week = skip_while is_wsp *> day_name <* skip_while is_wsp

  (* From RFC 2822

       day-of-week     =       ([FWS] day-name) / obs-day-of-week

     From RFC 5322

       day-of-week     =   ([FWS] day-name) / obs-day-of-week
  *)
  let day_of_week = obs_day_of_week <|> skip_while is_wsp *> day_name

  (* From RFC 822

       date        =  1*2DIGIT month 2DIGIT        ; day month year
                                                   ;  e.g. 20 Jun 82

     From RFC 2822

       date            =       day month year

     From RFC 5322

       date            =   day month year
  *)
  let date =
    lift3
      (fun day month year -> (day, month, year))
      (day <?> "day") (month <?> "month") (year <?> "year")

  (* From RFC 822

       hour        =  2DIGIT ":" 2DIGIT [":" 2DIGIT]

     From RFC 2822

       time-of-day     =       hour ":" minute [ ":" second ]

     From RFC 5322

       time-of-day     =   hour ":" minute [ ":" second ]
  *)
  let time_of_day =
    hour <?> "hour" >>= fun hour ->
    char ':' *> minute <?> "minute" >>= fun minute ->
    option None
      ( skip_while is_wsp *> char ':' *> second <?> "second" >>| fun second ->
        Some second )
    >>| fun second -> (hour, minute, second)

  (* From RFC 822

       zone        =  "UT"  / "GMT"                ; Universal Time
                                                   ; North American : UT
                   /  "EST" / "EDT"                ;  Eastern:  - 5/ - 4
                   /  "CST" / "CDT"                ;  Central:  - 6/ - 5
                   /  "MST" / "MDT"                ;  Mountain: - 7/ - 6
                   /  "PST" / "PDT"                ;  Pacific:  - 8/ - 7
                   /  1ALPHA                       ; Military: Z = UT;
                                                   ;  A:-1; (J not used)
                                                   ;  M:-12; N:+1; Y:+12
                   / ( ("+" / "-") 4DIGIT )        ; Local differential
                                                   ;  hours+min. (HHMM)

       Time zone may be indicated in several ways.  "UT" is Univer-
       sal  Time  (formerly called "Greenwich Mean Time"); "GMT" is per-
       mitted as a reference to Universal Time.  The  military  standard
       uses  a  single  character for each zone.  "Z" is Universal Time.
       "A" indicates one hour earlier, and "M" indicates 12  hours  ear-
       lier;  "N"  is  one  hour  later, and "Y" is 12 hours later.  The
       letter "J" is not used.  The other remaining two forms are  taken
       from ANSI standard X3.51-1975.  One allows explicit indication of
       the amount of offset from UT; the other uses  common  3-character
       strings for indicating time zones in North America.

     From RFC 2822

       obs-zone        =       "UT" / "GMT" /          ; Universal Time
                               "EST" / "EDT" /         ; Eastern:  - 5/ - 4
                               "CST" / "CDT" /         ; Central:  - 6/ - 5
                               "MST" / "MDT" /         ; Mountain: - 7/ - 6
                               "PST" / "PDT" /         ; Pacific:  - 8/ - 7
                                                       ;
                               %d65-73 /               ; Military zones - "A"
                               %d75-90 /               ; through "I" and "K"
                               %d97-105 /              ; through "Z", both
                               %d107-122               ; upper and lower case

       In the obsolete time zone, "UT" and "GMT" are indications of
       "Universal Time" and "Greenwich Mean Time" respectively and are both
       semantically identical to "+0000".

       The remaining three character zones are the US time zones.  The first
       letter, "E", "C", "M", or "P" stands for "Eastern", "Central",
       "Mountain" and "Pacific".  The second letter is either "S" for
       "Standard" time, or "D" for "Daylight" (or summer) time.  Their
       interpretations are as follows:

       EDT is semantically equivalent to -0400
       EST is semantically equivalent to -0500
       CDT is semantically equivalent to -0500
       CST is semantically equivalent to -0600
       MDT is semantically equivalent to -0600
       MST is semantically equivalent to -0700
       PDT is semantically equivalent to -0700
       PST is semantically equivalent to -0800

       The 1 character military time zones were defined in a non-standard
       way in [RFC822] and are therefore unpredictable in their meaning.
       The original definitions of the military zones "A" through "I" are
       equivalent to "+0100" through "+0900" respectively; "K", "L", and "M"
       are equivalent to  "+1000", "+1100", and "+1200" respectively; "N"
       through "Y" are equivalent to "-0100" through "-1200" respectively;
       and "Z" is equivalent to "+0000".  However, because of the error in
       [RFC822], they SHOULD all be considered equivalent to "-0000" unless
       there is out-of-band information confirming their meaning.

       Other multi-character (usually between 3 and 5) alphabetic time zones
       have been used in Internet messages.  Any such time zone whose
       meaning is not known SHOULD be considered equivalent to "-0000"
       unless there is out-of-band information confirming their meaning.

     From RFC 5322

       obs-zone        =   "UT" / "GMT" /     ; Universal Time
                                              ; North American UT
                                              ; offsets
                           "EST" / "EDT" /    ; Eastern:  - 5/ - 4
                           "CST" / "CDT" /    ; Central:  - 6/ - 5
                           "MST" / "MDT" /    ; Mountain: - 7/ - 6
                           "PST" / "PDT" /    ; Pacific:  - 8/ - 7
                                              ;
                           %d65-73 /          ; Military zones - "A"
                           %d75-90 /          ; through "I" and "K"
                           %d97-105 /         ; through "Z", both
                           %d107-122          ; upper and lower case

       Where a two or three digit year occurs in a date, the year is to be
       interpreted as follows: If a two digit year is encountered whose
       value is between 00 and 49, the year is interpreted by adding 2000,
       ending up with a value between 2000 and 2049.  If a two digit year is
       encountered with a value between 50 and 99, or any three digit year
       is encountered, the year is interpreted by adding 1900.

       In the obsolete time zone, "UT" and "GMT" are indications of
       "Universal Time" and "Greenwich Mean Time", respectively, and are
       both semantically identical to "+0000".

       The remaining three character zones are the US time zones.  The first
       letter, "E", "C", "M", or "P" stands for "Eastern", "Central",
       "Mountain", and "Pacific".  The second letter is either "S" for
       "Standard" time, or "D" for "Daylight Savings" (or summer) time.
       Their interpretations are as follows:

          EDT is semantically equivalent to -0400
          EST is semantically equivalent to -0500
          CDT is semantically equivalent to -0500
          CST is semantically equivalent to -0600
          MDT is semantically equivalent to -0600
          MST is semantically equivalent to -0700
          PDT is semantically equivalent to -0700
          PST is semantically equivalent to -0800

         The 1 character military time zones were defined in a non-standard
         way in [RFC0822] and are therefore unpredictable in their meaning.
         The original definitions of the military zones "A" through "I" are
         equivalent to "+0100" through "+0900", respectively; "K", "L", and
         "M" are equivalent to "+1000", "+1100", and "+1200", respectively;
         "N" through "Y" are equivalent to "-0100" through "-1200".
         respectively; and "Z" is equivalent to "+0000".  However, because of
         the error in [RFC0822], they SHOULD all be considered equivalent to
         "-0000" unless there is out-of-band information confirming their
         meaning.

         Other multi-character (usually between 3 and 5) alphabetic time zones
         have been used in Internet messages.  Any such time zone whose
         meaning is not known SHOULD be considered equivalent to "-0000"
         unless there is out-of-band information confirming their meaning.
  *)
  let obs_zone =
    string "UT" *> return Zone.UT
    <|> string "GMT" *> return Zone.GMT
    <|> string "EST" *> return Zone.EST
    <|> string "EDT" *> return Zone.EDT
    <|> string "CST" *> return Zone.CST
    <|> string "CDT" *> return Zone.CDT
    <|> string "MST" *> return Zone.MST
    <|> string "MDT" *> return Zone.MDT
    <|> string "PST" *> return Zone.PST
    <|> string "PDT" *> return Zone.PDT
    <|> ( satisfy Zone.is_military_zone >>= fun z ->
          return (Zone.Military_zone z) )

  (* From RFC 2822

       zone            =       (( "+" / "-" ) 4DIGIT) / obs-zone

       The zone specifies the offset from Coordinated Universal Time (UTC,
       formerly referred to as "Greenwich Mean Time") that the date and
       time-of-day represent.  The "+" or "-" indicates whether the
       time-of-day is ahead of (i.e., east of) or behind (i.e., west of)
       Universal Time.  The first two digits indicate the number of hours
       difference from Universal Time, and the last two digits indicate the
       number of minutes difference from Universal Time.  (Hence, +hhmm
       means +(hh * 60 + mm) minutes, and -hhmm means -(hh * 60 + mm)
       minutes).  The form "+0000" SHOULD be used to indicate a time zone at
       Universal Time.  Though "-0000" also indicates Universal Time, it is
       used to indicate that the time was generated on a system that may be
       in a local time zone other than Universal Time and therefore
       indicates that the date-time contains no information about the local
       time zone.

     From RFC 5322

       zone            =   (FWS ( "+" / "-" ) 4DIGIT) / obs-zone

       The zone specifies the offset from Coordinated Universal Time (UTC,
       formerly referred to as "Greenwich Mean Time") that the date and
       time-of-day represent.  The "+" or "-" indicates whether the time-of-
       day is ahead of (i.e., east of) or behind (i.e., west of) Universal
       Time.  The first two digits indicate the number of hours difference
       from Universal Time, and the last two digits indicate the number of
       additional minutes difference from Universal Time.  (Hence, +hhmm
       means +(hh * 60 + mm) minutes, and -hhmm means -(hh * 60 + mm)
       minutes).  The form "+0000" SHOULD be used to indicate a time zone at
       Universal Time.  Though "-0000" also indicates Universal Time, it is
       used to indicate that the time was generated on a system that may be
       in a local time zone other than Universal Time and that the date-time
       contains no information about the local time zone.
  *)
  let zone =
    (* XXX(dinosaure): we clearly have a bug in this place. Indeed, ABNF expects
       an explicit space between [zone] and [time_of_day]. However, if we see
       [second] or [minute], they are surrounded by [CFWS]. That mean, they
       consume trailing spaces. If we explicitly expect [FWS] here, we will fail -
       mostly because this expected space is a part of [minute] or [second]. To
       avoid an error, [FWS] is optional but a better way should to check if we
       consumed at least one space before [zone]. *)
    skip_while is_wsp *> satisfy (function '+' | '-' -> true | _ -> false)
    <?> "sign"
    >>= (fun sign ->
    four_digit <?> "four-digit" >>| fun zone ->
    let one =
      if sign = '-' then -int_of_string (String.sub zone 0 2)
      else int_of_string (String.sub zone 0 2)
    in
    let two = int_of_string (String.sub zone 2 2) in
    Zone.TZ (one, two))
    <|> skip_while is_wsp *> obs_zone

  (* From RFC 822

       time        =  hour zone                    ; ANSI and Military

     From RFC 2822

       time            =       time-of-day FWS zone

     From RFC 5322

       time            =   time-of-day zone
  *)
  let time =
    lift2
      (fun time zone -> (time, zone))
      (time_of_day <?> "time-of-day")
      (zone <?> "zone")

  (* From RFC 822

       date-time   =  [ day "," ] date time        ; dd mm yy
                                                   ;  hh:mm:ss zzz

     From RFC 2822

       Date and time occur in several header fields.  This section specifies
       the syntax for a full date and time specification.  Though folding
       white space is permitted throughout the date-time specification, it
       is RECOMMENDED that a single space be used in each place that FWS
       appears (whether it is required or optional); some older
       implementations may not interpret other occurrences of folding white
       space correctly.

       date-time       =       [ day-of-week "," ] date FWS time [CFWS]

     From RFC 5322

       Date and time values occur in several header fields.  This section
       specifies the syntax for a full date and time specification.  Though
       folding white space is permitted throughout the date-time
       specification, it is RECOMMENDED that a single space be used in each
       place that FWS appears (whether it is required or optional); some
       older implementations will not interpret longer sequences of folding
       white space correctly.

       date-time       =   [ day-of-week "," ] date time [CFWS]
  *)
  let date_time =
    lift3
      (fun day date (time, zone) -> { day; date; time; zone })
      (option None (day_of_week >>= fun day -> char ',' *> return (Some day)))
      date time
    <* skip_while is_wsp
end

let parse_cookie str =
  match String.split_on_char '=' str with
  | [ key; value ] ->
      if String.starts_with ~prefix:"__Host-" key then
        Some (key, value, Some `Host)
      else if String.starts_with ~prefix:"__Secure-" key then
        Some (key, value, Some `Secure)
      else Some (key, value, None)
  | _ -> None

type parameter =
  | SameSite of [ `Lax | `Strict | `None ]
  | Path of string
  | Domain of string
  | HttpOnly
  | Secure
  | Expires of Ptime.t
  | MaxAge of int

type cookie = {
    key: string
  ; value: string
  ; prefix: [ `Host | `Secure ] option
  ; parameters: parameter list
}

let parse_rfc822_date str =
  match Angstrom.parse_string ~consume:All Date.date_time str with
  | Error _ -> None
  | Ok date ->
      let z =
        let hh, mm = Zone.to_int date.zone in
        (hh * 3600) + (mm * 60)
      in
      let m =
        let _, m, _ = date.date in
        Month.to_int m
      in
      let d, _, y = date.date in
      let hh, mm, ss = date.time in
      let ss = Option.value ~default:0 ss in
      Ptime.of_date_time ((y, m, d), ((hh, mm, ss), z))

let parse_same_site str =
  match String.lowercase_ascii str with
  | "lax" -> Some `Lax
  | "strict" -> Some `Strict
  | "none" -> Some `None
  | _ -> None

let parse_parameter str =
  let ( let* ) = Option.bind in
  let tokens = String.split_on_char '=' str in
  let tokens = String.lowercase_ascii (List.hd tokens) :: List.tl tokens in
  match tokens with
  | [ "httponly" ] -> Some HttpOnly
  | [ "secure" ] -> Some Secure
  | [ "samesite"; str ] ->
      let* v = parse_same_site str in
      Some (SameSite v)
  | [ "path"; path ] -> Some (Path path)
  | [ "domain"; domain ] -> Some (Domain domain)
  | [ "expires"; str ] ->
      let* v = parse_rfc822_date str in
      Some (Expires v)
  | [ "max-age"; n ] -> begin
      try
        let n = int_of_string n in
        Some (MaxAge n)
      with _ -> None
    end
  | _ -> None

let parse str =
  let ( let* ) = Option.bind in
  match String.split_on_char ';' str with
  | [ cookie ] ->
      let* key, value, prefix = parse_cookie cookie in
      Some { key; value; prefix; parameters= [] }
  | cookie :: parameters ->
      let* key, value, prefix = parse_cookie cookie in
      let parameters = List.filter_map parse_parameter parameters in
      Some { key; value; prefix; parameters }
  | _ -> None
