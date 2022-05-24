--# -path=.:../abstract
concrete MicroLangPl of MicroLang = open MicroResPl, Prelude in {

-----------------------------------------------------
---------------- Grammar part -----------------------
-----------------------------------------------------

  lincat
    Utt = {s : Str} ;
    
    S  = {s : Str} ;
    VP = {verb : Verb ; compl : Str} ; ---s special case of Mini
    Comp = {s : Str} ;
    AP = Adjective ; --{s : Gender => Number => Case => Str}
    CN = Noun ; --{s : Number => Case => Str ; g : Gender}
    NP = {s : Case => Str ; a : GNAgreement} ; -- gender, number agreement
    Pron = {s : Case => Str ; a : GNAgreement} ;
    Det = {s : Gender => Case => Str ; n : Number} ;
    Prep = {s : Str} ;
    V = Verb ; --{s : VForm => Str}
    V2 = Verb2 ;  --Verb ** {c : Str}
    A = Adjective ; --{s : Gender => Number => Case => Str}
    N = Noun ;  --{s : Number => Case => Str ; g : Gender}
    Adv = {s : Str} ;

  lin
  -- Phrase
--    UttS s = s ;
--    UttNP np = {s = np.s ! Acc} ;  -- why ! Acc here? 

  -- Sentence
--    PredVPS np vp = variants {
--      s = np.s ! Nom ++ vp.verb.s ! agr2vform np.a ++ vp.compl
      -- add more options, use ; 
--      } ;  --use free variation with | or variants {}

  -- Verb  
    UseV v = {
      verb = v ;
      compl = [] ;
      } ;
      
    ComplV2 v2 np = {
      verb = v2 ;
      compl = v2.c ++ np.s ! Acc  -- NP object in the accusative, preposition first
      } ;
      
    UseComp comp = {
      verb = be_Verb ;     -- the verb is the copula "be"
      compl = comp.s
      } ;
      
--    CompAP ap = ap ;
      
    AdvVP vp adv =
      vp ** {compl = vp.compl ++ adv.s} ;
  
  -- Noun
--    DetCN det cn = {
--      s = \\c => det.s ++ cn.s ! det.n ;
--      a = Agr det.n ;
--      } ;
      
    UsePron p = p ;

    --how to make it dependent on gender? same as in the other file?    
    --these are also entirely optional and are more of determiners than articles. Should I keep them?   
    a_Det = variants {} ; --- a/an can get wrong
    aPl_Det = variants {} ;
    the_Det = {s = table {
      MascAnim => table {
                Nom => "ten" ;
                Gen => "tego" ;
                Dat => "temu" ;
                Acc => "tego" ;
                Ins => "tym" ;
                Loc => "tym" ;
                Voc => "ten"
      } ;
      Masc => table {
                Nom => "ten" ;
                Gen => "tego" ;
                Dat => "temu" ;
                Acc => "ten" ;
                Ins => "tym" ;
                Loc => "tym" ;
                Voc => "ten"
      } ;
      Fem => table {
                Nom => "ta" ;
                Gen => "tej" ;
                Dat => "tej" ;
                Acc => "tę" ;
                Ins => "tą" ;
                Loc => "tej" ;
                Voc => "ta"
      } ;
      Neut => table {
                Nom => "to" ;
                Gen => "tego" ;
                Dat => "temu" ;
                Acc => "to" ;
                Ins => "tym" ;
                Loc => "tym" ;
                Voc => "to"
      }
    } ; n = Sg } ;

    thePl_Det = {s = table {
      --here only the MascAnim one is different
      MascAnim => table {
                Nom => "ci" ;
                Gen => "tych" ;
                Dat => "tym" ;
                Acc => "tych" ;
                Ins => "tymi" ;
                Loc => "tych" ;
                Voc => "ci"
      } ;
      Masc => table {
                Nom => "te" ;
                Gen => "tych" ;
                Dat => "tym" ;
                Acc => "te" ;
                Ins => "tymi" ;
                Loc => "tych" ;
                Voc => "te"
      } ;
      Fem => table {
                Nom => "te" ;
                Gen => "tych" ;
                Dat => "tym" ;
                Acc => "te" ;
                Ins => "tymi" ;
                Loc => "tych" ;
                Voc => "te"
      } ;
      Neut => table {
                Nom => "te" ;
                Gen => "tych" ;
                Dat => "tym" ;
                Acc => "te" ;
                Ins => "tymi" ;
                Loc => "tych" ;
                Voc => "te"
      }
    } ; n = Pl} ;
    
    UseN n = n ;
    
    AdjCN ap cn = {
      s = table {n => table {
                c => ap.s ! cn.g ! n ! c ++ cn.s ! n ! c
       }} ; g = cn.g  -- need to change, need {s : Number => Case => Str ; g : Gender}
      } ;

  -- Adjective
    PositA a = a ;

  -- Adverb 
--    PrepNP prep np = \prep,np -> case prep of {
--      ("w"|"we"|"na") => s = prep.s ++ np.s ! Loc ;
--      ("z"|"ze") => s = prep.s ++ np.s ! Ins
--      } ; 

  -- Structural
    --variants depending on the first consonant of the noun added
    in_Prep = {s = pre {"w" => "we" ; _ => "w"}} ; --locative
    on_Prep = {s = "na"} ; --locative
    with_Prep = {s = pre {"z" => "ze" ; _ => "z"}} ; --instrumental

    --for the majority of pronouns there are alternative versions (e.g. for masculine accusative singular
    --there is "jego", "go", "niego", "-ń"); these have a relatively predictable distribution, but in some
    --contexts they are interchangeable. I think implementing all of them is beyond the scope of this assignment.
    --The "n" forms appear after prepositions. Short forms (without "je", wherever applicable) can only appear
    --in unstressed positions (which means what in the SVO sentences they are likely to appear in that form).
    --"-ń" is a clitic of some prepositions, and only works for the masculine.
    
    he_Pron = {
      s = table {
                Nom => "on" ;
                Gen => "jego" ;
                Dat => "mu" ; --"jemu" is also acceptable and even preferred in certain contexts (when fronted for emphasis).
                Acc => "go" ; --"jego" is treated the same way as above.
                Ins => "nim" ;
                Loc => "nim" ;
                Voc => "on"
                } ;
      a = GNAgr MascAnim Sg ;
      } ;
    she_Pron = {
      s = table {
                Nom => "ona" ;
                Gen => "jej" ;
                Dat => "jej" ; 
                Acc => "ją" ; 
                Ins => "nią" ;
                Loc => "niej" ;
                Voc => "ona"
                } ;
      a = GNAgr Fem Sg ;
      } ;
    they_Pron = {
      s = table {
                Nom => "oni" ;
                Gen => "ich" ;
                Dat => "im" ; 
                Acc => "ich" ;
                Ins => "nimi" ;
                Loc => "nich" ;
                Voc => "oni"
                } ;
      a = GNAgr MascAnim Pl ;
      } ;
      --this one is used for plurals where no masculine human noun is a part of the group denoted by the pronoun; since this distinction seems
      --to be outside the scope of this assignment, I just commented this option out. This is only really relevant in actual discourse when we
      --know what the pronoun refers to, and in sentences with past verb forms, where they have to align in gender. Adjectives also have to do
      --that, but that is more straightforward
      --they_nonmasc_Pron = {
      --s = table {
                --Nom => "one" ;
                --Gen => "ich" ;
                --Dat => "im" ; 
                --Acc => "je" ;
                --Ins => "nimi" ;
                --Loc => "nich" ;
                --Voc => "one"
                --} ;
      --a = Agr Pl ;
      --} ;

-----------------------------------------------------
---------------- Lexicon part -----------------------
-----------------------------------------------------

--lin already_Adv = mkAdv "już" ;
lin animal_N = mkN "zwierzę" "zwierzęta" Neut ;
lin apple_N = mkN "jabłko" "jabłka" Neut ;
lin baby_N = mkN "dziecko" "dziecka" "dziecku" "dziecko" "dzieckiem" "dziecku" "dziecko" "dzieci" "dzieci" "dzieciom" "dzieci" "dziećmi" "dzieciach" "dzieci" Neut ;
lin bad_A = mkA "zły" ;
lin beer_N = mkN "piwo" "piwa" Neut ;
lin big_A = mkA "duży" ;
lin bike_N = mkN "rower" "rowery" Masc ;
lin bird_N = mkN "ptak" "ptaki" Masc ;
lin black_A = mkA "czarny" ;
lin blood_N = mkN "krew" "krwi" Fem ;
lin blue_A = mkA "niebieski" ;
lin boat_N = mkN "łódka" "łódki" Fem ;
lin book_N = mkN "książka" "książki" Fem ;
lin boy_N = mkN "chłopiec" "chłopcy" MascAnim ;
lin bread_N = mkN "chleb" "chleby" Masc ;
--lin break_V2 = mkV2 (conjVerb "łamać" IX) ;
--lin buy_V2 = mkV2 (conjVerb "kupować" IV) ;
lin car_N = mkN "auto" "auta" Neut ;
lin cat_N = mkN "kot" "koty" Masc ;
lin child_N = mkN "dziecko" "dziecka" "dziecku" "dziecko" "dzieckiem" "dziecku" "dziecko" "dzieci" "dzieci" "dzieciom" "dzieci" "dziećmi" "dzieciach" "dzieci" Neut ;
lin city_N = mkN "miasto" "miasta" Neut ;
lin clean_A = mkA "czysty" ;
lin clever_A = mkA "sprytny" ;
lin cloud_N = mkN "chmura" "chmury" Fem ;
lin cold_A = mkA "zimny" ;
lin come_V = conjVerb "przychodzić" VIa ;
lin computer_N = mkN "komputer" "komputery" Masc;
lin cow_N = mkN "krowa" "krowy" Fem ;
lin dirty_A = mkA "brudny" ;
lin dog_N = mkN "pies" "psy" Masc ;
--lin drink_V2 = mkV2 (conjVerb "pić" Xa) ;
--lin eat_V2 = mkV2 (mkVerb "jeść" "jem" "jesz" "je" "jemy" "jecie" "jedzą") ;
--lin find_V2 = mkV2 (conjVerb "znajdować" IV) ;
lin fire_N = mkN "ogień" "ognie" Masc ;
lin fish_N = mkN "ryba" "ryby" Fem ;
lin flower_N = mkN "kwiat" "kwiaty" Masc ;
lin friend_N = mkN "przyjaciel" "przyjaciele" MascAnim ;
lin girl_N = mkN "dziewczyna" "dziewczyny" Fem ;
lin good_A = mkA "dobry" ;
lin go_V = mkVerb "iść" "idę" "idziesz" "idzie" "idziemy" "idziecie" "idą" ;
lin grammar_N = mkN "gramatyka" "gramatyki" Fem ;
lin green_A = mkA "zielony" ;
lin heavy_A = mkA "ciężki" ;
lin horse_N = mkN "koń" "konie" Masc ;
lin hot_A = mkA "gorący" ;
lin house_N = mkN "dom" "domy" Masc ;
--- lin john_PN = mkPN "John" ;
lin jump_V = conjVerb "skakać" IX ;
-- lin kill_V2 = mkV2 (conjVerb "zabijać" I) ;
--- lin know_VS = mkVS (mkVerb "wiedzieć" "wiem" "wiesz" "wie" "wiemy" "wiecie" "wiedzą") ;
lin language_N = mkN "język" "języki" Masc ;
lin live_V = conjVerb "żyć" Xa;
-- lin love_V2 = mkV2 (conjVerb "kochać" I) ;
lin man_N = mkN "mężczyzna" "mężczyźni" MascAnim ;
lin milk_N = mkN "mleko" "mleka" Neut ;
lin music_N = mkN "muzyka" "muzyki" Fem ;
lin new_A = mkA "nowy" ;
-- lin now_Adv = mkAdv "teraz" ;
lin old_A = mkA "stary" ;
-- lin paris_PN = mkPN "Paryż" ;
lin play_V = conjVerb "grać" I;
-- lin read_V2 = mkV2 (conjVerb "czytać" I) ;
lin ready_A = mkA "gotowy" ;
lin red_A = mkA "czerwony" ;
lin river_N = mkN "rzeka" "rzeki" Fem ;
lin run_V = conjVerb "biegać" I ;
lin sea_N = mkN "morze" "morza" "morzu" "morze" "morzem" "morzu" "morze" "morza" "mórz" "morzom" "morza" "morzami" "morzach" "morza" Neut ;
-- lin see_V2 = mkV2 (conjVerb "widzieć" VIIa) ;
lin ship_N = mkN "statek" "statki" Masc ;
lin sleep_V = mkVerb "spać" "śpię" "śpisz" "śpi" "śpimy" "śpicie" "śpią";
lin small_A = mkA "mały" ;
lin star_N = mkN "gwiazda" "gwiazdy" Fem ;
lin swim_V = conjVerb "pływać" I ;
-- lin teach_V2 = mkV2 (conjVerb "uczyć" VIb) ;
lin train_N = mkN "pociąg" "pociągi" Masc;
lin travel_V = conjVerb "podróżować" IV ;
lin tree_N = mkN "drzewo" "drzewa" Neut ;
-- lin understand_V2 = mkV2 (conjVerb "rozumieć" II) ;
-- lin wait_V2 = mkV2 (conjVerb "czekać" I) ;
lin walk_V = conjVerb "spacerować" IV ;
lin warm_A = mkA "ciepły" ;
lin water_N = mkN "woda" "wody" Fem ;
lin white_A = mkA "biały" ;
lin wine_N = mkN "wino" "wina" Neut ;
lin woman_N = mkN "kobieta" "kobiety" Fem ;
lin yellow_A = mkA "żółty" ;
lin young_A = mkA "młody" ;

---------------------------
-- Paradigms part ---------
---------------------------

oper
  mkN = overload {
    mkN :  Str -> Str -> Gender -> Noun   -- predictable nouns, e.g. jajo-jaja, kobieta-kobiety, byk-byki, król-królowie
      = \noun,plural,g -> lin N (smartNoun noun plural g) ;
    mkN : Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Gender -> Noun  -- irregular nouns, e.g. człowiek-ludzie
      = \sgnom, sggen, sgdat, sgacc, sgins, sgloc, sgvoc, plnom, plgen, pldat, placc, plins, plloc, plvoc, g -> 
      lin N (mkNoun sgnom sggen sgdat sgacc sgins sgloc sgvoc plnom plgen pldat placc plins plloc plvoc g) ;
    } ;

  mkA : Str -> A
    = \masc -> lin A (smartAdj masc) ;

--  mkV = overload {
--    mkV : Str -> Conjugation -> Verb  -- predictable verb, e.g. czytać-czytam-czytasz-etc.
--      = \inf, conj -> lin V (conjVerb inf conj) ;
--    mkV : (inf,pressg1,pressg2,pressg3,prespl1,prespl2,prespl3 : Str) -> V  -- irregular verb, e.g. iść-idę-idziesz-etc.
--      = \inf,pressg1,pressg2,pressg3,prespl1,prespl2,prespl3 -> lin V (mkVerb inf pressg1 pressg2 pressg3 prespl1 prespl2 prespl3) ;
--    } ;

--These cannot just take strings since constructing a verb requires the conjugation
--  mkV2 = overload {
--    mkV2 : V -> V2            -- any verb with direct object, e.g. "drink"
--      = \v   -> lin V2 (v ** {c = []}) ;
--    mkV2 : V -> Str -> V2     -- any verb with preposition
--      = \v,p -> lin V2 (v ** {c = p}) ;
--    } ;
--
--  mkAdv : Str -> Adv
--    = \s -> lin Adv {s = s} ;
  
--  mkPrep : Str -> Prep
--    = \s -> lin Prep {s = s} ;

}
