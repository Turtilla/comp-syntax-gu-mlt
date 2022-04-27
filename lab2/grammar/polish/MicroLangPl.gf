--# -path=.:../abstract
concrete MicroLangEng of MicroLang = open MicroResEng, Prelude in {

-----------------------------------------------------
---------------- Grammar part -----------------------
-----------------------------------------------------

  lincat
    Utt = {s : Str} ;
    
    S  = {s : Str} ;
    VP = {verb : Verb ; compl : Str} ; ---s special case of Mini
    Comp = {s : Str} ;
    AP = Adjective ;
    CN = Noun ;
    NP = {s : Case => Str ; a : Agreement} ;
    Pron = {s : Case => Str ; a : Agreement} ;
    Det = {s : Str ; n : Number} ;
    Prep = {s : Str} ;
    V = Verb ;
    V2 = Verb2 ;
    A = Adjective ;
    N = Noun ;
    Adv = {s : Str} ;

  lin
    UttS s = s ;
    UttNP np = {s = np.s ! Acc} ;

    PredVPS np vp = {
      s = np.s ! Nom ++ vp.verb.s ! agr2vform np.a ++ vp.compl
      } ;
      
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
      
    CompAP ap = ap ;
      
    AdvVP vp adv =
      vp ** {compl = vp.compl ++ adv.s} ;
      
    DetCN det cn = {
      s = \\c => det.s ++ cn.s ! det.n ;
      a = Agr det.n ;
      } ;
      
    UsePron p = p ;
            
    a_Det = {s = pre {"a"|"e"|"i"|"o" => "an" ; _ => "a"} ; n = Sg} ; --- a/an can get wrong
    aPl_Det = {s = "" ; n = Pl} ;
    the_Det = {s = "the" ; n = Sg} ;
    thePl_Det = {s = "the" ; n = Pl} ;
    
    UseN n = n ;
    
    AdjCN ap cn = {
      s = table {n => ap.s ++ cn.s ! n}
      } ;

    PositA a = a ;

    PrepNP prep np = {s = prep.s ++ np.s ! Acc} ;

    in_Prep = {s = "in"} ;
    on_Prep = {s = "on"} ;
    with_Prep = {s = "with"} ;

    he_Pron = {
      s = table {Nom => "he" ; Acc => "him"} ;
      a = Agr Sg ;
      } ;
    she_Pron = {
      s = table {Nom => "she" ; Acc => "her"} ;
      a = Agr Sg ;
      } ;
    they_Pron = {
      s = table {Nom => "they" ; Acc => "them"} ;
      a = Agr Pl ;
      } ;

-----------------------------------------------------
---------------- Lexicon part -----------------------
-----------------------------------------------------

lin already_Adv = mkAdv "już" ;
lin animal_N = mkN "zwierzę" ;
lin apple_N = mkN "jabłko" ;
lin baby_N = mkN "dziecko" ;
lin bad_A = mkA "zły" ;
lin beer_N = mkN "piwo" ;
lin big_A = mkA "duży" ;
lin bike_N = mkN "rower" ;
lin bird_N = mkN "ptak" ;
lin black_A = mkA "czarny" ;
lin blood_N = mkN "krew" ;
lin blue_A = mkA "niebieski" ;
lin boat_N = mkN "łódka" ;
lin book_N = mkN "książka" ;
lin boy_N = mkN "chłopiec" ;
lin bread_N = mkN "chleb" ;
lin break_V2 = mkV2 (mkV "łamać") ;
lin buy_V2 = mkV2 (mkV "kupować") ;
lin car_N = mkN "auto" ;
lin cat_N = mkN "kot" ;
lin child_N = mkN "dziecko" ;
lin city_N = mkN "miasto" ;
lin clean_A = mkA "czysty" ;
lin clever_A = mkA "sprytny" ;
lin cloud_N = mkN "chmura" ;
lin cold_A = mkA "zimny" ;
lin come_V = mkV "przychodzić" ;
lin computer_N = mkN "komputer" ;
lin cow_N = mkN "krowa" ;
lin dirty_A = mkA "brudny" ;
lin dog_N = mkN "pies" ;
lin drink_V2 = mkV2 (mkV "pić") ;
lin eat_V2 = mkV2 (mkV "jeść") ;
lin find_V2 = mkV2 (mkV "znajdować") ;
lin fire_N = mkN "ogień" ;
lin fish_N = mkN "ryba" ;
lin flower_N = mkN "kwiat" ;
lin friend_N = mkN "przyjaciel" ;
lin girl_N = mkN "dziewczyna" ;
lin good_A = mkA "dobry" ;
lin go_V = mkV "iść" ;
lin grammar_N = mkN "gramatyka" ;
lin green_A = mkA "zielony" ;
lin heavy_A = mkA "ciężki" ;
lin horse_N = mkN "koń" ;
lin hot_A = mkA "gorący" ;
lin house_N = mkN "dom" ;
-- lin john_PN = mkPN "John" ;
lin jump_V = mkV "skakać" ;
lin kill_V2 = mkV2 "zabijać" ;
-- lin know_VS = mkVS (mkV "wiedzieć") ;
lin language_N = mkN "język" ;
lin live_V = mkV "żyć" ;
lin love_V2 = mkV2 (mkV "kochać") ;
lin man_N = mkN "człowiek" "ludzie" ;
lin milk_N = mkN "mleko" ;
lin music_N = mkN "muzyka" ;
lin new_A = mkA "nowy" ;
lin now_Adv = mkAdv "teraz" ;
lin old_A = mkA "stary" ;
-- lin paris_PN = mkPN "Paryż" ;
lin play_V = mkV "grać" ;
lin read_V2 = mkV2 (mkV "czytać") ;
lin ready_A = mkA "gotowy" ;
lin red_A = mkA "czerwony" ;
lin river_N = mkN "rzeka" ;
lin run_V = mkV "biegać";
lin sea_N = mkN "morze" ;
lin see_V2 = mkV2 (mkV "widzieć") ;
lin ship_N = mkN "statek" ;
lin sleep_V = mkV "spać" ;
lin small_A = mkA "mały" ;
lin star_N = mkN "gwiazda" ;
lin swim_V = mkV "pływać" ;
lin teach_V2 = mkV2 (mkV "uczyć") ;
lin train_N = mkN "trenować" ;
lin travel_V = mkV "podróżować" ;
lin tree_N = mkN "drzewo" ;
lin understand_V2 = mkV2 (mkV "rozumieć") ;
lin wait_V2 = mkV2 "czekać" ;
lin walk_V = mkV "spacerować" ;
lin warm_A = mkA "ciepły" ;
lin water_N = mkN "woda" ;
lin white_A = mkA "biały" ;
lin wine_N = mkN "wino" ;
lin woman_N = mkN "kobieta" ;
lin yellow_A = mkA "żółty" ;
lin young_A = mkA "młody" ;

---------------------------
-- Paradigms part ---------
---------------------------

oper
  mkN = overload {
    mkN : Str -> Noun   -- predictable noun, e.g. car-cars, boy-boys, fly-flies, bush-bushes
      = \n -> lin N (smartNoun n) ;
    mkN : Str -> Str -> Noun  -- irregular noun, e.g. man-men
      = \sg,pl -> lin N (mkNoun sg pl) ;
    } ;

  mkA : Str -> A
    = \s -> lin A {s = s} ;

  mkV = overload {
    mkV : (inf : Str) -> V  -- predictable verb, e.g. play-plays, cry-cries, wash-washes
      = \s -> lin V (smartVerb s) ;
    mkV : (inf,pres,part : Str) -> V  -- irregular verb, e.g. drink-drank-drunk
      = \inf,pres,part -> lin V (irregVerb inf pres part) ;
    } ;

  mkV2 = overload {
    mkV2 : Str -> V2          -- predictable verb with direct object, e.g. "wash"
      = \s   -> lin V2 (smartVerb s ** {c = []}) ;
    mkV2 : Str  -> Str -> V2  -- predictable verb with preposition, e.g. "wait - for"
      = \s,p -> lin V2 (smartVerb s ** {c = p}) ;
    mkV2 : V -> V2            -- any verb with direct object, e.g. "drink"
      = \v   -> lin V2 (v ** {c = []}) ;
    mkV2 : V -> Str -> V2     -- any verb with preposition
      = \v,p -> lin V2 (v ** {c = p}) ;
    } ;

  mkAdv : Str -> Adv
    = \s -> lin Adv {s = s} ;
  
  mkPrep : Str -> Prep
    = \s -> lin Prep {s = s} ;

}
