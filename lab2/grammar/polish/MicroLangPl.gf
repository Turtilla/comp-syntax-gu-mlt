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
lin animal_N = smartNoun "zwierzę" Neut ;
lin apple_N = mkN "jabłko" Neut ;
lin baby_N = mkN "dziecko" "dziecka" "dziecku" "dziecko" "dzieckiem" "dziecku" "dziecko" "dzieci" "dzieci" "dzieciom" "dzieci" "dziećmi" "dzieciach" "dzieci" Neut ;
lin bad_A = mkA "zły" ;
lin beer_N = mkN "piwo" Neut ;
lin big_A = mkA "duży" ;
lin bike_N = mkN "rower" Masc ;
lin bird_N = mkN "ptak" Masc ;
lin black_A = mkA "czarny" ;
lin blood_N = mkN "krew" Fem ;
lin blue_A = mkA "niebieski" ;
lin boat_N = mkN "łódka" Fem ;
lin book_N = mkN "książka" Fem ;
lin boy_N = mkN "chłopiec" MascAnim ;
lin bread_N = mkN "chleb" Masc ;
lin break_V2 = mkV2 (conjVerb "łamać" IX) ;
lin buy_V2 = mkV2 (conjVerb "kupować" IV) ;
lin car_N = mkN "auto" Neut ;
lin cat_N = mkN "kot" Masc ;
lin child_N = mkN "dziecko" Neut ;
lin city_N = mkN "miasto" Neut ;
lin clean_A = mkA "czysty" ;
lin clever_A = mkA "sprytny" ;
lin cloud_N = mkN "chmura" Fem ;
lin cold_A = mkA "zimny" ;
lin come_V = conjVerb "przychodzić" VIa ;
lin computer_N = mkN "komputer" Masc;
lin cow_N = mkN "krowa" Fem ;
lin dirty_A = mkA "brudny" ;
lin dog_N = mkN "pies" Masc ;
lin drink_V2 = mkV2 (conjVerb "pić" Xa) ;
lin eat_V2 = mkV2 (mkVerb "jeść" "jem" "jesz" "je" "jemy" "jecie" "jedzą") ;
lin find_V2 = mkV2 (conjVerb "znajdować" IV) ;
lin fire_N = mkN "ogień" Masc ;
lin fish_N = mkN "ryba" Fem ;
lin flower_N = mkN "kwiat" Masc ;
lin friend_N = mkN "przyjaciel" MascAnim ;
lin girl_N = mkN "dziewczyna" Fem ;
lin good_A = mkA "dobry" ;
lin go_V = mkVerb "iść" "idę" "idziesz" "idzie" "idziemy" "idziecie" "idą" ;
lin grammar_N = mkN "gramatyka" Fem ;
lin green_A = mkA "zielony" ;
lin heavy_A = mkA "ciężki" ;
lin horse_N = mkN "koń" Masc ;
lin hot_A = mkA "gorący" ;
lin house_N = mkN "dom" Masc ;
-- lin john_PN = mkPN "John" ;
lin jump_V = conjVerb "skakać" IX ;
lin kill_V2 = mkV2 (conjVerb "zabijać" I) ;
-- lin know_VS = mkVS (mkVerb "wiedzieć" "wiem" "wiesz" "wie" "wiemy" "wiecie" "wiedzą") ;
lin language_N = mkN "język" Masc ;
lin live_V = conjVerb "żyć" Xa;
lin love_V2 = mkV2 (conjVerb "kochać" I) ;
lin man_N = mkN "mężczyzna" MascAnim ;
lin milk_N = mkN "mleko" Neut ;
lin music_N = mkN "muzyka" Fem ;
lin new_A = mkA "nowy" ;
lin now_Adv = mkAdv "teraz" ;
lin old_A = mkA "stary" ;
-- lin paris_PN = mkPN "Paryż" ;
lin play_V = conjVerb "grać" I;
lin read_V2 = mkV2 (conjVerb "czytać" I) ;
lin ready_A = mkA "gotowy" ;
lin red_A = mkA "czerwony" ;
lin river_N = mkN "rzeka" Fem ;
lin run_V = conjVerb "biegać" I ;
lin sea_N = mkN "morze" "morza" "morzu" "morze" "morzem" "morzu" "morze" "morza" "mórz" "morzom" "morza" "morzami" "morzach" "morza" Neut ;
lin see_V2 = mkV2 (conjVerb "widzieć" VIIa) ;
lin ship_N = mkN "statek" Masc ;
lin sleep_V = mkVerb "spać" "śpię" "śpisz" "śpi" "śpimy" "śpicie" "śpią";
lin small_A = mkA "mały" ;
lin star_N = mkN "gwiazda" Fem ;
lin swim_V = conjVerb "pływać" I ;
lin teach_V2 = mkV2 (conjVerb "uczyć" VIb) ;
lin train_N = mkN "pociąg" Masc;
lin travel_V = conjVerb "podróżować" IV ;
lin tree_N = mkN "drzewo" Neut ;
lin understand_V2 = mkV2 (conjVerb "rozumieć" II) ;
lin wait_V2 = mkV2 (conjVerb "czekać" I) ;
lin walk_V = conjVerb "spacerować" IV ;
lin warm_A = mkA "ciepły" ;
lin water_N = mkN "woda" Fem ;
lin white_A = mkA "biały" ;
lin wine_N = mkN "wino" Neut ;
lin woman_N = mkN "kobieta" Fem ;
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
