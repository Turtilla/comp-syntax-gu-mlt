resource MicroResEng = open Prelude in {

param
  Number = Sg | Pl ;
  Case = Nom | Gen | Dat | Acc | Ins | Loc | Voc ;
  Gender = MascAnim | Masc | Fem | Neut ;
  Person = 1st | 2nd | 3rd
  Conjugation = I | II | III | IV | V | VIa | VIb | VIIa | VIIb | VIII | IX | Xa | Xb | Xc | XI


  Agreement = Agr Number Person Gender Case ; -- not sure here

  -- I guess for now just present forms are ok?
  VForm = Inf | PresSg1 | PresSg2 | PresSg3 | PresPl1 | PresPl2 | PresPl3 ; 

oper
  Noun : Type = {s : Number => Str} ;

  mkNoun : Str -> Str -> Noun = \sg,pl -> {
    s = table {Sg => sg ; Pl => pl}
    } ;

  regNoun : Str -> Noun = \sg -> mkNoun sg (sg + "s") ;

  -- smart paradigm
  smartNoun : Str -> Noun = \sg -> case sg of {
    _ + ("ay"|"ey"|"oy"|"uy") => regNoun sg ;
    x + "y"                   => mkNoun sg (x + "ies") ;
    _ + ("ch"|"sh"|"s"|"o")   => mkNoun sg (sg + "es") ;
    _                         => regNoun sg
    } ;

  Adjective : Type = {s : NGCAgreement } ;

  mkAdj : (masc,fem,neut,mascpl,nonmascpl : Str) -> Adjective
    = \masc,fem,neut,mascpl,pl -> {
    s = table {
      Masc => masc ;
      Fem => fem ;
      Neut => neut ;
      MascPl => mascpl ;
      Pl => pl ;
      }
    } ;
  
  -- only nominative; any way to streamline this?
  smartAdj : Str -> Adjective = \masc -> case masc of {
     du  +  "ży" => mkAdj masc (du + "ża") (du + "że") (du + "zi") (du + "że") ;
     wes  +  "oły" => mkAdj masc (wes + "oła") (wes + "ołe") (wes + "eli") (wes + "ołe") ;
     mi  +  "ły" => mkAdj masc (mi + "ła") (mi + "łe") (mi + "li") (mi + "łe") ;
     mło  +  "dy" => mkAdj masc (mło + "da") (mło + "de") (mło + "dzi") (mło + "de") ;
     pro  +  "sty" => mkAdj masc (pro + "sta") (pro + "ste") (pro + "ści") (pro + "ste") ;
     pracowi  +  "ty" => mkAdj masc (pracowi + "ta") (pracowi + "te") (pracowi + "ci") (pracowi + "te") ;
     star  +  "szy" => mkAdj masc (star + "sza") (star + "sze") (star + "si") (star + "sze") ;
     zmęcz  +  "ony" => mkAdj masc (zmęcz + "ona") (zmęcz + "one") (zmęcz + "eni") (zmęcz + "one") ;
     dob  +  "ry" => mkAdj masc (dob + "ra") (dob + "re") (dob + "rzy") (dob + "re") ;
     elegan  +  "cki" => mkAdj masc (elegan + "cka") (elegan + "ckie") (elegan + "ccy") (elegan + "ckie") ;
     wyso  +  "ki" => mkAdj masc (wyso + "ka") (wyso + "kie") (wyso + "cy") (wyso + "kie") ;
     ubo  +  "gi"  => mkAdj masc (ubo + "ga") (ubo + "gie") (ubo + "dzy") (ubo + "gie") ;
     pachną  +  ("cy"|"czy"|"dzy") => mkAdj mask (pachną + "ca") (pachną + "ce") masc (pachną + "ce")
     słab  +  "y" => mkAdj masc (słab + "a") (słab + "e") (słab + "i") (słab + "e") ;
     tan + "i" => mkAdj masc (tan + "ia") (tan + "ie") masc (tan + "ie") ;
     } ;

  Verb : Type = {s : VForm => Str} ;

  mkVerb : (inf,pressg1,pressg2,pressg3,prespl1,prespl2,prespl3 : Str) -> Verb
    = \inf,pressg1,pressg2,pressg3,prespl1,prespl2,prespl3 -> {
    s = table {
      Inf => inf ;
      PresSg1 => pressg1 ;
      PresSg2 => pressg2 ;
      PresSg3 => pressg3 ;
      PresPl1 => prespl1 ;
      PresPl2 => prespl2 ;
      PresPl3 => prespl3 ;
      }
    } ;

  -- determining the conjugation could be done with the infinitive and one of the inflected forms
  -- or should we have separate functions?
  conjVerb : Str -> Conjugation -> Verb = \inf, conj -> case conj of {
     I => case inf of { czyt  +  "ać" => mkVerb inf (czyt + "am") (czyt + "asz") (czyt + "a") (czyt + "amy") (czyt + "acie") (czyt + "ają")} ; -- 1st conjugation
     II => case inf of { umi  +  "eć" =>  mkVerb inf (umi + "em") (umi + "esz") (umi + "e") (umi + "emy") (umi + "ecie") (umi + "eją") } ;  -- 2nd conjugation
     III => case inf of { tani + "eć"  => mkVerb inf (tani + "eję") (tani + "ejesz") (tani + "eje") (tani + "ejemy") (tani + "ejcie") (tani + "eją") } ;  -- 3rd conjugation
     IV => case inf of { mal  +  "ować" => mkVerb inf (mal + "uję") (mal + "ujesz") (mal + "uje") (mal + "ujemy") (mal + "ujecie") (mal + "ują") } ;  -- 4th conjugation
     V => case inf of { ciąg  +  "nąć" => mkVerb inf (ciąg + "nę") (ciąg + "niesz") (ciąg + "nie") (ciąg + "niemy") (ciąg + "niecie") (ciąg + "ną") } ;  -- 5th conjugation
     VIa => case inf of { rob  +  "ić" => mkVerb inf (rob + "ię") (rob + "isz") (rob + "i") (rob + "imy") (rob + "icie") (rob + "ią") } ;  -- 6th A conjugation
     VIb => case inf of { wierz  +  "yć" => mkVerb inf (wierz + "ę") (wierz + "ysz") (wierz + "y") (wierz + "ymy") (wierz + "ycie") (wierz + "ą") } ;  -- 6th B conjugation
     VIIa => case inf of { widz  +  ("ieć"|"eć") => mkVerb inf (widz + "ę") (widz + "isz") (widz + "i") (widz + "imy") (widz + "icie") (widz + "ą") } ;  -- 7th A conjugation
     VIIb => case inf of { leż  +  ("ieć"|"eć") => mkVerb inf (leż + "ę") (leż + "ysz") (leż + "y") (leż + "ymy") (leż + "ycie") (leż + "ą") } ;  -- 7th B conjugation
     VIII => case inf of { zysk  +  ("iwać"|"ywać") => mkVerb inf (zysk + "uję") (zysk + "ujesz") (zysk + "uje") (zysk + "ujemy") (zysk + "ujecie") (zysk + "ują") } ;  -- 8th conjugation, in pres same as 4th (merge?)
     IX => case inf of { ła  +  "mać" => mkVerb inf (ła + "mię") (ła + "miesz") (ła + "mie") (ła + "miemy") (ła + "miecie") (ła + "mią") ;
                         ska  +  "kać" => mkVerb inf (ska + "czę") (ska + "czesz") (ska + "cze") (ska + "czemy") (ska + "czecie") (ska + "czą") } ;
     -- some verbs in the 9th conugation mutates the root https://pl.wiktionary.org/wiki/Aneks:J%C4%99zyk_polski_-_koniugacja_IX
     Xa => case inf of { pi  +  "ć" => mkVerb inf (pi + "ję") (pi + "jesz") (pi + "je") (pi + "jemy") (pi + "jecie") (pi + "ją") } ; -- 10th A conjugation
     Xb => case inf of { l  +  "ać" => mkVerb inf (l + "eję") (l + "ejesz") (l + "eje") (l + "ejemy") (l + "ejecie") (l + "eją") } ;  -- 10th B conjugation
     -- 10th C conjugation mutates the root too and has variation in endings https://pl.wiktionary.org/wiki/Aneks:J%C4%99zyk_polski_-_koniugacja_Xc
     -- 11th conjugation mutates the root as well https://pl.wiktionary.org/wiki/Aneks:J%C4%99zyk_polski_-_koniugacja_XI
     } ;  

  -- two-place verb with "case" as preposition; for transitive verbs, c=[]
  Verb2 : Type = Verb ** {c : Str} ;

  be_Verb : Verb = mkVerb "być" "jestem" "jesteś" "jest" "jesteście" "jesteśmy" "są" ; ---s to be generalized


---s a very simplified verb agreement function for Micro
  agr2vform : Agreement -> VForm = \a -> case a of {
    Agr Sg => PresSg3 ;
    Agr Pl => Inf
    } ;

}
