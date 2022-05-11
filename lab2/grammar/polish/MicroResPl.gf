resource MicroResPl = open Prelude in {

param
  VForm = Inf | Pres NPAgreement ;
  Number = Sg | Pl ;
  Case = Nom | Gen | Dat | Acc | Ins | Loc | Voc ;
  Gender = MascAnim | Masc | Fem | Neut ;
  Person = First | Second | Third ;
  Conjugation = I | II | III | IV | V | VIa | VIb | VIIa | VIIb | VIII | IX | Xa | Xb ; -- | Xc | XI not implemented because of irregularities

  NPAgreement = NPAgr Number Person ;
  -- not sure here

oper
  Noun : Type = {s : Number => Case => Str ; g : Gender} ;

  mkNoun : Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Gender -> Noun 
        = \sgnom, sggen, sgdat, sgacc, sgins, sgloc, sgvoc, plnom, plgen, pldat, placc, plins, plloc, plvoc, g -> {
        s = table {
            Sg => table {
                Nom => sgnom ;
                Gen => sggen ;
                Dat => sgdat ;
                Acc => sgacc ;
                Ins => sgins ;
                Loc => sgloc ;
                Voc => sgvoc
            } ;
            Pl => table {
                Nom => plnom ;
                Gen => plgen ;
                Dat => pldat ;
                Acc => placc ;
                Ins => plins ;
                Loc => plloc ;
                Voc => plvoc 
            }
            } ;
            g = g
        } ;

  smartNoun : Str -> Gender -> Noun = \noun,g -> case g of {
            Neut => case noun of { 
              ---most neuter nouns that end with "o", like "jajo" or "mleko"; does not account for the changes to the root in some plutal genitives ("okno" - "okien") or for the relatively irregular "dziecko"
              jaj + "o" => mkNoun noun (jaj + "a") (jaj + "u") noun (jaj + "iem") (jaj + "u") noun (jaj + "a") x (jaj + "om") (jaj + "a") (jaj + "ami") (jaj + "ach") (jaj + "a") g ;
              --foreign neuter nouns that end with "um", where all the singular forms are the same, e.g. "muzeum", "memorandum".
              muze + "um" => mkNoun noun noun noun noun noun noun noun (muze + "a") (muze + "ów") (muze + "om") (muze + "a")(muze + "ami") (muze + "ach") (muze + "a") g ;
              --neuter nouns ending with "ę" that mostly denote the young of some species (e.g. "cielę" - calf, "prosię" - piglet).
              zwierz + "ę" => mkNoun noun (zwierz + "ęcia") (zwierz + "ęciu") noun (zwierz + "ęciem") (zwierz + "ęciu") noun (zwierz + "ęta") (zwierz + "ąt") (zwierz + "ętom") (zwierz + "ęta") (zwierz + "ętami") (zwierz + "ętach") (zwierz + "ęta") g ;
              --neuter verb-derived nouns describing actions, e.g. "jedzenie" - food/eating, "siedzenie" - seat/sitting, "myślenie" - thinking.
              jedze + "nie" => mkNoun noun (jedze + "nia") (jedze + "niu") noun (jedze + "niem") (jedze + "niu") noun (jedze + "nia") (jedze + "ń") (jedze + "niom") (jedze + "nia") (jedze + "niami") (jedze + "niach") (jedze + "nia") g 
              } ;
            Fem => case noun of {
              --feminine nouns ending in "ew" - the plurals for "krew" (blood) are not really used and the ones here are modelled after "brew" (eyebrow), and don't fully fit the dictionary entry for "krew", but sound acceptable to me as a native speaker.
              kr + "ew" => mkNoun noun (kr + "wi") (kr + "wi") noun (kr + "wią") (kr + "wi") (kr + "wi") (kr + "wi") (kr + "wi") (kr + "wiom") (kr + "wi") (kr + "wiami") (kr + "wiach") (kr + "wi") g ;
              --feminine nouns ending with "a" with a velar stem.
              wal + "ka" => mkNoun noun (wal + "ki") (wal + "ce") (wal + "kę") (wal + "ką") (wal + "ce") (wal + "ko") (wal + "ki") (wal + "k") (wal + "kom") (wal + "ki") (wal + "kami") (wal + "kach") (wal + "ki") g ;
              dro + "ga" => mkNoun noun (dro + "gi") (dro + "dze") (dro + "gę") (dro + "gą") (dro + "dze") (dro + "go") (dro + "gi") (dro + "g") (dro + "gom") (dro + "gi") (dro + "gami") (dro + "gach") (dro + "gi") g ;
              ce + "cha" => mkNoun noun (ce + "chy") (ce + "sze") (ce + "chę") (ce + "chą") (ce + "sze") (ce + "cho") (ce + "chy") (ce + "ch") (ce + "chom") (ce + "chy") (ce + "chami") (ce + "chach") (ce + "chy") g ;         
              wata + "ha" => mkNoun noun (wata + "hy") (wata + "sze") (wata + "hę") (wata + "hą") (wata + "sze") (wata + "ho") (wata + "hy") (wata + "h") (wata + "hom") (wata + "hy") (wata + "hami") (wata + "hach") (wata + "hy") g ;
              --feminine nouns ending with "a" with a historically soft stem.
              pra + ("ca"|"dza"|"sza"|"ża"|"rza"|"cza"|"dża") => case noun of {prac + "a" => mkNoun noun (prac + "y") (prac + "y") (prac + "ę") (prac + "ą") (prac + "y") (prac + "o") (prac + "e") prac (prac + "om") (prac + "e") (prac + "ami") (prac + "ach") (prac + "e") g } ;
              --feminine nouns ending with "a" with a soft stem; does not account for some animate nouns' vocatives in singular ending with "u" instead of "o".
              a + ("sia"|"cia"|"zia"|"dzia"|"nia"|"la"|"bia"|"mia"|"wia"|"pia"|"ja"|"ia") => case noun of {asi + "a" => mkNoun noun asi asi (asi + "ę") (asi + "ą") asi (asi + "o") (asi + "e") asi (asi + "om") (asi + "e") (asi + "ami") (asi + "ach") (asi + "e") g } ;
              --feminine nouns ending with "a" with a hard stem.
              la + ("ba"|"pa"|"fa"|"wa"|"ma"|"ła"|"ta"|"da"|"sa"|"za"|"na") => case noun of {lab + "a" => mkNoun noun (lab + "ę") (lab + "y") (lab + "ie") (lab + "ie") (lab + "ą") (lab + "o") (lab + "y") (lab + "y") lab (lab + "om") (lab + "ach") (lab + "ami") (lab + "y") g } ;
              kobie + "ta" => mkNoun noun (kobie + "ty") (kobie + "cie") (kobie + "tę") (kobie + "tą") (kobie + "cie") (kobie + "to") (kobie + "ty") (kobie + "t") (kobie + "tom") (kobie + "ty") (kobie + "tami") (kobie + "tach") (kobie + "ty") g ;
              szko + "ła" => mkNoun noun (szko + "ły") (szko + "le") (szko + "łę") (szko + "łą") (szko + "le") (szko + "ło") (szko + "ły") (szko + "ł") (szko + "łom") (szko + "ły") (szko + "łami") (szko + "łach") (szko + "ły") g ;
              ope + "ra" => mkNoun noun (ope + "ry") (ope + "rze") (ope + "rę") (ope + "rą") (ope + "rze") (ope + "ro") (ope + "ry") (ope + "r") (ope + "rom") (ope + "ry") (ope + "rami") (ope + "rach") (ope + "ry") g ;
              --feminine nouns ending with "a" with a vowel stem.
              id + "ea" => case noun of { ide + "a" => mkNoun noun (ide + "i") (ide + "i") (ide + "ę") (ide + "ą") (ide + "i") (ide + "o") (ide + "e") (ide + "i") (ide + "om") (ide + "e") (ide + "ami") (ide + "ach") (ide + "e") g } ;
              stat + "ua" => case noun of { statu + "a" => mkNoun noun (stau + "y") (stau + "i") (stau + "ę") (stau + "ą") (stau + "i") (stau + "o") (stau + "y") (stau + "i") (stau + "om") (stau + "y") (stau + "ami") (stau + "ach") (stau + "y") g } ;
              --feminine nouns ending with "i".
              mistrzy + "ni" => mkNoun noun noun noun (mistrzy + "nię") (mistrzy + "nią") noun noun (mistrzy + "nie") (mistrzy + "ń") (mistrzy + "niom") (mistrzy + "nie") (mistrzy + "niami") (mistrzy + "niach") (mistrzy + "nie") g ;
              --feminine nouns ending with a historically soft consonant.
              podro + ("c"|"dz"|"sz"|"ż"|"rz"|"cz"|"dż") => mkNoun noun (noun + "y") (noun + "y") noun (noun + "ą") (noun + "y") (noun + "y") (noun + "e") (noun + "y") (noun + "om") (noun + "e") (noun + "ami") (noun + "ach") (noun + "e") g ;
              --feminine nouns ending with a soft consonant.
              milo + "ść" => mkNoun noun (milo + "ści") (milo + "ści") noun (milo + "ścią") (milo + "ści") (milo + "ści") (milo + "ści") (milo + "ści") (milo + "ściom") (milo + "ści") (milo + "ściami") (milo + "ściach") (milo + "ści") g ;  
              ja + "źń" => mkNoun noun (ja + "źni") (ja + "źni") noun (ja + "źnią") (ja + "źni") (ja + "źni") (ja + "źnie") (ja + "źni") (ja + "źniom") (ja + "źnie") (ja + "źniami") (ja + "źniach") (ja + "źnie") g 
              } ;
              --remember mixed pattern!
            Masc => x + _ => mkNoun noun (noun + "a") (noun + "owi") noun (noun + "em") (noun + "u") (noun + "u") (noun + "y") (noun + "ów") (noun + "om") (noun + "y") (noun + "ami") (noun + "ach") (noun + "y") g      
            MascAnim => 
        } ;

  Adjective : Type = {s : Gender => Number => Case => Str} ;

  mkAdj : (root, plmasc : Str) -> Adjective
    = \root,plmasc -> {
    s = table {
        MascAnim => table {
          Sg => table {
            Nom => root + "y" ;
            Gen => root + "ego" ;
            Dat => root + "emu" ;
            Acc => root + "ego" ; -- "y" for nonanimate
            Ins => root + "ym" ;
            Loc => root + "ym" ;
            Voc => root + "y" 
          } ;
          Pl => table {
            Nom => plmasc ;
            Gen => root + "ych" ;
            Dat => root + "ym" ;
            Acc => root + "ych" ;
            Ins => root + "ymi" ;
            Loc => root + "ych" ;
            Voc => plmasc
          }
        } ;
        Masc => table {
          Sg => table {
            Nom => root + "y" ;
            Gen => root + "ego" ;
            Dat => root + "emu" ;
            Acc => root + "y" ; 
            Ins => root + "ym" ;
            Loc => root + "ym" ;
            Voc => root + "y" 
          } ;
          Pl => table {
            Nom => root + "e" ;
            Gen => root + "ych" ;
            Dat => root + "ym" ;
            Acc => root + "e" ;
            Ins => root + "ymi" ;
            Loc => root + "ych" ;
            Voc => root + "e"
          }
        } ;
        Fem => table {
          Sg => table {
            Nom => root + "a" ;
            Gen => root + "ej" ;
            Dat => root + "ej" ;
            Acc => root + "ą" ;
            Ins => root + "ą" ;
            Loc => root + "ej" ;
            Voc => root + "a"
          } ;
          Pl => table {
            Nom => root + "e" ;
            Gen => root + "ych" ;
            Dat => root + "ym" ;
            Acc => root + "e" ;
            Ins => root + "ymi" ;
            Loc => root + "ych" ;
            Voc => root + "e"
          }
        } ;
        Neut => table {
          Sg => table {
            Nom => root + "e" ;
            Gen => root + "ego" ;
            Dat => root + "emu" ;
            Acc => root + "e" ;
            Ins => root + "ym" ;
            Loc => root + "ym" ;
            Voc => root + "e"
          } ;
          Pl => table {
            Nom => root + "e" ;
            Gen => root + "ych" ;
            Dat => root + "ym" ;
            Acc => root + "e" ;
            Ins => root + "ymi" ;
            Loc => root + "ych" ;
            Voc => root + "e"
          }
        } 
      } 
    } ;
  
  -- only nominative; any way to streamline this?
  smartAdj : Str -> Adjective = \masc -> case masc of {
     du  +  "ży" => mkAdj (du + "ż") (du + "zi") ;
     wes  +  "oły" => mkAdj (wes + "oł") (wes + "eli") ;
     mi  +  "ły" => mkAdj (mi + "ł") (mi + "li") ;
     mlo  +  "dy" => mkAdj (mlo + "d") (mlo + "dzi") ;
     pro  +  "sty" => mkAdj (pro + "st") (pro + "ści") ;
     pracowi  +  "ty" => mkAdj (pracowi + "t") (pracowi + "ci") ;
     star  +  "szy" => mkAdj (star + "sz") (star + "si") ;
     zmecz  +  "ony" => mkAdj (zmecz + "on") (zmecz + "eni") ;
     dob  +  "ry" => mkAdj (dob + "r") (dob + "rzy") ;
     elegan  +  "cki" => mkAdj (elegan + "ck") (elegan + "ccy") ;
     wyso  +  "ki" => mkAdj (wyso + "k") (wyso + "cy") ;
     ubo  +  "gi"  => mkAdj (ubo + "g") (ubo + "dzy") ;
     pachna  +  "cy"  => mkAdj (pachna + "c") (pachna + "cy") ;
     proro  +  "czy"  => mkAdj (proro + "cz") (proro + "czy") ;
     cu  +  "dzy"  => mkAdj (cu + "dz") (cu + "dzy") ;
     slab  +  "y" => mkAdj slab (slab + "i") ;
     tan + "i" => mkAdj (tan + "i") (tan + "i") 
     } ;

  Verb : Type = {s : VForm => Str} ;

  mkVerb : (inf,pressg1,pressg2,pressg3,prespl1,prespl2,prespl3 : Str) -> Verb
    = \inf,pressg1,pressg2,pressg3,prespl1,prespl2,prespl3 -> {
    s = table {
      Inf => inf ; -- wil it work?
      Pres (NPAgr Sg First) => pressg1 ;
      Pres (NPAgr Sg Second) => pressg2 ;
      Pres (NPAgr Sg Third) => pressg3 ;
      Pres (NPAgr Pl First) => prespl1 ;
      Pres (NPAgr Pl Second) => prespl2 ;
      Pres (NPAgr Pl Third) => prespl3 
      }
    } ;

  -- determining the conjugation could be done with the infinitive and one of the inflected forms
  -- or should we have separate functions?
  conjVerb : Str -> Conjugation -> Verb = \inf, conj -> case conj of {
     I => case inf of { czyt  +  "ać" => mkVerb inf (czyt + "am") (czyt + "asz") (czyt + "a") (czyt + "amy") (czyt + "acie") (czyt + "ają")} ; -- 1st conjugation
     II => case inf of { umi  +  "eć" =>  mkVerb inf (umi + "em") (umi + "esz") (umi + "e") (umi + "emy") (umi + "ecie") (umi + "eją") } ;  -- 2nd conjugation
     III => case inf of { tani + "eć"  => mkVerb inf (tani + "eję") (tani + "ejesz") (tani + "eje") (tani + "ejemy") (tani + "ejcie") (tani + "eją") } ;  -- 3rd conjugation
     IV => case inf of { mal  +  "ować" => mkVerb inf (mal + "uję") (mal + "ujesz") (mal + "uje") (mal + "ujemy") (mal + "ujecie") (mal + "ują") } ;  -- 4th conjugation
     V => case inf of { ciag  +  "nąć" => mkVerb inf (ciag + "nę") (ciag + "niesz") (ciag + "nie") (ciag + "niemy") (ciag + "niecie") (ciag + "ną") } ;  -- 5th conjugation
     VIa => case inf of { rob  +  "ić" => mkVerb inf (rob + "ię") (rob + "isz") (rob + "i") (rob + "imy") (rob + "icie") (rob + "ią") } ;  -- 6th A conjugation
     VIb => case inf of { wierz  +  "yć" => mkVerb inf (wierz + "ę") (wierz + "ysz") (wierz + "y") (wierz + "ymy") (wierz + "ycie") (wierz + "ą") } ;  -- 6th B conjugation
     VIIa => case inf of { widz  +  ("ieć"|"eć") => mkVerb inf (widz + "ę") (widz + "isz") (widz + "i") (widz + "imy") (widz + "icie") (widz + "ą") } ;  -- 7th A conjugation
     VIIb => case inf of { lez  +  ("ieć"|"eć") => mkVerb inf (lez + "ę") (lez + "ysz") (lez + "y") (lez + "ymy") (lez + "ycie") (lez + "ą") } ;  -- 7th B conjugation
     VIII => case inf of { zysk  +  ("iwać"|"ywać") => mkVerb inf (zysk + "uję") (zysk + "ujesz") (zysk + "uje") (zysk + "ujemy") (zysk + "ujecie") (zysk + "ują") } ;  -- 8th conjugation, in pres same as 4th (merge?)
     IX => case inf of { la  +  "mać" => mkVerb inf (la + "mię") (la + "miesz") (la + "mie") (la + "miemy") (la + "miecie") (la + "mią") ;
                         ska  +  "kać" => mkVerb inf (ska + "czę") (ska + "czesz") (ska + "cze") (ska + "czemy") (ska + "czecie") (ska + "czą") } ;
     -- some verbs in the 9th conugation mutates the root https://pl.wiktionary.org/wiki/Aneks:J%C4%99zyk_polski_-_koniugacja_IX
     Xa => case inf of { pi  +  "ć" => mkVerb inf (pi + "ję") (pi + "jesz") (pi + "je") (pi + "jemy") (pi + "jecie") (pi + "ją") } ; -- 10th A conjugation
     Xb => case inf of { l  +  "ać" => mkVerb inf (l + "eję") (l + "ejesz") (l + "eje") (l + "ejemy") (l + "ejecie") (l + "eją") }  -- 10th B conjugation
     -- 10th C conjugation mutates the root too and has variation in endings https://pl.wiktionary.org/wiki/Aneks:J%C4%99zyk_polski_-_koniugacja_Xc
     -- 11th conjugation mutates the root as well https://pl.wiktionary.org/wiki/Aneks:J%C4%99zyk_polski_-_koniugacja_XI
     } ;  

  -- two-place verb with "case" as preposition; for transitive verbs, c=[]
  Verb2 : Type = Verb ** {c : Str} ;

  be_Verb : Verb = mkVerb "być" "jestem" "jesteś" "jest" "jesteście" "jesteśmy" "są" ; ---s to be generalized


---s a very simplified verb agreement function for Micro
---removed for now

}
